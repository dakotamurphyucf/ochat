open Core

type budget =
  { mutable remaining_entries : int
  ; mutable remaining_bytes : int
  ; parent : budget option
  ; depth : int
  }

type authority =
  | Host of Eio_unix.Stdenv.base
  | Borrowed of Eio.Fs.dir_ty Eio.Path.t

type t =
  { authority : authority
  ; root : string
  ; budget : budget
  }

let corrupt message = Error (Store_error.Corrupt message)

let with_checkpoint ~checkpoint ~f =
  let module Callback = struct
    exception Raised of exn * Stdlib.Printexc.raw_backtrace
  end
  in
  let poll () =
    Eio.Fiber.check ();
    (try checkpoint () with
     | exn ->
       let backtrace = Stdlib.Printexc.get_raw_backtrace () in
       raise (Callback.Raised (exn, backtrace)));
    Eio.Fiber.check ()
  in
  try f poll with
  | Callback.Raised (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;

let eio_path t path =
  match t.authority with
  | Host env -> Eio.Path.(Eio.Stdenv.fs env / path)
  | Borrowed root ->
    if String.equal path t.root
    then root
    else (
      let relative = String.chop_prefix_exn path ~prefix:(t.root ^ "/") in
      Eio.Path.(root / relative))
;;

let create ~env ~root ~max_entries ~max_bytes =
  let root = String.rstrip root ~drop:(Char.equal '/') in
  match Filename.is_absolute root && max_entries >= 0 && max_bytes >= 0 with
  | false -> corrupt "invalid retention scan root or limits"
  | true ->
    Ok
      { authority = Host env
      ; root
      ; budget =
          { remaining_entries = max_entries
          ; remaining_bytes = max_bytes
          ; parent = None
          ; depth = 0
          }
      }
;;

let create_at ~root ~max_entries ~max_bytes =
  match Eio.Path.native root with
  | None -> corrupt "bounded retention reader requires a native directory capability"
  | Some native ->
    let native = String.rstrip native ~drop:(Char.equal '/') in
    if String.is_empty native || max_entries < 0 || max_bytes < 0
    then corrupt "invalid borrowed retention root or limits"
    else
      Ok
        { authority = Borrowed root
        ; root = native
        ; budget =
            { remaining_entries = max_entries
            ; remaining_bytes = max_bytes
            ; parent = None
            ; depth = 0
            }
        }
;;

let root t = t.root

let uses_filesystem t ~env =
  match t.authority with
  | Host owner ->
    let owner_fs, owner_prefix = Eio.Stdenv.fs owner in
    let candidate_fs, candidate_prefix = Eio.Stdenv.fs env in
    phys_equal owner_fs candidate_fs && String.equal owner_prefix candidate_prefix
  | Borrowed _ -> false
;;

let rec budget_remaining_bytes budget =
  match budget.parent with
  | None -> budget.remaining_bytes
  | Some parent -> Int.min budget.remaining_bytes (budget_remaining_bytes parent)
;;

let remaining_bytes t = budget_remaining_bytes t.budget

let with_local_limits t ~max_entries ~max_bytes =
  if max_entries < 0 || max_bytes < 0 || t.budget.depth >= 8
  then corrupt "invalid nested retention limits"
  else
    Ok
      { t with
        budget =
          { remaining_entries = max_entries
          ; remaining_bytes = max_bytes
          ; parent = Some t.budget
          ; depth = t.budget.depth + 1
          }
      }
;;

let rec can_charge_bytes budget bytes =
  bytes <= budget.remaining_bytes
  && Option.for_all budget.parent ~f:(fun parent -> can_charge_bytes parent bytes)
;;

let rec debit_bytes budget bytes =
  budget.remaining_bytes <- budget.remaining_bytes - bytes;
  Option.iter budget.parent ~f:(fun parent -> debit_bytes parent bytes)
;;

let charge_bytes t bytes =
  match bytes >= 0 && can_charge_bytes t.budget bytes with
  | false -> corrupt "retention scan exceeded its byte budget"
  | true ->
    debit_bytes t.budget bytes;
    Ok ()
;;

let at_root t ~root =
  let root = String.rstrip root ~drop:(Char.equal '/') in
  match t.authority with
  | Borrowed _ ->
    if String.equal root t.root
    then Ok t
    else corrupt "borrowed retention reader cannot acquire another root"
  | Host _ ->
    if Filename.is_absolute root
    then Ok { t with root }
    else corrupt "invalid retention scan root"
;;

let rec can_charge_entry budget =
  budget.remaining_entries > 0 && Option.for_all budget.parent ~f:can_charge_entry
;;

let rec debit_entry budget =
  budget.remaining_entries <- budget.remaining_entries - 1;
  Option.iter budget.parent ~f:debit_entry
;;

let charge_entry t =
  match can_charge_entry t.budget with
  | false -> corrupt "retention scan exceeded its entry budget"
  | true ->
    debit_entry t.budget;
    Ok ()
;;

let resolve t relative ~directory =
  let open Result.Let_syntax in
  let parts = if String.equal relative "." then [] else String.split relative ~on:'/' in
  let%bind () =
    match
      Filename.is_relative relative
      && List.for_all parts ~f:(fun part ->
        not (List.mem [ ""; "."; ".." ] part ~equal:String.equal))
    with
    | true -> Ok ()
    | false -> corrupt "retention scan requires a normalized relative path"
  in
  let check_directory path =
    match Eio.Path.kind ~follow:false (eio_path t path) with
    | `Directory -> Ok ()
    | _ -> corrupt "retention scan encountered a missing or linked directory"
  in
  let%bind () = check_directory t.root in
  let rec walk path = function
    | [] -> Ok path
    | [ name ] when not directory -> Ok (Filename.concat path name)
    | name :: rest ->
      let path = Filename.concat path name in
      let%bind () = check_directory path in
      walk path rest
  in
  walk t.root parts
;;

let kind t ~path =
  let open Result.Let_syntax in
  try
    let%bind native_path = resolve t path ~directory:false in
    let%bind () = charge_entry t in
    match Eio.Path.kind ~follow:false (eio_path t native_path) with
    | `Directory -> Ok `Directory
    | `Regular_file -> Ok `File
    | _ -> corrupt "retention root is missing, linked or unsupported"
  with
  | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
  | exn -> Error (Store_error.of_exn ~operation:"inspect retention root" ~path exn)
;;

let list t ~directory =
  let open Result.Let_syntax in
  try
    let%bind path = resolve t directory ~directory:true in
    let%bind () = charge_entry t in
    let native_path = Eio.Path.native_exn (eio_path t path) in
    (* Eio.read_dir materializes the whole directory. A native directory stream
       lets this bounded scan stop immediately on excess entries. *)
    Eio_unix.run_in_systhread (fun () ->
      let handle = Core_unix.opendir native_path in
      Exn.protect
        ~finally:(fun () -> Core_unix.closedir handle)
        ~f:(fun () ->
          let rec loop names =
            match Core_unix.readdir_opt handle with
            | None -> Ok (List.sort names ~compare:String.compare)
            | Some ("." | "..") -> loop names
            | Some name ->
              let%bind () = charge_entry t in
              loop (name :: names)
          in
          loop []))
  with
  | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
  | exn ->
    Error (Store_error.of_exn ~operation:"enumerate retention roots" ~path:directory exn)
;;

let read_with_checkpoint t ~path:relative ~max_bytes ~checkpoint =
  with_checkpoint ~checkpoint ~f:(fun checkpoint ->
    let open Result.Let_syntax in
    try
      let%bind path = resolve t relative ~directory:false in
      let%bind () = charge_entry t in
      let file = eio_path t path in
      let%bind () =
        match Eio.Path.kind ~follow:false file with
        | `Regular_file -> Ok ()
        | _ -> corrupt "retention root is not a regular file"
      in
      let size = (Eio.Path.stat ~follow:false file).size |> Optint.Int63.to_int64 in
      let limit = Int.min max_bytes (remaining_bytes t) in
      let%bind () =
        match limit >= 0 && Int64.(size >= zero && size <= of_int limit) with
        | true -> Ok ()
        | false -> corrupt "retention root exceeds its byte budget"
      in
      Eio.Path.with_open_in file (fun input ->
        let buffer = Buffer.create (Int.min 8192 limit) in
        let chunk = Cstruct.create 8192 in
        let rec loop () =
          checkpoint ();
          let remaining = limit - Buffer.length buffer in
          let count =
            try
              Eio.Flow.single_read
                input
                (Cstruct.sub chunk 0 (if remaining >= 8192 then 8192 else remaining + 1))
            with
            | End_of_file -> 0
          in
          match count with
          | 0 ->
            (match Int64.equal size (Int64.of_int (Buffer.length buffer)) with
             | true -> Ok (Buffer.contents buffer)
             | false -> corrupt "retention root changed length during the scan")
          | count when count > remaining ->
            corrupt "retention root grew beyond its byte budget"
          | count ->
            let%bind () = charge_bytes t count in
            Buffer.add_string buffer (Cstruct.to_string (Cstruct.sub chunk 0 count));
            loop ()
        in
        loop ())
    with
    | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
      Error (Store_error.of_exn ~operation:"read retention root" ~path:relative exn))
;;

let read t ~path ~max_bytes =
  try read_with_checkpoint t ~path ~max_bytes ~checkpoint:(fun () -> ()) with
  | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
  | exn -> Error (Store_error.of_exn ~operation:"read retention root" ~path exn)
;;

let list_with_checkpoint t ~directory ~checkpoint =
  with_checkpoint ~checkpoint ~f:(fun poll ->
    let open Result.Let_syntax in
    try
      poll ();
      let%bind path = resolve t directory ~directory:true in
      let%bind () = charge_entry t in
      let native_path = Eio.Path.native_exn (eio_path t path) in
      let handle = Eio_unix.run_in_systhread (fun () -> Core_unix.opendir native_path) in
      Exn.protect
        ~finally:(fun () ->
          Eio.Cancel.protect (fun () ->
            Eio_unix.run_in_systhread (fun () -> Core_unix.closedir handle)))
        ~f:(fun () ->
          let rec loop names =
            poll ();
            let batch =
              Eio_unix.run_in_systhread (fun () ->
                let rec next remaining batch =
                  if remaining = 0
                  then Ok (`More batch)
                  else (
                    match Core_unix.readdir_opt handle with
                    | None -> Ok (`Done batch)
                    | Some ("." | "..") -> next (remaining - 1) batch
                    | Some name ->
                      let%bind () = charge_entry t in
                      next (remaining - 1) (name :: batch))
                in
                next 64 [])
            in
            let%bind batch = batch in
            poll ();
            match batch with
            | `More batch -> loop (List.rev_append batch names)
            | `Done batch ->
              let names = List.rev_append batch names in
              Ok
                (List.sort names ~compare:(fun a b ->
                   poll ();
                   String.compare a b))
          in
          loop [])
    with
    | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
      Error
        (Store_error.of_exn ~operation:"enumerate retention roots" ~path:directory exn))
;;
