open Core

type t =
  { env : Eio_unix.Stdenv.base
  ; root : string
  ; mutable remaining_entries : int
  ; mutable remaining_bytes : int
  }

let corrupt message = Error (Store_error.Corrupt message)
let eio_path t path = Eio.Path.(Eio.Stdenv.fs t.env / path)

let create ~env ~root ~max_entries ~max_bytes =
  let root = String.rstrip root ~drop:(Char.equal '/') in
  match Filename.is_absolute root && max_entries >= 0 && max_bytes >= 0 with
  | false -> corrupt "invalid retention scan root or limits"
  | true -> Ok { env; root; remaining_entries = max_entries; remaining_bytes = max_bytes }
;;

let charge_entry t =
  match t.remaining_entries > 0 with
  | false -> corrupt "retention scan exceeded its entry budget"
  | true ->
    t.remaining_entries <- t.remaining_entries - 1;
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
  | exn ->
    Error (Store_error.of_exn ~operation:"enumerate retention roots" ~path:directory exn)
;;

let read t ~path:relative ~max_bytes =
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
    let limit = Int.min max_bytes t.remaining_bytes in
    let%bind () =
      match limit >= 0 && Int64.(size >= zero && size <= of_int limit) with
      | true -> Ok ()
      | false -> corrupt "retention root exceeds its byte budget"
    in
    Eio.Path.with_open_in file (fun input ->
      let buffer = Buffer.create (Int.min 8192 limit) in
      let chunk = Cstruct.create 8192 in
      let rec loop () =
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
          t.remaining_bytes <- t.remaining_bytes - count;
          Buffer.add_string buffer (Cstruct.to_string (Cstruct.sub chunk 0 count));
          loop ()
      in
      loop ())
  with
  | exn -> Error (Store_error.of_exn ~operation:"read retention root" ~path:relative exn)
;;
