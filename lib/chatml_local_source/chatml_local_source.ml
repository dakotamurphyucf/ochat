open Core
module Native = Chatml_local_source_native

module Error = struct
  type operation =
    | Borrow_root
    | Open_parent
    | Open_directory
    | Open_file
    | Enumerate
    | Read
    | Check_revision
  [@@deriving equal, sexp_of]

  type code =
    | Missing
    | Denied
    | Unsupported_provider
    | Unsupported_kind
    | Symlink_or_non_directory
    | Changed
    | Too_large
    | Entry_limit
    | Invalid_limit
    | Not_retained_directory
    | Closed
    | Unavailable
  [@@deriving equal, sexp_of]

  type t =
    { operation : operation
    ; code : code
    }
  [@@deriving sexp_of]

  let operation t = t.operation
  let code t = t.code
  let create operation code = { operation; code }

  let native operation = function
    | Native.Code.Missing -> create operation Missing
    | Denied -> create operation Denied
    | Unsupported -> create operation Unsupported_provider
    | Kind -> create operation Unsupported_kind
    | Symlink_or_non_directory -> create operation Symlink_or_non_directory
    | Changed -> create operation Changed
    | Too_large -> create operation Too_large
    | Unavailable -> create operation Unavailable
  ;;
end

type t =
  { descriptor : Native.Descriptor.t
  ; max_path_components : int
  ; gate : Eio.Mutex.t
  ; mutable active : int
  ; mutable closing : bool
  ; drained : unit Eio.Promise.t
  ; resolve_drained : unit Eio.Promise.u
  }

let poll checkpoint =
  Eio.Fiber.check ();
  checkpoint ()
;;

let joined f = Eio.Cancel.protect (fun () -> Eio_unix.run_in_systhread f)
let close_descriptor descriptor = joined (fun () -> Native.Descriptor.close descriptor)
let close_stream stream = joined (fun () -> Native.Directory.close stream)

let close t =
  Eio.Cancel.protect (fun () ->
    Eio.Mutex.use_rw ~protect:true t.gate (fun () ->
      t.closing <- true;
      if t.active = 0 then Eio.Promise.resolve t.resolve_drained ());
    Eio.Promise.await t.drained;
    close_descriptor t.descriptor)
;;

let with_operation t operation ~f =
  let entered = ref false in
  Fun.protect
    ~finally:(fun () ->
      if !entered
      then
        Eio.Cancel.protect (fun () ->
          Eio.Mutex.use_rw ~protect:true t.gate (fun () ->
            t.active <- t.active - 1;
            if t.closing && t.active = 0 then Eio.Promise.resolve t.resolve_drained ())))
    (fun () ->
       match
         Eio.Mutex.use_rw ~protect:true t.gate (fun () ->
           if t.closing
           then Error (Error.create operation Closed)
           else if t.active = Int.max_value
           then Error (Error.create operation Unavailable)
           else (
             t.active <- t.active + 1;
             entered := true;
             Ok ()))
       with
       | Error error -> Error error
       | Ok () -> f ())
;;

let borrow_error = function
  | Eio.Fs.E (Not_found _) -> Error.create Borrow_root Missing
  | Eio.Fs.E (Permission_denied _) -> Error.create Borrow_root Denied
  | Eio.Fs.E (Not_native _) -> Error.create Borrow_root Unsupported_provider
  | _ -> Error.create Borrow_root Unavailable
;;

let with_root root ~max_path_components ~checkpoint ~f =
  let _, path = root in
  if max_path_components <= 0
  then Error (`Filesystem (Error.create Borrow_root Invalid_limit))
  else if not (String.is_empty path)
  then Error (`Filesystem (Error.create Borrow_root Not_retained_directory))
  else (
    poll checkpoint;
    Eio.Switch.run (fun sw ->
      let opened =
        try Ok (Eio.Path.open_in ~sw Eio.Path.(root / ".")) with
        | Eio.Io (error, _) -> Error (borrow_error error)
      in
      match opened with
      | Error error -> Error (`Filesystem error)
      | Ok resource ->
        (match Eio_unix.Resource.fd_opt resource with
         | None -> Error (`Filesystem (Error.create Borrow_root Unsupported_provider))
         | Some borrowed ->
           let owner = ref None in
           let descriptor = Native.Descriptor.create () in
           Fun.protect
             ~finally:(fun () ->
               match !owner with
               | None -> close_descriptor descriptor
               | Some t -> close t)
             (fun () ->
                let acquired =
                  Eio_unix.Fd.use_exn "borrow local source root" borrowed (fun fd ->
                    joined (fun () -> Native.Descriptor.duplicate_directory descriptor fd))
                in
                match acquired with
                | Error error -> Error (`Filesystem (Error.native Borrow_root error))
                | Ok () ->
                  let drained, resolve_drained = Eio.Promise.create () in
                  let t =
                    { descriptor
                    ; max_path_components
                    ; gate = Eio.Mutex.create ()
                    ; active = 0
                    ; closing = false
                    ; drained
                    ; resolve_drained
                    }
                  in
                  owner := Some t;
                  poll checkpoint;
                  Result.map_error (f t) ~f:(fun error -> `Callback error)))))
;;

let components t member operation =
  let parts = String.split (Source.Portable_path.to_string member) ~on:'/' in
  if List.length parts > t.max_path_components
  then Error (Error.create operation Too_large)
  else Ok parts
;;

(* Parent resources are local to an operation. No lifecycle gate is held while
   opening, invoking checkpoints, reading or calling a directory visitor. *)
let with_directories t parts ~checkpoint ~f =
  let owned = ref [] in
  Fun.protect
    ~finally:(fun () -> List.iter !owned ~f:close_descriptor)
    (fun () ->
       let open Result.Let_syntax in
       let%bind directory =
         List.fold_result parts ~init:t.descriptor ~f:(fun parent name ->
           poll checkpoint;
           let descriptor = Native.Descriptor.create () in
           owned := descriptor :: !owned;
           match
             joined (fun () -> Native.Descriptor.open_directory descriptor ~parent name)
           with
           | Error error -> Error (Error.native Open_parent error)
           | Ok () ->
             poll checkpoint;
             Ok descriptor)
       in
       f directory)
;;

let with_file parent name ~checkpoint ~f =
  let file = Native.Descriptor.create () in
  Fun.protect
    ~finally:(fun () -> close_descriptor file)
    (fun () ->
       poll checkpoint;
       match joined (fun () -> Native.Descriptor.open_file file ~parent name) with
       | Error error -> Error (Error.native Open_file error)
       | Ok () ->
         poll checkpoint;
         f file)
;;

let read t ~member ~max_bytes ~checkpoint =
  if max_bytes < 0
  then Error (Error.create Read Invalid_limit)
  else
    with_operation t Read ~f:(fun () ->
      let open Result.Let_syntax in
      let%bind parts = components t member Open_parent in
      match List.rev parts with
      | [] -> Error (Error.create Open_file Unavailable)
      | name :: reversed_parents ->
        with_directories t (List.rev reversed_parents) ~checkpoint ~f:(fun parent ->
          with_file parent name ~checkpoint ~f:(fun file ->
            let%bind before =
              joined (fun () -> Native.observe_file file)
              |> Result.map_error ~f:(Error.native Read)
            in
            let size = Native.Observation.byte_length before in
            if Int64.(size < zero || size > of_int max_bytes)
            then Error (Error.create Read Too_large)
            else (
              let buffer = Buffer.create (Int.min 4096 max_bytes) in
              let chunk = Bigstring.create 4096 in
              let rec loop () =
                poll checkpoint;
                let remaining = max_bytes - Buffer.length buffer in
                let requested = if remaining >= 4096 then 4096 else remaining + 1 in
                let%bind count =
                  joined (fun () -> Native.read file chunk ~max_bytes:requested)
                  |> Result.map_error ~f:(Error.native Read)
                in
                poll checkpoint;
                if count > remaining
                then Error (Error.create Read Too_large)
                else if count = 0
                then Ok ()
                else (
                  Buffer.add_string buffer (Bigstring.to_string chunk ~pos:0 ~len:count);
                  loop ())
              in
              let%bind () = loop () in
              let%bind () =
                if Int64.equal size (Int64.of_int (Buffer.length buffer))
                then Ok ()
                else Error (Error.create Check_revision Changed)
              in
              let%bind () =
                joined (fun () -> Native.unchanged file before)
                |> Result.map_error ~f:(Error.native Check_revision)
              in
              let%bind () =
                joined (fun () -> Native.same_entry parent ~name ~file)
                |> Result.map_error ~f:(Error.native Check_revision)
              in
              poll checkpoint;
              Ok (Buffer.contents buffer)))))
;;

module Batch = struct
  type t =
    { names : string list
    ; at_end : bool
    }

  let read stream buffer ~remaining =
    let rec loop names count =
      if count = 128
      then Ok { names = List.rev names; at_end = false }
      else (
        match Native.Directory.next stream buffer with
        | Error error -> Error (Error.native Enumerate error)
        | Ok None -> Ok { names = List.rev names; at_end = true }
        | Ok (Some name) ->
          if count = remaining
          then Error (Error.create Enumerate Entry_limit)
          else loop (name :: names) (count + 1))
    in
    loop [] 0
  ;;
end

let fold_directory t ~directory ~max_entries ~init ~checkpoint ~f =
  if max_entries < 0
  then Error (`Filesystem (Error.create Enumerate Invalid_limit))
  else (
    let outcome =
      with_operation t Enumerate ~f:(fun () ->
        let parts =
          match directory with
          | None -> Ok []
          | Some path -> components t path Open_directory
        in
        match parts with
        | Error error -> Ok (Error (`Filesystem error))
        | Ok parts ->
          with_directories t parts ~checkpoint ~f:(fun directory ->
            poll checkpoint;
            let stream = Native.Directory.create () in
            Fun.protect
              ~finally:(fun () -> close_stream stream)
              (fun () ->
                 match joined (fun () -> Native.Directory.open_ stream directory) with
                 | Error error -> Error (Error.native Open_directory error)
                 | Ok () ->
                   let buffer = Bigstring.create 1024 in
                   let rec loop state count =
                     poll checkpoint;
                     match
                       joined (fun () ->
                         Batch.read stream buffer ~remaining:(max_entries - count))
                     with
                     | Error error -> Ok (Error (`Filesystem error))
                     | Ok { Batch.names; at_end } ->
                       let delivered =
                         List.fold_result names ~init:state ~f:(fun state name ->
                           poll checkpoint;
                           Result.map_error (f state ~name) ~f:(fun error ->
                             `Callback error))
                       in
                       (match delivered with
                        | Error error -> Ok (Error error)
                        | Ok state ->
                          let count = count + List.length names in
                          if at_end
                          then (
                            poll checkpoint;
                            Ok (Ok (state, count)))
                          else loop state count)
                   in
                   loop init 0)))
    in
    match outcome with
    | Error error -> Error (`Filesystem error)
    | Ok result -> result)
;;
