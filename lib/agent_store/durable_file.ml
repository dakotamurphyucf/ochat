open Core

type durability =
  | Flush_file
  | Flush_file_and_directory
[@@deriving compare, equal, sexp]

let temporary_sequence = Atomic.make 0
let eio_path env path = Eio.Path.(Eio.Stdenv.fs env / path)

let temporary_path path =
  let sequence = Atomic.fetch_and_add temporary_sequence 1 in
  sprintf "%s.tmp-%d-%d" path (Core_unix.getpid () |> Pid.to_int) sequence
;;

let temporary_target filename =
  let digits text =
    (not (String.is_empty text)) && String.for_all text ~f:Char.is_digit
  in
  match String.rsplit2 filename ~on:'.' with
  | Some (target, suffix) when not (String.is_empty target) ->
    (match String.split suffix ~on:'-' with
     | [ "tmp"; pid; sequence ] when digits pid && digits sequence -> Some target
     | _ -> None)
  | _ -> None
;;

let write_temporary path contents ~on_open =
  Eio.Path.with_open_out ~create:(`Exclusive 0o600) path (fun flow ->
    on_open ();
    Eio.Flow.copy_string contents flow;
    Eio.File.sync flow)
;;

exception Unsupported_directory_sync_provider

let unsupported_directory_sync ~operation ~path =
  Store_error.Io
    { operation; path; message = "directory sync requires a native directory FD" }
;;

let sync_directory_exn path =
  (* Open "." relative to a retained directory capability: Linux openat2 rejects
     the empty relative path returned by Path.with_open_dir. *)
  Eio.Path.with_open_in
    Eio.Path.(path / ".")
    (fun directory ->
       (match (Eio.File.stat directory).kind with
        | `Directory -> ()
        | _ -> invalid_arg "sync_directory requires a directory");
       match Eio_unix.Resource.fd_opt directory with
       | None -> raise Unsupported_directory_sync_provider
       | Some descriptor ->
         Eio_unix.Fd.use_exn "fsync directory" descriptor (fun unix_descriptor ->
           Eio_unix.run_in_systhread (fun () -> Core_unix.fsync unix_descriptor)))
;;

let replace_paths ~durability ~path ~temporary_eio ~destination ~directory contents =
  let owned = ref false in
  Fun.protect
    (fun () ->
       try
         write_temporary temporary_eio contents ~on_open:(fun () -> owned := true);
         Eio.Path.rename temporary_eio destination;
         owned := false;
         (match durability with
          | Flush_file -> ()
          | Flush_file_and_directory -> sync_directory_exn directory);
         Ok ()
       with
       | Unsupported_directory_sync_provider ->
         Error (unsupported_directory_sync ~operation:"replace" ~path)
       | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
         Error (Store_error.of_exn ~operation:"replace" ~path exn))
    ~finally:(fun () ->
      if !owned
      then
        Eio.Cancel.protect (fun () ->
          try Eio.Path.unlink temporary_eio with
          | Eio.Io _ | Core_unix.Unix_error _ -> ()))
;;

let replace_eio ~env ~durability ~path contents =
  replace_paths
    ~durability
    ~path
    ~temporary_eio:(eio_path env (temporary_path path))
    ~destination:(eio_path env path)
    ~directory:(eio_path env (Filename.dirname path))
    contents
;;

let replace_in ~directory ~durability ~basename contents =
  if
    String.is_empty basename
    || String.mem basename '\000'
    || (not (String.equal (Filename.basename basename) basename))
    || String.equal basename "."
    || String.equal basename ".."
  then Error (Store_error.Corrupt "atomic replacement requires a child basename")
  else
    replace_paths
      ~durability
      ~path:basename
      ~temporary_eio:Eio.Path.(directory / temporary_path basename)
      ~destination:Eio.Path.(directory / basename)
      ~directory
      contents
;;

let validate_path path =
  if Filename.is_absolute path
  then Ok ()
  else
    Error
      (Store_error.Io { operation = "validate"; path; message = "path must be absolute" })
;;

let replace ~env ~durability ~path contents =
  Result.bind (validate_path path) ~f:(fun () ->
    replace_eio ~env ~durability ~path contents)
;;

let load ~env ~path =
  Result.bind (validate_path path) ~f:(fun () ->
    let file = eio_path env path in
    match Eio.Path.kind ~follow:true file with
    | `Not_found -> Error (Store_error.Missing path)
    | `Regular_file ->
      (try Ok (Eio.Path.load file) with
       | exn -> Error (Store_error.of_exn ~operation:"load" ~path exn))
    | _ ->
      Error
        (Store_error.Io
           { operation = "load"; path; message = "path is not a regular file" }))
;;

let load_bounded_at ~path ~follow file ~max_bytes =
  if max_bytes < 0
  then Error (Store_error.Corrupt "negative bounded read limit")
  else (
    try
      match Eio.Path.kind ~follow file with
      | `Not_found -> Error (Store_error.Missing path)
      | `Regular_file ->
        Eio.Path.with_open_in file (fun input ->
          let size = (Eio.File.stat input).size |> Optint.Int63.to_int64 in
          if Int64.(size < zero || size > of_int max_bytes)
          then
            Error
              (Store_error.Document (Document_schema.Error.Limit_exceeded "file bytes"))
          else (
            let buffer = Buffer.create (Int.min 8192 max_bytes) in
            let chunk = Cstruct.create 8192 in
            let rec loop () =
              let remaining = max_bytes - Buffer.length buffer in
              let count =
                try
                  Eio.Flow.single_read
                    input
                    (Cstruct.sub
                       chunk
                       0
                       (if remaining >= 8192 then 8192 else remaining + 1))
                with
                | End_of_file -> 0
              in
              if count = 0
              then Ok (Buffer.contents buffer)
              else if count > remaining
              then
                Error
                  (Store_error.Document
                     (Document_schema.Error.Limit_exceeded "file bytes"))
              else (
                Buffer.add_string buffer (Cstruct.to_string (Cstruct.sub chunk 0 count));
                loop ())
            in
            loop ()))
      | _ ->
        Error
          (Store_error.Io
             { operation = "load"; path; message = "path is not a regular file" })
    with
    | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
      Error (Store_error.of_exn ~operation:"bounded load" ~path exn))
;;

let load_bounded ~env ~path ~max_bytes =
  Result.bind (validate_path path) ~f:(fun () ->
    load_bounded_at ~path ~follow:true (eio_path env path) ~max_bytes)
;;

let load_bounded_in ~directory ~basename ~max_bytes =
  if
    String.is_empty basename
    || String.mem basename '\000'
    || (not (String.equal (Filename.basename basename) basename))
    || String.equal basename "."
    || String.equal basename ".."
  then Error (Store_error.Corrupt "bounded load requires a child basename")
  else
    load_bounded_at
      ~path:basename
      ~follow:false
      Eio.Path.(directory / basename)
      ~max_bytes
;;

let sync_directory ~env ~path =
  Result.bind (validate_path path) ~f:(fun () ->
    try
      sync_directory_exn (eio_path env path);
      Ok ()
    with
    | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
    | Unsupported_directory_sync_provider ->
      Error (unsupported_directory_sync ~operation:"sync directory" ~path)
    | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
      Error (Store_error.of_exn ~operation:"sync directory" ~path exn))
;;
