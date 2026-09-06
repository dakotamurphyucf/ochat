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

let write_temporary path contents =
  Eio.Path.with_open_out ~create:(`Exclusive 0o600) path (fun flow ->
    Eio.Flow.copy_string contents flow;
    Eio.File.sync flow)
;;

let sync_directory_exn path =
  Eio.Path.with_open_in path (fun directory ->
    (match (Eio.File.stat directory).kind with
     | `Directory -> ()
     | _ -> invalid_arg "sync_directory requires a directory");
    match Eio_unix.Resource.fd_opt directory with
    | None -> failwith "directory sync requires a native directory FD"
    | Some descriptor ->
      Eio_unix.Fd.use_exn "fsync directory" descriptor (fun unix_descriptor ->
        Eio_unix.run_in_systhread (fun () -> Core_unix.fsync unix_descriptor)))
;;

let replace_eio ~env ~durability ~path contents =
  let temporary = temporary_path path in
  let temporary_eio = eio_path env temporary in
  let destination = eio_path env path in
  try
    write_temporary temporary_eio contents;
    Eio.Path.rename temporary_eio destination;
    (match durability with
     | Flush_file -> ()
     | Flush_file_and_directory ->
       sync_directory_exn (eio_path env (Filename.dirname path)));
    Ok ()
  with
  | exn ->
    (try Eio.Path.unlink temporary_eio with
     | _ -> ());
    Error (Store_error.of_exn ~operation:"replace" ~path exn)
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

let sync_directory ~env ~path =
  Result.bind (validate_path path) ~f:(fun () ->
    try
      sync_directory_exn (eio_path env path);
      Ok ()
    with
    | exn -> Error (Store_error.of_exn ~operation:"sync directory" ~path exn))
;;
