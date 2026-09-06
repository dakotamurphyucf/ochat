open Core

type owner =
  { server_id : Agent_protocol.Id.Server.t
  ; process_id : int
  ; process_start_identity : string option
  ; hostname : string
  ; acquired_at : Agent_protocol.Timestamp.t
  ; nonce : string
  }
[@@deriving sexp]

type t =
  { path : string
  ; file : Eio.File.rw_ty Eio.Resource.t
  ; owner : owner
  ; mutable released : bool
  }

let owner t = t.owner
let path t = t.path
let eio_path env path = Eio.Path.(Eio.Stdenv.fs env / path)

let timestamp env =
  Eio.Time.now (Eio.Stdenv.clock env)
  |> Time_ns.Span.of_sec
  |> Time_ns.of_span_since_epoch
  |> Agent_protocol.Timestamp.of_time_ns
;;

let read_owner_path path =
  if Eio.Path.is_file path
  then (
    match Eio.Path.load path with
    | "" -> None
    | contents -> Some (owner_of_sexp (Sexp.of_string contents)))
  else None
;;

let read_owner ~env ~path =
  if not (Filename.is_absolute path)
  then
    Error
      (Store_error.Io
         { operation = "read lock owner"; path; message = "path must be absolute" })
  else (
    try Ok (read_owner_path (eio_path env path)) with
    | exn -> Error (Store_error.of_exn ~operation:"read lock owner" ~path exn))
;;

let owner_summary owner =
  sprintf
    "%s pid=%d host=%s nonce=%s"
    (Agent_protocol.Id.Server.to_string owner.server_id)
    owner.process_id
    owner.hostname
    owner.nonce
;;

let with_unix_descriptor operation file f =
  match Eio_unix.Resource.fd_opt file with
  | None -> failwith (operation ^ " requires a Unix-backed Eio filesystem")
  | Some descriptor -> Eio_unix.Fd.use_exn operation descriptor f
;;

let set_lock ~env file command =
  with_unix_descriptor "agent store lock" file (fun descriptor ->
    Eio_unix.run_in_systhread (fun () -> Core_unix.flock descriptor command))
;;

let install_owner file owner =
  let encoded = Sexp.to_string_hum (sexp_of_owner owner) ^ "\n" in
  Eio.File.truncate file Optint.Int63.zero;
  ignore (Eio.File.seek file Optint.Int63.zero `Set : Optint.Int63.t);
  Eio.Flow.copy_string encoded file;
  Eio.File.sync file
;;

let close_file file =
  try Eio.Resource.close file with
  | _ -> ()
;;

let acquire_eio ~env ~sw ~path ~server_id ~process_start_identity ~nonce =
  let lock_path = eio_path env path in
  let file = Eio.Path.open_out ~sw ~create:(`If_missing 0o600) lock_path in
  match set_lock ~env file Core_unix.Flock_command.lock_exclusive with
  | false ->
    close_file file;
    let existing = read_owner_path lock_path |> Option.map ~f:owner_summary in
    Error (Store_error.Locked existing)
  | true ->
    let owner =
      { server_id
      ; process_id = Core_unix.getpid () |> Pid.to_int
      ; process_start_identity
      ; hostname = Core_unix.gethostname ()
      ; acquired_at = timestamp env
      ; nonce
      }
    in
    (match install_owner file owner with
     | () -> Ok { path; file; owner; released = false }
     | exception exn ->
       ignore (set_lock ~env file Core_unix.Flock_command.unlock : bool);
       close_file file;
       Error (Store_error.of_exn ~operation:"write lock owner" ~path exn))
;;

let acquire ~env ~sw ~path ~server_id ~process_start_identity ~nonce =
  if not (Filename.is_absolute path)
  then
    Error
      (Store_error.Io
         { operation = "acquire lock"; path; message = "path must be absolute" })
  else (
    try acquire_eio ~env ~sw ~path ~server_id ~process_start_identity ~nonce with
    | exn -> Error (Store_error.of_exn ~operation:"acquire lock" ~path exn))
;;

let release_eio ~env t =
  if not t.released
  then (
    Eio.File.truncate t.file Optint.Int63.zero;
    Eio.File.sync t.file;
    ignore (set_lock ~env t.file Core_unix.Flock_command.unlock : bool);
    close_file t.file;
    t.released <- true)
;;

let release ~env t =
  try
    release_eio ~env t;
    Ok ()
  with
  | exn -> Error (Store_error.of_exn ~operation:"release lock" ~path:t.path exn)
;;
