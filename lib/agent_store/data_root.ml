open Core

type t =
  { path : string
  ; schema_path : string
  ; daemon_lock_path : string
  ; server_id_path : string
  ; indexes_path : string
  ; prompt_artifacts_path : string
  ; temporary_blobs_path : string
  ; durable_blobs_path : string
  ; audit_path : string
  ; sessions_path : string
  ; migrations_path : string
  ; lost_and_found_path : string
  }

let path t = t.path
let schema_path t = t.schema_path
let daemon_lock_path t = t.daemon_lock_path
let server_id_path t = t.server_id_path
let indexes_path t = t.indexes_path
let prompt_artifacts_path t = t.prompt_artifacts_path
let temporary_blobs_path t = t.temporary_blobs_path
let durable_blobs_path t = t.durable_blobs_path
let audit_path t = t.audit_path
let sessions_path t = t.sessions_path
let migrations_path t = t.migrations_path
let lost_and_found_path t = t.lost_and_found_path
let child path name = Filename.concat path name

let make path =
  let blobs = child path "blobs" in
  { path
  ; schema_path = child path "schema.sexp"
  ; daemon_lock_path = child path "daemon.lock"
  ; server_id_path = child path "server-id"
  ; indexes_path = child path "indexes"
  ; prompt_artifacts_path = child path "prompt-artifacts"
  ; temporary_blobs_path = child blobs "temporary"
  ; durable_blobs_path = child blobs "durable"
  ; audit_path = child path "audit"
  ; sessions_path = child path "sessions"
  ; migrations_path = child path "migrations"
  ; lost_and_found_path = child path "lost-and-found"
  }
;;

let validate_path path =
  let path = String.rstrip path ~drop:(Char.equal '/') in
  if String.is_empty path || not (Filename.is_absolute path)
  then
    Error
      (Store_error.Io { operation = "validate"; path; message = "path must be absolute" })
  else if String.equal path "/"
  then
    Error
      (Store_error.Io
         { operation = "validate"; path; message = "filesystem root is forbidden" })
  else Ok path
;;

let directories t =
  [ t.path
  ; t.indexes_path
  ; t.prompt_artifacts_path
  ; Filename.dirname t.temporary_blobs_path
  ; t.temporary_blobs_path
  ; t.durable_blobs_path
  ; t.audit_path
  ; t.sessions_path
  ; t.migrations_path
  ; t.lost_and_found_path
  ]
;;

let create_directories ~env t =
  let fs = Eio.Stdenv.fs env in
  try
    List.iter (directories t) ~f:(fun path ->
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 Eio.Path.(fs / path));
    Ok t
  with
  | exn -> Error (Store_error.of_exn ~operation:"create data root" ~path:t.path exn)
;;

let create ~env ~path =
  Result.bind (validate_path path) ~f:(fun path -> create_directories ~env (make path))
;;

let open_existing ~env ~path =
  let open Result.Let_syntax in
  let%bind path = validate_path path in
  let t = make path in
  try
    let root = Eio.Path.(Eio.Stdenv.fs env / path) in
    if Eio.Path.is_directory root then Ok t else Error (Store_error.Missing path)
  with
  | exn -> Error (Store_error.of_exn ~operation:"open data root" ~path exn)
;;

let session_path t id = child t.sessions_path (Agent_protocol.Id.Session.to_string id)
