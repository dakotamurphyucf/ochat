open Core

type event =
  | Session_stop
  | Session_delete
[@@deriving compare, equal, sexp]

type protected_roots =
  { data_root : string
  ; physical_workspaces : string list
  ; managed_roots : string list
  }

let normalize path =
  let suffix =
    if String.equal path "/" then path else String.rstrip path ~drop:(Char.equal '/')
  in
  if String.is_empty suffix then "/" else suffix
;;

let eio_path env path = Eio.Path.(Eio.Stdenv.fs env / path)

let canonical_path path =
  try Eio_posix.Low_level.realpath path with
  | _ -> normalize path
;;

let below ~root path =
  let root = normalize root in
  let path = normalize path in
  (not (String.equal root path)) && String.is_prefix path ~prefix:(root ^ "/")
;;

let policy_matches cleanup event =
  match cleanup, event with
  | Workspace_definition.On_session_stop, Session_stop
  | On_session_stop, Session_delete
  | On_session_delete, Session_delete -> true
  | On_session_delete, Session_stop | Retain, _ -> false
;;

let validate_path protected_roots ~expected_path path =
  let path = canonical_path path in
  let expected_path = canonical_path expected_path in
  let protected_paths =
    protected_roots.data_root :: protected_roots.physical_workspaces
    |> List.map ~f:canonical_path
  in
  if not (String.equal (normalize path) (normalize expected_path))
  then Error (Agent_store.Store_error.Corrupt "workspace cleanup identity is unexpected")
  else if
    String.equal (normalize path) "/"
    || List.exists protected_paths ~f:(fun protected ->
      String.equal (normalize path) (normalize protected))
  then Error (Agent_store.Store_error.Corrupt "workspace cleanup target is protected")
  else if
    not
      (List.exists protected_roots.managed_roots ~f:(fun root ->
         below ~root:(canonical_path root) path))
  then
    Error
      (Agent_store.Store_error.Corrupt "workspace cleanup target is outside managed roots")
  else Ok ()
;;

let validate_identity ~env instance path =
  try
    let stat = Eio.Path.stat ~follow:false (eio_path env path) in
    if not (Poly.equal stat.kind `Directory)
    then
      Error
        (Agent_store.Store_error.Corrupt "workspace cleanup target is not a directory")
    else if
      Int64.equal stat.dev instance.Workspace_instance.canonical_root.device
      && Int64.equal stat.ino instance.canonical_root.inode
    then Ok ()
    else Error (Agent_store.Store_error.Corrupt "workspace canonical identity changed")
  with
  | exn ->
    Error
      (Agent_store.Store_error.of_exn ~operation:"verify workspace identity" ~path exn)
;;

let validate_marker ~env instance path =
  let marker = Filename.concat path Workspace_resolver.ownership_marker in
  try
    let marker_path = eio_path env marker in
    let stat = Eio.Path.stat ~follow:false marker_path in
    let expected =
      Agent_protocol.Id.Workspace_instance.to_string instance.Workspace_instance.id
    in
    if not (Poly.equal stat.kind `Regular_file)
    then Error (Agent_store.Store_error.Corrupt "workspace ownership marker is invalid")
    else if String.equal (String.strip (Eio.Path.load marker_path)) expected
    then Ok ()
    else
      Error (Agent_store.Store_error.Corrupt "workspace ownership marker does not match")
  with
  | exn ->
    Error
      (Agent_store.Store_error.of_exn
         ~operation:"verify workspace marker"
         ~path:marker
         exn)
;;

let rec validate_tree path =
  match Eio.Path.kind ~follow:false path with
  | `Directory ->
    Eio.Path.read_dir path
    |> List.fold_result ~init:() ~f:(fun () name -> validate_tree Eio.Path.(path / name))
  | `Regular_file -> Ok ()
  | `Symbolic_link ->
    Error (Agent_store.Store_error.Corrupt "workspace cleanup refuses symbolic links")
  | `Not_found ->
    Error (Agent_store.Store_error.Corrupt "workspace cleanup target vanished")
  | `Unknown | `Socket | `Fifo | `Character_special | `Block_device ->
    Error (Agent_store.Store_error.Corrupt "workspace cleanup target has an unknown kind")
;;

let remove ~env ~protected_roots ~expected_path ~has_active_lease ~now instance =
  let path = instance.Workspace_instance.canonical_root.native_path in
  let open Result.Let_syntax in
  let%bind () = validate_path protected_roots ~expected_path path in
  let%bind () = validate_identity ~env instance path in
  let%bind () = validate_marker ~env instance path in
  let%bind () = validate_tree (eio_path env path) in
  if has_active_lease ~conflict_domain:instance.conflict_domain
  then Error (Agent_store.Store_error.Locked (Some "workspace lease is active"))
  else (
    try
      Eio.Path.rmtree ~missing_ok:false (eio_path env path);
      Ok
        (Workspace_instance.with_cleanup_completion
           instance
           { completed_at = now; reason = "managed workspace removed" })
    with
    | exn ->
      Error (Agent_store.Store_error.of_exn ~operation:"cleanup workspace" ~path exn))
;;

let cleanup ~env ~protected_roots ~expected_path ~has_active_lease ~event ~now instance =
  match
    instance.Workspace_instance.source_kind, instance.cleanup, instance.cleanup_completion
  with
  | Temporary _, Some cleanup, None
    when instance.server_created && policy_matches cleanup event ->
    remove ~env ~protected_roots ~expected_path ~has_active_lease ~now instance
  | Temporary _, Some _, Some _ -> Ok instance
  | Temporary _, Some _, None | Physical, _, _ | Current, _, _ -> Ok instance
  | Temporary _, None, _ ->
    Error (Agent_store.Store_error.Corrupt "temporary workspace has no cleanup policy")
;;
