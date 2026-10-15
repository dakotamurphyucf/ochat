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
  | Core_unix.Unix_error (Core_unix.ENOENT, _, _) -> normalize path
;;

module Limits = struct
  type t = Agent_store.Bounded_tree_cleanup.Limits.t

  let create = Agent_store.Bounded_tree_cleanup.Limits.create
  let tree t = t
end

module Progress = struct
  type verified =
    { path : string
    ; id : Agent_protocol.Id.Workspace_instance.t
    ; device : int64
    ; inode : int64
    }

  type t = { mutable marker_removed : verified option }

  let create () = { marker_removed = None }
end

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
    if
      match stat.kind with
      | `Directory -> false
      | `Unknown
      | `Fifo
      | `Character_special
      | `Block_device
      | `Regular_file
      | `Symbolic_link
      | `Socket -> true
    then
      Error
        (Agent_store.Store_error.Corrupt "workspace cleanup target is not a directory")
    else if
      Int64.equal stat.dev instance.Workspace_instance.canonical_root.device
      && Int64.equal stat.ino instance.canonical_root.inode
    then Ok ()
    else Error (Agent_store.Store_error.Corrupt "workspace canonical identity changed")
  with
  | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
    Error
      (Agent_store.Store_error.of_exn ~operation:"verify workspace identity" ~path exn)
;;

let validate_marker ~env instance path =
  let marker = Filename.concat path Workspace_ownership.marker in
  try
    let marker_path = eio_path env marker in
    let stat = Eio.Path.stat ~follow:false marker_path in
    let expected =
      Agent_protocol.Id.Workspace_instance.to_string instance.Workspace_instance.id
    in
    if
      match stat.kind with
      | `Regular_file -> false
      | `Unknown
      | `Fifo
      | `Character_special
      | `Block_device
      | `Directory
      | `Symbolic_link
      | `Socket -> true
    then Error (Agent_store.Store_error.Corrupt "workspace ownership marker is invalid")
    else if String.equal (String.strip (Eio.Path.load marker_path)) expected
    then Ok ()
    else
      Error (Agent_store.Store_error.Corrupt "workspace ownership marker does not match")
  with
  | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
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

let remove_bounded
      ~env
      ~protected_roots
      ~expected_path
      ~has_active_lease
      ~now
      ~limits
      ~progress
      ~checkpoint
      instance
  =
  let path = instance.Workspace_instance.canonical_root.native_path in
  let open Result.Let_syntax in
  let%bind () = validate_path protected_roots ~expected_path path in
  let%bind () = validate_identity ~env instance path in
  let same_tail (verified : Progress.verified) =
    String.equal verified.path path
    && Agent_protocol.Id.Workspace_instance.equal verified.id instance.id
    && Int64.equal verified.device instance.canonical_root.device
    && Int64.equal verified.inode instance.canonical_root.inode
  in
  let validate_bounded_marker () =
    let open Result.Let_syntax in
    let%bind reader =
      Agent_store.Retention_reader.create ~env ~root:path ~max_entries:4 ~max_bytes:128
    in
    let%bind bytes =
      Agent_store.Retention_reader.read_with_checkpoint
        reader
        ~path:Workspace_ownership.marker
        ~max_bytes:128
        ~checkpoint
    in
    let expected = Agent_protocol.Id.Workspace_instance.to_string instance.id ^ "\n" in
    if String.equal bytes expected
    then Ok ()
    else Error (Agent_store.Store_error.Corrupt "workspace marker changed")
  in
  let%bind () =
    match progress.Progress.marker_removed with
    | None -> validate_bounded_marker ()
    | Some verified when same_tail verified -> Ok ()
    | Some _ ->
      Error (Agent_store.Store_error.Corrupt "workspace cleanup progress changed owner")
  in
  if has_active_lease ~conflict_domain:instance.conflict_domain
  then Error (Agent_store.Store_error.Locked (Some "workspace lease is active"))
  else (
    let marker = Filename.concat path Workspace_ownership.marker in
    let%bind () =
      Agent_store.Bounded_tree_cleanup.remove_children
        ~env
        ~root:path
        ~expected_root:(instance.canonical_root.device, instance.canonical_root.inode)
        ~marker_name:Workspace_ownership.marker
        ~limits
        ~checkpoint
    in
    checkpoint ();
    let%bind () = validate_identity ~env instance path in
    let%bind () =
      match progress.marker_removed with
      | Some _ -> Ok ()
      | None ->
        let%bind () = validate_bounded_marker () in
        (try
           Eio.Path.unlink (eio_path env marker);
           progress.marker_removed
           <- Some
                { path
                ; id = instance.id
                ; device = instance.canonical_root.device
                ; inode = instance.canonical_root.inode
                };
           Ok ()
         with
         | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
           Error
             (Agent_store.Store_error.of_exn ~operation:"cleanup marker" ~path:marker exn))
    in
    checkpoint ();
    let%bind () = validate_identity ~env instance path in
    try
      Eio.Path.rmdir (eio_path env path);
      Ok
        (Workspace_instance.with_cleanup_completion
           instance
           { completed_at = now; reason = "managed workspace removed" })
    with
    | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
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
