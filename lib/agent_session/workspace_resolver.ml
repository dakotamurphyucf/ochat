open Core

let eio_path env path = Eio.Path.(Eio.Stdenv.fs env / path)
let ownership_marker = ".ochat-workspace-owner"

let timestamp env =
  Eio.Time.now (Eio.Stdenv.clock env)
  |> Time_ns.Span.of_sec
  |> Time_ns.of_span_since_epoch
  |> Agent_protocol.Timestamp.of_time_ns
;;

let identity ~env path =
  try
    let file = eio_path env path in
    let stat = Eio.Path.stat ~follow:true file in
    if Poly.equal stat.kind `Directory
    then (
      let native_path = Eio_posix.Low_level.realpath path in
      Ok Workspace_instance.{ native_path; device = stat.dev; inode = stat.ino })
    else Error (Agent_store.Store_error.Corrupt ("workspace is not a directory: " ^ path))
  with
  | exn -> Error (Agent_store.Store_error.of_exn ~operation:"resolve workspace" ~path exn)
;;

let conflict_domain definition identity =
  Option.value
    definition.Workspace_definition.conflict_domain
    ~default:(sprintf "fs:%Ld:%Ld" identity.Workspace_instance.device identity.inode)
;;

let make
      definition
      ~instance_id
      ~source_kind
      ~configured_root
      ~canonical_root
      ~cleanup
      ~server_created
      ~created_at
  =
  Workspace_instance.
    { id = instance_id
    ; definition_id = Some definition.Workspace_definition.id
    ; source_kind
    ; configured_root
    ; canonical_root
    ; conflict_domain = conflict_domain definition canonical_root
    ; access = definition.access
    ; cleanup
    ; server_created
    ; created_at
    ; cleanup_completion = None
    }
;;

let resolve_physical ~env ~instance_id definition configured_root =
  Result.map (identity ~env configured_root) ~f:(fun canonical_root ->
    make
      definition
      ~instance_id
      ~source_kind:Physical
      ~configured_root:(Some configured_root)
      ~canonical_root
      ~cleanup:None
      ~server_created:false
      ~created_at:(timestamp env))
;;

let write_ownership_marker ~env ~instance_id path =
  let marker = Filename.concat path ownership_marker |> eio_path env in
  Eio.Path.save
    ~create:(`Exclusive 0o600)
    marker
    (Agent_protocol.Id.Workspace_instance.to_string instance_id ^ "\n")
;;

let ensure_temporary ~env ~instance_id ~exclusive path =
  try
    if exclusive
    then Eio.Path.mkdir ~perm:0o700 (eio_path env path)
    else Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 (eio_path env path);
    write_ownership_marker ~env ~instance_id path;
    identity ~env path
  with
  | exn -> Error (Agent_store.Store_error.of_exn ~operation:"create workspace" ~path exn)
;;

let temporary_path definition ~instance_id session_directory location =
  match location with
  | Workspace_definition.Session_dir -> Filename.concat session_directory "workspace"
  | System_tmp ->
    let root =
      match definition.Workspace_definition.source with
      | Temporary { managed_root = Some root; _ } -> root
      | Temporary { managed_root = None; _ } | Physical _ -> assert false
    in
    Filename.concat root (Agent_protocol.Id.Workspace_instance.to_string instance_id)
;;

let resolve_temporary ~env ~instance_id ~session_directory definition location cleanup =
  let path = temporary_path definition ~instance_id session_directory location in
  let exclusive = Workspace_definition.equal_temporary_location location System_tmp in
  Result.map
    (ensure_temporary ~env ~instance_id ~exclusive path)
    ~f:(fun canonical_root ->
      make
        definition
        ~instance_id
        ~source_kind:(Temporary location)
        ~configured_root:(Some path)
        ~canonical_root
        ~cleanup:(Some cleanup)
        ~server_created:true
        ~created_at:(timestamp env))
;;

let resolve ~env ~instance_id ~session_directory definition =
  match definition.Workspace_definition.source with
  | Physical { configured_root } ->
    resolve_physical ~env ~instance_id definition configured_root
  | Temporary { location; cleanup; _ } ->
    resolve_temporary ~env ~instance_id ~session_directory definition location cleanup
;;

let resolve_current ~env ~instance_id ~path ~access ~created_at =
  Result.map (identity ~env path) ~f:(fun canonical_root ->
    Workspace_instance.
      { id = instance_id
      ; definition_id = None
      ; source_kind = Current
      ; configured_root = Some path
      ; canonical_root
      ; conflict_domain = sprintf "fs:%Ld:%Ld" canonical_root.device canonical_root.inode
      ; access
      ; cleanup = None
      ; server_created = false
      ; created_at
      ; cleanup_completion = None
      })
;;

let verify_available ~env instance =
  let open Result.Let_syntax in
  let%bind current =
    identity ~env instance.Workspace_instance.canonical_root.native_path
  in
  if
    Int64.equal current.device instance.canonical_root.device
    && Int64.equal current.inode instance.canonical_root.inode
  then Ok ()
  else Error (Agent_store.Store_error.Corrupt "workspace canonical identity changed")
;;
