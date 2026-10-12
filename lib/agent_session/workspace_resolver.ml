open Core

let eio_path env path = Eio.Path.(Eio.Stdenv.fs env / path)
let ownership_marker = Workspace_ownership.marker

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
    match stat.kind with
    | `Directory ->
      let native_path = Eio_posix.Low_level.realpath path in
      Ok Workspace_instance.{ native_path; device = stat.dev; inode = stat.ino }
    | `Unknown
    | `Fifo
    | `Character_special
    | `Block_device
    | `Regular_file
    | `Symbolic_link
    | `Socket ->
      Error (Agent_store.Store_error.Corrupt ("workspace is not a directory: " ^ path))
  with
  | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
    Error (Agent_store.Store_error.of_exn ~operation:"resolve workspace" ~path exn)
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

let planned_root definition ~instance_id ~session_directory =
  match definition.Workspace_definition.source with
  | Physical { configured_root } -> configured_root
  | Temporary { location; _ } ->
    temporary_path definition ~instance_id session_directory location
;;

let resolve_temporary
      ~adopt
      ~env
      ~instance_id
      ~session_directory
      definition
      location
      cleanup
  =
  let path = planned_root definition ~instance_id ~session_directory in
  let exclusive = Workspace_definition.equal_temporary_location location System_tmp in
  let owned = ref false in
  let close_failed () =
    if !owned
    then (
      try Eio.Cancel.protect (fun () -> Eio.Path.rmtree (eio_path env path)) with
      | _ -> ())
  in
  Exn.protect ~finally:close_failed ~f:(fun () ->
    let resolve () =
      if exclusive
      then
        Eio.Cancel.protect (fun () ->
          Eio.Path.mkdir ~perm:0o700 (eio_path env path);
          owned := true)
      else Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 (eio_path env path);
      write_ownership_marker ~env ~instance_id path;
      Result.map (identity ~env path) ~f:(fun canonical_root ->
        make
          definition
          ~instance_id
          ~source_kind:(Temporary location)
          ~configured_root:(Some path)
          ~canonical_root
          ~cleanup:(Some cleanup)
          ~server_created:true
          ~created_at:(timestamp env))
    in
    let result =
      try resolve () with
      | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
        Error (Agent_store.Store_error.of_exn ~operation:"create workspace" ~path exn)
    in
    match result with
    | Error _ as failure -> failure
    | Ok instance as success ->
      adopt instance;
      owned := false;
      success)
;;

let resolve ~env ~instance_id ~session_directory definition =
  match definition.Workspace_definition.source with
  | Physical { configured_root } ->
    resolve_physical ~env ~instance_id definition configured_root
  | Temporary { location; cleanup; _ } ->
    resolve_temporary
      ~adopt:ignore
      ~env
      ~instance_id
      ~session_directory
      definition
      location
      cleanup
;;

module Owned = struct
  exception Cleanup_failed of Agent_store.Store_error.t

  type t =
    { env : Eio_unix.Stdenv.base
    ; domain : int
    ; physical_roots : string list
    ; managed_root : string option
    ; mutable acquired : Workspace_instance.t option
    ; mutable transferred : bool
    ; mutable cleanup_exception : (exn * Backtrace.t) option
    }

  let check_domain_exn t =
    if not (Int.equal t.domain (Stdlib.Domain.self () :> int))
    then invalid_arg "workspace creation lease belongs to another Eio domain"
  ;;

  let instance t =
    check_domain_exn t;
    Option.value_exn t.acquired
  ;;

  let transfer t =
    check_domain_exn t;
    t.transferred <- true
  ;;

  let close_resources t =
    check_domain_exn t;
    Eio.Cancel.protect (fun () ->
      if t.transferred
      then Ok ()
      else (
        match t.acquired with
        | None -> Ok ()
        | Some instance ->
          (match instance.Workspace_instance.source_kind, t.managed_root with
           | Temporary System_tmp, Some managed_root ->
             Workspace_cleanup.remove
               ~env:t.env
               ~protected_roots:
                 { data_root = managed_root
                 ; physical_workspaces = t.physical_roots
                 ; managed_roots = [ managed_root ]
                 }
               ~expected_path:instance.canonical_root.native_path
               ~has_active_lease:(fun ~conflict_domain:_ -> false)
               ~now:instance.created_at
               instance
             |> Result.map ~f:(fun _ -> t.transferred <- true)
           | (Physical | Current | Temporary Session_dir), _ ->
             t.transferred <- true;
             Ok ()
           | Temporary System_tmp, None ->
             Error
               (Agent_store.Store_error.Corrupt
                  "owned temporary workspace has no managed root"))))
  ;;

  let close t =
    match close_resources t with
    | result -> result
    | exception exn ->
      let trace = Backtrace.Exn.most_recent () in
      t.cleanup_exception <- Some (exn, trace);
      Exn.raise_with_original_backtrace exn trace
  ;;
end

let resolve_owned ~sw ~env ~physical_roots ~instance_id ~session_directory definition =
  Eio.Switch.check sw;
  let owned =
    Owned.
      { env
      ; domain = (Stdlib.Domain.self () :> int)
      ; physical_roots
      ; managed_root =
          (match definition.Workspace_definition.source with
           | Temporary { managed_root; _ } -> managed_root
           | Physical _ -> None)
      ; acquired = None
      ; transferred = false
      ; cleanup_exception = None
      }
  in
  Eio.Switch.on_release sw (fun () ->
    let previous = owned.Owned.cleanup_exception in
    let result = Owned.close owned in
    match previous with
    | Some (exn, trace) -> Exn.raise_with_original_backtrace exn trace
    | None ->
      (match result with
       | Ok () -> ()
       | Error failure -> raise (Owned.Cleanup_failed failure)));
  let adopt instance = owned.acquired <- Some instance in
  let resolved =
    match definition.Workspace_definition.source with
    | Physical { configured_root } ->
      resolve_physical ~env ~instance_id definition configured_root
      |> Result.map ~f:(fun instance ->
        adopt instance;
        instance)
    | Temporary { location; cleanup; _ } ->
      resolve_temporary
        ~adopt
        ~env
        ~instance_id
        ~session_directory
        definition
        location
        cleanup
  in
  Result.map resolved ~f:(fun _ -> owned)
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

let published_instance instance ~final_directory =
  match instance.Workspace_instance.source_kind with
  | Temporary Session_dir ->
    let parent = Eio_posix.Low_level.realpath (Filename.dirname final_directory) in
    let final_directory = Filename.concat parent (Filename.basename final_directory) in
    let native_path = Filename.concat final_directory "workspace" in
    { instance with
      canonical_root = { instance.canonical_root with native_path }
    ; configured_root = Some native_path
    }
  | Physical | Current | Temporary System_tmp -> instance
;;
