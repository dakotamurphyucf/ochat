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
      ~adopt_unverified
      ~adopt_provisional
      ~mark_provisional
      ~bounded_provisional
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
    if !owned && not bounded_provisional
    then (
      try Eio.Cancel.protect (fun () -> Eio.Path.rmtree (eio_path env path)) with
      | _ -> ())
  in
  Exn.protect ~finally:close_failed ~f:(fun () ->
    let resolve () =
      if exclusive || bounded_provisional
      then
        Eio.Cancel.protect (fun () ->
          Eio.Path.mkdir ~perm:0o700 (eio_path env path);
          owned := true;
          if bounded_provisional
          then (
            (* The exclusive mkdir succeeded. Until stat captures its inode,
               keep an owner receipt that refuses deletion and lease release. *)
            adopt_unverified path;
            let stat = Eio.Path.stat ~follow:false (eio_path env path) in
            adopt_provisional path stat.dev stat.ino))
      else Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 (eio_path env path);
      Eio.Cancel.protect (fun () ->
        write_ownership_marker ~env ~instance_id path;
        mark_provisional ());
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
      ~adopt_unverified:ignore
      ~adopt_provisional:(fun _ _ _ -> ())
      ~mark_provisional:(fun () -> ())
      ~bounded_provisional:false
      ~env
      ~instance_id
      ~session_directory
      definition
      location
      cleanup
;;

module Owned = struct
  exception Cleanup_failed of Agent_store.Store_error.t

  type provisional =
    | Unverified of string
    | Verified of string * int64 * int64 * bool * bool

  type t =
    { env : Eio_unix.Stdenv.base
    ; domain : int
    ; physical_roots : string list
    ; managed_root : string option
    ; mutable acquired : Workspace_instance.t option
    ; mutable provisional : provisional option
    ; mutable transferred : bool
    ; mutable cleanup_exception : (exn * Backtrace.t) option
    ; cleanup_limits : Workspace_cleanup.Limits.t option
    ; cleanup_checkpoint : unit -> unit
    ; cleanup_has_active_lease : conflict_domain:string -> bool
    ; cleanup_progress : Workspace_cleanup.Progress.t
    ; cleanup_on_complete : unit -> unit
    ; mutable cleanup_notified : bool
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

  let close_resources_unprotected t =
    check_domain_exn t;
    if t.transferred
    then Ok ()
    else (
      match t.acquired with
      | None ->
        (match t.provisional, t.cleanup_limits with
         | None, _ -> Ok ()
         | Some (Unverified _), _ ->
           Error
             (Agent_store.Store_error.Corrupt
                "provisional workspace identity was not established")
         | ( Some (Verified (path, device, inode, marker_created, marker_removed))
           , Some limits ) ->
           let native = eio_path t.env path in
           let stat = Eio.Path.stat ~follow:false native in
           if not (Int64.equal stat.dev device && Int64.equal stat.ino inode)
           then
             Error
               (Agent_store.Store_error.Corrupt "provisional workspace identity changed")
           else
             let open Result.Let_syntax in
             let%bind () =
               Agent_store.Bounded_tree_cleanup.remove_children
                 ~env:t.env
                 ~root:path
                 ~expected_root:(device, inode)
                 ~marker_name:Workspace_ownership.marker
                 ~limits:(Workspace_cleanup.Limits.tree limits)
                 ~checkpoint:t.cleanup_checkpoint
             in
             if marker_created && not marker_removed
             then (
               Eio.Path.unlink
                 (eio_path t.env (Filename.concat path Workspace_ownership.marker));
               t.provisional
               <- Some (Verified (path, device, inode, marker_created, true)));
             Eio.Path.rmdir native;
             t.provisional <- None;
             t.transferred <- true;
             Ok ()
         | Some (Verified _), None ->
           Error
             (Agent_store.Store_error.Corrupt
                "provisional workspace lacks bounded cleanup"))
      | Some instance ->
        (match instance.Workspace_instance.source_kind, t.managed_root with
         | Temporary (System_tmp | Session_dir), Some managed_root ->
           (match t.cleanup_limits with
            | None
              when match instance.source_kind with
                   | Temporary System_tmp -> true
                   | Physical | Current | Temporary Session_dir -> false ->
              Workspace_cleanup.remove
                ~env:t.env
                ~protected_roots:
                  { data_root = managed_root
                  ; physical_workspaces = t.physical_roots
                  ; managed_roots = [ managed_root ]
                  }
                ~expected_path:instance.canonical_root.native_path
                ~has_active_lease:t.cleanup_has_active_lease
                ~now:instance.created_at
                instance
            | None -> Ok instance
            | Some limits ->
              Workspace_cleanup.remove_bounded
                ~env:t.env
                ~protected_roots:
                  { data_root = managed_root
                  ; physical_workspaces = t.physical_roots
                  ; managed_roots = [ managed_root ]
                  }
                ~expected_path:instance.canonical_root.native_path
                ~has_active_lease:t.cleanup_has_active_lease
                ~now:instance.created_at
                ~limits
                ~progress:t.cleanup_progress
                ~checkpoint:t.cleanup_checkpoint
                instance)
           |> Result.map ~f:(fun _ -> t.transferred <- true)
         | (Physical | Current), _ ->
           t.transferred <- true;
           Ok ()
         | Temporary Session_dir, None ->
           Error
             (Agent_store.Store_error.Corrupt
                "owned temporary workspace has no managed root")
         | Temporary System_tmp, None ->
           Error
             (Agent_store.Store_error.Corrupt
                "owned temporary workspace has no managed root")))
  ;;

  let close_resources t =
    match t.cleanup_limits with
    | None -> Eio.Cancel.protect (fun () -> close_resources_unprotected t)
    | Some _ -> close_resources_unprotected t
  ;;

  let close t =
    match close_resources t with
    | Ok () as result ->
      if not t.cleanup_notified
      then (
        t.cleanup_on_complete ();
        t.cleanup_notified <- true);
      t.cleanup_exception <- None;
      result
    | Error _ as result -> result
    | exception exn ->
      let trace = Backtrace.Exn.most_recent () in
      t.cleanup_exception <- Some (exn, trace);
      Exn.raise_with_original_backtrace exn trace
  ;;
end

let resolve_owned
      ?cleanup_limits
      ?(cleanup_checkpoint = fun () -> ())
      ?(cleanup_has_active_lease = fun ~conflict_domain:_ -> false)
      ?(cleanup_on_complete = fun () -> ())
      ~sw
      ~env
      ~physical_roots
      ~instance_id
      ~session_directory
      definition
  =
  (try Eio.Switch.check sw with
   | exn ->
     (* No owner has been registered and no workspace acquisition has begun.
        Release any caller-held provisional lease at this sole early exit. *)
     cleanup_on_complete ();
     raise exn);
  let owned =
    Owned.
      { env
      ; domain = (Stdlib.Domain.self () :> int)
      ; physical_roots
      ; managed_root =
          (match definition.Workspace_definition.source with
           | Temporary { location = Session_dir; _ } when Option.is_some cleanup_limits ->
             Some session_directory
           | Temporary { managed_root; _ } -> managed_root
           | Physical _ -> None)
      ; acquired = None
      ; provisional = None
      ; transferred = false
      ; cleanup_exception = None
      ; cleanup_limits
      ; cleanup_checkpoint
      ; cleanup_has_active_lease
      ; cleanup_progress = Workspace_cleanup.Progress.create ()
      ; cleanup_on_complete
      ; cleanup_notified = false
      }
  in
  (try
     Eio.Switch.on_release sw (fun () ->
       let previous = owned.Owned.cleanup_exception in
       let result = Owned.close owned in
       match previous, cleanup_limits with
       | Some _, Some _ when Result.is_ok result -> ()
       | Some (exn, trace), _ -> Exn.raise_with_original_backtrace exn trace
       | None, _ ->
         (match result with
          | Ok () -> ()
          | Error failure -> raise (Owned.Cleanup_failed failure)))
   with
   | exn ->
     let trace = Backtrace.Exn.most_recent () in
     (* Registration failed before any workspace acquisition. The switch owns
        no cleanup callback in this case, so release the provisional lease. *)
     if not owned.cleanup_notified
     then (
       cleanup_on_complete ();
       owned.cleanup_notified <- true);
     Exn.raise_with_original_backtrace exn trace);
  let adopt instance =
    owned.acquired <- Some instance;
    owned.provisional <- None
  in
  let adopt_unverified path = owned.provisional <- Some (Owned.Unverified path) in
  let adopt_provisional path device inode =
    owned.provisional <- Some (Owned.Verified (path, device, inode, false, false))
  in
  let mark_provisional () =
    owned.provisional
    <- Option.map owned.provisional ~f:(function
         | Owned.Unverified path -> Owned.Unverified path
         | Owned.Verified (path, device, inode, _, marker_removed) ->
           Owned.Verified (path, device, inode, true, marker_removed))
  in
  let resolved =
    try
      Eio.Switch.check sw;
      match definition.Workspace_definition.source with
      | Physical { configured_root } ->
        resolve_physical ~env ~instance_id definition configured_root
        |> Result.map ~f:(fun instance ->
          adopt instance;
          instance)
      | Temporary { location; cleanup; _ } ->
        resolve_temporary
          ~adopt
          ~adopt_unverified
          ~adopt_provisional
          ~mark_provisional
          ~bounded_provisional:(Option.is_some cleanup_limits)
          ~env
          ~instance_id
          ~session_directory
          definition
          location
          cleanup
    with
    | exn ->
      let trace = Backtrace.Exn.most_recent () in
      (try
         Eio.Cancel.protect (fun () ->
           ignore (Owned.close owned : (unit, Agent_store.Store_error.t) result))
       with
       | _ -> ());
      Exn.raise_with_original_backtrace exn trace
  in
  match resolved with
  | Ok _ -> Ok owned
  | Error error ->
    ignore (Owned.close owned : (unit, Agent_store.Store_error.t) result);
    Error error
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
