open! Core

let protocol_of_store error =
  Agent_protocol.Error.create
    Persistence_error
    ~message:(Sexp.to_string_hum ([%sexp_of: Agent_store.Store_error.t] error))
    ~retryable:false
    ()
;;

let identity principal request session_id timestamp =
  let spec = Agent_protocol.Session_seed.Request.spec request in
  Agent_session.Session_state.Identity.
    { session_id
    ; display_name = spec.display_name
    ; creating_principal = Some principal.Agent_protocol.Principal.id
    ; created_at = timestamp
    ; updated_at = timestamp
    ; labels = spec.labels
    ; generation = 0
    ; metadata_revision = 0L
    ; organization = Agent_protocol.Session_organization.Values.empty
    }
;;

let workspace_projection store workspace ~instance_id ~session_id ~timestamp =
  let final_directory =
    Agent_store.Data_root.session_path
      (Agent_store.Session_store.data_root store)
      session_id
  in
  let configured_root =
    Agent_session.Workspace_resolver.planned_root
      workspace
      ~instance_id
      ~session_directory:final_directory
  in
  let canonical () =
    match workspace.Agent_session.Workspace_definition.source with
    | Physical _ -> Eio_posix.Low_level.realpath configured_root
    | Temporary { location = System_tmp; _ } ->
      Filename.concat
        (Eio_posix.Low_level.realpath (Filename.dirname configured_root))
        (Filename.basename configured_root)
    | Temporary { location = Session_dir; _ } ->
      Filename.concat
        (Filename.concat
           (Eio_posix.Low_level.realpath (Filename.dirname final_directory))
           (Filename.basename final_directory))
        "workspace"
  in
  try
    Ok
      (Agent_session.Workspace_instance.creation_projection
         workspace
         ~instance_id
         ~configured_root
         ~canonical_root:(canonical ())
         ~created_at:timestamp)
  with
  | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
    Error
      (protocol_of_store
         (Agent_store.Store_error.of_exn
            ~operation:"plan seed workspace"
            ~path:configured_root
            exn))
;;

type t =
  { env : Eio_unix.Stdenv.base
  ; store : Agent_store.Session_store.t
  ; instance_id : Agent_protocol.Id.Workspace_instance.t
  ; workspace : Agent_session.Workspace_definition.t
  ; final_directory : string
  ; prelayout : Session_seed_admission.Prelayout.t
  }

let prepare
      recipe
      ~env
      ~store
      ~principal
      ~request
      ~command_audit
      ~timestamp
      ~limits
      ~max_work_steps
      ~checkpoint
  =
  let open Result.Let_syntax in
  checkpoint ();
  let session_id = Agent_protocol.Id.Session.create () in
  let instance_id = Agent_protocol.Id.Workspace_instance.create () in
  let workspace = Session_seed_admission.workspace recipe in
  let%bind workspace_projection =
    workspace_projection store workspace ~instance_id ~session_id ~timestamp
  in
  let%map prelayout =
    Session_seed_admission.Prelayout.admit
      recipe
      ~identity:(identity principal request session_id timestamp)
      ~workspace_instance_id:instance_id
      ~workspace_projection
      ~command_audit
      ~limits
      ~max_work_steps
      ~checkpoint
  in
  { env
  ; store
  ; instance_id
  ; workspace
  ; prelayout
  ; final_directory =
      Agent_store.Data_root.session_path
        (Agent_store.Session_store.data_root store)
        session_id
  }
;;

let close_handle t handle =
  try
    Eio.Cancel.protect (fun () ->
      ignore
        (Agent_store.Session_store.close_session t.store handle
         : (unit, Agent_store.Store_error.t) Result.t))
  with
  | _ -> ()
;;

let install t ~sw ~physical_roots ~checkpoint =
  let open Result.Let_syntax in
  let state = ref None in
  let workspace_owner = ref None in
  let publication = ref None in
  let release_workspace () =
    match !workspace_owner with
    | None -> Ok ()
    | Some owned ->
      if
        Option.exists
          !publication
          ~f:Agent_store.Session_store.Initial_projection.installed
      then (
        Agent_session.Workspace_resolver.Owned.transfer owned;
        Ok ())
      else Agent_session.Workspace_resolver.Owned.close owned
  in
  let install () =
    Agent_store.Session_store.create_session_initialized
      t.store
      ~sw
      ~transaction_id:(Agent_protocol.Id.Transaction.create ())
      ~actor_lock_nonce:Agent_protocol.Id.Transaction.(create () |> to_string)
      ~initialize:(fun ~staging_directory ->
        let open Result.Let_syntax in
        let%bind owned =
          Agent_session.Workspace_resolver.resolve_owned
            ~sw
            ~env:t.env
            ~physical_roots
            ~instance_id:t.instance_id
            ~session_directory:staging_directory
            t.workspace
        in
        workspace_owner := Some owned;
        let instance =
          Agent_session.Workspace_resolver.Owned.instance owned
          |> fun instance ->
          Agent_session.Workspace_resolver.published_instance
            instance
            ~final_directory:t.final_directory
        in
        let%bind admitted =
          Session_seed_admission.Prelayout.state
            t.prelayout
            ~workspace:instance
            ~checkpoint
          |> Result.map_error ~f:(fun error ->
            Agent_store.Store_error.Corrupt error.message)
        in
        state := Some admitted;
        let%map initial =
          Agent_store.Session_store.Initial_projection.create
            ~metadata:(Session_projection.metadata admitted)
            ~entry:(Session_projection.index_entry admitted)
        in
        (* Retain the exact store-owned evidence even if installation later throws. *)
        publication := Some initial;
        initial)
  in
  let%bind handle =
    match install () with
    | result ->
      (match release_workspace () with
       | exception exn ->
         let trace = Backtrace.Exn.most_recent () in
         (match result with
          | Error _ ->
            (* The exact lease retains this cleanup exception for release/report. *)
            result
          | Ok handle ->
            close_handle t handle;
            Exn.raise_with_original_backtrace exn trace)
       | Ok () -> result
       | Error cleanup ->
         (match result with
          | Error _ -> ()
          | Ok handle -> close_handle t handle);
         let primary =
           match result with
           | Error primary -> [%sexp_of: Agent_store.Store_error.t] primary
           | Ok _ -> [%sexp "workspace cleanup failed without a primary rejection"]
         in
         Error
           (Agent_store.Store_error.Corrupt
              (Sexp.to_string_hum
                 [%message
                   "seed workspace cleanup failed"
                     (primary : Sexp.t)
                     (cleanup : Agent_store.Store_error.t)])))
    | exception exn ->
      let trace = Backtrace.Exn.most_recent () in
      (* Failed native cleanup retains this exact lease for switch-release retry;
          it never substitutes a new directory or hides the original exception. *)
      (match release_workspace () with
       | Ok () | Error _ -> ()
       | exception _ ->
         (* The same Owned lease retained the cleanup exception/backtrace for
             switch-release reporting after its retry. Preserve this primary. *)
         ());
      Exn.raise_with_original_backtrace exn trace
  in
  match !state with
  | Some state -> Ok (handle, state)
  | None ->
    (* Store cannot return a Handle without a successful initializer. Retain the
       installed evidence and release the native lock even on invariant failure. *)
    close_handle t handle;
    Error (Agent_store.Store_error.Corrupt "seed initializer returned no admitted state")
;;
