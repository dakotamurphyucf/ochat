open! Core
module P = Agent_protocol
module Catalogs = Session_factory.Developer_catalog
module Workspace = Developer_workspace_lifetime
module Runtime = Chat_response.Agent_runtime
module Capabilities = Chat_response.Tool_capability
module Scripts = Agent_session.Runtime_script_preparation
module Prompt = Agent_session.Prompt_catalog
module Revision = Agent_session.Prompt_revision
module Definition = Agent_session.Prompt_definition
module Store = Agent_store.Developer_run_store

module Owner = struct
  exception Stop

  type failure = exn * Stdlib.Printexc.raw_backtrace

  type t =
    { stop : unit Eio.Promise.t
    ; stop_u : unit Eio.Promise.u
    ; exited : (unit, failure) result Eio.Promise.t
    ; exited_u : (unit, failure) result Eio.Promise.u
    ; stopping : bool Atomic.t
    }

  let create () =
    let stop, stop_u = Eio.Promise.create () in
    let exited, exited_u = Eio.Promise.create () in
    { stop; stop_u; exited; exited_u; stopping = Atomic.make false }
  ;;

  let signal_stop t =
    if not (Atomic.exchange t.stopping true) then Eio.Promise.resolve t.stop_u ()
  ;;

  let join t =
    Eio.Cancel.protect (fun () ->
      signal_stop t;
      Eio.Promise.await t.exited)
  ;;

  let start t ~daemon_sw ~build =
    (* [Fiber.fork] silently skips a cancelled switch, which would otherwise
       leave both preparation and joined-completion promises unresolved. *)
    Eio.Switch.check daemon_sw;
    let ready, ready_u = Eio.Promise.create () in
    Eio.Fiber.fork ~sw:daemon_sw (fun () ->
      let delivered = ref false in
      let deliver outcome =
        delivered := true;
        Eio.Promise.resolve ready_u outcome
      in
      let outcome =
        try
          Eio.Cancel.sub (fun context ->
            Eio.Switch.run (fun sw ->
              Eio.Fiber.fork_daemon ~sw (fun () ->
                Eio.Promise.await t.stop;
                Eio.Cancel.cancel context Stop;
                `Stop_daemon);
              let prepared =
                try Ok (build sw) with
                | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
              in
              deliver prepared;
              (match prepared with
               | Ok (Ok _) -> ()
               | Ok (Error _) | Error _ -> signal_stop t);
              Eio.Promise.await t.stop));
          Ok ()
        with
        | Eio.Cancel.Cancelled Stop when Atomic.get t.stopping -> Ok ()
        | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
      in
      Eio.Promise.resolve t.exited_u outcome;
      if not !delivered
      then (
        match outcome with
        | Error failure -> deliver (Error failure)
        | Ok () ->
          deliver
            (Error
               ( Invalid_argument "developer resource owner exited without preparation"
               , Stdlib.Printexc.get_raw_backtrace () ))));
    ready
  ;;
end

type t =
  { sw : Eio.Switch.t
  ; env : Eio_unix.Stdenv.base
  ; prompts : Prompt.t
  ; catalogs : Catalogs.t
  ; workspace_leases : Agent_session.Workspace_lease.t
  ; physical_roots : string list
  ; workspace_cleanup : Agent_session.Workspace_cleanup.Limits.t
  ; script_preparation : Scripts.t option
  ; tool_dir : Eio.Fs.dir_ty Eio.Path.t
  ; home : Eio.Fs.dir_ty Eio.Path.t
  ; cache : Chat_response.Cache.t
  ; approval_broker : Shell_runtime.Approval_broker.t option
  ; max_native_bindings : int
  ; child_sources : Session_factory.Child_source_policy.t option
  ; content_limits : Chatmd_prompt_content.Limits.t option
  ; artifact_store : Agent_store.Prompt_artifact_store.t
  ; agent_source_limits : Developer_agent_source_runtime.Limits.t
  ; gate : Eio.Mutex.t
  ; mutable unresolved : Workspace.t option
  }

type live =
  { owner : Owner.t
  ; host : t
  ; workspace_owner : Workspace.t
  ; revision : Revision.t
  ; definition : Definition.t
  ; workspace_definition : Agent_session.Workspace_definition.t
  ; original_actor : Operator_authorization.t
  ; runtime : Runtime.t
  ; selected : Capabilities.t
  ; managed : Chat_response.Managed_tool_registry.t
  ; sources : Developer_model_job_runner.Sources.t
  ; shell_host : Shell_runtime.Host.t
  ; manifest_authorizer : Shell_runtime.Manifest_authorizer.t
  ; approval_provider : Shell_runtime.Approval_broker.provider
  ; source_binding : Agent_session.Captured_agent_runner.Source_binding.t option
  ; agent_source_runtime : Developer_agent_source_runtime.t option
  ; mutable closing : bool
  ; mutable closed : bool
  }

let invalid message = P.Error.invalid_request message

let workspace_error = function
  | Workspace.Error.Lease error -> error
  | Store _ | Workspace _ | Unsupported_cleanup _ | Foreign_or_stale ->
    invalid "developer workspace is unavailable"
;;

let diagnostic_error diagnostics =
  match diagnostics with
  | [] -> invalid "developer native resource preparation failed"
  | _ :: _ -> invalid "developer native resource preparation failed"
;;

let create
      ~sw
      ~env
      ~prompts
      ~catalogs
      ~workspace_leases
      ~physical_roots
      ~workspace_cleanup
      ~script_preparation
      ~tool_dir
      ~home
      ~cache
      ~approval_broker
      ~max_native_bindings
      ~child_sources
      ~content_limits
      ~artifact_store
      ~agent_source_limits
  =
  if max_native_bindings <= 0
  then Error (invalid "developer native binding limit must be positive")
  else
    Ok
      { sw
      ; env
      ; prompts
      ; catalogs
      ; workspace_leases
      ; physical_roots
      ; workspace_cleanup
      ; script_preparation
      ; tool_dir
      ; home
      ; cache
      ; approval_broker
      ; max_native_bindings
      ; child_sources
      ; content_limits
      ; artifact_store
      ; agent_source_limits
      ; gate = Eio.Mutex.create ()
      ; unresolved = None
      }
;;

let cleanup_failed_workspace t workspace_owner =
  t.unresolved <- Some workspace_owner;
  Eio.Cancel.protect (fun () ->
    match Workspace.close_and_wait workspace_owner with
    | Ok () ->
      t.unresolved <- None;
      Ok ()
    | Error error -> Error (workspace_error error))
;;

let drain_unresolved_locked t =
  match t.unresolved with
  | None -> Ok ()
  | Some workspace_owner -> cleanup_failed_workspace t workspace_owner
;;

let drain_unresolved t =
  Eio.Mutex.use_rw ~protect:false t.gate (fun () -> drain_unresolved_locked t)
;;

let selected_pair t ~prompt ~workspace =
  let open Result.Let_syntax in
  let%bind entry =
    Prompt.find t.prompts prompt
    |> Result.of_option ~error:(invalid "developer prompt is unavailable")
  in
  let%bind revision =
    match entry.availability with
    | Prompt.Ready revision -> Ok revision
    | Unavailable _ | Disabled -> Error (invalid "developer prompt is unavailable")
  in
  let%bind workspace_definition =
    Catalogs.find_workspace t.catalogs workspace
    |> Result.of_option ~error:(invalid "developer workspace is unavailable")
  in
  if not (Definition.allows_workspace entry.definition workspace)
  then Error (invalid "developer prompt does not admit this workspace")
  else Ok (entry.definition, revision, workspace_definition)
;;

let same_pair t ~prompt ~workspace ~definition ~revision ~workspace_definition =
  match selected_pair t ~prompt ~workspace with
  | Error _ -> false
  | Ok (now_definition, now_revision, now_workspace) ->
    phys_equal now_definition definition
    && phys_equal now_revision revision
    && phys_equal now_workspace workspace_definition
;;

let approval_store () =
  Shell_access.Approval.create_store
    ~lookup:(fun ~now:_ ~session_id:_ _ -> Ok false)
    ~remember:(fun ~session_id:_ _ scope _ ->
      match scope with
      | Shell_access.Approval.Once -> Ok ()
      | Exact_session _ | Prefix_session _ | Durable_exact _ ->
        Error "developer shell grants are limited to one invocation")
    ()
;;

let manifest_authorizer
      t
      ~(definition : Definition.t)
      ~(workspace_definition : Agent_session.Workspace_definition.t)
      ~revision
      ~original_actor
  =
  let artifact = Revision.artifact revision in
  fun (request : Shell_runtime.Manifest_authorizer.request) ->
    let open Shell_runtime.Manifest_authorizer in
    let principal_id = (Operator_authorization.principal original_actor).id in
    match artifact.shell_manifest_sha256 with
    | None -> Reject "prompt has no retained shell manifest"
    | Some expected
      when not
             (String.equal
                expected
                (Chatmd_shell_spec.Manifest.resource_sha256 request.manifest)) ->
      Reject "compiled shell manifest does not match the prompt artifact"
    | Some _ ->
      if
        Operator_authorization.is_current original_actor
        && List.exists (Catalogs.manifest_grants t.catalogs) ~f:(fun grant ->
          Operator_manifest_grant.authorizes
            grant
            ~prompt_definition_id:definition.id
            ~workspace_definition_id:workspace_definition.id
            ~principal_id
            ~source_sha256:artifact.root_sha256
            ~manifest_sha256:request.manifest.sha256)
      then Authorize_once
      else Reject "no current operator grant authorizes this exact shell manifest"
;;

let check_prepared
      t
      ~prompt
      ~workspace
      ~definition
      ~revision
      ~workspace_definition
      ~original_actor
      workspace_owner
  =
  let open Result.Let_syntax in
  if not (Operator_authorization.is_current original_actor)
  then Error (invalid "developer actor is no longer current")
  else if not (same_pair t ~prompt ~workspace ~definition ~revision ~workspace_definition)
  then Error (invalid "developer prompt or workspace selection changed")
  else
    Workspace.check_live workspace_owner
    |> Result.map_error ~f:workspace_error
    >>= fun () ->
    Developer_method_authorization.check_now
      Agent_store.Developer_command_receipt.Kind.Live_run
      ~original:original_actor
      ~current:original_actor
;;

let prepare_on_switch t ~owner ~sw ~handle ~original_actor ~prompt ~workspace ~checkpoint =
  let open Result.Let_syntax in
  if Option.is_some t.unresolved
  then Error (invalid "developer workspace cleanup is incomplete")
  else
    Catalogs.with_admission t.catalogs ~f:(fun () ->
      let%bind definition, revision, workspace_definition =
        selected_pair t ~prompt ~workspace
      in
      let%bind () =
        Developer_method_authorization.check_now
          Agent_store.Developer_command_receipt.Kind.Live_run
          ~original:original_actor
          ~current:original_actor
      in
      let%bind workspace_owner =
        Workspace.open_live
          ~env:t.env
          ~sw
          ~handle
          ~definition:workspace_definition
          ~workspace_leases:t.workspace_leases
          ~physical_roots:t.physical_roots
          ~cleanup_limits:t.workspace_cleanup
          ~checkpoint
        |> Result.map_error ~f:workspace_error
      in
      t.unresolved <- Some workspace_owner;
      let build () =
        let%bind () =
          check_prepared
            t
            ~prompt
            ~workspace
            ~definition
            ~revision
            ~workspace_definition
            ~original_actor
            workspace_owner
        in
        let%bind () =
          Agent_store.Prompt_artifact_store.verify_tree_with_checkpoint
            ~root:(Revision.materialized_tree revision)
            (Revision.artifact revision)
            ~checkpoint
          |> Result.map_error ~f:(fun _ -> invalid "developer prompt artifact changed")
        in
        let instance = Workspace.instance workspace_owner in
        let%bind paths =
          Agent_session.Runtime_paths.create
            ~env:t.env
            ~tool_dir:(Eio.Path.native_exn t.tool_dir)
            ~workspace:instance.canonical_root.native_path
            ~prompt_dir:
              (Filename.concat
                 (Eio.Path.native_exn (Revision.materialized_tree revision))
                 (Filename.dirname (Revision.root_relative_path revision)))
            ~session_dir:(Workspace.runtime_directory workspace_owner)
            ~cache_dir:(Workspace.cache_directory workspace_owner)
            ~home:(Eio.Path.native_exn t.home)
          |> Result.map_error ~f:(fun _ ->
            invalid "developer runtime paths are unavailable")
        in
        let elements = Revision.elements revision in
        let%bind shell_host =
          Runtime.host
            ~env:t.env
            ~workspace:paths.workspace
            ~tool_dir:paths.tool_dir
            ~prompt_dir:paths.prompt_dir
            ~session_dir:paths.session_dir
            ~cache_dir:paths.cache_dir
            ~home:paths.home
            ~session_id:
              ("developer:" ^ P.Id.Developer_run.to_string (Store.owner handle).run_id)
            ~prompt_elements:elements
          |> Result.map_error ~f:diagnostic_error
        in
        let approval_provider =
          match t.approval_broker with
          | None -> Shell_runtime.Approval_broker.None_available
          | Some broker -> Shell_runtime.Approval_broker.Callback broker
        in
        let authorizer =
          manifest_authorizer
            t
            ~definition
            ~workspace_definition
            ~revision
            ~original_actor
        in
        let%bind source_binding, agent_source_runtime =
          match t.child_sources with
          | None -> Ok (None, None)
          | Some child_sources ->
            let%bind script_preparation =
              t.script_preparation
              |> Result.of_option
                   ~error:(invalid "developer child source preparation is unavailable")
            in
            let receipt = (Revision.artifact revision).source_receipt in
            let selection =
              Option.bind receipt ~f:(fun receipt ->
                Prompt.source_selection
                  t.prompts
                  (Agent_store.Prompt_source_receipt.definition_id receipt))
            in
            let source_lease =
              Agent_session.Prompt_source_lease.create receipt ~selection ~sw ~checkpoint
            in
            let source_binding =
              Session_factory.Child_source_policy.bind
                child_sources
                ~tool_contract:(Definition.tool_contract definition)
                ~content_limits:t.content_limits
                ~artifact_store:t.artifact_store
                ~source_lease
            in
            let%map source_runtime =
              Developer_agent_source_runtime.prepare
                ~selected_target:
                  Developer_agent_source_runtime.Selected_target.Literal_only
                ~source:source_binding
                ~script_preparation
                ~manifest_authorizer:authorizer
                ~approval_provider
                ~response_dir:
                  Eio.Path.(t.env#fs / Workspace.response_directory workspace_owner)
                ~env:t.env
                ~revision
                ~sw
                ~limits:t.agent_source_limits
                ~checkpoint
            in
            Some source_binding, Some source_runtime
        in
        let invocation =
          Scripts.Invocation.of_root (Definition.root_invocation definition)
        in
        let%bind resources =
          Scripts.with_revision_plan
            ~tool_contract:(Definition.tool_contract definition)
            t.script_preparation
            revision
            ~env:t.env
            ~invocation
            ~checkpoint
            ~f:(fun startup ~compilation_limits ~plan:_ ~programs ->
              let%bind () =
                check_prepared
                  t
                  ~prompt
                  ~workspace
                  ~definition
                  ~revision
                  ~workspace_definition
                  ~original_actor
                  workspace_owner
              in
              let preparation_denial = ref None in
              let check_preparation_now () =
                match
                  check_prepared
                    t
                    ~prompt
                    ~workspace
                    ~definition
                    ~revision
                    ~workspace_definition
                    ~original_actor
                    workspace_owner
                with
                | Ok () -> Ok ()
                | Error error ->
                  preparation_denial := Some error;
                  Error
                    Chat_response.Agent_runtime.
                      { code = "developer.preparation_revoked"
                      ; message = "developer native preparation authority expired"
                      ; source = None
                      }
              in
              Runtime.prepare_extensions_without_inference
                ~startup
                ~tool_contract:(Definition.tool_contract definition)
                ~compilation_limits
                ?programs
                ~checkpoint
                ~check_preparation_now
                ~native_registrations:
                  (Option.value_map
                     agent_source_runtime
                     ~default:[]
                     ~f:Developer_agent_source_runtime.registrations)
                ~sw
                ~host:shell_host
                ~platform:(Runtime.platform ())
                ~prompt_elements:elements
                ~manifest_authorizer:authorizer
                ~approval_provider
                ~approval_store:(approval_store ())
                ()
              |> Result.map_error ~f:(fun diagnostics ->
                match !preparation_denial with
                | Some error -> error
                | None -> diagnostic_error diagnostics))
        in
        let selected =
          Chat_response.Managed_tool_registry.capabilities resources.managed
        in
        if Capabilities.length selected > t.max_native_bindings
        then Error (invalid "developer native binding limit exceeded")
        else (
          let%bind () =
            check_prepared
              t
              ~prompt
              ~workspace
              ~definition
              ~revision
              ~workspace_definition
              ~original_actor
              workspace_owner
          in
          let sources =
            Developer_model_job_runner.Sources.create
              ~env:t.env
              ~prompt_dir:paths.prompt_dir
              ~tool_dir:paths.tool_dir
              ~response_dir:
                Eio.Path.(t.env#fs / Workspace.response_directory workspace_owner)
              ~cache:t.cache
              ~manifest_authorizer:authorizer
              ~approval_provider
          in
          Ok
            { owner
            ; host = t
            ; workspace_owner
            ; revision
            ; definition
            ; workspace_definition
            ; original_actor
            ; runtime = resources.native
            ; selected
            ; managed = resources.managed
            ; sources
            ; shell_host
            ; manifest_authorizer = authorizer
            ; approval_provider
            ; source_binding
            ; agent_source_runtime
            ; closing = false
            ; closed = false
            })
      in
      match build () with
      | Ok live -> Ok live
      | Error _ as error -> error)
    |> Result.map_error ~f:(function
      | Catalogs.Admission.Error.Busy -> invalid "developer catalog is busy"
      | Publication_uncertain -> invalid "developer catalog publication is uncertain"
      | Foreign_domain -> invalid "developer catalog belongs to another domain")
    |> Result.join
;;

let preparation_cleanup_error original =
  P.Error.create
    P.Error.Persistence_error
    ~message:"developer resource preparation cleanup is incomplete"
    ~retryable:true
    ~data:(`Object [ "preparation", P.Error.to_json original ])
    ()
;;

let prepare_locked t ~handle ~original_actor ~prompt ~workspace ~checkpoint ~retain =
  let owner = Owner.create () in
  let ready =
    Eio.Cancel.protect (fun () ->
      Owner.start owner ~daemon_sw:t.sw ~build:(fun sw ->
        prepare_on_switch
          t
          ~owner
          ~sw
          ~handle
          ~original_actor
          ~prompt
          ~workspace
          ~checkpoint))
  in
  let join_and_drain () =
    let joined = Owner.join owner in
    let drained = drain_unresolved_locked t in
    joined, drained
  in
  let rethrow_original exn backtrace =
    let _cleanup =
      try Some (join_and_drain ()) with
      | _ -> None
    in
    Stdlib.Printexc.raise_with_backtrace exn backtrace
  in
  match Eio.Promise.await ready with
  | Ok (Ok live) ->
    (match
       Eio.Cancel.protect (fun () ->
         match t.unresolved with
         | Some workspace_owner when phys_equal workspace_owner live.workspace_owner ->
           retain live;
           t.unresolved <- None
         | None | Some _ ->
           raise
             (Invalid_argument
                "developer live resource transfer lost its physical workspace"))
     with
     | () -> Ok live
     | exception exn -> rethrow_original exn (Stdlib.Printexc.get_raw_backtrace ()))
  | Ok (Error original) ->
    let joined, drained = join_and_drain () in
    (match joined, drained with
     | Ok (), Ok () -> Error original
     | (Ok () | Error _), (Ok () | Error _) -> Error (preparation_cleanup_error original))
  | Error (exn, backtrace) -> rethrow_original exn backtrace
  | exception exn -> rethrow_original exn (Stdlib.Printexc.get_raw_backtrace ())
;;

let prepare t ~handle ~original_actor ~prompt ~workspace ~checkpoint ~retain =
  Eio.Mutex.use_rw ~protect:false t.gate (fun () ->
    prepare_locked t ~handle ~original_actor ~prompt ~workspace ~checkpoint ~retain)
;;

let workspace t = Workspace.instance t.workspace_owner
let conflict_domain t = Workspace.conflict_domain t.workspace_owner
let native_runtime t = t.runtime
let native_registry t = t.selected
let selected_native t = t.selected
let managed_definition t = t.managed
let model_sources t = t.sources

let check_current t =
  if t.closing || t.closed
  then Error (invalid "developer native resources are closed")
  else
    check_prepared
      t.host
      ~prompt:t.definition.id
      ~workspace:t.workspace_definition.id
      ~definition:t.definition
      ~revision:t.revision
      ~workspace_definition:t.workspace_definition
      ~original_actor:t.original_actor
      t.workspace_owner
;;

let bind_agent_sources t ~root ~models ~checkpoint =
  let open Result.Let_syntax in
  let%bind () = check_current t in
  match t.agent_source_runtime with
  | None -> Ok ()
  | Some prepared ->
    (match Developer_agent_source_runtime.registrations prepared, models with
     | [], None -> Ok ()
     | _ :: _, None ->
       Error (invalid "developer Agent source requires an admitted model provider")
     | _, Some models ->
       Developer_agent_source_runtime.bind
         prepared
         ~root
         ~models
         ~agent_runtime:t.runtime
         ~run_selected_target:None
         ~checkpoint)
;;

let close_and_wait t =
  if t.closed
  then Ok ()
  else (
    t.closing <- true;
    Option.iter t.agent_source_runtime ~f:Developer_agent_source_runtime.revoke;
    let joined = Owner.join t.owner in
    let closed = Workspace.close_and_wait t.workspace_owner in
    (match closed with
     | Ok () -> t.closed <- true
     | Error _ -> ());
    match joined, closed with
    | Ok (), Ok () -> Ok ()
    | Ok (), Error error -> Error (workspace_error error)
    | Error (exn, backtrace), (Ok () | Error _) ->
      Stdlib.Printexc.raise_with_backtrace exn backtrace)
;;
