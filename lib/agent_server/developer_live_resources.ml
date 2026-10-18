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
module Root = Developer_root_owner
module Saved = Agent_session.Captured_agent_runner.Claimed_developer.Selected
module Target = Saved.Target
module Content_target = Agent_session.Captured_agent_runner.Selected_content_target
module Model_target = Agent_session.Captured_agent_runner.Selected_model_target
module Source_binding = Agent_session.Captured_agent_runner.Source_binding

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
  ; remote_content : Developer_child_remote_content.t option
  ; artifact_store : Agent_store.Prompt_artifact_store.t
  ; agent_source_limits : Developer_agent_source_runtime.Limits.t
  ; gate : Eio.Mutex.t
  ; mutable unresolved : Workspace.t option
  }

type live =
  { owner : Owner.t
  ; switch : Eio.Switch.t
  ; host : t
  ; workspace_owner : Workspace.t
  ; paths : Agent_session.Runtime_paths.t
  ; response_dir : Eio.Fs.dir_ty Eio.Path.t
  ; shell_owner_id : string
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
  ; mutable selected_content_runner :
      Agent_session.Captured_agent_runner.selected_content_runner option
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
      ~remote_content
      ~artifact_store
      ~agent_source_limits
  =
  let open Result.Let_syntax in
  let%bind remote_content =
    match remote_content with
    | None -> Ok None
    | Some policy ->
      Developer_child_remote_content.create
        ~policy
        ~net:(Eio.Stdenv.net env)
        ~clock:(Eio.Stdenv.mono_clock env)
      |> Result.map ~f:Option.some
  in
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
      ; remote_content
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
        let response_dir =
          Eio.Path.(t.env#fs / Workspace.response_directory workspace_owner)
        in
        let shell_owner_id =
          "developer:" ^ P.Id.Developer_run.to_string (Store.owner handle).run_id
        in
        let%bind shell_host =
          Runtime.host
            ~env:t.env
            ~workspace:paths.workspace
            ~tool_dir:paths.tool_dir
            ~prompt_dir:paths.prompt_dir
            ~session_dir:paths.session_dir
            ~cache_dir:paths.cache_dir
            ~home:paths.home
            ~session_id:shell_owner_id
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
                  Developer_agent_source_runtime.Selected_target.Bound_runner
                ~source:source_binding
                ~script_preparation
                ~manifest_authorizer:authorizer
                ~approval_provider
                ~response_dir
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
              ~response_dir
              ~cache:t.cache
              ~manifest_authorizer:authorizer
              ~approval_provider
          in
          Ok
            { owner
            ; switch = sw
            ; host = t
            ; workspace_owner
            ; paths
            ; response_dir
            ; shell_owner_id
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
            ; selected_content_runner = None
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

let check_pair_now t =
  if
    same_pair
      t.host
      ~prompt:t.definition.id
      ~workspace:t.workspace_definition.id
      ~definition:t.definition
      ~revision:t.revision
      ~workspace_definition:t.workspace_definition
  then Ok ()
  else Error (invalid "developer prompt or workspace selection changed")
;;

let check_source_bound_now live ~root =
  let open Result.Let_syntax in
  let%bind source_runtime =
    live.agent_source_runtime
    |> Result.of_option ~error:(invalid "developer saved-Agent source is unavailable")
  in
  Developer_agent_source_runtime.bound_to source_runtime ~root ~agent_runtime:live.runtime
;;

type child_target =
  | Saved of Target.t
  | Content of Content_target.t
  | Model of Model_target.t

type child_prepared =
  { live : live
  ; root : Root.run
  ; target : child_target
  ; saved_ancestor : Target.t
  ; content_ports : Source_binding.Content_ports.t
  ; revision : Revision.t
  ; switch : Eio.Switch.t
  ; active : bool ref
  ; runtime : Runtime.t
  ; selected : Capabilities.t
  ; managed : Chat_response.Managed_tool_registry.t
  ; needs_dynamic_source : bool
  }

module Child_prepared = struct
  type t = child_prepared

  module Admission_data = struct
    type mode =
      | Direct
      | Descendant of Agent_session.Native_tool_invocation.selected_borrowed

    type t =
      { prepared : child_prepared
      ; witness : Root.Current.t
      ; mode : mode
      }
  end

  let matches_target t target =
    match t.target with
    | Saved actual -> phys_equal actual target
    | Content _ | Model _ -> false
  ;;

  let matches_content_target t target =
    match t.target with
    | Content actual -> phys_equal actual target
    | Saved _ | Model _ -> false
  ;;

  let matches_model_target t target =
    match t.target with
    | Model actual -> phys_equal actual target
    | Saved _ | Content _ -> false
  ;;

  let check_target t =
    match t.target with
    | Saved target -> Target.check target
    | Content target -> Content_target.check target
    | Model target -> Model_target.check target
  ;;

  let check_target_now t =
    match t.target with
    | Saved target -> Target.check_now target
    | Content target -> Content_target.check_now target
    | Model target -> Model_target.check_now target
  ;;

  let check_target_descendant_now t ~child =
    match t.target with
    | Saved target -> Target.check_descendant_now target ~child
    | Content target -> Content_target.check_descendant_now target ~child
    | Model target -> Model_target.check_descendant_now target ~child
  ;;

  let revision t = t.revision
  let runtime t = t.runtime
  let selected_native t = t.selected
  let managed_definition t = t.managed
  let needs_dynamic_source t = t.needs_dynamic_source
  let remote_content t = t.live.host.remote_content
  let content_limits t = t.live.host.content_limits
  let tool_contract t = Definition.tool_contract t.live.definition
  let response_dir t = t.live.response_dir

  let check_scope_now t witness =
    Eio.Switch.check t.switch;
    Eio.Switch.check t.live.switch;
    if (not !(t.active)) || t.live.closing || t.live.closed
    then Error (invalid "developer child resource scope has expired")
    else if not (Operator_authorization.is_current t.live.original_actor)
    then Error (invalid "developer actor is no longer current")
    else
      let open Result.Let_syntax in
      let%bind () = check_pair_now t.live in
      let%bind () = check_source_bound_now t.live ~root:t.root in
      Root.check_effect_now t.root witness
  ;;

  let check_source_full t =
    let open Result.Let_syntax in
    let%bind () = check_source_bound_now t.live ~root:t.root in
    check_current t.live
  ;;

  let check_direct_full t =
    let open Result.Let_syntax in
    let%bind () = check_target t in
    let%bind borrowed = Agent_session.Native_tool_invocation.borrow_selected () in
    let%bind () =
      if Saved.matches_borrowed (Target.saved t.saved_ancestor) borrowed
      then
        Root.Model_inference.check_saved_agent_source
          t.root
          ~selected:(Target.saved t.saved_ancestor)
      else (
        let%bind _ = Root.current_after_mutations t.root in
        Root.Model_inference.check_saved_agent_source_descendant_now
          t.root
          ~selected:(Target.saved t.saved_ancestor)
          ~child:borrowed)
    in
    check_source_full t
  ;;

  let check_descendant_full t ~child =
    let open Result.Let_syntax in
    let%bind () = check_source_full t in
    let%bind _ = Root.current_after_mutations t.root in
    let%bind () = check_target_descendant_now t ~child in
    Root.Model_inference.check_saved_agent_source_descendant_now
      t.root
      ~selected:(Target.saved t.saved_ancestor)
      ~child
  ;;

  let check_admission_now (admission : Admission_data.t) =
    let open Result.Let_syntax in
    let t = admission.prepared in
    let%bind () = check_scope_now t admission.witness in
    match admission.mode with
    | Admission_data.Direct -> check_target_now t
    | Admission_data.Descendant child ->
      let%bind () = check_target_descendant_now t ~child in
      Root.Model_inference.check_saved_agent_source_descendant_now
        t.root
        ~selected:(Target.saved t.saved_ancestor)
        ~child
  ;;

  let refresh_with t ~mode ~check_full =
    let open Result.Let_syntax in
    let%bind () = check_full () in
    let%bind _ = Root.current_after_mutations t.root in
    let%bind () = check_full () in
    let%bind witness = Root.current_after_mutations t.root in
    let admission : Admission_data.t = { prepared = t; witness; mode } in
    let%map () = check_admission_now admission in
    admission
  ;;

  let refresh t =
    refresh_with t ~mode:Admission_data.Direct ~check_full:(fun () -> check_direct_full t)
  ;;

  let refresh_descendant t ~child =
    refresh_with t ~mode:(Admission_data.Descendant child) ~check_full:(fun () ->
      check_descendant_full t ~child)
  ;;

  module Admission = struct
    type t = Admission_data.t

    let check_now = check_admission_now

    let checked_root_now t =
      let open Result.Let_syntax in
      let%map () = check_now t in
      t.witness
    ;;
  end

  let content_source t =
    let open Result.Let_syntax in
    let%bind source =
      t.live.source_binding
      |> Result.of_option ~error:(invalid "selected child has no original source binding")
    in
    let%bind admission = refresh t in
    let%bind () = Admission.check_now admission in
    let%bind port =
      match t.target with
      | Saved target -> Source_binding.selected_content_source source ~target
      | Content target -> Source_binding.selected_descendant_content_source source ~target
      | Model target -> Source_binding.selected_model_content_source source ~target
    in
    let%bind current = refresh t in
    let%map () = Admission.check_now current in
    port
  ;;

  let run_content_agent t ~side ~declaration ~ctx =
    let open Result.Let_syntax in
    let%bind admission = refresh t in
    let%bind () = Admission.check_now admission in
    let%bind result =
      Source_binding.Content_ports.run t.content_ports ~side ~declaration ~ctx
    in
    let%bind after = refresh t in
    let%map () = Admission.check_now after in
    result
  ;;

  let with_selected_model_member t ~budget ~side ~reference ~mode ~checkpoint ~f =
    let open Result.Let_syntax in
    let%bind binding =
      t.live.source_binding
      |> Result.of_option ~error:(invalid "selected Model has no original source binding")
    in
    let%bind source = content_source t in
    let%bind member =
      Source_binding.Content_source.select_local_member
        source
        ~side
        ~reference
        ~mode
        ~checkpoint
    in
    let%bind script_preparation =
      t.live.host.script_preparation
      |> Result.of_option
           ~error:(invalid "selected Model script preparation is unavailable")
    in
    let%bind selected =
      Developer_child_content_projection.Budget.with_executable_source_read
        budget
        ~f:(fun ~max_input_bytes ~max_work_steps ~on_acquire ->
          let open Result.Let_syntax in
          let%bind admission = refresh t in
          let%bind () = Admission.check_now admission in
          let%bind selected =
            Source_binding.select_model_member
              binding
              ~source
              ~member
              ~script_preparation
              ~env:t.live.host.env
              ~max_input_bytes
              ~max_work_steps
              ~checkpoint
              ~on_acquire
          in
          let%bind after = refresh t in
          let%bind () = Admission.check_now after in
          let%bind cost =
            Developer_child_content_projection.Budget.Executable_cost.create
              ~input_bytes:(Source_binding.Selected_model_source.input_bytes selected)
              ~work_steps:(Source_binding.Selected_model_source.work_steps selected)
            |> Result.map_error ~f:(fun _ ->
              invalid "selected Model source reported invalid cost")
          in
          Ok (selected, cost))
    in
    Source_binding.Content_ports.with_selected_model_target
      t.content_ports
      ~source
      ~member
      ~selected
      ~f
  ;;
end

let with_child
      live
      ~root
      ~target
      ~saved_ancestor
      ~daemon_sw
      ~startup
      ~compilation_limits
      ~programs
      ~input
      ~f
  =
  let open Result.Let_syntax in
  let checkpoint () = Chat_response.Startup_compilation.checkpoint startup in
  let check_target () =
    match target with
    | Saved target -> Target.check target
    | Content target -> Content_target.check target
    | Model target -> Model_target.check target
  in
  let check_target_now () =
    match target with
    | Saved target -> Target.check_now target
    | Content target -> Content_target.check_now target
    | Model target -> Model_target.check_now target
  in
  let revision =
    match target with
    | Saved target -> Target.revision target
    | Content target -> Content_target.revision target
    | Model target -> Model_target.revision target
  in
  let check_full () =
    let%bind () = check_source_bound_now live ~root in
    let%bind () = check_target () in
    let%bind () =
      let saved = Target.saved saved_ancestor in
      let%bind borrowed = Agent_session.Native_tool_invocation.borrow_selected () in
      if Saved.matches_borrowed saved borrowed
      then Root.Model_inference.check_saved_agent_source root ~selected:saved
      else (
        let%bind _ = Root.current_after_mutations root in
        Root.Model_inference.check_saved_agent_source_descendant_now
          root
          ~selected:saved
          ~child:borrowed)
    in
    check_current live
  in
  let owner = Owner.create () in
  let ready =
    Owner.start owner ~daemon_sw ~build:(fun sw ->
      let%bind () = check_full () in
      let%bind parsed =
        Revision.parser_context revision
        |> Result.of_option
             ~error:(invalid "selected child has no retained parser context")
      in
      let%bind needs_dynamic_source =
        Agent_session.Developer_child_preflight.classify_selected
          ~revision
          ~parsed
          ~input
          ~checkpoint
      in
      let%bind () = check_full () in
      let%bind () =
        Agent_store.Prompt_artifact_store.verify_tree_with_checkpoint
          ~root:(Revision.materialized_tree revision)
          (Revision.artifact revision)
          ~checkpoint
        |> Result.map_error ~f:(fun _ -> invalid "developer child artifact changed")
      in
      let%bind () = check_full () in
      let prompt_dir =
        Eio.Path.(
          Revision.materialized_tree revision
          / Filename.dirname (Revision.root_relative_path revision))
      in
      let elements = Revision.elements revision in
      let%bind host =
        Runtime.host
          ~env:live.host.env
          ~workspace:live.paths.workspace
          ~tool_dir:live.paths.tool_dir
          ~prompt_dir
          ~session_dir:live.paths.session_dir
          ~cache_dir:live.paths.cache_dir
          ~home:live.paths.home
          ~session_id:live.shell_owner_id
          ~prompt_elements:elements
        |> Result.map_error ~f:diagnostic_error
      in
      let authorizer =
        manifest_authorizer
          live.host
          ~definition:live.definition
          ~workspace_definition:live.workspace_definition
          ~revision
          ~original_actor:live.original_actor
      in
      let preparation_denial = ref None in
      let check_preparation_now () =
        let check () =
          let%bind () = check_full () in
          let%bind witness = Root.current_after_mutations root in
          let%bind () = check_pair_now live in
          let%bind () =
            if Operator_authorization.is_current live.original_actor
            then Ok ()
            else Error (invalid "developer actor is no longer current")
          in
          let%bind () = check_source_bound_now live ~root in
          let%bind () = check_target_now () in
          Root.check_effect_now root witness
        in
        match check () with
        | Ok () -> Ok ()
        | Error error ->
          preparation_denial := Some error;
          Error
            Runtime.
              { code = "developer.child_preparation_revoked"
              ; message = "developer child preparation authority expired"
              ; source = None
              }
      in
      let%bind resources =
        Runtime.prepare_extensions_without_inference
          ~startup
          ~tool_contract:(Definition.tool_contract live.definition)
          ~compilation_limits
          ?programs
          ~checkpoint
          ~check_preparation_now
          ~native_registrations:[]
          ~sw
          ~host
          ~platform:(Runtime.platform ())
          ~prompt_elements:elements
          ~manifest_authorizer:authorizer
          ~approval_provider:live.approval_provider
          ~approval_store:(approval_store ())
          ()
        |> Result.map_error ~f:(fun diagnostics ->
          match !preparation_denial with
          | Some error -> error
          | None -> diagnostic_error diagnostics)
      in
      let selected = Chat_response.Managed_tool_registry.capabilities resources.managed in
      if Capabilities.length selected > live.host.max_native_bindings
      then Error (invalid "developer child native binding limit exceeded")
      else (
        let%bind () = check_full () in
        let%bind binding =
          live.source_binding
          |> Result.of_option
               ~error:(invalid "selected child has no original source binding")
        in
        let%bind run_selected_content =
          live.selected_content_runner
          |> Result.of_option
               ~error:(invalid "selected child content runner is unavailable")
        in
        let%bind script_preparation =
          live.host.script_preparation
          |> Result.of_option
               ~error:(invalid "selected child script preparation is unavailable")
        in
        let parent =
          match target with
          | Saved target -> Content_target.Saved target
          | Content target -> Content_target.Content target
          | Model target -> Content_target.Model target
        in
        let%bind content_ports =
          Source_binding.bind_selected_child
            binding
            ~parent
            ~run_selected_content
            ~script_preparation
            ~manifest_authorizer:authorizer
            ~approval_provider:live.approval_provider
            ~response_dir:live.response_dir
            ~env:live.host.env
            ~revision
            ~sw
        in
        let%bind () = check_full () in
        let active = ref true in
        let prepared =
          { live
          ; root
          ; target
          ; saved_ancestor
          ; content_ports
          ; revision
          ; switch = sw
          ; active
          ; runtime = resources.native
          ; selected
          ; managed = resources.managed
          ; needs_dynamic_source
          }
        in
        let run () =
          let%bind admission = Child_prepared.refresh prepared in
          let%bind () = Child_prepared.Admission.check_now admission in
          f prepared
        in
        match run () with
        | result ->
          active := false;
          result
        | exception exn ->
          active := false;
          Exn.raise_with_original_backtrace exn (Stdlib.Printexc.get_raw_backtrace ())))
  in
  let primary =
    try `Ready (Eio.Promise.await ready) with
    | exn -> `Raised (exn, Stdlib.Printexc.get_raw_backtrace ())
  in
  let joined = Owner.join owner in
  match primary, joined with
  | `Raised (exn, backtrace), (Ok () | Error _) ->
    Stdlib.Printexc.raise_with_backtrace exn backtrace
  | `Ready (Error (exn, backtrace)), (Ok () | Error _) ->
    Stdlib.Printexc.raise_with_backtrace exn backtrace
  | `Ready (Ok (Error error)), (Ok () | Error _) -> Error error
  | `Ready (Ok (Ok value)), Ok () -> Ok value
  | `Ready (Ok (Ok _)), Error (exn, backtrace) ->
    Stdlib.Printexc.raise_with_backtrace exn backtrace
;;

let with_saved_child live ~root ~target ~startup ~compilation_limits ~programs ~input ~f =
  with_child
    live
    ~root
    ~target:(Saved target)
    ~saved_ancestor:target
    ~daemon_sw:live.switch
    ~startup
    ~compilation_limits
    ~programs
    ~input
    ~f
;;

let with_selected_content_child
      ~parent
      ~target
      ~startup
      ~compilation_limits
      ~programs
      ~input
      ~f
  =
  let open Result.Let_syntax in
  let expected_parent =
    match parent.target with
    | Saved target -> Content_target.Saved target
    | Content target -> Content_target.Content target
    | Model target -> Content_target.Model target
  in
  let%bind () =
    if Content_target.matches_parent target expected_parent
    then Ok ()
    else Error (invalid "nested Agent belongs to another immediate child")
  in
  let%bind admission = Child_prepared.refresh parent in
  let%bind () = Child_prepared.Admission.check_now admission in
  let%bind () = Content_target.check target in
  with_child
    parent.live
    ~root:parent.root
    ~target:(Content target)
    ~saved_ancestor:parent.saved_ancestor
    ~daemon_sw:parent.switch
    ~startup
    ~compilation_limits
    ~programs
    ~input
    ~f
;;

let with_selected_model_child ~parent ~target ~startup ~input ~f =
  let open Result.Let_syntax in
  let expected_parent =
    match parent.target with
    | Saved target -> Content_target.Saved target
    | Content target -> Content_target.Content target
    | Model target -> Content_target.Model target
  in
  let%bind () =
    if Model_target.matches_parent target expected_parent
    then Ok ()
    else Error (invalid "Model member belongs to another immediate child")
  in
  let%bind admission = Child_prepared.refresh parent in
  let%bind () = Child_prepared.Admission.check_now admission in
  let%bind () = Model_target.check target in
  let%bind policy =
    parent.live.host.script_preparation
    |> Result.of_option ~error:(invalid "selected Model has no script preparation policy")
  in
  Scripts.with_revision
    (Some policy)
    ~startup
    ~tool_contract:(Definition.tool_contract parent.live.definition)
    (Model_target.revision target)
    ~env:parent.live.host.env
    ~invocation:Scripts.Invocation.Headless
    ~checkpoint:(fun () -> Chat_response.Startup_compilation.checkpoint startup)
    ~f:(fun startup ~compilation_limits ~programs ->
      let%bind admission = Child_prepared.refresh parent in
      let%bind () = Child_prepared.Admission.check_now admission in
      let%bind () = Model_target.check target in
      with_child
        parent.live
        ~root:parent.root
        ~target:(Model target)
        ~saved_ancestor:parent.saved_ancestor
        ~daemon_sw:parent.switch
        ~startup
        ~compilation_limits
        ~programs
        ~input
        ~f)
;;

let bind_agent_sources
      t
      ~root
      ~models
      ~run_selected_target
      ~run_selected_content
      ~checkpoint
  =
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
       let%map () =
         Developer_agent_source_runtime.bind
           prepared
           ~root
           ~models
           ~agent_runtime:t.runtime
           ~run_selected_target:(Some run_selected_target)
           ~checkpoint
       in
       t.selected_content_runner <- Some run_selected_content)
;;

let close_and_wait t =
  if t.closed
  then Ok ()
  else (
    t.closing <- true;
    t.selected_content_runner <- None;
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
