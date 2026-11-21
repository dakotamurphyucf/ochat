open! Core
module Close = Runtime_close
module Product_callbacks = Product_callback_data.Observation
module Source = Runtime_source_preparation
module Unmoderated_source = Runtime_unmoderated_source
module Agent_runtime = Chat_response.Agent_runtime
module Config = Chat_response.Config
module Converter = Chat_response.Converter
module Manager = Chat_response.Moderator_manager
module Moderation = Chat_response.Moderation
module Request = Openai.Responses.Request
module Script_preparation = Runtime_script_preparation

type moderator_drain =
  { moderator_snapshot : Jsonaf.t option
  ; runtime_requests : Moderation.Runtime_request.t list
  ; notifications : string list
  ; remaining_events : bool
  }

type model_job_outcome =
  | Model_succeeded of Jsonaf.t
  | Model_failed of string

(** Retained ancestor bindings under the original ancestor scope. Normal use
    checks strict resource custody. Policy restoration may compare only opaque
    evidence for the exact promoting owner, with strict custody for other owners. *)
type ancestor_capabilities =
  { for_execution :
      Agent_protocol.Id.Session.t
      -> (Chat_response.Tool_capability.t, Agent_protocol.Error.t) result
  ; for_policy_restoration :
      promoting:Agent_protocol.Id.Session.t
      -> Agent_protocol.Id.Session.t
      -> (Delegation_authority.Binding_evidence.t, Agent_protocol.Error.t) result
  }

(** Retained physical Source lookup under the actual ancestor native scopes.
    A callback receives the SAME selected Source while its existing custody is
    current. This is neither path permission nor policy admission. It retains no
    first dependent's execution check and performs no load or preparation. *)
type ancestor_sources =
  { with_source :
      'a.
      Agent_protocol.Id.Session.t
      -> f:(source:Source.t -> ('a, Agent_protocol.Error.t) result)
      -> ('a, Agent_protocol.Error.t) result
  }

type resources =
  { source_lease : Prompt_source_lease.t
  ; native : Agent_runtime.t
  ; definition : Chat_response.Extension_compiler.definition option
  ; managed : Chat_response.Managed_tool_registry.t option
  ; authoring : Authoring_runtime.t option
  ; ancestor_capabilities : ancestor_capabilities option
  ; ancestor_sources : ancestor_sources option
  }

type prepare_enqueue =
  before:Session.Moderator_state.Identity_snapshot.t
  -> snapshot:Session.Moderator_state.Identity_snapshot.t
  -> (unit, Agent_protocol.Error.t) result

type extension_services =
  { runtime_policy : Chat_response.Runtime_semantics.policy
  ; native_service_revision : string option
  ; script_tools : Agent_runtime.t -> Script_tool_calls.t
  ; standalone_execution_limits :
      Chat_response.Extension_compiler.t -> Chatml_execution.limits
  ; one_off_policy : Chat_response.One_off_request.policy
  ; authoring_validation_host : Chat_response.Authoring_validation.host option
  ; initial_agent_guidance : Chat_response.Agent_guidance.t option
  ; claim_lifecycle : event:Moderation.Event.t -> Moderator_event.claim
  ; run_actions :
      Agent_protocol.Moderator_execution.t
      -> (Run_action_service.t option, Agent_protocol.Error.t) result
  ; lifecycle_started : Agent_protocol.Invocation.observer -> bool
  ; history : unit -> History_entry.t list
  ; standalone_completion :
      tools:Script_tool_calls.t
      -> Agent_protocol.Job.t
      -> (unit, Agent_protocol.Error.t) result
  ; idle_notifications :
      source:Agent_protocol.Invocation.observer option
      -> tools:Script_tool_calls.t
      -> unit
      -> (bool, Agent_protocol.Error.t) result
  ; notification_input :
      source:Agent_protocol.Invocation.observer option
      -> tools:Script_tool_calls.t
      -> consume:
           Notification_delivery.t Operation_worker.Capabilities.notification_consumer
      -> unit
      -> ( Chat_response.In_memory_stream.Safe_point_input.batch
           , Agent_protocol.Error.t )
           result
  ; initial_notification_input :
      source:Agent_protocol.Invocation.observer option
      -> tools:Script_tool_calls.t
      -> consume:
           Notification_delivery.idle Operation_worker.Capabilities.notification_consumer
      -> unit
      -> ( Chat_response.In_memory_stream.Safe_point_input.batch
           , Agent_protocol.Error.t )
           result
  }

type moderator_activation =
  { pending : unit -> bool
  ; startup_pending : unit -> bool
  ; run :
      ?capture_cleanup:(Callback_cleanup.Completion.t -> unit)
      -> unit
      -> (bool, Agent_protocol.Error.t) result
  }

type background_executor =
  { policy : Chat_response.One_off_request.policy
  ; now : unit -> Agent_protocol.Timestamp.t
  ; run :
      job:Agent_protocol.Job.t
      -> deadline:Agent_protocol.Timestamp.t
      -> execute:Native_tool_invocation.executor
      -> moderator_execute:Native_tool_invocation.moderator_executor
      -> claim_event:(event:Moderation.Event.t -> Moderator_event.claim)
      -> is_halted:(unit -> bool)
      -> request:Chat_response.Background_request.t
      -> (Background_execution.result, Agent_protocol.Error.t) result
  }

module Authored_start = struct
  type disposition =
    | Legacy_acknowledged
    | Extension_pending
    | No_callback

  type kind =
    | Legacy of (unit -> (unit, Agent_protocol.Error.t) result)
    | Extension of
        { lifecycle : Moderator_event.Lifecycle.t
        ; run :
            claim:(event:Moderation.Event.t -> Moderator_event.claim)
            -> (Moderator_event.Lifecycle.outcome, Agent_protocol.Error.t) result
        }
    | Absent

  type t =
    { worker : Operation_worker.t
    ; manager : Manager.t option
    ; kind : kind
    }

  let disposition t =
    match t.kind with
    | Legacy _ -> Legacy_acknowledged
    | Extension _ -> Extension_pending
    | Absent -> No_callback
  ;;

  let worker t = t.worker
  let manager t = t.manager

  let unavailable message =
    Agent_protocol.Error.create Conflict ~message ~retryable:false ()
  ;;

  let check_pending t =
    match t.kind with
    | Legacy check -> check ()
    | Absent -> Ok ()
    | Extension { lifecycle; run = _ } ->
      if Moderator_event.Lifecycle.startup_pending lifecycle
      then Ok ()
      else
        Error
          (unavailable "authored startup requires an unconsumed Session_start lifecycle")
  ;;

  let run t ~claim =
    let open Result.Let_syntax in
    let%bind () = check_pending t in
    match t.kind with
    | Legacy _ | Absent -> Ok ()
    | Extension { lifecycle = _; run } ->
      let%bind outcome = run ~claim in
      (match outcome with
       | Activated _ -> Ok ()
       | Unavailable -> Error (unavailable "authored startup callback was unavailable")
       | Already_active ->
         Error (unavailable "authored startup lifecycle was already activated"))
  ;;
end

module Runtime_resume = struct
  type t =
    { worker : Operation_worker.t
    ; manager : Manager.t option
    ; kind : Authored_start.kind
    }

  let worker t = t.worker
  let manager t = t.manager

  let unavailable message =
    Agent_protocol.Error.create Conflict ~message ~retryable:false ()
  ;;

  let check_pending t =
    match t.kind with
    | Legacy check -> check ()
    | Absent -> Ok ()
    | Extension { lifecycle; run = _ } ->
      if Moderator_event.Lifecycle.resume_pending lifecycle
      then Ok ()
      else
        Error
          (unavailable "runtime resume requires an unconsumed Session_resume lifecycle")
  ;;

  let run t ~claim =
    let open Result.Let_syntax in
    let%bind () = check_pending t in
    match t.kind with
    | Legacy _ | Absent -> Ok ()
    | Extension { lifecycle = _; run } ->
      let%bind outcome = run ~claim in
      (match outcome with
       | Activated _ -> Ok ()
       | Unavailable -> Error (unavailable "runtime resume callback was unavailable")
       | Already_active ->
         Error (unavailable "runtime resume lifecycle was already activated"))
  ;;
end

type t =
  { source_lease : Prompt_source_lease.t
  ; worker : Operation_worker.t
  ; product_callbacks : Product_callbacks.t option
  ; unmoderated_source : Unmoderated_source.t option
  ; inference_execution : Inference_client.Execution.t
  ; now : unit -> Agent_protocol.Timestamp.t
  ; parse_user_content :
      ?asset_capture:Chat_response.Converter_asset_capture.t
      -> id:History_entry.Id.t
      -> Agent_protocol.Session.Message_content.t
      -> (History_entry.t, Agent_protocol.Error.t) result
  ; parse_prepared_input :
      execution:Runtime_input_preparation.Execution.t
      -> tracking:Runtime_input_preparation.Tracking.t
      -> (Runtime_input_preparation.Prepared_input.t, Agent_protocol.Error.t) result
  ; initial_history : History_entry.t list
  ; initial_prompt_entry_count : int
  ; initial_installed_originals : Recovery_installed_originals.Initial.t option
  ; reserved_history_through : int
  ; moderator_snapshot : unit -> Jsonaf.t option
  ; moderator_manager : Manager.t option
  ; model_recipes :
      Model_job_admission.t -> Moderation.Capabilities.model_recipe String.Map.t
  ; moderator_tools : Request.Tool.t list
  ; idle_notifications : (unit -> (bool, Agent_protocol.Error.t) result) option
  ; moderator_script_tools : Script_tool_calls.t option
  ; standalone_completion :
      (Agent_protocol.Job.t -> (unit, Agent_protocol.Error.t) result) option
  ; background_executor : background_executor option
  ; moderator_activation : moderator_activation option
  ; automatic_turn_policy : Chat_response.Runtime_semantics.policy option
  ; check_execution : (unit -> (unit, Agent_protocol.Error.t) result) option
  ; check_ancestry :
      (unit -> (Delegation_authority.Checked_ancestry.t, Agent_protocol.Error.t) result)
        option
  ; ancestor_capabilities : ancestor_capabilities option
  ; retained_ancestor_sources : ancestor_sources option
  ; activity : Runtime_activity.t option
  ; native_runtime : Agent_runtime.t option
  ; authored_start : unit -> (Authored_start.t, Agent_protocol.Error.t) result
  ; runtime_resume : unit -> (Runtime_resume.t, Agent_protocol.Error.t) result
  ; start_moderator : unit -> (Jsonaf.t option, Agent_protocol.Error.t) result
  ; enqueue_internal_event :
      ?prepare:prepare_enqueue
      -> Jsonaf.t
      -> (Jsonaf.t option, Agent_protocol.Error.t) result
  ; legacy_events :
      (capture_cleanup:(Callback_cleanup.Completion.t -> unit) option
       -> max_events:int
       -> history:History_entry.t list
       -> (Chat_response.Moderation.Outcome.t list, Agent_protocol.Error.t) result)
        option
  ; drain_internal_events :
      History_entry.t list -> (moderator_drain, Agent_protocol.Error.t) result
  ; execute_model_job :
      allocate_inference:Inference_allocation.Port.t
      -> inference_context:Inference_runtime.Context.t
      -> capture_recipe_target:
           (Inference.Request.Target.t -> (unit, Agent_protocol.Error.t) result)
      -> recipe:string
      -> payload:Jsonaf.t
      -> (model_job_outcome, Agent_protocol.Error.t) result
  ; enqueue_model_job_completion :
      ?prepare:prepare_enqueue
      -> Agent_protocol.Job.t
      -> (Jsonaf.t option, Agent_protocol.Error.t) result
  ; close : Close.t
  }

let close (t : t) = Runtime_close.run t.close
let close_reported (t : t) = Runtime_close.run_reported t.close
let authored_start t = t.authored_start ()
let runtime_resume t = t.runtime_resume ()

type schedule_services = Legacy_callback_runtime.Services.schedules =
  { after_ms : delay_ms:int -> payload:Jsonaf.t -> (string, string) result
  ; cancel : id:string -> (unit, string) result
  }

type job_services = Runtime_model_services.factory

type legacy_callback_services = Legacy_callback_runtime.Services.t =
  { jobs : Runtime_model_services.t
  ; schedules : schedule_services
  ; execute : Native_tool_invocation.executor
  ; executing : Agent_protocol.Moderator_execution.t
  ; policy_caller : unit -> (Delegated_policy_caller.t, Agent_protocol.Error.t) result
  ; check_current : unit -> (unit, Agent_protocol.Error.t) result
  ; is_halted : unit -> bool
  ; checkpoint : unit -> (Jsonaf.t option, Agent_protocol.Error.t) result
  }

type legacy_callback_claim = Legacy_callback_runtime.claim

module Legacy_startup = struct
  type t =
    | Constructor
    | Installed_resume
end

type legacy_construction =
  { startup : Legacy_startup.t
  ; claim : legacy_callback_claim
  ; installed_claim :
      ?capture_cleanup:(Callback_cleanup.Completion.t -> unit)
      -> operation_id:Agent_protocol.Id.Operation.t option
      -> legacy_callback_claim
  ; history : unit -> (History_entry.t list, Agent_protocol.Error.t) result
  ; script_tools : Agent_runtime.t -> Script_tool_calls.t
  }

let failure message =
  Agent_protocol.Error.create Invalid_state ~message ~retryable:false ()
;;

let moderation_source t =
  match t.moderator_manager with
  | None -> Ok None
  | Some manager ->
    (match Manager.extension_definition manager with
     | Some _ -> Ok (Manager.invocation_observer manager)
     | None ->
       Error
         (failure
            "delegation.owner_mediation_unavailable: parent requires an extensibility \
             moderator"))
;;

let diagnostics values =
  List.map values ~f:Agent_runtime.diagnostic_to_string |> String.concat ~sep:"\n"
;;

let map_diagnostics result =
  Result.map_error result ~f:(fun values -> failure (diagnostics values))
;;

let fetch_prompt ~ctx ~prompt ~is_local =
  try
    let xml = Chat_response.Fetch.get ~ctx prompt ~is_local in
    let prompt_dir =
      if is_local then Chat_response.Fetch.resolve_local_dir ~ctx prompt else None
    in
    Ok (xml, prompt_dir)
  with
  | exn -> Error (Exn.to_string exn)
;;

let cache paths =
  let file = Eio.Path.(paths.Runtime_paths.cache_dir / "cache.bin") in
  Chat_response.Cache.load ~file ~max_size:1_000 ()
;;

let context
      ~env
      ~inference_context
      ~inference_identity
      ~on_inference_attempt
      ~on_inference_completion
      ~on_inference_observation
      paths
      cache
  =
  Chat_response.Ctx.create
    ~inference_context
    ~inference_identity
    ~on_inference_attempt
    ~on_inference_completion
    ~on_inference_observation
    ~env
    ~dir:paths.Runtime_paths.prompt_dir
    ~tool_dir:paths.tool_dir
    ~cache
    ()
;;

let response_dir paths = Eio.Path.(paths.Runtime_paths.session_dir / "responses")

let host ~env ~paths ~session_id ~elements =
  Agent_runtime.host
    ~env
    ~workspace:paths.Runtime_paths.workspace
    ~tool_dir:paths.tool_dir
    ~prompt_dir:paths.prompt_dir
    ~session_dir:paths.session_dir
    ~cache_dir:paths.cache_dir
    ~home:paths.home
    ~session_id:(Agent_protocol.Id.Session.to_string session_id)
    ~prompt_elements:elements
  |> map_diagnostics
;;

let run_agent
      ~manifest_authorizer
      ~approval_provider
      ~response_dir
      ?prompt_dir
      ?session_id
      ?observer
      ~source
      ~ctx
      prompt
      items
  =
  let ctx = Script_tool_calls.agent_context ctx in
  Chat_response.Driver.run_agent
    ~history_compaction:false
    ?prompt_dir
    ?session_id
    ?observer
    ~source
    ~response_dir
    ~shell_manifest_authorizer:manifest_authorizer
    ~shell_approval_provider:approval_provider
    ~ctx
    prompt
    items
;;

(* This port is consumed only by declared native Agent tools. Binding it for a
   tool-free legacy revision would require custody for an unused capability.
   Actual child declarations must still pass the strict captured binding. *)
let requires_agent_source_runner elements =
  List.exists elements ~f:(function
    | Prompt.Chat_markdown.Tool (Agent _ | Persistent_agent _) -> true
    | _ -> false)
;;

let create_agent_runtime
      ~mcp_checks
      ~checkpoint
      ~invocation
      ~agent_source_runner
      ~startup
      ~compilation_limits
      ~programs
      ~extensions
      ~delegated_moderator
      ~native_registrations
      ~native_service_revision
      ~sw
      ~ctx
      ~host
      ~elements
      ~manifest_authorizer
      ~approval_provider
      ~approval_store
      ~response_dir
  =
  match extensions with
  | false ->
    Agent_runtime.create
      ?mcp_checks
      ?agent_source_runner
      ~startup
      ~checkpoint
      ~compilation_limits
      ?programs
      ~moderator_surface:(Script_preparation.Invocation.moderator_surface invocation)
      ?native_service_revision
      ~sw
      ~ctx
      ~host
      ~platform:(Agent_runtime.platform ())
      ~prompt_elements:elements
      ~manifest_authorizer
      ~approval_provider
      ~approval_store
      ~run_agent:(run_agent ~manifest_authorizer ~approval_provider ~response_dir)
      ()
    |> map_diagnostics
    |> Result.map ~f:(fun native -> native, None, None)
  | true ->
    Agent_runtime.prepare_extensions
      ?mcp_checks
      ?agent_source_runner
      ~startup
      ~checkpoint
      ~compilation_limits
      ?programs
      ~moderator_surface:(Script_preparation.Invocation.moderator_surface invocation)
      ~delegated_moderator
      ?native_service_revision
      ~native_registrations
      ~sw
      ~ctx
      ~host
      ~platform:(Agent_runtime.platform ())
      ~prompt_elements:elements
      ~manifest_authorizer
      ~approval_provider
      ~approval_store
      ~run_agent:(run_agent ~manifest_authorizer ~approval_provider ~response_dir)
      ()
    |> map_diagnostics
    |> Result.map ~f:(fun resources ->
      resources.Agent_runtime.native, Some resources.definition, Some resources.managed)
;;

let build_native_registrations = Extensibility_native_tools.registrations
let declares_native = Extensibility_native_tools.declares

(* Nonconsumers retain the exact original resource result. Only the authored
   specialist consumer captures Source metadata in the same preparation scope. *)
let resources_only ~startup:_ ~compilation_limits:_ ~plan:_ ~programs:_ resources =
  Ok resources
;;

let create_authored_resources
      ~mcp_checks
      ~checkpoint
      ~startup
      ~agent_source
      ~script_preparation
      ~revision
      ~additional_native_registrations
      ~native_service_revision
      ~invocation
      ~env
      ~sw
      ~ctx
      ~host
      ~one_off_policy
      ~authoring_validation_host
      ~manifest_authorizer
      ~approval_provider
      ~approval_store
      ~response_dir
      ~f
  =
  let delegated_moderator = Script_preparation.Invocation.equal invocation Delegated_ui in
  Script_preparation.with_revision_plan
    script_preparation
    ?startup
    revision
    ~env
    ~invocation
    ~checkpoint
    ~f:(fun startup ~compilation_limits ~plan ~programs ->
      let elements = Prompt_revision.elements revision in
      let open Result.Let_syntax in
      let%bind agent_source_runner =
        match requires_agent_source_runner elements, agent_source, script_preparation with
        | false, _, _ | true, None, _ -> Ok None
        | true, Some _, None ->
          Error (failure "installed child source requires explicit script policy")
        | true, Some binding, Some policy ->
          Captured_agent_runner.Source_binding.bind
            binding
            ~script_preparation:policy
            ~manifest_authorizer
            ~approval_provider
            ~response_dir
            ~env
            ~revision
            ~sw
          |> Result.map ~f:Option.some
      in
      let%bind () =
        match
          delegated_moderator
          && List.exists elements ~f:(function
            | Prompt.Chat_markdown.Script _ -> true
            | _ -> false)
        with
        | false -> Ok ()
        | true ->
          Error (failure "authored delegation requires an extensibility-v1 moderator")
      in
      let%bind authoring_validation_host =
        match one_off_policy with
        | None -> Ok authoring_validation_host
        | Some policy ->
          Authoring_runtime.configure_host ?host:authoring_validation_host ~policy ()
          |> Result.map ~f:(fun host ->
            Some
              (if delegated_moderator
               then Chat_response.Authoring_validation.for_delegated host
               else host))
          |> Result.map_error ~f:failure
      in
      let%bind native_registrations =
        build_native_registrations
          ~env
          ~elements
          ~one_off_policy
          ~authoring_validation_host
      in
      let native_registrations = native_registrations @ additional_native_registrations in
      let source_elements = elements in
      let%bind elements =
        match authoring_validation_host with
        | None -> Ok elements
        | Some _ ->
          Authoring_runtime.augment ~registrations:native_registrations elements
          |> Result.map_error ~f:failure
      in
      let%bind helpers =
        let elements =
          List.filter elements ~f:(function
            | Prompt.Chat_markdown.Tool (Builtin name) ->
              not
                (List.exists native_registrations ~f:(fun current ->
                   String.equal
                     current.Agent_runtime.implementation.info.function_.name
                     name))
            | _ -> false)
        in
        build_native_registrations
          ~env
          ~elements
          ~one_off_policy:None
          ~authoring_validation_host
      in
      let native_registrations =
        native_registrations
        @ List.filter helpers ~f:(fun helper ->
          not
            (List.exists native_registrations ~f:(fun current ->
               String.equal
                 current.Agent_runtime.implementation.info.function_.name
                 helper.implementation.info.function_.name)))
      in
      let extensions =
        delegated_moderator
        || (not (List.is_empty additional_native_registrations))
        || (Option.is_some one_off_policy
            && (Extensibility_native_tools.declares_any elements
                || List.exists elements ~f:(function
                  | Prompt.Chat_markdown.Extension_script _
                  | Tool (Extension _)
                  | Authoring_context _ | Authoring_help _ -> true
                  | _ -> false)))
      in
      let%bind native, definition, managed =
        create_agent_runtime
          ~mcp_checks
          ~checkpoint
          ~invocation
          ~agent_source_runner
          ~startup
          ~compilation_limits
          ~programs
          ~extensions
          ~delegated_moderator
          ~native_registrations
          ~native_service_revision
          ~sw
          ~ctx
          ~host
          ~elements
          ~manifest_authorizer
          ~approval_provider
          ~approval_store
          ~response_dir
      in
      let%bind authoring =
        match authoring_validation_host with
        | None -> Ok None
        | Some host ->
          let%bind capabilities =
            Lazy.force native.capabilities
            |> Result.map_error ~f:(fun error ->
              failure error.Chat_response.Tool_capability.message)
          in
          Authoring_runtime.prepare ~host ~elements:source_elements ~capabilities ()
          |> Result.map_error ~f:failure
      in
      let resources =
        { source_lease = Prompt_source_lease.legacy
        ; native
        ; definition
        ; managed
        ; authoring
        ; ancestor_capabilities = None
        ; ancestor_sources = None
        }
      in
      f ~startup ~compilation_limits ~plan ~programs resources)
;;

let prepare_resources_internal
      ~startup
      ~agent_source
      ~script_preparation
      ~native_registrations
      ~invocation
      ~native_service_revision
      ~env
      ~inference_context
      ~inference_identity
      ~on_inference_attempt
      ~on_inference_completion
      ~on_inference_observation
      ~sw
      ~paths
      ~storage_paths
      ~revision
      ~session_id
      ~one_off_policy
      ~authoring_validation_host
      ~manifest_authorizer
      ~approval_provider
      ~approval_store
      ~f
  =
  let open Result.Let_syntax in
  let%bind () =
    Agent_store.Prompt_artifact_store.verify_tree
      ~root:(Prompt_revision.materialized_tree revision)
      (Prompt_revision.artifact revision)
    |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
  in
  let elements = Prompt_revision.elements revision in
  let response_dir = response_dir storage_paths in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 response_dir;
  let ctx =
    context
      ~env
      ~inference_context
      ~inference_identity
      ~on_inference_attempt
      ~on_inference_completion
      ~on_inference_observation
      paths
      (cache storage_paths)
  in
  let%bind host = host ~env ~paths ~session_id ~elements in
  let domain_id = (Stdlib.Domain.self () :> int) in
  let check_context () =
    Option.iter ctx.effect_checkpoint ~f:(fun check -> check ());
    Ok ()
  in
  let resource () =
    if not (Int.equal domain_id (Stdlib.Domain.self () :> int))
    then
      Error
        Chat_response.Tool.Native_error.
          { code = "mcp.foreign_domain"
          ; message = "native resources belong to another domain"
          }
    else (
      Eio.Switch.check sw;
      Ok ())
  in
  let mcp_checks =
    Some
      (Chat_response.Tool.Mcp_checks.canonical_native
         ~prepare:check_context
         ~original_context:check_context
         ~resource)
  in
  create_authored_resources
    ~mcp_checks
    ~checkpoint:Eio.Fiber.check
    ~startup
    ~agent_source
    ~script_preparation
    ~revision
    ~additional_native_registrations:native_registrations
    ~native_service_revision
    ~invocation
    ~env
    ~sw
    ~ctx
    ~host
    ~one_off_policy:(Some one_off_policy)
    ~authoring_validation_host
    ~manifest_authorizer
    ~approval_provider
    ~approval_store
    ~response_dir
    ~f
;;

let prepare_resources ~startup ~agent_source ~root_invocation =
  prepare_resources_internal
    ~startup
    ~agent_source
    ~invocation:(Script_preparation.Invocation.of_root root_invocation)
    ~f:resources_only
;;

type authored_resources =
  { source : Authored_agent_source.t
  ; revision : Prompt_revision.t
  ; prepared_revision : Source.Prepared_revision.t
  ; resources : resources
  ; agent_source : Captured_agent_runner.Source_binding.t option
  ; code_authorizer : Prompt_revision.t -> Shell_runtime.Manifest_authorizer.t
  }

let authored_source_binding t = t.agent_source
let authored_code_authorizer t = t.code_authorizer

let prepare_authored_resources_from_source
      prepared_source
      ~startup
      ~agent_source
      ~native_registrations
      ~native_service_revision
      ~env
      ~inference_context
      ~inference_identity
      ~on_inference_attempt
      ~on_inference_completion
      ~on_inference_observation
      ~sw
      ~paths
      ~storage_paths
      ~session_id
      ~one_off_policy
      ~authoring_validation_host
      ~manifest_authorizer
      ~approval_provider
      ~approval_store
  =
  let script_preparation =
    Authored_source_preparation.script_preparation prepared_source
  in
  let source = Authored_source_preparation.source prepared_source in
  let revision = Authored_source_preparation.revision prepared_source in
  let limits =
    Option.value_map
      script_preparation
      ~default:Chatml_compilation.default_limits
      ~f:Script_preparation.compilation
  in
  Chat_response.Startup_compilation.with_context
    ~startup
    ~limits
    ~env
    ~checkpoint:Eio.Fiber.check
    ~map_error:(fun error ->
      Agent_protocol.Error.create
        Invalid_state
        ~message:(error.Chatml_compilation.code ^ ": " ^ error.message)
        ~retryable:false
        ())
    ~f:(fun startup ->
      let open Result.Let_syntax in
      let paths =
        { paths with
          Runtime_paths.prompt_dir =
            Eio.Path.(
              Prompt_revision.materialized_tree revision
              / Filename.dirname (Prompt_revision.root_relative_path revision))
        }
      in
      let%map resources, prepared_revision =
        prepare_resources_internal
          ~startup:(Some startup)
          ~agent_source
          ~script_preparation
          ~native_registrations
          ~invocation:Delegated_ui
          ~inference_context
          ~inference_identity
          ~on_inference_attempt
          ~on_inference_completion
          ~on_inference_observation
          ~native_service_revision
          ~env
          ~sw
          ~paths
          ~storage_paths
          ~revision
          ~session_id
          ~one_off_policy
          ~authoring_validation_host
          ~manifest_authorizer:(manifest_authorizer revision)
          ~approval_provider
          ~approval_store
          ~f:(fun ~startup ~compilation_limits ~plan ~programs resources ->
            let%map prepared_revision =
              Source.Prepared_revision.capture
                revision
                ~invocation:Delegated_ui
                ~startup
                ~compilation_limits
                ~plan
                ~programs
                ~checkpoint:(fun () ->
                  Chat_response.Startup_compilation.checkpoint startup)
            in
            resources, prepared_revision)
      in
      { source
      ; revision
      ; prepared_revision
      ; resources
      ; agent_source
      ; code_authorizer = manifest_authorizer
      })
    ()
;;

let prepare_authored_resources
      ~startup
      ~agent_source
      ~script_preparation
      ~content_limits
      ~native_registrations
      ~parent_revision
      ~tool_name
      ~native_service_revision
      ~env
      ~inference_context
      ~inference_identity
      ~on_inference_attempt
      ~on_inference_completion
      ~on_inference_observation
      ~sw
      ~paths
      ~storage_paths
      ~session_id
      ~one_off_policy
      ~authoring_validation_host
      ~manifest_authorizer
      ~approval_provider
      ~approval_store
  =
  Authored_source_preparation.with_prepared
    ~startup
    ~script_preparation
    ~content_limits
    ~parent:parent_revision
    ~tool_name
    ~env
    ~checkpoint:Eio.Fiber.check
    ~f:(fun startup prepared_source ->
      prepare_authored_resources_from_source
        prepared_source
        ~startup
        ~agent_source
        ~native_registrations
        ~native_service_revision
        ~env
        ~inference_context
        ~inference_identity
        ~on_inference_attempt
        ~on_inference_completion
        ~on_inference_observation
        ~sw
        ~paths
        ~storage_paths
        ~session_id
        ~one_off_policy
        ~authoring_validation_host
        ~manifest_authorizer
        ~approval_provider
        ~approval_store)
;;

let inherit_prepared_resources ~parent_runtime ~inherited_managed ~definition =
  let open Result.Let_syntax in
  let admission = Generated_definition.admission definition in
  let%map native =
    Agent_runtime.inherit_native
      ?managed:inherited_managed
      ~parent:parent_runtime
      ~capabilities:(Chat_response.Generated_admission.capabilities admission)
      ()
    |> map_diagnostics
  in
  { source_lease = Prompt_source_lease.legacy
  ; native
  ; authoring = None
  ; ancestor_capabilities = None
  ; ancestor_sources = None
  ; definition = Some (Chat_response.Generated_admission.definition admission)
  ; managed =
      Option.map
        inherited_managed
        ~f:Chat_response.Managed_tool_registry.delegation_registry
  }
;;

let inherit_resources ~(parent : resources) ~definition =
  let open Result.Let_syntax in
  let%bind inherited_managed =
    match parent.managed with
    | None -> Ok None
    | Some managed ->
      Chat_response.Managed_tool_registry.delegate_generated_foreground
        managed
        ~selected:
          (Chat_response.Generated_admission.capabilities
             (Generated_definition.admission definition))
      |> Result.map ~f:Option.some
      |> Result.map_error ~f:(fun error ->
        Agent_protocol.Error.create
          Permission_denied
          ~message:error.Chat_response.Tool_capability.message
          ~retryable:false
          ())
  in
  inherit_prepared_resources ~parent_runtime:parent.native ~inherited_managed ~definition
  |> Result.map ~f:(fun resources ->
    { resources with source_lease = Prompt_source_lease.borrow parent.source_lease })
;;

exception Construction_rejected of Agent_protocol.Error.t

let check_construction = function
  | None -> Ok ()
  | Some check -> check ()
;;

let initial_items
      ~construction_checkpoint
      ~ctx
      ~elements
      ~installed_projection
      ~manifest_authorizer
      ~approval_provider
      ~response_dir
  =
  let run_agent ?prompt_dir ?session_id ~ctx prompt items =
    (match check_construction construction_checkpoint with
     | Ok () -> ()
     | Error failure -> raise (Construction_rejected failure));
    Chat_response.Driver.run_agent
      ~history_compaction:false
      ?prompt_dir
      ?session_id
      ~response_dir
      ~shell_manifest_authorizer:manifest_authorizer
      ~shell_approval_provider:approval_provider
      ~ctx
      prompt
      items
  in
  try
    Ok
      (match installed_projection with
       | None ->
         List.map (Converter.to_items ~ctx ~run_agent elements) ~f:(fun item -> item, [])
       | Some (revision, projection) ->
         Converter.to_items_with_installed_documents ~ctx ~run_agent projection
         |> List.map ~f:(fun (item, captures) ->
           let captured =
             List.filter_map captures ~f:(fun (part, occurrence) ->
               if part >= 4096
               then None
               else
                 Option.map
                   (Prompt_revision.installed_original_key revision occurrence)
                   ~f:(fun key -> part, key))
           in
           item, captured))
  with
  | Construction_rejected failure -> Error failure
;;

let allocate_initial ~namespace ~next_sequence items =
  let open Result.Let_syntax in
  let%bind allocator =
    History_entry.Allocator.create ~namespace ~next_sequence
    |> Result.map_error ~f:failure
  in
  let%map history =
    List.map items ~f:(fun (item, originals) ->
      let%map.Result row = Openai.Responses_history.create ~allocator item in
      row, originals)
    |> Result.all
    |> Result.map_error ~f:failure
  in
  history, History_entry.Allocator.next_sequence allocator
;;

let model_executor ~sw ~ctx ~manifest_authorizer ~approval_provider ~response_dir =
  let exec_context : Chat_response.Model_executor.exec_context =
    { ctx
    ; run_agent =
        (fun ?history_compaction ?prompt_dir ?session_id ~ctx prompt items ->
          Chat_response.Driver.run_agent
            ?history_compaction
            ?prompt_dir
            ?session_id
            ~response_dir
            ~shell_manifest_authorizer:manifest_authorizer
            ~shell_approval_provider:approval_provider
            ~ctx
            prompt
            items)
    ; fetch_prompt
    }
  in
  Chat_response.Model_executor.create ~sw ~exec_context ()
;;

let legacy_model_recipes
      ~delegated
      executor
      ~session_text
      ~bind_inference_identity
      (jobs : Runtime_model_services.t)
  =
  let durable_recipe : Moderation.Capabilities.model_recipe =
    { call =
        (fun ~payload ->
          jobs.call_model
            ~recipe:Chat_response.Model_executor.agent_prompt_v1_name
            ~payload
            ~execute:
              (fun
                ~inference_context
                ~allocate_inference
                ~before_inference_attempt
                ~capture_recipe_target
              ->
              let recipe =
                Chat_response.Model_executor.recipe_agent_prompt_v1
                  executor
                  ~session_id:session_text
                  ~inference_context
                  ~inference_identity:(bind_inference_identity allocate_inference)
                  ~before_inference_attempt
                  ~capture_recipe_target:(fun target ->
                    capture_recipe_target target
                    |> Result.map_error ~f:(fun error ->
                      error.Agent_protocol.Error.message))
                  ()
              in
              recipe.call ~payload))
    ; spawn =
        (fun ~payload ->
          jobs.spawn_model
            ~recipe:Chat_response.Model_executor.agent_prompt_v1_name
            ~payload)
    }
  in
  match delegated with
  | true -> Map.empty (module String)
  | false ->
    Map.singleton
      (module String)
      Chat_response.Model_executor.agent_prompt_v1_name
      durable_recipe
;;

let run_legacy_constructor
      host
      ~bind_inference_identity
      ~script_tools
      ~selected
      ~manager
      ~executor
      ~delegated
      ~session_text
      ~now_ms
      ~history
      ~tools
      ~selection
  =
  let open Result.Let_syntax in
  let%bind result =
    Legacy_callback_runtime.run
      manager
      ~claim:host.claim
      ~model_recipes:
        (legacy_model_recipes ~delegated executor ~session_text ~bind_inference_identity)
      ~script_tools
      ~selected
      ~selection
      ~session_id:session_text
      ~history
      ~available_tools:tools
      ~session_meta:`Null
      ~now_ms
  in
  match result with
  | Some result -> Ok result.checkpoint
  | None -> Error (failure "legacy constructor did not acknowledge its checkpoint")
;;

module Installed_legacy_resume = struct
  module Completion = struct
    type t =
      | Ordinary_activation
      | Scoped_resume_ack
  end

  type state =
    | Ready
    | Completed of Completion.t
    | Failed of Agent_protocol.Error.t

  type t =
    { state : state Atomic.t
    ; gate : Chat_response.Execution_gate.t
    ; run :
        capture_cleanup:(Callback_cleanup.Completion.t -> unit) option
        -> operation_id:Agent_protocol.Id.Operation.t option
        -> (Legacy_callback_runtime.t option, Agent_protocol.Error.t) result
    ; acknowledge : Jsonaf.t option -> unit
    }

  let create ~run ~acknowledge =
    { state = Atomic.make Ready
    ; gate = Chat_response.Execution_gate.create ()
    ; run
    ; acknowledge
    }
  ;;

  let pending t =
    match Atomic.get t.state with
    | Ready -> true
    | Completed _ | Failed _ -> false
  ;;

  let state t = Atomic.get t.state

  let run_acknowledged t ~completion ~run =
    match
      Chat_response.Execution_gate.with_access t.gate (fun () ->
        match Atomic.get t.state with
        | Completed _ -> Ok false
        | Failed error -> Error error
        | Ready ->
          (match run () with
           | Ok None -> Ok false
           | Ok (Some checkpoint) ->
             (* The actual callback ACK is retained by the acknowledger before
                the same pending owner becomes completed. Neither step yields. *)
             t.acknowledge checkpoint;
             Atomic.set t.state (Completed completion);
             Ok true
           | Error error ->
             Atomic.set t.state (Failed error);
             Error error
           | exception exn ->
             let backtrace = Stdlib.Printexc.get_raw_backtrace () in
             Atomic.set
               t.state
               (Failed (failure "installed legacy resume was interrupted"));
             Exn.raise_with_original_backtrace exn backtrace))
    with
    | Ok result -> result
    | Error error -> Error (failure (Chat_response.Execution_gate.error_message error))
  ;;

  let run ?capture_cleanup t ~operation_id =
    run_acknowledged t ~completion:Completion.Ordinary_activation ~run:(fun () ->
      Result.map
        (t.run ~capture_cleanup ~operation_id)
        ~f:(Option.map ~f:(fun outcome -> outcome.Legacy_callback_runtime.checkpoint)))
  ;;

  let run_scoped t ~run =
    let open Result.Let_syntax in
    let%bind _ =
      run_acknowledged t ~completion:Completion.Scoped_resume_ack ~run:(fun () ->
        Result.map (run ()) ~f:Option.some)
    in
    (* Selection under Ready is not a claim. Ordinary activation may have won
       the SAME gate before this caller entered it; its ACK is not scoped. *)
    match Atomic.get t.state with
    | Completed Scoped_resume_ack -> Ok ()
    | Completed Ordinary_activation ->
      Error
        (Agent_protocol.Error.create
           Conflict
           ~message:"ordinary activation completed before scoped runtime resume"
           ~retryable:true
           ())
    | Failed error -> Error error
    | Ready -> Error (failure "scoped runtime resume has no acknowledged completion")
  ;;
end

type moderator_selection =
  | Verified_no_moderator
  | Selected_moderator

module Moderator_attachment = struct
  type t =
    { session_id : Agent_protocol.Id.Session.t
    ; mutable executor : Chat_response.Model_executor.t option
    }

  let create ~session_id = { session_id; executor = None }
  let record_executor t executor = t.executor <- Some executor

  let close t =
    Option.iter t.executor ~f:(fun executor ->
      Chat_response.Model_executor.unregister_session_wakeup
        executor
        ~session_id:(Agent_protocol.Id.Session.to_string t.session_id))
  ;;
end

let create_moderator
      ~attachment
      ~construction_checkpoint
      ~definition
      ~delegated
      ~runtime_policy
      ~sw
      ~env
      ~ctx
      ~session_id
      ~history
      ~tools
      ~allocator
      ~agent_runtime
      ~manifest_authorizer
      ~approval_provider
      ~response_dir
      ~schedule_services
      ~(job_services : job_services)
      ~legacy_construction
      ~script_tools
      ~bind_inference_identity
      ~snapshot
  =
  let open Result.Let_syntax in
  let legacy_artifact () =
    Ok (Manager.Registry.empty, agent_runtime.Agent_runtime.legacy_moderator)
  in
  let%bind _, artifact =
    (match definition with
     | Some definition ->
       let%bind registry, artifact =
         Manager.Registry.of_definition Manager.Registry.empty definition
       in
       (match artifact with
        | Some _ -> Ok (registry, artifact)
        | None -> legacy_artifact ())
     | None -> legacy_artifact ())
    |> Result.map_error ~f:failure
  in
  let selection =
    match artifact with
    | None -> Verified_no_moderator
    | Some _ -> Selected_moderator
  in
  let%map runtime =
    match artifact with
    | None ->
      Ok
        ( None
        , (fun () -> Ok None)
        , (fun _ -> String.Map.empty)
        , (fun () -> Error (failure "runtime has no host-backed legacy startup ACK"))
        , (fun () ->
            Error
              (Runtime_resume.unavailable "runtime has no host-backed legacy resume ACK"))
        , None )
    | Some artifact ->
      let executor =
        model_executor ~sw ~ctx ~manifest_authorizer ~approval_provider ~response_dir
      in
      (* Retain attachment-local cleanup before Manager construction can yield. *)
      Moderator_attachment.record_executor attachment executor;
      let session_text = Agent_protocol.Id.Session.to_string session_id in
      let capabilities =
        { Moderation.Capabilities.default with
          model_recipes =
            legacy_model_recipes
              ~delegated
              executor
              ~session_text
              ~bind_inference_identity
              job_services.initialization
        ; on_schedule_after_ms =
            (fun ~delay_ms ~payload ->
              let open Result.Let_syntax in
              let%bind payload = Chatml.Chatml_value_codec.Snapshot.of_value payload in
              let payload = Chatml.Chatml_value_codec.Snapshot.to_jsonaf payload in
              schedule_services.after_ms ~delay_ms ~payload)
        ; on_schedule_cancel = schedule_services.cancel
        }
      in
      let%bind () = check_construction construction_checkpoint in
      let%bind manager =
        Manager.create_entries
          ~env
          ~artifact
          ~capabilities
          ~allocator
          ?on_process_run:
            (match delegated with
             | true -> None
             | false -> Agent_runtime.moderator_process_handler agent_runtime)
          ?snapshot
          ()
        |> Result.map_error ~f:failure
      in
      Chat_response.Model_executor.register_session
        executor
        ~session_id:session_text
        ~manager;
      let moderator =
        Chat_response.In_memory_stream.
          { manager
          ; session_id = session_text
          ; session_meta = `Null
          ; runtime_policy
          ; event_handlers = None
          }
      in
      let moderator_pair = moderator, executor in
      let%bind installed_resume =
        match Manager.extension_definition manager, legacy_construction, snapshot with
        | None, Some { startup = Installed_resume; _ }, None ->
          Error (failure "installed legacy resume requires an initialized checkpoint")
        | None, Some { startup = Installed_resume; _ }, Some _ -> Ok true
        | None, (Some { startup = Constructor; _ } | None), _ | Some _, _, _ -> Ok false
      in
      let started = ref false in
      let acknowledged_snapshot = ref None in
      let current_snapshot () =
        Manager.identity_snapshot manager
        |> Result.map_error ~f:failure
        |> Result.map ~f:(fun snapshot -> Some (Moderator_checkpoint.encode snapshot))
      in
      let start () =
        match
          ( Manager.extension_definition manager
          , legacy_construction
          , !started
          , !acknowledged_snapshot )
        with
        | None, Some _, true, Some snapshot ->
          (* This is the exact already-acknowledged constructor result, not another
           startup effect under the retired construction owner. *)
          Ok snapshot
        | _ ->
          let%bind () = check_construction construction_checkpoint in
          if Option.is_some (Manager.extension_definition manager)
          then current_snapshot ()
          else if !started
          then (
            match legacy_construction, !acknowledged_snapshot with
            | Some _, Some snapshot -> Ok snapshot
            | Some _, None ->
              Error (failure "legacy startup lost its acknowledged checkpoint")
            | None, _ -> current_snapshot ())
          else if installed_resume
          then current_snapshot ()
          else (
            let now_ms =
              Eio.Time.now (Eio.Stdenv.clock env) *. 1_000. |> Float.iround_nearest_exn
            in
            let open Result.Let_syntax in
            match legacy_construction, script_tools with
            | Some host, Some script_tools ->
              let%bind selected =
                Lazy.force agent_runtime.Agent_runtime.capabilities
                |> Result.map_error ~f:(fun error ->
                  failure error.Chat_response.Tool_capability.message)
              in
              let%bind () = check_construction construction_checkpoint in
              let%map checkpoint =
                run_legacy_constructor
                  host
                  ~bind_inference_identity
                  ~script_tools
                  ~selected
                  ~manager
                  ~executor
                  ~delegated
                  ~session_text
                  ~now_ms
                  ~history
                  ~tools
                  ~selection:
                    (Boundary
                       (if Option.is_some snapshot then Session_resume else Session_start))
              in
              acknowledged_snapshot := Some checkpoint;
              started := true;
              checkpoint
            | Some _, None ->
              Error (failure "legacy construction requires actual native services")
            | None, _ ->
              let%bind outcome =
                Manager.handle_event_entries
                  manager
                  ~session_id:session_text
                  ~now_ms
                  ~history
                  ~available_tools:tools
                  ~session_meta:`Null
                  ~event:
                    (if Option.is_some snapshot
                     then Moderation.Event.Session_resume
                     else Session_start)
                |> Result.map_error ~f:failure
              in
              let%bind _ =
                if
                  Option.is_some
                    (Chat_response.Runtime_semantics.should_end_session
                       outcome.runtime_requests)
                then Ok []
                else
                  Manager.drain_internal_events_entries
                    manager
                    ~session_id:session_text
                    ~now_ms
                    ~history
                    ~available_tools:tools
                    ~session_meta:`Null
                  |> Result.map_error ~f:failure
              in
              let%map snapshot = current_snapshot () in
              started := true;
              snapshot)
      in
      let check_legacy_acknowledged () =
        match
          ( Manager.extension_definition manager
          , legacy_construction
          , !started
          , !acknowledged_snapshot )
        with
        | None, Some _, true, Some _ -> Ok ()
        | (Some _ | None), (Some _ | None), (true | false), (Some _ | None) ->
          Error (failure "authored legacy startup requires its actual host-backed ACK")
      in
      let check_legacy_resume_acknowledged () =
        match
          ( Manager.extension_definition manager
          , legacy_construction
          , snapshot
          , !started
          , !acknowledged_snapshot )
        with
        | None, Some _, Some _, true, Some _ -> Ok ()
        | ( (Some _ | None)
          , (Some _ | None)
          , (Some _ | None)
          , (true | false)
          , (Some _ | None) ) ->
          Error
            (Runtime_resume.unavailable
               "runtime resume requires its actual host-backed Session_resume ACK")
      in
      Ok
        ( Some moderator_pair
        , start
        , (fun admission ->
            if delegated
            then String.Map.empty
            else
              legacy_model_recipes
                ~delegated
                executor
                ~session_text
                ~bind_inference_identity
                (job_services.for_execution admission))
        , check_legacy_acknowledged
        , check_legacy_resume_acknowledged
        , if installed_resume
          then
            Some
              (fun checkpoint ->
                acknowledged_snapshot := Some checkpoint;
                started := true)
          else None )
  in
  runtime, selection
;;

let encode_moderator_snapshot = Moderator_checkpoint.encode

let moderator_snapshot = function
  | None -> Ok None
  | Some (moderator, _) ->
    Chat_response.Moderator_manager.identity_snapshot
      moderator.Chat_response.In_memory_stream.manager
    |> Result.map_error ~f:failure
    |> Result.map ~f:(fun snapshot -> Some (encode_moderator_snapshot snapshot))
;;

let decode_moderator_snapshot = Moderator_checkpoint.decode

let moderator_snapshot_has_queued_events snapshot =
  let open Result.Let_syntax in
  let%map snapshot = decode_moderator_snapshot snapshot in
  Option.value_map snapshot ~default:false ~f:(fun snapshot ->
    not
      (List.is_empty
         snapshot.Session.Moderator_state.Identity_snapshot.queued_internal_events))
;;

let moderator_snapshot_is_halted = Moderator_checkpoint.is_halted
let moderator_snapshot_observer = Moderator_checkpoint.observer

let enqueue_internal_value ?prepare moderator value =
  match moderator with
  | None -> Error (failure "session prompt has no ChatML moderator")
  | Some (moderator, _) ->
    let open Result.Let_syntax in
    let%map snapshot =
      Manager.enqueue_internal_event_entries
        moderator.Chat_response.In_memory_stream.manager
        ~event:value
        ~prepare:(fun ~before ~snapshot ->
          match prepare with
          | None -> Ok ()
          | Some prepare ->
            prepare ~before ~snapshot
            |> Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message))
      |> Result.map_error ~f:failure
    in
    Some (encode_moderator_snapshot snapshot)
;;

let enqueue_internal_event moderator ?prepare payload =
  let open Result.Let_syntax in
  let%bind snapshot = Session.Snapshot.of_jsonaf payload |> Result.map_error ~f:failure in
  let%bind value =
    Chatml.Chatml_value_codec.Snapshot.to_value snapshot |> Result.map_error ~f:failure
  in
  enqueue_internal_value ?prepare moderator value
;;

let execute_model_job
      moderator
      session_id
      ~inference_identity
      ~inference_context
      ~capture_recipe_target
      ~recipe
      ~payload
  =
  match moderator with
  | None -> Error (failure "session prompt has no ChatML moderator")
  | Some (_, executor) ->
    if not (String.equal recipe Chat_response.Model_executor.agent_prompt_v1_name)
    then Error (failure (sprintf "unknown model job recipe %S" recipe))
    else (
      let handler =
        Chat_response.Model_executor.recipe_agent_prompt_v1
          executor
          ~session_id:(Agent_protocol.Id.Session.to_string session_id)
          ~inference_context
          ~inference_identity
          ~capture_recipe_target:(fun target ->
            capture_recipe_target target
            |> Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message))
          ()
      in
      handler.call ~payload
      |> Result.map_error ~f:failure
      |> Result.map ~f:(function
        | Moderation.Capabilities.Model_ok value -> Model_succeeded value
        | Model_refused message | Model_error message -> Model_failed message))
;;

let model_job_event = Model_job_event.legacy

let enqueue_model_job_completion moderator ?prepare job =
  let format =
    match moderator with
    | None -> Model_job_event.Format.Legacy
    | Some (moderator, _) ->
      Model_job_event.Format.of_manager moderator.Chat_response.In_memory_stream.manager
  in
  Result.bind
    (Model_job_event.render ~format job)
    ~f:(enqueue_internal_value ?prepare moderator)
;;

let collapse_drain_outcomes outcomes =
  let runtime_requests =
    List.concat_map outcomes ~f:(fun outcome ->
      outcome.Moderation.Outcome.runtime_requests)
    |> Chat_response.Runtime_semantics.collapse
  in
  let notifications =
    List.concat_map outcomes ~f:(fun outcome ->
      outcome.Moderation.Outcome.ui_notifications)
  in
  runtime_requests, notifications
;;

let drain_moderator ~env ~session_id ~tools moderator history =
  let open Result.Let_syntax in
  let manager = moderator.Chat_response.In_memory_stream.manager in
  let now_ms =
    Eio.Time.now (Eio.Stdenv.clock env) *. 1_000. |> Float.iround_nearest_exn
  in
  let max_events =
    Chat_response.Runtime_semantics.default_policy.budget.max_internal_event_drain
  in
  let%bind outcomes =
    Manager.drain_internal_events_entries
      ~max_events
      manager
      ~session_id
      ~now_ms
      ~history
      ~available_tools:tools
      ~session_meta:`Null
    |> Result.map_error ~f:failure
  in
  let%map snapshot = Manager.identity_snapshot manager |> Result.map_error ~f:failure in
  let runtime_requests, notifications = collapse_drain_outcomes outcomes in
  { moderator_snapshot = Some (encode_moderator_snapshot snapshot)
  ; runtime_requests
  ; notifications
  ; remaining_events = not (List.is_empty snapshot.queued_internal_events)
  }
;;

let drain_internal_events ~env ~session_id ~tools moderator history =
  match moderator with
  | None -> Error (failure "session prompt has no ChatML moderator")
  | Some (moderator, _) ->
    drain_moderator
      ~env
      ~session_id:(Agent_protocol.Id.Session.to_string session_id)
      ~tools
      moderator
      history
;;

let plain_user_item text =
  Openai.Responses.Item.Input_message
    { role = User; content = [ Text { text; _type = "input_text" } ]; _type = "message" }
;;

let chatmd_user_item ?asset_capture ~ctx ~run_agent paths text =
  let xml =
    if String.is_prefix (String.strip text) ~prefix:"<"
    then text
    else sprintf "<user>\n%s\n</user>" text
  in
  try
    Prompt.Chat_markdown.parse_chat_inputs ~dir:paths.Runtime_paths.workspace xml
    |> List.find_map ~f:(function
      | Prompt.Chat_markdown.User message ->
        Some (Converter.convert_user_msg ?asset_capture ~ctx ~run_agent message)
      | _ -> None)
    |> Result.of_option ~error:(failure "ChatMD input does not contain a user message")
  with
  | exn -> Error (failure ("ChatMD input parse failed: " ^ Exn.to_string exn))
;;

let parse_user_content ~ctx ~manifest_authorizer ~approval_provider ~response_dir paths =
  (* Submitted ChatMD content construction precedes operation admission and is
     an explicitly authored separate prompt. It cannot borrow a starter's lease
     or a later turn's session guidance. The actual submitted root turn includes
     its own guidance under its admitted operation owner. *)
  let ctx = Chat_response.Ctx.without_agent_guidance ctx in
  let converter_runner ?prompt_dir ?session_id ~ctx prompt items =
    run_agent
      ~manifest_authorizer
      ~approval_provider
      ~response_dir
      ?prompt_dir
      ?session_id
      ~source:"session-input.chatmd"
      ~ctx
      prompt
      items
  in
  fun ?asset_capture ~id content ->
    let open Result.Let_syntax in
    let%map item =
      match content.Agent_protocol.Session.Message_content.kind with
      | Plain_text -> Ok (plain_user_item content.text)
      | Chatmd ->
        chatmd_user_item
          ?asset_capture
          ~ctx
          ~run_agent:converter_runner
          paths
          content.text
    in
    Openai.Responses_history.create_with_id_exn ~id item
;;

(* The caller owns and seals one actual input graph around this parser. No
   constructor tracker or future Operation is borrowed for these effects. *)
let parse_prepared_input
      ~ctx
      ~manifest_authorizer
      ~approval_provider
      ~response_dir
      ~delegated
      paths
      ~(execution : Runtime_input_preparation.Execution.t)
      ~(tracking : Runtime_input_preparation.Tracking.t)
  =
  let require = function
    | Ok value -> value
    | Error error -> raise (Chat_response.Ctx.Effect_rejected error)
  in
  let checkpoint () = require (execution.checkpoint ()) in
  let refresh () =
    let final = require (execution.refresh ()) in
    fun () -> require (final ())
  in
  let ctx =
    Chat_response.Ctx.without_agent_guidance ctx
    |> fun ctx ->
    Chat_response.Ctx.with_inference_tracking
      ctx
      ~identity:tracking.identity
      ~on_attempt:tracking.on_attempt
      ~on_observation:tracking.on_observation
      ~on_completion:tracking.on_completion
    |> fun ctx -> Chat_response.Ctx.with_effect_checkpoint ctx ~check:checkpoint
    (* Preserve the inherited parent submission guard. The bound allocator
       supplies the actual same-registration input guard after admission;
       fresh native/Agent/file effects retain the checkpoint above. *)
  in
  let convert content =
    let open Result.Let_syntax in
    checkpoint ();
    let%bind item =
      match content.Agent_protocol.Session.Message_content.kind with
      | Plain_text ->
        if delegated && not (List.is_empty content.attachments)
        then
          Error
            (failure
               "generated sessions accept plain_text messages without implicit resource \
                loading")
        else Ok (plain_user_item content.text)
      | Chatmd ->
        if delegated
        then
          Error
            (failure
               "generated sessions accept plain_text messages without implicit resource \
                loading")
        else (
          let xml =
            if String.is_prefix (String.strip content.text) ~prefix:"<"
            then content.text
            else sprintf "<user>\n%s\n</user>" content.text
          in
          let%bind elements =
            try
              let source_loader =
                Source_loader.with_checkpoint
                  (Source_loader.filesystem ~root:paths.Runtime_paths.workspace)
                  ~checkpoint
              in
              Ok
                (Prompt.Chat_markdown.parse_chat_inputs
                   ~source_loader
                   ~dir:paths.Runtime_paths.workspace
                   xml)
            with
            | Chat_response.Ctx.Effect_rejected _ as exn ->
              Exn.raise_with_original_backtrace exn (Stdlib.Printexc.get_raw_backtrace ())
            | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn ->
              Exn.raise_with_original_backtrace exn (Stdlib.Printexc.get_raw_backtrace ())
            | exn -> Error (failure ("ChatMD input parse failed: " ^ Exn.to_string exn))
          in
          checkpoint ();
          List.find_map elements ~f:(function
            | Prompt.Chat_markdown.User message ->
              Some
                (Converter.convert_user_msg
                   ~ctx
                   ~run_agent:(fun ?prompt_dir ?session_id ~ctx prompt items ->
                     checkpoint ();
                     run_agent
                       ~manifest_authorizer
                       ~approval_provider
                       ~response_dir
                       ?prompt_dir
                       ?session_id
                       ~source:"session-input.chatmd"
                       ~ctx
                       prompt
                       items)
                   message)
            | _ -> None)
          |> Result.of_option
               ~error:(failure "ChatMD input does not contain a user message"))
    in
    checkpoint ();
    let entry = Openai.Responses_history.create_with_id_exn ~id:execution.id item in
    Runtime_input_preparation.Prepared_input.create
      ~request:execution.request
      ~id:execution.id
      ~entry:(History_codec.to_canonical entry)
  in
  try
    checkpoint ();
    let open Result.Let_syntax in
    let%bind content =
      match execution.request.input with
      | User_submission content -> Ok content
      | Authored_start -> Error (failure "prepared input requires User_submission")
    in
    Shell_runtime.Call_context.with_narrowed_check
      ~refresh:(fun () ->
        let check = refresh () in
        Ok
          (fun () ->
            check ();
            Ok ()))
      (fun () -> convert content)
    |> Result.map_error ~f:failure
    |> Result.join
  with
  | Chat_response.Ctx.Effect_rejected error
  | Inference_allocation.Rejected error
  | Converter.Source_error error -> Error error
;;

type inherited_managed =
  { delegation : Chat_response.Managed_tool_registry.delegation
  ; current : unit -> Chat_response.Tool_capability.t
  }

type source =
  | Authored of
      { revision : Prompt_revision.t
      ; script_preparation : Script_preparation.t option
      ; root_invocation :
          Chat_response.Script_contract_plan.Program.Role.Moderator_invocation.t
      }
  | Captured_artifact of
      { captured : Captured_recurrence_source.t
      ; script_preparation : Script_preparation.t option
      ; authority : Delegation_authority.t
      }
  | Authored_child of
      { revision : Prompt_revision.t
      ; prepared : authored_resources
      ; code : Authored_code_preparation.t option
      ; authority : Delegation_authority.t
      }
  | Generated of
      { definition : Generated_definition.t
      ; artifact_store : Agent_store.Prompt_artifact_store.t
      ; parent_runtime : Agent_runtime.t
      ; inherited_managed : inherited_managed option
      ; authority : Delegation_authority.t
      }

module Native_identity = struct
  type t = unit ref

  let equal = phys_equal
end

module Native_preparation = struct
  type t =
    | Original_execution
    | Resource_setup of
        { prefix : Delegation_authority.Resource_path.Prefix.t
        ; check_resource : unit -> (unit, Agent_protocol.Error.t) result
        }

  let check_root = function
    | Original_execution -> Ok ()
    | Resource_setup _ ->
      Error (failure "an authored root has no delegated resource preparation purpose")
  ;;

  let check_captured t authority =
    match t with
    | Original_execution ->
      Delegation_authority.check_captured_artifact_preparation authority
    | Resource_setup { prefix; check_resource } ->
      Delegation_authority.check_native_resource_captured_artifact_preparation
        authority
        ~prefix
        ~check_resource
  ;;

  let check_authored t authority =
    match t with
    | Original_execution -> Delegation_authority.check_authored_preparation authority
    | Resource_setup { prefix; check_resource } ->
      Delegation_authority.check_native_resource_authored_preparation
        authority
        ~prefix
        ~check_resource
  ;;

  let check_generated t authority =
    match t with
    | Original_execution -> Delegation_authority.check_preparation authority
    | Resource_setup { prefix; check_resource } ->
      Delegation_authority.check_native_resource_preparation
        authority
        ~prefix
        ~check_resource
  ;;
end

module Native_input = struct
  type family =
    | Authored of
        { revision : Prompt_revision.t
        ; root_invocation :
            Chat_response.Script_contract_plan.Program.Role.Moderator_invocation.t
        ; agent_source : Captured_agent_runner.Source_binding.t option
        ; script_preparation : Script_preparation.t option
        ; native_registrations : Agent_runtime.native_registration list
        }
    | Captured of
        { captured : Captured_recurrence_source.t
        ; script_preparation : Script_preparation.t option
        ; authority : Delegation_authority.t
        ; agent_source : Captured_agent_runner.Source_binding.t option
        ; native_registrations : Agent_runtime.native_registration list
        }
    | Generated of
        { definition : Generated_definition.t
        ; artifact_store : Agent_store.Prompt_artifact_store.t
        ; parent_runtime : Agent_runtime.t
        ; source_lease : Prompt_source_lease.t
        ; inherited_managed : inherited_managed option
        ; authority : Delegation_authority.t
        }
    | Authored_child of
        { revision : Prompt_revision.t
        ; prepared : authored_resources
        ; code : Authored_code_preparation.t option
        ; authority : Delegation_authority.t
        }

  type t =
    { selected : Source.t
    ; construction : source
    ; agent_source : Captured_agent_runner.Source_binding.t option
    ; native_registrations : Agent_runtime.native_registration list
    ; inherited_source_lease : Prompt_source_lease.t option
    }

  let same_artifact a b =
    Agent_protocol.Id.Prompt_revision.equal
      a.Agent_store.Prompt_artifact_store.Artifact.revision_id
      b.Agent_store.Prompt_artifact_store.Artifact.revision_id
    && Agent_store.Prompt_artifact_store.Artifact.same_content a b
    && String.equal a.manifest_sha256 b.manifest_sha256
  ;;

  let matches_source (construction : source) selected =
    match construction, Source.selection selected with
    | Authored { revision; root_invocation; _ }, Authored retained ->
      same_artifact
        (Prompt_revision.artifact revision)
        (Prompt_revision.artifact retained)
      && Script_preparation.Invocation.equal
           (Script_preparation.Invocation.of_root root_invocation)
           (Source.invocation selected)
    | Authored_child { revision; authority; _ }, Authored_child { source; reference } ->
      same_artifact
        (Prompt_revision.artifact revision)
        (Prompt_revision.artifact (Prompt_child_source.revision source))
      && Agent_store.Delegation_store.Reference.equal
           reference
           (Delegation_authority.reference authority)
    | Authored_child { revision; prepared; authority; _ }, Captured retained ->
      let origin = Captured_recurrence_source.origin retained in
      (match Recurrence_child_origin.source_kind origin with
       | Artifact -> false
       | Authored_tool selector ->
         same_artifact
           (Prompt_revision.artifact revision)
           (Prompt_revision.artifact (Captured_recurrence_source.revision retained))
         && String.equal
              selector.name
              (Authored_agent_source.identity prepared.source).tool_name
         && String.equal
              selector.source_sha256
              (Authored_agent_source.fingerprint prepared.source)
         && Recurrence_child_origin.matches_reference
              origin
              (Delegation_authority.reference authority)
         && Chat_response.Script_contract_plan.Program.Role.Moderator_invocation.equal
              (Recurrence_child_origin.root_invocation origin)
              Ui
         && Script_preparation.Invocation.equal (Source.invocation selected) Delegated_ui)
    | Generated { definition; _ }, Generated retained ->
      same_artifact
        (Generated_definition.artifact definition)
        (Generated_definition.artifact retained)
      && List.equal
           (Tuple2.equal ~eq1:String.equal ~eq2:String.equal)
           (Generated_definition.capability_pins definition)
           (Generated_definition.capability_pins retained)
    | Captured_artifact { captured; _ }, Captured retained ->
      same_artifact
        (Prompt_revision.artifact (Captured_recurrence_source.revision captured))
        (Prompt_revision.artifact (Captured_recurrence_source.revision retained))
      && Agent_store.Captured_source_kind.equal
           (Recurrence_child_origin.source_kind
              (Captured_recurrence_source.origin captured))
           (Recurrence_child_origin.source_kind
              (Captured_recurrence_source.origin retained))
      && Recurrence_child_reservation.equal
           (Recurrence_child_origin.reservation
              (Captured_recurrence_source.origin captured))
           (Recurrence_child_origin.reservation
              (Captured_recurrence_source.origin retained))
    | ( (Authored _ | Captured_artifact _ | Authored_child _ | Generated _)
      , (Authored _ | Captured _ | Authored_child _ | Generated _ | Generated_artifact _)
      ) -> false
  ;;

  let same_selection a b =
    let same_family =
      match Source.selection a, Source.selection b with
      | Authored _, Authored _ -> true
      | Captured a, Captured b ->
        Agent_store.Captured_source_kind.equal
          (Recurrence_child_origin.source_kind (Captured_recurrence_source.origin a))
          (Recurrence_child_origin.source_kind (Captured_recurrence_source.origin b))
        && Recurrence_child_reservation.equal
             (Recurrence_child_origin.reservation (Captured_recurrence_source.origin a))
             (Recurrence_child_origin.reservation (Captured_recurrence_source.origin b))
      | Authored_child { reference = a; _ }, Authored_child { reference = b; _ } ->
        Agent_store.Delegation_store.Reference.equal a b
      | Generated a, Generated b ->
        List.equal
          (Tuple2.equal ~eq1:String.equal ~eq2:String.equal)
          (Generated_definition.capability_pins a)
          (Generated_definition.capability_pins b)
      | ( (Authored _ | Captured _ | Authored_child _ | Generated _ | Generated_artifact _)
        , (Authored _ | Captured _ | Authored_child _ | Generated _ | Generated_artifact _)
        ) -> false
    in
    same_family
    && same_artifact (Source.artifact a) (Source.artifact b)
    && Option.equal
         Agent_protocol.Id.Prompt_definition.equal
         (Source.definition_id a)
         (Source.definition_id b)
    && Script_preparation.Invocation.equal (Source.invocation a) (Source.invocation b)
    && Source.Moderator.equal (Source.moderator a) (Source.moderator b)
  ;;

  let create ~source ~family =
    let construction, agent_source, native_registrations =
      match family with
      | Authored
          { revision
          ; root_invocation
          ; agent_source
          ; script_preparation
          ; native_registrations
          } ->
        ( (Authored { revision; root_invocation; script_preparation } : source)
        , agent_source
        , native_registrations )
      | Captured
          { captured; script_preparation; authority; agent_source; native_registrations }
        ->
        ( (Captured_artifact { captured; script_preparation; authority } : source)
        , agent_source
        , native_registrations )
      | Generated
          { definition
          ; artifact_store
          ; parent_runtime
          ; source_lease = _
          ; inherited_managed
          ; authority
          } ->
        ( (Generated
             { definition; artifact_store; parent_runtime; inherited_managed; authority }
           : source)
        , None
        , [] )
      | Authored_child { revision; prepared; code; authority } ->
        (Authored_child { revision; prepared; code; authority } : source), None, []
    in
    if matches_source construction source
    then (
      let inherited_source_lease =
        match family with
        | Generated { source_lease; _ } -> Some (Prompt_source_lease.borrow source_lease)
        | Authored _ | Captured _ | Authored_child _ -> None
      in
      Ok
        { selected = source
        ; construction
        ; agent_source
        ; native_registrations
        ; inherited_source_lease
        })
    else Error (failure "native input differs from its selected executable source family")
  ;;
end

module Native_host = struct
  type t =
    { preparation : Native_preparation.t
    ; check_source : unit -> (unit, Agent_protocol.Error.t) result
    ; startup : Chat_response.Startup_compilation.t option
    ; env : Eio_unix.Stdenv.base
    ; inference_context : Inference_runtime.Context.t
    ; inference_identity : Chat_response.Neutral_turn.Identity.t
    ; bind_inference_identity :
        Inference_allocation.Port.t -> Chat_response.Neutral_turn.Identity.t
    ; on_inference_attempt : Inference_runtime.Attempt.t -> unit
    ; on_inference_completion : Inference_client.Completion.t -> unit
    ; on_inference_observation : Inference.Observation.t -> unit
    ; paths : Runtime_paths.t
    ; storage_paths : Runtime_paths.t
    ; session_id : Agent_protocol.Id.Session.t
    ; permission_profile : Permission_policy.t
    ; native_service_revision : string option
    ; one_off_policy : Chat_response.One_off_request.policy option
    ; authoring_validation_host : Chat_response.Authoring_validation.host option
    ; initial_agent_guidance : Chat_response.Agent_guidance.t option
    ; ancestor_capabilities : ancestor_capabilities option
    ; ancestor_sources : ancestor_sources option
    ; manifest_authorizer : Shell_runtime.Manifest_authorizer.t
    ; approval_provider : Shell_runtime.Approval_broker.provider
    ; approval_store : Shell_access.Approval.store
    }
end

let source_authority = function
  | Authored _ -> None
  | Captured_artifact { authority; _ }
  | Authored_child { authority; _ }
  | Generated { authority; _ } -> Some authority
;;

let require_authority authority =
  match Delegation_authority.check_execution authority with
  | Ok () -> ()
  | Error error -> raise (Chat_response.Ctx.Effect_rejected error)
;;

let guarded_services authority (services : extension_services) =
  let checked f =
    let open Result.Let_syntax in
    let%bind () = Delegation_authority.check_execution authority in
    let%bind result = f () in
    let%map () = Delegation_authority.check_execution authority in
    result
  in
  { services with
    script_tools =
      (fun native ->
        Script_tool_calls.with_authorization_guard
          (services.script_tools native)
          ~check:(fun () -> Delegation_authority.check_execution authority))
  ; claim_lifecycle =
      (fun ~event ~snapshot f ->
        checked (fun () -> services.claim_lifecycle ~event ~snapshot f))
  ; run_actions = (fun executing -> checked (fun () -> services.run_actions executing))
  ; notification_input =
      (fun ~source ~tools ~consume () ->
        checked (fun () -> services.notification_input ~source ~tools ~consume ()))
  ; initial_notification_input =
      (fun ~source ~tools ~consume () ->
        checked (fun () -> services.initial_notification_input ~source ~tools ~consume ()))
  ; idle_notifications =
      (fun ~source ~tools () ->
        checked (fun () -> services.idle_notifications ~source ~tools ()))
  ; standalone_completion =
      (fun ~tools job -> checked (fun () -> services.standalone_completion ~tools job))
  }
;;

type runtime = t

module Product_identity = struct
  type t = unit ref

  let equal = phys_equal
end

module Product_source = struct
  module Family = struct
    type t =
      | Authored
      | Captured of Agent_store.Captured_source_kind.t
      | Authored_child
      | Generated of (string * string) list
    [@@deriving equal]
  end

  type t =
    { session_id : Agent_protocol.Id.Session.t
    ; artifact : Agent_store.Prompt_artifact_store.Artifact.t
    ; family : Family.t
    ; authored_invocation : Runtime_script_preparation.Invocation.t option
    }

  let create ?selected source ~session_id =
    let artifact, family =
      match source with
      | Authored { revision; _ } -> Prompt_revision.artifact revision, Family.Authored
      | Captured_artifact { captured; _ } ->
        ( Prompt_revision.artifact (Captured_recurrence_source.revision captured)
        , Family.Captured
            (Recurrence_child_origin.source_kind
               (Captured_recurrence_source.origin captured)) )
      | Authored_child { revision; _ } ->
        Prompt_revision.artifact revision, Family.Authored_child
      | Generated { definition; _ } ->
        ( Generated_definition.artifact definition
        , Family.Generated (Generated_definition.capability_pins definition) )
    in
    (* Physical specialist preparation keeps its borrowed Authored_child path.
       The validated Native_input retains the genuine captured source family;
       never turn an Authored_tool occurrence into an Artifact/root Product. *)
    let family =
      match source, selected with
      | Authored_child _, Some selected ->
        (match Source.selection selected with
         | Captured captured ->
           (match
              Recurrence_child_origin.source_kind
                (Captured_recurrence_source.origin captured)
            with
            | Authored_tool _ as kind -> Family.Captured kind
            | Artifact -> family)
         | Authored _ | Authored_child _ | Generated _ | Generated_artifact _ -> family)
      | ( (Authored _ | Captured_artifact _ | Authored_child _ | Generated _)
        , (None | Some _) ) -> family
    in
    let authored_invocation =
      match source with
      | Authored { root_invocation; _ } ->
        Some (Runtime_script_preparation.Invocation.of_root root_invocation)
      | Captured_artifact _ | Authored_child _ | Generated _ -> None
    in
    { session_id; artifact; family; authored_invocation }
  ;;

  let check_authored t ~revision ~(expected : Session_state.t) =
    let artifact = Prompt_revision.artifact revision in
    if
      Agent_protocol.Id.Session.equal t.session_id expected.identity.session_id
      && Option.is_none expected.spec.delegation
      && Family.equal t.family Authored
      && Option.equal
           Runtime_script_preparation.Invocation.equal
           t.authored_invocation
           (Some
              (Runtime_script_preparation.Invocation.of_root
                 expected.spec.root_invocation))
      && Agent_protocol.Id.Prompt_revision.equal
           t.artifact.revision_id
           artifact.revision_id
      && Agent_protocol.Id.Prompt_revision.equal
           artifact.revision_id
           expected.spec.prompt_revision_id
      && Agent_store.Prompt_artifact_store.Artifact.same_content t.artifact artifact
      &&
      match expected.spec.prompt_definition_id with
      | Some id ->
        Agent_protocol.Id.Prompt_definition.equal
          id
          (Prompt_revision.definition revision).id
      | None -> Option.is_none artifact.prompt_definition_id
    then Ok ()
    else
      Error (failure "retained authored binding differs from its actual Product source")
  ;;

  let check t ~source ~(expected : Session_state.t) =
    let open Result.Let_syntax in
    let%bind () = Source.validate_target source expected in
    let family =
      match Source.selection source with
      | Authored _ -> Family.Authored
      | Captured captured ->
        Family.Captured
          (Recurrence_child_origin.source_kind
             (Captured_recurrence_source.origin captured))
      | Authored_child _ -> Family.Authored_child
      | Generated definition ->
        Family.Generated (Generated_definition.capability_pins definition)
      | Generated_artifact source ->
        Family.Generated (Generated_definition.Source.capability_pins source)
    in
    let artifact = Source.artifact source in
    if
      Agent_protocol.Id.Session.equal t.session_id expected.identity.session_id
      && Agent_protocol.Id.Prompt_revision.equal
           t.artifact.revision_id
           artifact.revision_id
      && Agent_store.Prompt_artifact_store.Artifact.same_content t.artifact artifact
      && Family.equal t.family family
    then Ok ()
    else Error (failure "retained binding differs from its actual Product source")
  ;;
end

module Retained_lifecycle = struct
  module Basis = struct
    type source =
      | Prepared of Source.t
      | Authored_root of
          { revision : Prompt_revision.t
          ; invocation : Runtime_script_preparation.Invocation.t
          }

    type t =
      { source : source
      ; session_id : Agent_protocol.Id.Session.t
      ; generation : int
      ; installed_source : Agent_protocol.Run_source.t option
      }

    let installed_source (expected : Session_state.t) =
      let open Result.Let_syntax in
      let%bind observer = Moderator_checkpoint.observer expected.moderator in
      match observer with
      | None -> Ok None
      | Some _ ->
        Moderator_source_installation.capture expected
        |> Result.map ~f:(fun (_, source) -> Some source)
    ;;

    let capture ~source ~(expected : Session_state.t) =
      let open Result.Let_syntax in
      let%bind () = Source.validate_target source expected in
      let%map installed_source = installed_source expected in
      { source = Prepared source
      ; session_id = expected.identity.session_id
      ; generation = expected.identity.generation
      ; installed_source
      }
    ;;

    let capture_authored ~revision ~(expected : Session_state.t) =
      let open Result.Let_syntax in
      let%map installed_source = installed_source expected in
      { source =
          Authored_root
            { revision
            ; invocation =
                Runtime_script_preparation.Invocation.of_root
                  expected.spec.root_invocation
            }
      ; session_id = expected.identity.session_id
      ; generation = expected.identity.generation
      ; installed_source
      }
    ;;

    let matches_source t source =
      match t.source with
      | Prepared actual -> phys_equal actual source
      | Authored_root { revision; invocation } ->
        (match Source.selection source with
         | Authored actual ->
           let expected = Prompt_revision.artifact revision in
           let actual = Prompt_revision.artifact actual in
           Runtime_script_preparation.Invocation.equal
             invocation
             (Source.invocation source)
           && Agent_protocol.Id.Prompt_revision.equal
                expected.revision_id
                actual.revision_id
           && Agent_store.Prompt_artifact_store.Artifact.same_content expected actual
         | Captured _ | Authored_child _ | Generated _ | Generated_artifact _ -> false)
    ;;
  end

  type t =
    { domain : int
    ; basis : Basis.t
    ; worker : Operation_worker.t
    ; manager : Manager.t option
    ; check_owner : unit -> (unit, Agent_protocol.Error.t) result
    }

  let check t ~source ~(expected : Session_state.t) ~worker ~manager =
    let open Result.Let_syntax in
    let%bind () =
      if Int.equal t.domain (Stdlib.Domain.self () :> int)
      then Ok ()
      else Error (failure "retained lifecycle belongs to another domain")
    in
    let%bind () = t.check_owner () in
    let%bind () = Source.validate_target source expected in
    let%bind installed_source = Basis.installed_source expected in
    if
      Basis.matches_source t.basis source
      && Agent_protocol.Id.Session.equal t.basis.session_id expected.identity.session_id
      && Int.equal t.basis.generation expected.identity.generation
      && Option.equal
           Agent_protocol.Run_source.equal
           t.basis.installed_source
           installed_source
      && phys_equal t.worker worker
      && Option.equal phys_equal t.manager manager
    then Ok ()
    else Error (failure "retained lifecycle lost its exact source or runtime binding")
  ;;
end

module Pending_legacy_resume = struct
  module Inference = struct
    type t =
      { identity : Inference_client.Identity.t
      ; bind_identity : Inference_allocation.Port.t -> Inference_client.Identity.t
      ; on_attempt : Inference_runtime.Attempt.t -> unit
      ; on_observation : Inference.Observation.t -> unit
      ; on_completion : Inference_client.Completion.t -> unit
      }
  end

  type t =
    { run :
        host:legacy_construction
        -> inference:(Inference.t, Agent_protocol.Error.t) result Lazy.t
        -> (Runtime_resume.t, Agent_protocol.Error.t) result
    }

  let run_scoped t ~host ~inference =
    match host.startup with
    | Legacy_startup.Constructor -> t.run ~host ~inference
    | Installed_resume ->
      Error (failure "retained legacy Resume requires its actual scoped constructor")
  ;;
end

module Attachment = struct
  module Resume = struct
    type t =
      | Continue_live
      | Pending of Runtime_resume.t
      | Pending_legacy of Pending_legacy_resume.t
  end

  type origin =
    | Cold
    | Retained

  type t =
    { runtime : runtime
    ; product : Product_identity.t option
    ; native_identity : unit -> Native_identity.t option
    ; origin : origin
    ; resume : unit -> (Resume.t, Agent_protocol.Error.t) result
    ; retained_lifecycle :
        unit -> (Retained_lifecycle.t option, Agent_protocol.Error.t) result
    }

  let runtime t = t.runtime
  let close_reported t = Runtime_close.run_reported t.runtime.close
  let product t = t.product
  let native_identity t = t.native_identity ()
  let origin t = t.origin
  let runtime_resume t = t.resume ()
  let retained_lifecycle t = t.retained_lifecycle ()

  let cold runtime =
    { runtime
    ; product = None
    ; native_identity = (fun () -> None)
    ; origin = Cold
    ; resume =
        (fun () -> Result.map (runtime.runtime_resume ()) ~f:(fun r -> Resume.Pending r))
    ; retained_lifecycle = (fun () -> Ok None)
    }
  ;;
end

(* Factual affinity of an already-completed retained physical policy runtime.
   Unlike Retained_lifecycle this is independent of any retired worker binding. *)
module Policy_access = struct
  type t =
    { domain : int
    ; session_id : Agent_protocol.Id.Session.t
    ; generation : int
    ; stop_epoch : int64
    ; spec : Session_state.Spec.Resource_identity.t
    ; source : Agent_protocol.Run_source.t
    ; check_resource : unit -> (unit, Agent_protocol.Error.t) result
    }

  let check t ~(expected : Session_state.t) =
    let open Result.Let_syntax in
    let%bind () =
      if Int.equal t.domain (Stdlib.Domain.self () :> int)
      then Ok ()
      else Error (failure "retained policy access belongs to another domain")
    in
    let%bind () = t.check_resource () in
    let%bind _, source = Moderator_source_installation.capture expected in
    if
      Agent_protocol.Id.Session.equal t.session_id expected.identity.session_id
      && Int.equal t.generation expected.identity.generation
      && Int64.equal t.stop_epoch expected.stop_epoch
      && Session_state.Spec.Resource_identity.equal
           t.spec
           (Session_state.Spec.Resource_identity.capture expected.spec)
      && Agent_protocol.Run_source.equal t.source source
    then Ok ()
    else Error (failure "retained policy access lost its actual source or Stop basis")
  ;;
end

module Restored_policy_access = struct
  type t =
    { domain : int
    ; product_identity : Product_identity.t
    ; basis : Policy_restore_basis.t
    ; check_resource : unit -> (unit, Agent_protocol.Error.t) result
    }

  let product_identity t = t.product_identity
  let basis t = t.basis

  let check t ~expected =
    let open Result.Let_syntax in
    let%bind () =
      if Int.equal t.domain (Stdlib.Domain.self () :> int)
      then Ok ()
      else Error (failure "restored policy access belongs to another domain")
    in
    let%bind () = t.check_resource () in
    Policy_restore_basis.check_policy t.basis ~state:expected
  ;;
end

module Entry_inference_binding : sig
  module Allocation : sig
    type t

    val close : t -> unit
  end

  type t

  val create
    :  ?on_allocated:(Allocation.t -> unit)
    -> Inference_runtime.Context.t
    -> sw:Eio.Switch.t
    -> (t, Agent_protocol.Error.t) result

  val context : t -> Inference_runtime.Context.t
  val root_binding : t -> Chat_response.Root_binding.t
  val is_closed : t -> bool
end = struct
  module Allocation = struct
    type t =
      { session : Inference_runtime.Session.t
      ; root_binding : Chat_response.Root_binding.t
      ; mutable closing : bool
      ; mutable root_closed : bool
      ; mutable session_closed : bool
      }

    let close t =
      t.closing <- true;
      let first_failure = ref None in
      let release ~done_ ~mark_done f =
        if not done_
        then (
          try
            f ();
            mark_done ()
          with
          | exn ->
            if Option.is_none !first_failure
            then first_failure := Some (exn, Stdlib.Printexc.get_raw_backtrace ()))
      in
      release
        ~done_:t.root_closed
        ~mark_done:(fun () -> t.root_closed <- true)
        (fun () -> Chat_response.Root_binding.close t.root_binding);
      release
        ~done_:t.session_closed
        ~mark_done:(fun () -> t.session_closed <- true)
        (fun () -> Inference_runtime.Session.close t.session);
      Option.iter !first_failure ~f:(fun (exn, backtrace) ->
        Exn.raise_with_original_backtrace exn backtrace)
    ;;
  end

  type t =
    { allocation : Allocation.t
    ; context : Inference_runtime.Context.t
    }

  let create ?(on_allocated = ignore) inference_context ~sw =
    let open Result.Let_syntax in
    let session = Inference_runtime.Session.create ~sw in
    let root_binding = Chat_response.Root_binding.create session in
    let allocation =
      { Allocation.session
      ; root_binding
      ; closing = false
      ; root_closed = false
      ; session_closed = false
      }
    in
    on_allocated allocation;
    let%map context =
      Inference_runtime.Context.with_session
        (Inference_runtime.Context.detach inference_context)
        session
      |> Result.map_error ~f:(fun error ->
        failure (Sexp.to_string_hum (Inference_runtime.Preparation_error.sexp_of_t error)))
    in
    { allocation; context }
  ;;

  let context t = t.context
  let root_binding t = t.allocation.root_binding

  let is_closed t =
    t.allocation.closing || Inference_runtime.Session.is_closed t.allocation.session
  ;;
end

module Native_release = struct
  module Attempt = struct
    type outcome = unit Close_observation.t

    type t =
      | Idle
      | Running of outcome Eio.Promise.t
  end

  type t =
    { sw : Eio.Switch.t option
    ; mutable allocation : Entry_inference_binding.Allocation.t option
    ; mutable cache : (Chat_response.Cache.t * Runtime_paths.t) option
    ; mutable cache_saved : bool
    ; mutable closing : bool
    ; mutable attempt : Attempt.t
    }

  let create ~sw =
    { sw
    ; allocation = None
    ; cache = None
    ; cache_saved = false
    ; closing = false
    ; attempt = Attempt.Idle
    }
  ;;

  let close_reported t ~before_cache =
    t.closing <- true;
    Eio.Cancel.protect (fun () ->
      match t.attempt with
      | Attempt.Running pending -> Eio.Promise.await pending
      | Attempt.Idle ->
        let pending, resolve = Eio.Promise.create () in
        t.attempt <- Attempt.Running pending;
        let failures = ref [] in
        let release f =
          try f () with
          | exn -> failures := (exn, Stdlib.Printexc.get_raw_backtrace ()) :: !failures
        in
        let selected =
          try
            release (fun () ->
              Option.iter t.allocation ~f:Entry_inference_binding.Allocation.close);
            release (fun () ->
              if not t.cache_saved
              then (
                before_cache ();
                Option.iter t.cache ~f:(fun (cache, paths) ->
                  Chat_response.Cache.save
                    ~file:Eio.Path.(paths.Runtime_paths.cache_dir / "cache.bin")
                    cache);
                t.cache_saved <- true));
            List.hd (List.rev !failures)
          with
          | exn ->
            let failure = exn, Stdlib.Printexc.get_raw_backtrace () in
            failures := failure :: !failures;
            Some failure
        in
        let diagnostic =
          match List.rev !failures with
          | [] -> None
          | failures ->
            Some
              (Cleanup_diagnostic.create
                 (List.map failures ~f:(fun (exn, trace) ->
                    Callback_cleanup.Failure.Raised (exn, trace))))
        in
        let outcome =
          Close_observation.create
            (match selected with
             | None -> Returned ()
             | Some (exn, trace) -> Raised (exn, trace))
            ~diagnostics:(Option.to_list diagnostic)
            ?primary:diagnostic
            ()
        in
        (* All joiners observe this SAME actual attempt before raw projection.
           A failed attempt keeps completed legs for a later explicit retry. *)
        t.attempt <- Attempt.Idle;
        Eio.Promise.resolve resolve outcome;
        outcome)
  ;;

  let close t ~before_cache = close_reported t ~before_cache |> Close_observation.resume

  let record_allocation t allocation =
    if Option.is_some t.allocation then invalid_arg "native allocation already recorded";
    t.allocation <- Some allocation;
    (* Register after Session.create's own finalizer. The common close runs first
       on switch unwind and covers a failed validation or a lost final return. *)
    Option.iter t.sw ~f:(fun sw ->
      Eio.Switch.on_release sw (fun () -> close t ~before_cache:Fn.id))
  ;;

  let record_cache t cache paths =
    if Option.is_some t.cache then invalid_arg "native cache already recorded";
    t.cache <- Some (cache, paths)
  ;;

  let is_closed t = t.closing
end

module Prepared_native_data = struct
  module Environment = struct
    type t =
      { sw : Eio.Switch.t
      ; env : Eio_unix.Stdenv.base
      ; paths : Runtime_paths.t
      ; session_id : Agent_protocol.Id.Session.t
      ; bind_inference_identity :
          Inference_allocation.Port.t -> Chat_response.Neutral_turn.Identity.t
      ; on_inference_attempt : Inference_runtime.Attempt.t -> unit
      ; on_inference_completion : Inference_client.Completion.t -> unit
      ; on_inference_observation : Inference.Observation.t -> unit
      ; manifest_authorizer : Shell_runtime.Manifest_authorizer.t
      ; approval_provider : Shell_runtime.Approval_broker.provider
      }
  end

  type t =
    { identity : Native_identity.t
    ; domain : int
    ; source : source
    ; artifact : Agent_store.Prompt_artifact_store.Artifact.t
    ; permission_profile_digest : string
    ; selected : Source.t option
    ; environment : Environment.t
    ; entry_inference_binding : Entry_inference_binding.t
    ; release : Native_release.t
    ; storage_paths : Runtime_paths.t
    ; ctx : Eio_unix.Stdenv.base Chat_response.Ctx.t
    ; mutable resources : resources
    ; mutable source_lease_attached : bool
    ; elements : Prompt.Chat_markdown.top_level_elements list
    ; delegated : bool
    ; declares_one_off : bool
    }

  let create
        ~source
        ~artifact
        ~permission_profile_digest
        ~selected
        ~environment
        ~entry_inference_binding
        ~release
        ~storage_paths
        ~ctx
        ~resources
        ~elements
        ~delegated
        ~declares_one_off
    =
    { identity = ref ()
    ; domain = (Stdlib.Domain.self () :> int)
    ; source
    ; artifact
    ; permission_profile_digest
    ; selected
    ; environment
    ; entry_inference_binding
    ; release
    ; storage_paths
    ; ctx
    ; resources
    ; source_lease_attached = false
    ; elements
    ; delegated
    ; declares_one_off
    }
  ;;

  let resources t = t.resources
  let context t = t.ctx
  let root_binding t = Entry_inference_binding.root_binding t.entry_inference_binding

  let check_open t =
    if not (Int.equal t.domain (Stdlib.Domain.self () :> int))
    then Error (failure "prepared native resources belong to another domain")
    else if
      Native_release.is_closed t.release
      || Entry_inference_binding.is_closed t.entry_inference_binding
    then Error (failure "prepared native resources have been closed")
    else Ok ()
  ;;
end

let validate_native_source source ~preparation ~session_id ~permission_profile =
  let open Result.Let_syntax in
  match source with
  | Authored _ -> Native_preparation.check_root preparation
  | Captured_artifact { captured; authority; _ } ->
    let artifact =
      Prompt_revision.artifact (Captured_recurrence_source.revision captured)
    in
    Native_preparation.check_captured
      preparation
      authority
      ~origin:(Captured_recurrence_source.origin captured)
      ~session_id
      ~revision_id:artifact.revision_id
      ~manifest_sha256:artifact.manifest_sha256
      ~permission_profile
  | Authored_child { revision; prepared; code; authority } ->
    let artifact = Prompt_revision.artifact revision in
    let%bind () =
      match code with
      | Some code ->
        Authored_code_preparation.check
          code
          ~original:
            { source = prepared.source
            ; revision = prepared.revision
            ; native = prepared.resources.native
            ; definition = prepared.resources.definition
            ; managed = prepared.resources.managed
            ; agent_source = prepared.agent_source
            ; prepare_persistent_source = None
            }
          ~selected:revision
      | None ->
        let%bind expected =
          Authored_agent_source.artifact
            prepared.source
            ~revision_id:artifact.revision_id
            ~created_at:artifact.created_at
          |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
        in
        if String.equal expected.manifest_sha256 artifact.manifest_sha256
        then Ok ()
        else
          Error (failure "authored runtime source differs from its prepared specialist")
    in
    let%bind () =
      Agent_store.Prompt_artifact_store.verify_tree
        ~root:(Prompt_revision.materialized_tree prepared.revision)
        (Prompt_revision.artifact prepared.revision)
      |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
    in
    let%bind capabilities =
      Lazy.force prepared.resources.native.capabilities
      |> Result.map_error ~f:(fun error ->
        failure error.Chat_response.Tool_capability.message)
    in
    Native_preparation.check_authored
      preparation
      authority
      ~origin:
        { name = (Authored_agent_source.identity prepared.source).tool_name
        ; source_sha256 = Authored_agent_source.fingerprint prepared.source
        }
      ~capabilities
      ~session_id
      ~revision_id:artifact.revision_id
      ~manifest_sha256:artifact.manifest_sha256
      ~permission_profile
  | Generated { definition; authority; _ } ->
    Native_preparation.check_generated
      preparation
      authority
      ~session_id
      ~revision_id:(Generated_definition.artifact definition).revision_id
      ~manifest_sha256:(Generated_definition.artifact definition).manifest_sha256
      ~permission_profile
;;

let prepare_native_resources
      ~mcp_checks
      ~preparation
      source
      ~startup
      ~agent_source
      ~native_registrations
      ~native_checkpoint
      ~native_service_revision
      ~one_off_policy
      ~authoring_validation_host
      ~sw
      ~env
      ~paths
      ~session_id
      ~permission_profile
      ~elements
      ~ctx
      ~manifest_authorizer
      ~approval_provider
      ~approval_store
      ~response_dir
  =
  let open Result.Let_syntax in
  match source with
  | Authored_child { prepared; code; _ } ->
    let resources = prepared.resources in
    (match code with
     | None -> Ok resources
     | Some code ->
       Ok
         { resources with
           native = Authored_code_preparation.native code
         ; definition = Authored_code_preparation.definition code
         ; managed = Authored_code_preparation.managed code
         })
  | Generated { definition; parent_runtime; inherited_managed; _ } ->
    inherit_prepared_resources
      ~inherited_managed:
        (Option.map inherited_managed ~f:(fun inherited -> inherited.delegation))
      ~parent_runtime
      ~definition
  | (Authored _ | Captured_artifact _) as source ->
    let revision, script_preparation, root_invocation =
      match source with
      | Authored { revision; script_preparation; root_invocation } ->
        revision, script_preparation, root_invocation
      | Captured_artifact { captured; script_preparation; _ } ->
        ( Captured_recurrence_source.revision captured
        , script_preparation
        , Recurrence_child_origin.root_invocation
            (Captured_recurrence_source.origin captured) )
      | Authored_child _ | Generated _ -> assert false
    in
    let checkpoint () =
      Eio.Fiber.check ();
      Option.iter native_checkpoint ~f:(fun check ->
        match check () with
        | Ok () -> ()
        | Error error -> raise (Construction_rejected error));
      match preparation with
      | Native_preparation.Original_execution ->
        Option.iter (source_authority source) ~f:require_authority
      | Resource_setup _ ->
        (match
           validate_native_source source ~preparation ~session_id ~permission_profile
         with
         | Ok () -> ()
         | Error error -> raise (Construction_rejected error))
    in
    checkpoint ();
    let%bind host = host ~env ~paths ~session_id ~elements in
    create_authored_resources
      ~mcp_checks
      ~checkpoint
      ~startup
      ~agent_source
      ~script_preparation
      ~revision
      ~additional_native_registrations:native_registrations
      ~invocation:(Script_preparation.Invocation.of_root root_invocation)
      ~native_service_revision
      ~env
      ~one_off_policy
      ~authoring_validation_host
      ~sw
      ~ctx
      ~host
      ~manifest_authorizer
      ~approval_provider
      ~approval_store
      ~response_dir
      ~f:resources_only
;;

let prepare_extension_services source ~env extension_services =
  let open Result.Let_syntax in
  let extension_services =
    match source_authority source, extension_services with
    | Some authority, Some services -> Some (guarded_services authority services)
    | None, Some services ->
      Some
        { services with
          script_tools =
            (fun native ->
              Script_tool_calls.with_local_execution_current
                (services.script_tools native))
        }
    | (None | Some _), None -> None
  in
  let extension_services =
    match source, extension_services with
    | Authored_child { code = Some code; _ }, Some services ->
      Some
        { services with
          script_tools =
            (fun native ->
              let tools =
                Script_tool_calls.with_agent_source_code_view
                  (services.script_tools native)
                  (Authored_code_preparation.agent_source_code_view code)
              in
              Option.value_map
                (Authored_code_preparation.shell_code_view code)
                ~default:tools
                ~f:(Script_tool_calls.with_shell_code_view tools))
        }
    | _, services -> services
  in
  let%bind extension_services =
    match extension_services with
    | None -> Ok None
    | Some services ->
      let%bind host =
        Authoring_runtime.configure_host
          ?host:services.authoring_validation_host
          ~policy:services.one_off_policy
          ()
        |> Result.map_error ~f:failure
      in
      let host =
        Some
          (match source with
           | Authored _ | Captured_artifact _ -> host
           | Generated _ | Authored_child _ ->
             Chat_response.Authoring_validation.for_delegated host)
      in
      Ok
        (Some
           { services with
             authoring_validation_host = host
           ; script_tools =
               (fun native ->
                 Script_tool_calls.with_authoring_validation_host
                   ~env
                   (services.script_tools native)
                   host)
           })
  in
  Ok extension_services
;;

let prepare_entry_native
      source
      ~preparation
      ~selected
      ~ancestor_capabilities
      ~ancestor_sources
      ~native_checkpoint
      ~construction_checkpoint
      ~startup
      ~agent_source
      ~native_registrations
      ~native_service_revision
      ~one_off_policy
      ~authoring_validation_host
      ~initial_agent_guidance
      ~permission_profile
      ~entry_inference_binding
      ~release
      ~sw
      ~env
      ~inference_identity
      ~bind_inference_identity
      ~on_inference_attempt
      ~on_inference_completion
      ~on_inference_observation
      ~paths
      ~storage_paths
      ~session_id
      ~manifest_authorizer
      ~approval_provider
      ~approval_store
  =
  let open Result.Let_syntax in
  let inference_context = Entry_inference_binding.context entry_inference_binding in
  let elements, artifact, materialized_tree, delegated =
    match source with
    | Captured_artifact { captured; _ } ->
      let revision = Captured_recurrence_source.revision captured in
      ( Prompt_revision.elements revision
      , Prompt_revision.artifact revision
      , Prompt_revision.materialized_tree revision
      , false )
    | Authored { revision; _ } ->
      ( Prompt_revision.elements revision
      , Prompt_revision.artifact revision
      , Prompt_revision.materialized_tree revision
      , false )
    | Authored_child { revision; _ } ->
      ( Prompt_revision.elements revision
      , Prompt_revision.artifact revision
      , Prompt_revision.materialized_tree revision
      , true )
    | Generated { definition; artifact_store; _ } ->
      let artifact = Generated_definition.artifact definition in
      ( Chat_response.Generated_admission.elements
          (Generated_definition.admission definition)
      , artifact
      , Agent_store.Prompt_artifact_store.materialized_tree
          artifact_store
          artifact.revision_id
      , true )
  in
  let%bind () =
    Agent_store.Prompt_artifact_store.verify_tree ~root:materialized_tree artifact
    |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
  in
  let response_dir = response_dir storage_paths in
  let%bind () = check_construction construction_checkpoint in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 response_dir;
  let cache = cache storage_paths in
  Native_release.record_cache release cache storage_paths;
  let ctx =
    context
      ~env
      ~inference_context
      ~inference_identity
      ~on_inference_attempt
      ~on_inference_completion
      ~on_inference_observation
      paths
      cache
  in
  let ctx =
    match source with
    | Captured_artifact { authority; _ } ->
      Chat_response.Ctx.with_effect_checkpoint ctx ~check:(fun () ->
        require_authority authority)
    | Authored _ | Authored_child _ | Generated _ -> ctx
  in
  let ctx =
    Option.value_map
      initial_agent_guidance
      ~default:ctx
      ~f:(Chat_response.Ctx.with_agent_guidance ctx)
  in
  let domain_id = (Stdlib.Domain.self () :> int) in
  let check_context () =
    Option.iter ctx.effect_checkpoint ~f:(fun check -> check ());
    Ok ()
  in
  let resource () =
    if not (Int.equal domain_id (Stdlib.Domain.self () :> int))
    then
      Error
        Chat_response.Tool.Native_error.
          { code = "mcp.foreign_domain"
          ; message = "native resources belong to another domain"
          }
    else if Native_release.is_closed release
    then
      Error
        Chat_response.Tool.Native_error.
          { code = "mcp.resources_closed"
          ; message = "prepared native resources have closed"
          }
    else Ok ()
  in
  (* The lexical setup check is consumed only by MCP construction. Retained
     Use receives check_context (original M) and resource, never preparation. *)
  let check_preparation () =
    match preparation with
    | Native_preparation.Original_execution -> check_context ()
    | Resource_setup _ ->
      (match
         validate_native_source source ~preparation ~session_id ~permission_profile
       with
       | Ok () -> Ok ()
       | Error error -> raise (Chat_response.Ctx.Effect_rejected error))
  in
  let mcp_checks =
    Some
      (Chat_response.Tool.Mcp_checks.canonical_native
         ~prepare:check_preparation
         ~original_context:check_context
         ~resource)
  in
  let declares_one_off = declares_native elements Run_chatml_tool.name in
  let%bind resources =
    prepare_native_resources
      ~mcp_checks
      ~preparation
      source
      ~startup
      ~agent_source
      ~native_registrations
      ~native_checkpoint
      ~native_service_revision
      ~one_off_policy
      ~authoring_validation_host
      ~sw
      ~env
      ~paths
      ~session_id
      ~permission_profile
      ~elements
      ~ctx
      ~manifest_authorizer
      ~approval_provider
      ~approval_store
      ~response_dir
  in
  let { source_lease = _
      ; native = _
      ; definition = _
      ; managed = _
      ; authoring
      ; ancestor_capabilities = _
      ; ancestor_sources = _
      }
    =
    resources
  in
  let%bind () = check_construction construction_checkpoint in
  let%bind authoring =
    match source with
    | Authored _ | Captured_artifact _ | Authored_child _ -> Ok authoring
    | Generated { definition; _ } ->
      let%bind host =
        Authoring_runtime.configure_host
          ?host:authoring_validation_host
          ~policy:
            (Option.value
               one_off_policy
               ~default:Chat_response.One_off_request.default_policy)
          ()
        |> Result.map_error ~f:failure
      in
      let admission = Generated_definition.admission definition in
      Authoring_runtime.prepare
        ~admitted:(Chat_response.Generated_admission.authoring admission)
        ~host:(Chat_response.Authoring_validation.for_delegated host)
        ~elements
        ~capabilities:(Chat_response.Generated_admission.capabilities admission)
        ()
      |> Result.map_error ~f:failure
  in
  let resources = { resources with ancestor_capabilities; ancestor_sources } in
  let resources =
    match source with
    | Generated _ -> { resources with authoring }
    | Authored _ | Captured_artifact _ | Authored_child _ -> resources
  in
  Ok
    (Prepared_native_data.create
       ~source
       ~artifact
       ~permission_profile_digest:permission_profile.Permission_policy.revision_digest
       ~selected
       ~environment:
         { sw
         ; env
         ; paths
         ; session_id
         ; bind_inference_identity
         ; on_inference_attempt
         ; on_inference_completion
         ; on_inference_observation
         ; manifest_authorizer
         ; approval_provider
         }
       ~entry_inference_binding
       ~release
       ~storage_paths
       ~ctx
       ~resources
       ~elements
       ~delegated
       ~declares_one_off)
;;

let admit_source_attachment current incoming =
  if not (Prompt_source_lease.is_legacy current)
  then Or_error.error_string "runtime original-source loan already attached"
  else Prompt_source_lease.claim incoming
;;

let with_resource_source_lease (resources : resources) ~source_lease =
  let open Or_error.Let_syntax in
  let%map () = admit_source_attachment resources.source_lease source_lease in
  { resources with source_lease }
;;

module Prepared_native = struct
  include Prepared_native_data

  module Construction = struct
    module Phase = struct
      type t =
        | Fresh
        | Preparing
        | Finished
    end

    type t =
      { domain : int
      ; sw : Eio.Switch.t
      ; release : Native_release.t
      ; mutable phase : Phase.t
      }

    let create ~sw =
      Eio.Switch.check sw;
      { domain = (Stdlib.Domain.self () :> int)
      ; sw
      ; release = Native_release.create ~sw:(Some sw)
      ; phase = Fresh
      }
    ;;

    let begin_prepare t =
      if not (Int.equal t.domain (Stdlib.Domain.self () :> int))
      then Error (failure "native construction belongs to another domain")
      else if Native_release.is_closed t.release
      then Error (failure "native construction has already been closed")
      else (
        match t.phase with
        | Fresh ->
          t.phase <- Preparing;
          Ok ()
        | Preparing | Finished ->
          Error (failure "native construction already attempted preparation"))
    ;;

    let close_reported t =
      if not (Int.equal t.domain (Stdlib.Domain.self () :> int))
      then invalid_arg "native construction belongs to another domain";
      (match t.phase with
       | Preparing -> invalid_arg "native preparation must join before construction close"
       | Fresh | Finished -> ());
      Native_release.close_reported t.release ~before_cache:Fn.id
    ;;

    let close t = close_reported t |> Close_observation.resume
  end

  let identity t = t.identity

  let selected_source t =
    let open Result.Let_syntax in
    let%bind () = check_open t in
    Result.of_option
      t.selected
      ~error:(failure "prepared native unit has no retained executable source")
  ;;

  let check t ~source ~session_id ~permission_profile_digest =
    let open Result.Let_syntax in
    let%bind () = check_open t in
    let matches =
      match t.selected with
      | None -> Native_input.matches_source t.source source
      | Some selected -> Native_input.same_selection selected source
    in
    if
      matches
      && Agent_protocol.Id.Session.equal t.environment.session_id session_id
      && String.equal t.permission_profile_digest permission_profile_digest
    then Ok ()
    else Error (failure "prepared native resources differ from the selected source")
  ;;

  module Delegation = struct
    type t =
      { authority : Delegation_authority.t
      ; selected : Chat_response.Tool_capability.t
      ; inherited_managed : inherited_managed option
      }
  end

  let with_delegation t ~f =
    let open Result.Let_syntax in
    let%bind () = check_open t in
    let context =
      match t.source with
      | Authored _ -> None
      | Generated { definition; authority; inherited_managed; _ } ->
        Some
          Delegation.
            { authority
            ; inherited_managed
            ; selected =
                Chat_response.Generated_admission.capabilities
                  (Generated_definition.admission definition)
            }
      | Captured_artifact { authority; _ } | Authored_child { authority; _ } ->
        Some
          Delegation.
            { authority
            ; inherited_managed = None
            ; selected = Delegation_authority.original_capabilities authority
            }
    in
    f context
  ;;

  let with_resources t ~f =
    let open Result.Let_syntax in
    let%bind () = check_open t in
    f t.resources
  ;;

  let attach_source_lease t ~source_lease =
    let open Or_error.Let_syntax in
    let%bind () =
      check_open t
      |> Result.map_error ~f:(fun error ->
        Core.Error.of_string error.Agent_protocol.Error.message)
    in
    let%bind () =
      match t.source with
      | Authored _ | Captured_artifact _ -> Ok ()
      | Authored_child _ | Generated _ ->
        Or_error.error_string
          "prepared source association requires an actual authored or captured root"
    in
    let%bind () =
      if t.source_lease_attached
      then Or_error.error_string "prepared original-source loan already attached"
      else admit_source_attachment t.resources.source_lease source_lease
    in
    (* Root lifetime remains owned by the actual source switch. Record its
       borrowed view only after the original one-time claim succeeds. *)
    t.resources
    <- { t.resources with source_lease = Prompt_source_lease.borrow source_lease };
    t.source_lease_attached <- true;
    Ok ()
  ;;

  let close_reported t =
    if not (Int.equal t.domain (Stdlib.Domain.self () :> int))
    then invalid_arg "prepared native resources belong to another domain";
    Native_release.close_reported t.release ~before_cache:Fn.id
  ;;

  let close t = close_reported t |> Close_observation.resume

  let prepare
        (input : Native_input.t)
        ~(construction : Construction.t)
        ~(host : Native_host.t)
    =
    let prepare () =
      let open Result.Let_syntax in
      Eio.Switch.check construction.sw;
      let%bind () = host.check_source () in
      let sw = construction.sw in
      let release = construction.release in
      let%bind entry_inference_binding =
        Entry_inference_binding.create
          ~on_allocated:(Native_release.record_allocation release)
          host.inference_context
          ~sw
      in
      let%bind () =
        validate_native_source
          input.construction
          ~preparation:host.preparation
          ~session_id:host.session_id
          ~permission_profile:host.permission_profile
      in
      let%bind () = host.check_source () in
      let%bind authoring_validation_host =
        match host.one_off_policy with
        | None -> Ok host.authoring_validation_host
        | Some policy ->
          let%map configured =
            Authoring_runtime.configure_host
              ?host:host.authoring_validation_host
              ~policy
              ()
            |> Result.map_error ~f:failure
          in
          Some
            (match input.construction with
             | Authored _ | Captured_artifact _ -> configured
             | Generated _ | Authored_child _ ->
               Chat_response.Authoring_validation.for_delegated configured)
      in
      let%bind () = host.check_source () in
      let%bind native =
        prepare_entry_native
          input.construction
          ~preparation:host.preparation
          ~selected:(Some input.selected)
          ~ancestor_capabilities:host.ancestor_capabilities
          ~ancestor_sources:host.ancestor_sources
          ~native_checkpoint:(Some host.check_source)
          ~construction_checkpoint:(Some host.check_source)
          ~startup:host.startup
          ~agent_source:input.agent_source
          ~native_registrations:input.native_registrations
          ~native_service_revision:host.native_service_revision
          ~one_off_policy:host.one_off_policy
          ~authoring_validation_host
          ~initial_agent_guidance:host.initial_agent_guidance
          ~permission_profile:host.permission_profile
          ~entry_inference_binding
          ~release
          ~sw
          ~env:host.env
          ~inference_identity:host.inference_identity
          ~bind_inference_identity:host.bind_inference_identity
          ~on_inference_attempt:host.on_inference_attempt
          ~on_inference_completion:host.on_inference_completion
          ~on_inference_observation:host.on_inference_observation
          ~paths:host.paths
          ~storage_paths:host.storage_paths
          ~session_id:host.session_id
          ~manifest_authorizer:host.manifest_authorizer
          ~approval_provider:host.approval_provider
          ~approval_store:host.approval_store
      in
      let%bind () = host.check_source () in
      let%map () =
        match input.inherited_source_lease with
        | None -> Ok ()
        | Some source_lease ->
          let%map resources =
            with_resource_source_lease native.resources ~source_lease
            |> Result.map_error ~f:(fun error -> failure (Core.Error.to_string_hum error))
          in
          native.resources <- resources;
          native.source_lease_attached <- true
      in
      native
    in
    let open Result.Let_syntax in
    let%bind () = Construction.begin_prepare construction in
    Exn.protect
      ~finally:(fun () -> construction.phase <- Finished)
      ~f:(fun () ->
        try prepare () with
        | Construction_rejected error -> Error error)
  ;;
end

module Product_attachment = struct
  module Phase = struct
    type t =
      | Fresh
      | Building
      | Built
      | Failed
      | Closed
  end

  type t =
    { domain : int
    ; native : Prepared_native.t
    ; moderator : Moderator_attachment.t
    ; callbacks : Product_callback_data.t
    ; mutable phase : Phase.t
    }

  let create native =
    let open Result.Let_syntax in
    let%map () = Prepared_native_data.check_open native in
    { domain = (Stdlib.Domain.self () :> int)
    ; native
    ; moderator =
        Moderator_attachment.create
          ~session_id:native.Prepared_native_data.environment.session_id
    ; callbacks = Product_callback_data.create ()
    ; phase = Fresh
    }
  ;;

  let native_identity t = Prepared_native.identity t.native

  let check_open t =
    if not (Int.equal t.domain (Stdlib.Domain.self () :> int))
    then Error (failure "Product attachment belongs to another domain")
    else
      let open Result.Let_syntax in
      let%bind () = Prepared_native_data.check_open t.native in
      match t.phase with
      | Fresh | Building | Built | Failed -> Ok ()
      | Closed -> Error (failure "Product attachment has been closed")
  ;;

  let check_built t =
    let open Result.Let_syntax in
    let%bind () = check_open t in
    match t.phase with
    | Built -> Ok ()
    | Fresh | Building | Failed | Closed ->
      Error (failure "Product attachment has no completed construction")
  ;;

  let build t ~native ~f =
    let open Result.Let_syntax in
    let%bind () = check_open t in
    let%bind () =
      if phys_equal t.native native
      then Ok ()
      else Error (failure "Product attachment belongs to another prepared native unit")
    in
    let%bind () =
      match t.phase with
      | Fresh ->
        t.phase <- Building;
        Ok ()
      | Building | Built | Failed | Closed ->
        Error (failure "Product attachment already attempted construction")
    in
    Exn.protect
      ~finally:(fun () ->
        match t.phase with
        | Building -> t.phase <- Failed
        | Fresh | Built | Failed | Closed -> ())
      ~f:(fun () ->
        match f () with
        | Ok _ as result ->
          t.phase <- Built;
          result
        | Error _ as result ->
          t.phase <- Failed;
          result)
  ;;

  let close t =
    if not (Int.equal t.domain (Stdlib.Domain.self () :> int))
    then invalid_arg "Product attachment belongs to another domain";
    match t.phase with
    | Building -> invalid_arg "Product construction must join before attachment close"
    | Closed -> ()
    | Fresh | Built | Failed ->
      (* This is the actual attachment-local physical leg. The worker is an inert
         record; installed work is joined by Runtime_owner before Cell release.
         No Manager close primitive exists and common native cleanup stays out. *)
      Moderator_attachment.close t.moderator;
      t.phase <- Closed
  ;;
end

module Product = struct
  module Identity = Product_identity

  module Close = struct
    type outcome = unit Close_observation.t

    type t =
      | Open
      | Running of outcome Eio.Promise.t
      | Joined of outcome
  end

  type t =
    { identity : Identity.t
    ; attachment : Product_attachment.t
    ; domain : int
    ; source : Product_source.t
    ; completed : unit -> (bool, Agent_protocol.Error.t) result
    ; policy_lifecycle : Moderator_event.Lifecycle.t option
    ; policy_manager : Manager.t option
    ; worker : Operation_worker.t
    ; snapshot : unit -> (Jsonaf.t option, Agent_protocol.Error.t) result
    ; make_binding :
        worker:Operation_worker.t
        -> history:History_entry.t list
        -> checkpoint:Jsonaf.t option
        -> close:Runtime_close.t
        -> runtime
    ; resume :
        Operation_worker.t -> unit -> (Attachment.Resume.t, Agent_protocol.Error.t) result
    ; close_resource : unit -> unit
    ; phase : Close.t Atomic.t
    }

  type product = t

  let identity t = t.identity
  let native_identity t = Product_attachment.native_identity t.attachment

  let rec close_reported t =
    Eio.Cancel.protect (fun () ->
      match Atomic.get t.phase with
      | Close.Joined observation -> observation
      | Running pending -> Eio.Promise.await pending
      | Open as previous ->
        let pending, resolver = Eio.Promise.create () in
        if not (Atomic.compare_and_set t.phase previous (Close.Running pending))
        then close_reported t
        else (
          (* This is the original Product physical producer. Cache its immutable
             observation before releasing joined callers; bindings and Cell
             fallback must forward this SAME receipt, not recapture raw close. *)
          let observation =
            Runtime_close.run_reported (Runtime_close.legacy t.close_resource)
          in
          Atomic.set t.phase (Close.Joined observation);
          Eio.Promise.resolve resolver observation;
          observation))
  ;;

  let close t = Close_observation.resume (close_reported t)

  let check_open t =
    if not (Int.equal t.domain (Stdlib.Domain.self () :> int))
    then Error (failure "runtime Product belongs to another domain")
    else (
      match Atomic.get t.phase with
      | Close.Open -> Product_attachment.check_open t.attachment
      | Running _ | Joined _ ->
        Error
          (Agent_protocol.Error.create
             Conflict
             ~message:"retained physical runtime resource is closing or closed"
             ~retryable:true
             ()))
  ;;

  let check_attachment t ~attachment =
    let open Result.Let_syntax in
    let%bind () = check_open t in
    let%bind () = Product_attachment.check_built attachment in
    if phys_equal t.attachment attachment
    then Ok ()
    else Error (failure "Product belongs to another actual attachment holder")
  ;;

  let selected_source t =
    let open Result.Let_syntax in
    let%bind () = check_open t in
    Prepared_native.selected_source t.attachment.native
  ;;

  let with_native_resources t ~f =
    let open Result.Let_syntax in
    let%bind () = check_open t in
    Prepared_native.with_resources t.attachment.native ~f
  ;;

  (* These paths are deliberately extension-only. They never invoke the generic
     completed closure, whose legacy branch can perform host checks. *)
  let check_policy_manager t ~manager =
    let open Result.Let_syntax in
    let%bind () = check_open t in
    let%bind () =
      if
        Option.exists t.policy_manager ~f:(phys_equal manager)
        && Option.is_some (Manager.extension_definition manager)
      then Ok ()
      else Error (failure "retained policy access requires its actual extension Manager")
    in
    Ok ()
  ;;

  let check_policy t ~manager =
    let open Result.Let_syntax in
    let%bind () = check_policy_manager t ~manager in
    match t.policy_lifecycle with
    | None -> Error (failure "retained policy has no actual lifecycle owner")
    | Some lifecycle ->
      (match Moderator_event.Lifecycle.status lifecycle with
       | Completed -> Ok ()
       | Ready -> Error (failure "retained policy lifecycle is still pending")
       | Failed error -> Error error)
  ;;

  let policy_access t ~(runtime : runtime) ~(expected : Session_state.t) =
    let open Result.Let_syntax in
    let%bind () = check_open t in
    let%bind manager =
      Result.of_option
        runtime.moderator_manager
        ~error:(failure "retained policy runtime has no actual Manager")
    in
    let%bind () =
      if phys_equal t.worker runtime.worker
      then Ok ()
      else Error (failure "retained policy runtime is not its actual physical Product")
    in
    let%bind () = check_policy t ~manager in
    let%map _, source = Moderator_source_installation.capture expected in
    { Policy_access.domain = t.domain
    ; session_id = expected.identity.session_id
    ; generation = expected.identity.generation
    ; stop_epoch = expected.stop_epoch
    ; spec = Session_state.Spec.Resource_identity.capture expected.spec
    ; source
    ; check_resource = (fun () -> check_policy t ~manager)
    }
  ;;

  let check_restored_policy t ~manager =
    let open Result.Let_syntax in
    let%bind () = check_policy_manager t ~manager in
    match t.policy_lifecycle with
    | None -> Error (failure "restored policy has no actual lifecycle owner")
    | Some lifecycle ->
      (match Moderator_event.Lifecycle.status lifecycle with
       | Failed error -> Error error
       | Ready when Moderator_event.Lifecycle.resume_pending lifecycle -> Ok ()
       | Ready | Completed ->
         Error (failure "restored policy requires its genuine pending Session_resume"))
  ;;

  let check_snapshot t ~moderator_checkpoint =
    let open Result.Let_syntax in
    let%bind expected = Moderator_checkpoint.decode moderator_checkpoint in
    let%bind actual = t.snapshot () in
    let%bind actual = Moderator_checkpoint.decode actual in
    let equal_snapshot left right =
      Jsonaf.exactly_equal
        (Moderator_checkpoint.encode left)
        (Moderator_checkpoint.encode right)
    in
    if Option.equal equal_snapshot expected actual
    then Ok ()
    else
      Error
        (Agent_protocol.Error.create
           Conflict
           ~message:"retained Manager differs from the checkpoint identity state"
           ~retryable:true
           ())
  ;;

  let restored_policy_access t ~(runtime : runtime) ~source ~basis ~expected =
    let open Result.Let_syntax in
    let%bind () = check_open t in
    let%bind () = Product_source.check t.source ~source ~expected in
    let%bind () = Policy_restore_basis.check_construction basis ~state:expected in
    let%bind manager =
      Result.of_option
        runtime.moderator_manager
        ~error:(failure "restored policy runtime has no actual Manager")
    in
    let%bind () =
      if phys_equal t.worker runtime.worker
      then Ok ()
      else Error (failure "restored policy runtime is not its actual physical Product")
    in
    let%bind () = check_restored_policy t ~manager in
    let%bind () =
      check_snapshot t ~moderator_checkpoint:expected.Session_state.moderator
    in
    let access : Restored_policy_access.t =
      { domain = t.domain
      ; product_identity = t.identity
      ; basis
      ; check_resource = (fun () -> check_restored_policy t ~manager)
      }
    in
    let%map () = Restored_policy_access.check access ~expected in
    access
  ;;

  module Binding = struct
    type t =
      { product : product
      ; runtime : runtime
      ; origin : Attachment.origin
      ; resume : unit -> (Attachment.Resume.t, Agent_protocol.Error.t) result
      ; basis : Retained_lifecycle.Basis.t option
      ; live : bool Atomic.t
      }

    let runtime t = t.runtime

    let product_identity t =
      let open Result.Let_syntax in
      let%bind () = check_open t.product in
      if Atomic.get t.live
      then Ok t.product.identity
      else Error (failure "Product binding has retired")
    ;;

    let check_completed t =
      let open Result.Let_syntax in
      let%bind () = check_open t.product in
      let%bind completed = t.product.completed () in
      if Atomic.get t.live && completed
      then Ok ()
      else Error (failure "retained lifecycle binding is retired or still pending")
    ;;

    let retained_lifecycle t =
      let open Result.Let_syntax in
      match t.origin, t.basis with
      | Cold, _ -> Ok None
      | Retained, None -> Error (failure "retained binding lost its actual source basis")
      | Retained, Some basis ->
        let%bind () = check_open t.product in
        let%bind completed = t.product.completed () in
        if not (Atomic.get t.live)
        then Error (failure "retained lifecycle binding has retired")
        else if not completed
        then Ok None
        else
          Ok
            (Some
               { Retained_lifecycle.domain = t.product.domain
               ; basis
               ; worker = t.runtime.worker
               ; manager = t.runtime.moderator_manager
               ; check_owner = (fun () -> check_completed t)
               })
    ;;

    let attach_reported t ~(runtime : runtime) ~close =
      let open Result.Let_syntax in
      let%bind () = check_open t.product in
      if
        phys_equal t.runtime.worker runtime.worker
        && Option.equal phys_equal t.runtime.moderator_manager runtime.moderator_manager
        && Option.equal phys_equal t.runtime.native_runtime runtime.native_runtime
      then (
        let close_reported () =
          Atomic.set t.live false;
          close ()
        in
        Ok
          ({ runtime = { runtime with close = Runtime_close.reported close_reported }
           ; product = Some t.product.identity
           ; native_identity =
               (fun () ->
                 match check_open t.product with
                 | Error _ -> None
                 | Ok () ->
                   if Atomic.get t.live then Some (native_identity t.product) else None)
           ; origin = t.origin
           ; resume = t.resume
           ; retained_lifecycle = (fun () -> retained_lifecycle t)
           }
           : Attachment.t))
      else
        Error
          (Agent_protocol.Error.create
             Conflict
             ~message:"runtime attachment does not belong to its actual product binding"
             ~retryable:false
             ())
    ;;

    let attach t ~runtime ~close =
      let close = Runtime_close.legacy close in
      attach_reported t ~runtime ~close:(fun () -> Runtime_close.run_reported close)
    ;;
  end

  let create
        ~attachment
        ~source
        ~completed
        ~policy_lifecycle
        ~policy_manager
        ~worker
        ~snapshot
        ~make_binding
        ~resume
        ~close_resource
    =
    { identity = ref ()
    ; attachment
    ; domain = (Stdlib.Domain.self () :> int)
    ; source
    ; completed
    ; policy_lifecycle
    ; policy_manager
    ; worker
    ; snapshot
    ; make_binding
    ; resume
    ; close_resource
    ; phase = Atomic.make Close.Open
    }
  ;;

  let first t (runtime : runtime) =
    let runtime_resume = runtime.runtime_resume in
    { Binding.product = t
    ; runtime =
        { runtime with close = Runtime_close.reported (fun () -> close_reported t) }
    ; origin = Attachment.Cold
    ; resume =
        (fun () ->
          Result.map (runtime_resume ()) ~f:(fun r -> Attachment.Resume.Pending r))
    ; basis = None
    ; live = Atomic.make true
    }
  ;;

  let bind_after_basis t ~basis ~history ~moderator_checkpoint =
    let open Result.Let_syntax in
    let%bind () = check_snapshot t ~moderator_checkpoint in
    let%bind () = check_open t in
    let worker =
      Operation_worker.map_run t.worker ~f:Fn.id
    in
    let runtime =
      t.make_binding
        ~worker
        ~history
        ~checkpoint:moderator_checkpoint
        ~close:(Runtime_close.reported (fun () -> close_reported t))
    in
    Ok
      { Binding.product = t
      ; runtime
      ; origin = Attachment.Retained
      ; resume = t.resume worker
      ; basis = Some basis
      ; live = Atomic.make true
      }
  ;;

  let bind t ~source ~expected ~history ~moderator_checkpoint =
    let open Result.Let_syntax in
    let%bind () = check_open t in
    let%bind () = Product_source.check t.source ~source ~expected in
    let%bind basis = Retained_lifecycle.Basis.capture ~source ~expected in
    bind_after_basis t ~basis ~history ~moderator_checkpoint
  ;;

  let bind_authored t ~revision ~expected ~history ~moderator_checkpoint =
    let open Result.Let_syntax in
    let%bind () = check_open t in
    let%bind () = Product_source.check_authored t.source ~revision ~expected in
    let%bind basis = Retained_lifecycle.Basis.capture_authored ~revision ~expected in
    bind_after_basis t ~basis ~history ~moderator_checkpoint
  ;;
end

let build_product_attachment
      prepared_native
      ~initial_source_lease
      ~attachment
      ~close_resource
      ~construction_checkpoint
      ~extension_services
      ~history_namespace
      ~next_history_sequence
      ~existing_history
      ~existing_moderator_snapshot
      ~moderator_reservation_size
      ~permission_profile
      ~review_permission
      ~schedule_services
      ~job_services
      ~legacy_construction
  =
  let open Result.Let_syntax in
  let { Prepared_native_data.Environment.sw
      ; env
      ; paths
      ; session_id
      ; bind_inference_identity
      ; on_inference_attempt
      ; on_inference_completion
      ; on_inference_observation
      ; manifest_authorizer
      ; approval_provider
      }
    =
    prepared_native.Prepared_native_data.environment
  in
  let inference_context =
    Entry_inference_binding.context
      prepared_native.Prepared_native_data.entry_inference_binding
  in
  let source = prepared_native.Prepared_native_data.source in
  let ctx = Prepared_native.context prepared_native in
  let elements = prepared_native.Prepared_native_data.elements in
  let artifact = prepared_native.Prepared_native_data.artifact in
  let delegated = prepared_native.Prepared_native_data.delegated in
  let declares_one_off = prepared_native.Prepared_native_data.declares_one_off in
  let storage_paths = prepared_native.Prepared_native_data.storage_paths in
  let response_dir = response_dir storage_paths in
  let { source_lease = _
      ; native = agent_runtime
      ; definition
      ; managed
      ; authoring
      ; ancestor_capabilities
      ; ancestor_sources
      }
    =
    Prepared_native.resources prepared_native
  in
  let%bind () =
    match definition with
    | None -> Ok ()
    | Some definition ->
      if
        (declares_one_off
         || List.exists
              (Chat_response.Extension_compiler.prepared_tools definition)
              ~f:(fun prepared ->
                match
                  (Chat_response.Extension_compiler.declaration prepared).implementation
                with
                | Standalone _ -> true
                | Moderator _ -> false))
        && List.exists elements ~f:(function
          | Prompt.Chat_markdown.Script _ -> true
          | _ -> false)
      then
        Error
          (failure
             "one-off and standalone tools with a legacy moderator require an \
              extensibility-v1 moderator")
      else Ok ()
  in
  let inherited =
    match source with
    | Generated { inherited_managed; _ } -> inherited_managed
    | Authored _ | Captured_artifact _ | Authored_child _ -> None
  in
  let standalone_definition =
    match inherited with
    | Some inherited ->
      Some
        (Chat_response.Managed_tool_registry.delegation_definition inherited.delegation)
    | None -> definition
  in
  let comp_tools, tool_tbl = Ochat_function.functions agent_runtime.functions in
  let tools = Chat_response.Tool.convert_tools comp_tools in
  let tools =
    tools
    @ ((Option.to_list definition
        @
        match inherited with
        | None -> []
        | Some _ -> Option.to_list standalone_definition)
       |> List.concat_map ~f:(fun definition ->
         Chat_response.Extension_compiler.prepared_tools definition
         |> List.map ~f:(fun prepared ->
           let tool = Chat_response.Extension_compiler.declaration prepared in
           Request.Tool.Function
             { name = tool.name
             ; description = tool.description
             ; parameters = Jsonaf.of_string tool.input_schema.source_text
             ; strict = false
             ; type_ = "function"
             })))
  in
  let%bind tools =
    match
      Option.bind extension_services ~f:(fun services ->
        services.authoring_validation_host)
    with
    | None -> Ok tools
    | Some host ->
      let%map capabilities =
        Lazy.force agent_runtime.capabilities
        |> Result.map_error ~f:(fun error ->
          failure error.Chat_response.Tool_capability.message)
      in
      List.map tools ~f:(function
        | Request.Tool.Function tool ->
          Request.Tool.Function
            { tool with
              description =
                Chat_response.Authoring_tool_description.describe
                  ~host
                  ~capabilities
                  ~name:tool.name
                  ~description:tool.description
            }
        | tool -> tool)
  in
  let%bind initial_history, initial_end, initial_installed_originals =
    match existing_history with
    | Some history -> Ok (history, next_history_sequence, None)
    | None ->
      let%bind () = check_construction construction_checkpoint in
      let installed =
        match source with
        | Authored { revision; _ } when Option.is_some artifact.originals_sha256 ->
          let open Option.Let_syntax in
          let%bind definition = artifact.prompt_definition_id in
          let%map projection = Prompt_revision.document_projection revision in
          definition, revision, projection
        | Authored _ | Captured_artifact _ | Authored_child _ | Generated _ -> None
      in
      let installed_projection =
        Option.map installed ~f:(fun (_, revision, projection) -> revision, projection)
      in
      let%bind items =
        initial_items
          ~construction_checkpoint
          ~ctx
          ~elements
          ~installed_projection
          ~manifest_authorizer
          ~approval_provider
          ~response_dir
      in
      let%bind rows, initial_end =
        allocate_initial
          ~namespace:history_namespace
          ~next_sequence:next_history_sequence
          items
      in
      let%map originals =
        Option.value_map installed ~default:(Ok None) ~f:(fun (definition, revision, _) ->
          Recovery_installed_originals.Initial.create
            ~definition
            ~revision:(Prompt_revision.id revision)
            rows)
      in
      List.map rows ~f:fst, initial_end, originals
  in
  let reserved_history_through = initial_end + moderator_reservation_size in
  let%bind restored_moderator_snapshot =
    decode_moderator_snapshot existing_moderator_snapshot
  in
  let%bind moderator_allocator =
    History_entry.Allocator.create_bounded
      ~namespace:history_namespace
      ~next_sequence:initial_end
      ~limit_exclusive:reserved_history_through
    |> Result.map_error ~f:failure
  in
  let base_script_tools =
    match managed, extension_services with
    | Some definition, Some services ->
      let tools_service =
        match inherited with
        | Some inherited ->
          Script_tool_calls.with_inherited_managed_tools
            (services.script_tools agent_runtime)
            ~env
            ~delegation:inherited.delegation
            ~current:inherited.current
            ~execution_limits:services.standalone_execution_limits
        | None ->
          Script_tool_calls.with_managed_tools
            (services.script_tools agent_runtime)
            ~env
            ~definition
            ~execution_limits:services.standalone_execution_limits
      in
      Some tools_service
    | None, Some services -> Some (services.script_tools agent_runtime)
    | _, None ->
      Option.map legacy_construction ~f:(fun host -> host.script_tools agent_runtime)
  in
  let base_script_tools =
    match source_authority source, extension_services, base_script_tools with
    | Some authority, None, Some tools ->
      Some
        (Script_tool_calls.with_authorization_guard tools ~check:(fun () ->
           Delegation_authority.check_execution authority))
    | (None | Some _), (None | Some _), tools -> tools
  in
  let base_script_tools =
    Option.map base_script_tools ~f:(fun tools ->
      Script_tool_calls.with_inference_tracking
        tools
        { bind_identity = bind_inference_identity
        ; on_attempt = on_inference_attempt
        ; on_observation = on_inference_observation
        ; on_completion = on_inference_completion
        })
  in
  let moderator_attachment = attachment.Product_attachment.moderator in
  let%bind
      ( ( moderator
        , start_moderator_once
        , model_recipes
        , check_legacy_acknowledged
        , check_legacy_resume_acknowledged
        , installed_legacy_resume )
      , moderator_selection )
    =
    create_moderator
      ~attachment:moderator_attachment
      ~construction_checkpoint
      ~definition
      ~delegated
      ~runtime_policy:
        (Option.value_map
           extension_services
           ~default:Chat_response.Runtime_semantics.default_policy
           ~f:(fun services -> services.runtime_policy))
      ~sw
      ~env
      ~ctx
      ~session_id
      ~history:initial_history
      ~tools
      ~allocator:moderator_allocator
      ~agent_runtime
      ~manifest_authorizer
      ~approval_provider
      ~response_dir
      ~schedule_services
      ~job_services
      ~legacy_construction
      ~script_tools:base_script_tools
      ~bind_inference_identity
      ~snapshot:restored_moderator_snapshot
  in
  Product_callback_data.bind_manager
    attachment.Product_attachment.callbacks
    ~manager:(Option.map moderator ~f:(fun (moderator, _) -> moderator.manager));
  let read_moderator_snapshot () = moderator_snapshot moderator in
  let%bind moderator_snapshot = read_moderator_snapshot () in
  let now () =
    Eio.Time.now (Eio.Stdenv.clock env)
    |> Time_ns.Span.of_sec
    |> Time_ns.of_span_since_epoch
    |> Agent_protocol.Timestamp.of_time_ns
  in
  let script_tools =
    match extension_services with
    | None -> None
    | Some _ ->
      Option.map base_script_tools ~f:(fun tools_service ->
        match managed, extension_services, moderator with
        | Some definition, Some services, Some (moderator, _)
          when (match source with
                | Generated _ -> false
                | Authored _ | Captured_artifact _ | Authored_child _ -> true)
               && Option.is_some (Manager.extension_definition moderator.manager) ->
          Script_tool_calls.with_moderator_dispatch
            tools_service
            ~dispatch:
              (Managed_moderator_dispatch.create
                 ~model_recipes
                 ~definition
                 ~manager:moderator.manager
                 ~history:services.history
                 ~available_tools:tools
                 ~session_meta:`Null
                 ~now)
        | _ -> tools_service)
  in
  let%bind legacy_callback_run =
    match legacy_construction, moderator, base_script_tools with
    | Some host, Some (moderator, executor), Some script_tools
      when Option.is_none (Manager.extension_definition moderator.manager) ->
      let%map selected =
        Lazy.force agent_runtime.capabilities
        |> Result.map_error ~f:(fun error ->
          failure error.Chat_response.Tool_capability.message)
      in
      Some
        ( moderator.manager
        , fun ~capture_cleanup
            ~operation_id
            ~unavailable
            ~selection
            ~history
            ~available_tools
            ~now_ms ->
            Legacy_callback_runtime.run
              moderator.manager
              ~claim:(fun ~manager ~callback f ->
                let result =
                  host.installed_claim ?capture_cleanup ~operation_id ~manager ~callback f
                in
                (match result, unavailable with
                 | Ok false, Some unavailable -> unavailable := true
                 | Ok (true | false), None | Ok true, Some _ | Error _, _ -> ());
                result)
              ~model_recipes:
                (legacy_model_recipes
                   ~delegated
                   executor
                   ~session_text:(Agent_protocol.Id.Session.to_string session_id)
                   ~bind_inference_identity)
              ~script_tools
              ~selected
              ~selection
              ~session_id:(Agent_protocol.Id.Session.to_string session_id)
              ~history
              ~available_tools
              ~session_meta:`Null
              ~now_ms )
    | _ -> Ok None
  in
  let legacy_callbacks =
    Option.map legacy_callback_run ~f:(fun (manager, run) ->
      fun ~capture_cleanup ~operation_id ->
      Legacy_callback_runtime.Installed.create
        manager
        ~run:(run ~capture_cleanup ~operation_id ~unavailable:None))
  in
  let now_ms () =
    Agent_protocol.Timestamp.to_time_ns (now ())
    |> Time_ns.to_int_ns_since_epoch
    |> fun n -> n / 1_000_000
  in
  let legacy_events =
    Option.map legacy_callbacks ~f:(fun callbacks ->
      fun ~capture_cleanup ~max_events ~history ->
      let idle = callbacks ~capture_cleanup ~operation_id:None in
      Legacy_callback_runtime.Installed.drain
        idle
        ~max_events
        ~history
        ~available_tools:tools
        ~now_ms:(now_ms ()))
  in
  let lifecycle =
    match definition, moderator, extension_services with
    | Some _, Some (moderator, _), Some services
      when Option.is_some (Manager.extension_definition moderator.manager) ->
      Some
        (Moderator_event.Lifecycle.create
           ~manager:moderator.manager
           ~resume:
             (Option.exists
                (Manager.invocation_observer moderator.manager)
                ~f:services.lifecycle_started))
    | _ -> None
  in
  let activate lifecycle ~claim ~run_actions ~history =
    Moderator_event.Lifecycle.run
      ~model_recipes
      lifecycle
      ~claim
      ~run_actions
      ?script_tools
      ~history
      ~available_tools:tools
      ~session_meta:`Null
      ~now
      ()
  in
  let moderator_activation =
    match lifecycle, extension_services with
    | Some lifecycle, Some services ->
      Some
        { pending = (fun () -> Moderator_event.Lifecycle.pending lifecycle)
        ; startup_pending =
            (fun () -> Moderator_event.Lifecycle.startup_pending lifecycle)
        ; run =
            (fun ?capture_cleanup:_ () ->
              activate
                lifecycle
                ~claim:services.claim_lifecycle
                ~run_actions:services.run_actions
                ~history:services.history
              |> Result.map ~f:(function
                | Activated _ -> true
                | Unavailable | Already_active -> false))
        }
    | _ -> None
  in
  let%bind moderator_activation, legacy_foreground_resume, installed_resume_owner =
    match installed_legacy_resume, legacy_callback_run, legacy_construction with
    | None, _, _ -> Ok (moderator_activation, None, None)
    | Some acknowledge, Some (_, run), Some host ->
      let activate ~capture_cleanup ~operation_id =
        let%bind history = host.history () in
        let unavailable = ref false in
        let result =
          run
            ~capture_cleanup
            ~operation_id
            ~unavailable:(Some unavailable)
            ~selection:(Boundary Session_resume)
            ~history
            ~available_tools:tools
            ~now_ms:(now_ms ())
        in
        (* Only the actual host's Ok false means no callback entered. Preserve
           all genuine admission, handler, writer and cleanup errors. *)
        match !unavailable, result with
        | true, Error _ -> Ok None
        | (true | false), (Ok _ as result) | false, (Error _ as result) -> result
      in
      let resume = Installed_legacy_resume.create ~run:activate ~acknowledge in
      let activation =
        { pending = (fun () -> Installed_legacy_resume.pending resume)
        ; startup_pending = (fun () -> false)
        ; run =
            (fun ?capture_cleanup () ->
              Installed_legacy_resume.run ?capture_cleanup resume ~operation_id:None)
        }
      in
      Ok
        ( Some activation
        , Some
            (fun ~operation_id ->
              let%bind _ = Installed_legacy_resume.run resume ~operation_id in
              if Installed_legacy_resume.pending resume
              then Error (failure "foreground requires its installed legacy resume ACK")
              else Ok ())
        , Some resume )
    | Some _, (None | Some _), None | Some _, None, Some _ ->
      Error (failure "installed legacy resume lost its callback host")
  in
  let moderator_events =
    match lifecycle, moderator, legacy_callbacks with
    | _, _, Some callbacks ->
      Some
        (fun ~input ~capabilities ->
          let%bind () =
            Option.value_map legacy_foreground_resume ~default:(Ok ()) ~f:(fun resume ->
              resume ~operation_id:(Some input.Operation_worker.Input.operation.id))
          in
          Legacy_callback_runtime.Installed.foreground_handlers
            (callbacks
               ~capture_cleanup:None
               ~operation_id:(Some input.Operation_worker.Input.operation.id))
            ~capabilities)
    | Some lifecycle, Some (moderator, _), None ->
      Some
        (fun ~input ~capabilities ->
          let%bind activation =
            activate
              lifecycle
              ~claim:(fun ~event ->
                capabilities.Operation_worker.Capabilities.with_moderator_event ~event)
              ~run_actions:capabilities.run_actions
              ~history:(fun () -> input.Operation_worker.Input.history)
          in
          match activation with
          | Unavailable ->
            Error (failure "foreground moderator activation was not admitted")
          | Activated _ | Already_active ->
            Moderator_event.foreground_handlers
              ~model_recipes
              ?script_tools
              ~capabilities
              ~manager:moderator.manager
              ~session_meta:`Null
              ~now
              ())
    | _ -> None
  in
  let dispatch_tools =
    match legacy_callbacks with
    | Some _ -> base_script_tools
    | None -> script_tools
  in
  let%bind advertised_dispatch =
    match dispatch_tools with
    | None -> Ok None
    | Some script_tools ->
      Lazy.force agent_runtime.capabilities
      |> Result.map ~f:(fun declared -> Some (script_tools, declared))
      |> Result.map_error ~f:(fun error ->
        failure error.Chat_response.Tool_capability.message)
  in
  let dispatch_tool =
    Option.map advertised_dispatch ~f:(fun (script_tools, declared) ->
      fun ~input ~capabilities ->
      let event_handlers =
        match legacy_callbacks, moderator_events with
        | Some _, Some handlers -> Some (handlers ~input ~capabilities)
        | _ ->
          Option.map moderator ~f:(fun (moderator, _) ->
            Moderator_event.foreground_handlers
              ~model_recipes
              ~script_tools
              ~capabilities
              ~manager:moderator.manager
              ~session_meta:`Null
              ~now
              ())
      in
      let observer =
        Option.bind moderator ~f:(fun (moderator, _) ->
          Manager.invocation_observer moderator.manager)
      in
      let moderate_tool call =
        match event_handlers, extension_services with
        | None, _ -> Ok None
        | Some (Error error), _ -> Error error.Agent_protocol.Error.message
        | Some (Ok handlers), services ->
          let open Result.Let_syntax in
          let%bind history =
            match legacy_callbacks, legacy_construction, services with
            | Some _, Some host, _ ->
              host.history ()
              |> Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message)
            | _, _, Some services -> Ok (services.history ())
            | _, _, None -> Error "moderator services are not installed"
          in
          handlers.handle
            ~history
            ~available_tools:tools
            ~now_ms:(now_ms ())
            ~event:(Moderation.Event.Pre_tool_call call)
      in
      let native =
        Script_tool_calls.native_dispatch script_tools ~declared ~input ~capabilities
      in
      let native =
        { native with
          run =
            (fun ?run_native request ~authorize ->
              Native_tool_moderation.with_handler
                ~observer
                ~prepare:(fun call ->
                  let%bind outcome = moderate_tool call in
                  match outcome with
                  | None -> Ok None
                  | Some outcome ->
                    let%map () =
                      Chat_response.Runtime_request_scope.emit outcome.runtime_requests
                    in
                    (match
                       Chat_response.Runtime_semantics.should_end_session
                         outcome.runtime_requests
                     with
                     | Some _ ->
                       Some (Moderation.Tool_moderation.Reject "The session has ended.")
                     | None -> outcome.tool_moderation))
                (fun () -> native.run ?run_native request ~authorize))
        }
      in
      let standalone =
        match standalone_definition, extension_services with
        | Some definition, Some services ->
          [ Standalone_tool_dispatch.create
              ?observer
              ~env
              ~definition
              ~input
              ~capabilities
              ~script_tools
              ~now
              ~is_halted:(fun () -> Script_tool_calls.is_halted script_tools)
              ~execution_limits:services.standalone_execution_limits
              ~admit:(fun _ ->
                Script_tool_calls.validate_definition script_tools definition)
              ~revalidate:(fun _ ->
                Script_tool_calls.validate_definition script_tools definition)
              ~prepare_outcome:(fun outcome ->
                Agent_protocol.Invocation.validate_outcome outcome
                |> Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message))
              ~moderate_tool:(fun _ call -> moderate_tool call)
              ()
          ]
        | _ -> []
      in
      let moderator_dispatch =
        match definition, moderator with
        | Some definition, Some (moderator, _) ->
          [ Moderator_tool_dispatch.create
              ~model_recipes
              ~script_tools
              ~definition
              ~manager:moderator.manager
              ~input
              ~capabilities
              ~available_tools:tools
              ~session_meta:`Null
              ~now
              ~validate_work:(fun _ ->
                Error "background work completion is not installed")
              ~admit:(fun _ ->
                Script_tool_calls.validate_definition script_tools definition)
              ~revalidate:(fun _ ->
                Script_tool_calls.validate_definition script_tools definition)
              ~prepare_outcome:(fun outcome ->
                Agent_protocol.Invocation.validate_outcome outcome
                |> Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message))
              ()
          ]
        | _ -> []
      in
      Chat_response.In_memory_stream.Tool_dispatch.chain
        (standalone @ moderator_dispatch @ [ native ])
      |> Script_tool_calls.with_model_preparation
           script_tools
           ~capture_caller:capabilities.policy_caller
           ~selected:declared
           ~input)
  in
  let notification_source =
    Option.bind moderator ~f:(fun (moderator, _) ->
      Manager.invocation_observer moderator.manager)
  in
  let notification_input =
    match extension_services, script_tools with
    | Some services, Some tools ->
      Some
        (fun ~(capabilities : Operation_worker.Capabilities.t) ->
          services.notification_input
            ~source:notification_source
            ~tools
            ~consume:capabilities.consume_notifications)
    | _ -> None
  in
  let initial_notification_input =
    match extension_services, script_tools with
    | Some services, Some tools ->
      Some
        (fun ~(capabilities : Operation_worker.Capabilities.t) ->
          services.initial_notification_input
            ~source:notification_source
            ~tools
            ~consume:capabilities.consume_initial_notifications)
    | _ -> None
  in
  let idle_notifications =
    match extension_services, script_tools with
    | Some services, Some tools ->
      Some (services.idle_notifications ~source:notification_source ~tools)
    | _ -> None
  in
  let worker =
    Turn_worker.create
      ?checkpoint_tools:
        (Option.map advertised_dispatch ~f:(fun (_, selected) ->
           Recovery_checkpoint_candidate.selected_tools selected))
      ?successor_guidance:
        (Option.bind script_tools ~f:Script_tool_calls.successor_guidance)
      ~root_binding:(Prepared_native.root_binding prepared_native)
      ?authoring_context:(Option.map authoring ~f:Authoring_runtime.materialize)
      ?agent_guidance:
        (Option.bind script_tools ~f:(fun tools ->
           Option.map
             (Script_tool_calls.instruction_service tools)
             ~f:(fun service ~input:_ ~capabilities ->
               Instruction_service.agent_guidance
                 service
                 ~borrow_actor:
                   capabilities.Operation_worker.Capabilities.original_agent_actor)))
      ?runtime_policy:
        (Option.map extension_services ~f:(fun services -> services.runtime_policy))
      ?dispatch_tool
      ?moderator_events
      ?notification_input
      ?initial_notification_input
      { env
      ; inference_context
      ; inference_identity = bind_inference_identity
      ; on_inference_attempt
      ; on_inference_completion
      ; on_inference_observation
      ; response_dir
      ; tools
      ; tool_tbl
      ; moderator = Option.map moderator ~f:fst
      ; permission_profile
      ; review_permission
      ; history_compaction = false
      ; parallel_tool_calls = true
      ; agent_page_classifications = agent_runtime.classifications
      ; delegated_permission_tools =
          (match script_tools with
           | None -> agent_runtime.shell_tool_names
           | Some _ ->
             String.Set.of_list
               (List.map agent_runtime.functions ~f:(fun fn ->
                  fn.Ochat_function.info.function_.name)))
      ; redact_tool_payload =
          (fun ~name payload ->
            Option.value_map
              agent_runtime.shell_registry
              ~default:payload
              ~f:(fun registry ->
                Shell_runtime.Registry.redact_tool_input registry ~tool_name:name payload))
      }
  in
  let check_execution =
    Option.map (source_authority source) ~f:(fun authority () ->
      Delegation_authority.check_execution authority)
  in
  let check_ancestry =
    Option.map (source_authority source) ~f:(fun authority () ->
      Delegation_authority.check_ancestry authority)
  in
  let worker =
    match check_execution with
    | None -> worker
    | Some check ->
      Operation_worker.map_run worker ~f:(fun run ~sw ~input capabilities ->
        match check () with
        | Error error -> Operation_worker.Failed error
        | Ok () -> run ~sw ~input capabilities)
  in
  let activity =
    Option.map (source_authority source) ~f:(fun _ -> Runtime_activity.create ~sw)
  in
  let with_activity f =
    match activity with
    | None -> f ()
    | Some activity -> Runtime_activity.run activity f
  in
  let worker =
    match activity with
    | None -> worker
    | Some activity ->
      Operation_worker.map_run worker ~f:(fun run ~sw:_ ~input capabilities ->
        Runtime_activity.with_switch activity (fun ~sw ->
          run ~sw ~input capabilities))
  in
  let worker =
    Operation_worker.map_run worker ~f:(fun run ~sw ~input capabilities ->
      let enrolled =
        let open Result.Let_syntax in
        let%bind receipt =
          capabilities.Operation_worker.Capabilities.callback_cleanup ()
        in
        Product_callback_data.enroll
          attachment.Product_attachment.callbacks
          ~operation:input.Operation_worker.Input.operation
          receipt
      in
      match enrolled with
      | Error failure -> Operation_worker.Failed failure
      | Ok () -> run ~sw ~input capabilities)
  in
  let runtime_lifecycle_kind =
    match moderator, lifecycle, extension_services with
    | None, _, _ -> Ok Authored_start.Absent
    | Some (moderator, _), Some lifecycle, Some services
      when Option.is_some (Manager.extension_definition moderator.manager) ->
      Ok
        (Authored_start.Extension
           { lifecycle
           ; run =
               (fun ~claim ->
                 with_activity (fun () ->
                   activate
                     lifecycle
                     ~claim
                     ~run_actions:services.run_actions
                     ~history:services.history))
           })
    | Some (moderator, _), _, _ ->
      if Option.is_some (Manager.extension_definition moderator.manager)
      then Error (failure "authored extension startup has no actual lifecycle owner")
      else Ok (Authored_start.Legacy check_legacy_acknowledged)
  in
  let authored_start_for worker =
    let startup =
      Result.map runtime_lifecycle_kind ~f:(fun kind ->
        ({ worker
         ; manager = Option.map moderator ~f:(fun (moderator, _) -> moderator.manager)
         ; kind
         }
         : Authored_start.t))
    in
    fun () ->
      let%bind startup = startup in
      let%map () = Authored_start.check_pending startup in
      startup
  in
  let runtime_resume_for worker =
    let resume =
      Result.map runtime_lifecycle_kind ~f:(fun kind ->
        let kind =
          match kind with
          | Authored_start.Legacy _ ->
            Authored_start.Legacy check_legacy_resume_acknowledged
          | Extension _ | Absent -> kind
        in
        ({ worker
         ; manager = Option.map moderator ~f:(fun (moderator, _) -> moderator.manager)
         ; kind
         }
         : Runtime_resume.t))
    in
    fun () ->
      let%bind resume = resume in
      let%map () = Runtime_resume.check_pending resume in
      resume
  in
  let authored_start = authored_start_for worker in
  let runtime_resume = runtime_resume_for worker in
  let parse_user_content =
    match delegated with
    | false ->
      parse_user_content ~ctx ~manifest_authorizer ~approval_provider ~response_dir paths
    | true ->
      fun ?asset_capture:_ ~id content ->
        (match
           content.Agent_protocol.Session.Message_content.kind, content.attachments
         with
         | Plain_text, [] ->
           Ok
             (Openai.Responses_history.create_with_id_exn
                ~id
                (plain_user_item content.text))
         | Chatmd, _ | Plain_text, _ :: _ ->
           Error
             (failure
                "generated sessions accept plain_text messages without implicit resource \
                 loading"))
  in
  let published_moderator_snapshot = ref moderator_snapshot in
  (* Worker binding fields stay separate from physical services. Rebinding a
     retained product supplies its own worker and lifecycle views; construction
     owners are never selected through a later mutable ambient cell. *)
  let make_runtime
        ~source_lease
        ~worker
        ~initial_history
        ~initial_prompt_entry_count
        ~initial_installed_originals
        ~moderator_snapshot
        ~authored_start
        ~runtime_resume
        ~start_moderator
        ~close
    =
    let unmoderated_source =
      match moderator_selection with
      | Verified_no_moderator ->
        Some (Runtime_unmoderated_source.Host.create ~session_id ~artifact ~worker)
      | Selected_moderator -> None
    in
    { source_lease
    ; worker
    ; product_callbacks = Some attachment.Product_attachment.callbacks
    ; unmoderated_source
    ; inference_execution = Chat_response.Ctx.inference_execution ctx
    ; now
    ; parse_user_content
    ; parse_prepared_input =
        (fun ~execution ~tracking ->
          with_activity (fun () ->
            parse_prepared_input
              ~ctx
              ~manifest_authorizer
              ~approval_provider
              ~response_dir
              ~delegated
              paths
              ~execution
              ~tracking))
    ; initial_history
    ; initial_prompt_entry_count
    ; initial_installed_originals
    ; reserved_history_through
    ; moderator_snapshot
    ; moderator_manager =
        Option.map moderator ~f:(fun (moderator, _) ->
          moderator.Chat_response.In_memory_stream.manager)
    ; model_recipes
    ; moderator_tools = tools
    ; idle_notifications
    ; moderator_script_tools = script_tools
    ; standalone_completion =
        (match extension_services, script_tools with
         | Some services, Some tools -> Some (services.standalone_completion ~tools)
         | _ -> None)
    ; automatic_turn_policy =
        (match script_tools, extension_services with
         | Some _, Some services -> Some services.runtime_policy
         | _ -> None)
    ; background_executor =
        (match script_tools, extension_services with
         | Some script_tools, Some services ->
           Some
             { policy = services.one_off_policy
             ; now
             ; run =
                 (fun ~job
                   ~deadline
                   ~execute
                   ~moderator_execute
                   ~claim_event
                   ~is_halted
                   ~request ->
                   with_activity (fun () ->
                     let%bind () =
                       match check_execution with
                       | None -> Ok ()
                       | Some check -> check ()
                     in
                     let script_tools =
                       Script_tool_calls.with_lifecycle script_tools ~is_halted
                       |> Script_tool_calls.with_durable_requests
                     in
                     let observer =
                       Option.bind moderator ~f:(fun (moderator, _) ->
                         Manager.invocation_observer
                           moderator.Chat_response.In_memory_stream.manager)
                     in
                     let moderate_tool _ call =
                       match moderator with
                       | None -> Ok None
                       | Some (moderator, _) ->
                         let open Result.Let_syntax in
                         let event = Moderation.Event.Pre_tool_call call in
                         let%bind outcome =
                           Moderator_event.run_ordinary
                             ~model_recipes
                             ~event
                             ~claim:(claim_event ~event)
                             ~script_tools
                             ~manager:moderator.manager
                             ~history:services.history
                             ~available_tools:tools
                             ~session_meta:`Null
                             ~now
                             ()
                         in
                         (match outcome with
                          | None ->
                            Error (failure "background moderator event was not admitted")
                          | Some outcome ->
                            let tool_moderation =
                              match
                                Chat_response.Runtime_semantics.should_end_session
                                  outcome.runtime_requests
                              with
                              | Some _ ->
                                Some
                                  (Moderation.Tool_moderation.Reject
                                     "The session has ended.")
                              | None -> outcome.tool_moderation
                            in
                            (* The event checkpoint owns these durable requests. Its
                             follow-up scheduler consumes them exactly once. *)
                            Ok
                              (Some
                                 { outcome with tool_moderation; runtime_requests = [] }))
                     in
                     Background_execution.run
                       ?observer
                       ~moderator_execute
                       ~env
                       ~job
                       ~deadline
                       ~execute
                       ~request
                       ~policy:services.one_off_policy
                       ~script_tools
                       ~now
                       ~moderate_tool:(fun invocation call ->
                         moderate_tool invocation call
                         |> Result.map_error ~f:(fun error ->
                           error.Agent_protocol.Error.message))
                       ~prepare_outcome:(fun outcome ->
                         Agent_protocol.Invocation.validate_outcome outcome
                         |> Result.map_error ~f:(fun error ->
                           error.Agent_protocol.Error.message))
                       ()))
             }
         | _ -> None)
    ; moderator_activation
    ; check_execution
    ; check_ancestry
    ; ancestor_capabilities
    ; retained_ancestor_sources = ancestor_sources
    ; activity
    ; native_runtime =
        (match extension_services with
         | None -> None
         | Some _ -> Some agent_runtime)
    ; authored_start
    ; runtime_resume
    ; start_moderator
    ; enqueue_internal_event = enqueue_internal_event moderator
    ; legacy_events
    ; drain_internal_events = drain_internal_events ~env ~session_id ~tools moderator
    ; execute_model_job =
        (match delegated with
         | false ->
           fun ~allocate_inference ->
             execute_model_job
               moderator
               session_id
               ~inference_identity:(bind_inference_identity allocate_inference)
         | true ->
           fun ~allocate_inference:_
             ~inference_context:_
             ~capture_recipe_target:_
             ~recipe:_
             ~payload:_ ->
             Error
               (failure "generated moderators must use inherited tools for model work"))
    ; enqueue_model_job_completion = enqueue_model_job_completion moderator
    ; close
    }
  in
  let retained_resume worker =
    (* Force only after the shared owner has an actual ACK. Repeated factual
       reads retain the SAME worker-bound result; no read consumes installation. *)
    let factual_resume = lazy (runtime_resume_for worker ()) in
    let scoped_resume () = Lazy.force factual_resume in
    let actual_resume () = runtime_resume_for worker () in
    fun () ->
      match installed_resume_owner, lifecycle, runtime_lifecycle_kind with
      | Some owner, _, _ ->
        (match Installed_legacy_resume.state owner with
         | Failed error -> Error error
         | Completed Ordinary_activation -> Ok Attachment.Resume.Continue_live
         | Completed Scoped_resume_ack ->
           Result.map (scoped_resume ()) ~f:(fun resume ->
             Attachment.Resume.Pending resume)
         | Ready ->
           let%map moderator, _executor =
             match moderator with
             | Some moderator -> Ok moderator
             | None -> Error (failure "pending legacy resume lost its physical Manager")
           in
           Attachment.Resume.Pending_legacy
             { Pending_legacy_resume.run =
                 (fun ~host ~inference ->
                   let%bind _ =
                     Installed_legacy_resume.run_scoped owner ~run:(fun () ->
                       (* Force only after this exact pending gate elects Ready.
                          Cached scoped ACKs and an ordinary winner allocate no
                          graph and never replay constructor effects. *)
                       let%bind (tracking : Pending_legacy_resume.Inference.t) =
                         Lazy.force inference
                       in
                       let scoped_ctx =
                         Chat_response.Ctx.with_inference_tracking
                           ctx
                           ~identity:tracking.identity
                           ~on_attempt:tracking.on_attempt
                           ~on_observation:tracking.on_observation
                           ~on_completion:tracking.on_completion
                       in
                       let executor =
                         model_executor
                           ~sw
                           ~ctx:scoped_ctx
                           ~manifest_authorizer
                           ~approval_provider
                           ~response_dir
                       in
                       let%bind script_tools =
                         match base_script_tools with
                         | Some tools ->
                           Ok
                             (Script_tool_calls.with_inference_tracking
                                tools
                                { bind_identity = tracking.bind_identity
                                ; on_attempt = tracking.on_attempt
                                ; on_observation = tracking.on_observation
                                ; on_completion = tracking.on_completion
                                })
                         | None ->
                           Error
                             (failure "pending legacy resume lost its native services")
                       in
                       let%bind selected =
                         Lazy.force agent_runtime.capabilities
                         |> Result.map_error ~f:(fun error ->
                           failure error.Chat_response.Tool_capability.message)
                       in
                       let%bind history = host.history () in
                       run_legacy_constructor
                         host
                         ~bind_inference_identity:tracking.bind_identity
                         ~script_tools
                         ~selected
                         ~manager:moderator.manager
                         ~executor
                         ~delegated
                         ~session_text:(Agent_protocol.Id.Session.to_string session_id)
                         ~now_ms:(now_ms ())
                         ~history
                         ~tools
                         ~selection:(Boundary Session_resume))
                   in
                   scoped_resume ())
             })
      | None, Some lifecycle, _ ->
        (match Moderator_event.Lifecycle.status lifecycle with
         | Completed -> Ok Continue_live
         | Failed error -> Error error
         | Ready ->
           Result.map (actual_resume ()) ~f:(fun resume ->
             Attachment.Resume.Pending resume))
      | None, None, Ok (Legacy check) ->
        let%map () = check () in
        Attachment.Resume.Continue_live
      | None, None, Ok Absent -> Ok Continue_live
      | None, None, Ok (Extension _) ->
        Error (failure "retained extension lost its actual lifecycle owner")
      | None, None, Error error -> Error error
  in
  let completed_lifecycle () =
    (* Read the stable actual owner only. A caller's Continue_live selection is
       not proof, and Scoped ACK still requires its factual Runtime_resume leg. *)
    match installed_resume_owner, lifecycle, runtime_lifecycle_kind with
    | Some owner, _, _ ->
      (match Installed_legacy_resume.state owner with
       | Completed Ordinary_activation -> Ok true
       | Completed Scoped_resume_ack | Ready -> Ok false
       | Failed error -> Error error)
    | None, Some lifecycle, _ ->
      (match Moderator_event.Lifecycle.status lifecycle with
       | Completed -> Ok true
       | Ready -> Ok false
       | Failed error -> Error error)
    | None, None, Ok (Legacy check) -> Result.map (check ()) ~f:(fun () -> true)
    | None, None, Ok Absent -> Ok true
    | None, None, Ok (Extension _) ->
      Error (failure "retained extension lost its actual lifecycle owner")
    | None, None, Error error -> Error error
  in
  let initial_prompt_entry_count = List.length initial_history in
  let make_binding ~worker ~history ~checkpoint ~close =
    make_runtime
      ~source_lease:Prompt_source_lease.legacy
      ~worker
      ~initial_history:history
      ~initial_prompt_entry_count
      ~initial_installed_originals:None
      ~moderator_snapshot:(fun () -> checkpoint)
      ~authored_start:(authored_start_for worker)
      ~runtime_resume:(runtime_resume_for worker)
        (* Retained binding publishes no new startup fact. Its original pending
         owner remains on moderator_activation; explicit Goal Resume selects the
         same owner through Attachment.runtime_resume instead. *)
      ~start_moderator:(fun () -> Ok checkpoint)
      ~close
  in
  let product =
    Product.create
      ~attachment
      ~source:
        (Product_source.create
           ?selected:prepared_native.Prepared_native_data.selected
           source
           ~session_id)
      ~completed:completed_lifecycle
      ~policy_lifecycle:lifecycle
      ~policy_manager:
        (Option.map moderator ~f:(fun (moderator, _) ->
           moderator.Chat_response.In_memory_stream.manager))
      ~worker
      ~snapshot:read_moderator_snapshot
      ~make_binding
      ~resume:retained_resume
      ~close_resource
  in
  let runtime =
    make_runtime
      ~source_lease:initial_source_lease
      ~worker
      ~initial_history
      ~initial_prompt_entry_count
      ~initial_installed_originals
      ~moderator_snapshot:(fun () -> !published_moderator_snapshot)
      ~authored_start
      ~runtime_resume
      ~start_moderator:(fun () ->
        let%map snapshot = start_moderator_once () in
        published_moderator_snapshot := snapshot;
        snapshot)
      ~close:(Runtime_close.reported (fun () -> Product.close_reported product))
  in
  Ok (product, Product.first product runtime)
;;

let build_with_services
      ~ancestor_capabilities
      ~ancestor_sources
      ~selected_source
      ~construction_checkpoint
      ~startup
      ~agent_source
      ~native_registrations
      ~extension_services
      ~sw
      ~env
      ~inference_context
      ~inference_identity
      ~bind_inference_identity
      ~on_inference_attempt
      ~on_inference_completion
      ~on_inference_observation
      ~paths
      ~storage_paths
      ~source
      ~session_id
      ~history_namespace
      ~next_history_sequence
      ~existing_history
      ~existing_moderator_snapshot
      ~moderator_reservation_size
      ~manifest_authorizer
      ~approval_provider
      ~approval_store
      ~permission_profile
      ~review_permission
      ~schedule_services
      ~job_services
      ~legacy_construction
  =
  let open Result.Let_syntax in
  let%bind () = check_construction construction_checkpoint in
  let%bind () =
    match selected_source with
    | None -> Ok ()
    | Some selected ->
      if Native_input.matches_source source selected
      then Ok ()
      else Error (failure "selected source differs from its runtime construction")
  in
  let release = Native_release.create ~sw:None in
  let%bind entry_inference_binding =
    Entry_inference_binding.create
      ~on_allocated:(Native_release.record_allocation release)
      inference_context
      ~sw
  in
  let%bind () =
    validate_native_source
      source
      ~preparation:Native_preparation.Original_execution
      ~session_id
      ~permission_profile
  in
  let%bind extension_services =
    prepare_extension_services source ~env extension_services
  in
  let%bind prepared_native =
    prepare_entry_native
      source
      ~preparation:Native_preparation.Original_execution
      ~selected:selected_source
      ~ancestor_capabilities
      ~ancestor_sources
      ~native_checkpoint:None
      ~construction_checkpoint
      ~startup
      ~agent_source
      ~native_registrations
      ~native_service_revision:
        (Option.bind extension_services ~f:(fun services ->
           services.native_service_revision))
      ~one_off_policy:
        (Option.map extension_services ~f:(fun services -> services.one_off_policy))
      ~authoring_validation_host:
        (Option.bind extension_services ~f:(fun services ->
           services.authoring_validation_host))
      ~initial_agent_guidance:
        (Option.bind extension_services ~f:(fun services ->
           services.initial_agent_guidance))
      ~permission_profile
      ~entry_inference_binding
      ~release
      ~sw
      ~env
      ~inference_identity
      ~bind_inference_identity
      ~on_inference_attempt
      ~on_inference_completion
      ~on_inference_observation
      ~paths
      ~storage_paths
      ~session_id
      ~manifest_authorizer
      ~approval_provider
      ~approval_store
  in
  let%bind attachment = Product_attachment.create prepared_native in
  Product_attachment.build attachment ~native:prepared_native ~f:(fun () ->
    build_product_attachment
      prepared_native
      ~initial_source_lease:Prompt_source_lease.legacy
      ~attachment
      ~close_resource:(fun () ->
        Native_release.close
          prepared_native.Prepared_native_data.release
          ~before_cache:(fun () -> Product_attachment.close attachment))
      ~construction_checkpoint
      ~extension_services
      ~history_namespace
      ~next_history_sequence
      ~existing_history
      ~existing_moderator_snapshot
      ~moderator_reservation_size
      ~permission_profile
      ~review_permission
      ~schedule_services
      ~job_services
      ~legacy_construction)
;;

let build_product_from_native
      prepared_native
      ~attachment
      ~source
      ~construction_checkpoint
      ~legacy_construction
      ~services
      ~history_namespace
      ~next_history_sequence
      ~existing_history
      ~existing_moderator_snapshot
      ~moderator_reservation_size
      ~permission_profile
      ~review_permission
      ~schedule_services
      ~job_services
  =
  let open Result.Let_syntax in
  let%bind () = construction_checkpoint () in
  let%bind () =
    Prepared_native.check
      prepared_native
      ~source
      ~session_id:prepared_native.Prepared_native_data.environment.session_id
      ~permission_profile_digest:permission_profile.Permission_policy.revision_digest
  in
  Product_attachment.build attachment ~native:prepared_native ~f:(fun () ->
    let%bind extension_services =
      prepare_extension_services
        prepared_native.Prepared_native_data.source
        ~env:prepared_native.Prepared_native_data.environment.env
        services
    in
    let%bind () = construction_checkpoint () in
    build_product_attachment
      prepared_native
      ~initial_source_lease:
        (Prompt_source_lease.borrow
           (Prepared_native.resources prepared_native).source_lease)
      ~attachment
      ~close_resource:(fun () -> Product_attachment.close attachment)
      ~construction_checkpoint:(Some construction_checkpoint)
      ~extension_services
      ~history_namespace
      ~next_history_sequence
      ~existing_history
      ~existing_moderator_snapshot
      ~moderator_reservation_size
      ~permission_profile
      ~review_permission
      ~schedule_services
      ~job_services
      ~legacy_construction)
;;

let build_product
      ~ancestor_capabilities
      ~ancestor_sources
      ~selected_source
      ~construction_checkpoint
      ~startup
      ~legacy_construction
      ~agent_source
      ~root_invocation
      ~script_preparation
      ~sw
      ~env
      ~inference_context
      ~inference_identity
      ~bind_inference_identity
      ~on_inference_attempt
      ~on_inference_completion
      ~on_inference_observation
      ~paths
      ~storage_paths
      ~revision
  =
  build_with_services
    ~ancestor_capabilities
    ~ancestor_sources
    ~selected_source
    ~construction_checkpoint
    ~legacy_construction
    ~startup
    ~agent_source
    ~native_registrations:[]
    ~sw
    ~env
    ~inference_context
    ~inference_identity
    ~bind_inference_identity
    ~on_inference_attempt
    ~on_inference_completion
    ~on_inference_observation
    ~paths
    ~storage_paths
    ~source:(Authored { revision; script_preparation; root_invocation })
    ~extension_services:None
;;

let build_product_with_extensions
      ~ancestor_capabilities
      ~ancestor_sources
      ~selected_source
      ~construction_checkpoint
      ~startup
      ~legacy_construction
      ~agent_source
      ~root_invocation
      ~script_preparation
      ~native_registrations
      ~services
      ~sw
      ~env
      ~inference_context
      ~inference_identity
      ~bind_inference_identity
      ~on_inference_attempt
      ~on_inference_completion
      ~on_inference_observation
      ~paths
      ~storage_paths
      ~revision
  =
  build_with_services
    ~ancestor_capabilities
    ~ancestor_sources
    ~selected_source
    ~construction_checkpoint
    ~legacy_construction
    ~startup
    ~agent_source
    ~native_registrations
    ~sw
    ~env
    ~inference_context
    ~inference_identity
    ~bind_inference_identity
    ~on_inference_attempt
    ~on_inference_completion
    ~on_inference_observation
    ~paths
    ~storage_paths
    ~source:(Authored { revision; script_preparation; root_invocation })
    ~extension_services:(Some services)
;;

let build_product_captured_artifact
      ~ancestor_capabilities
      ~ancestor_sources
      ~selected_source
      ~captured
      ~authority
      ~construction_checkpoint
      ~startup
      ~legacy_construction
      ~agent_source
      ~script_preparation
      ~native_registrations
      ~services
      ~sw
      ~env
      ~inference_context
      ~inference_identity
      ~bind_inference_identity
      ~on_inference_attempt
      ~on_inference_completion
      ~on_inference_observation
      ~paths
      ~storage_paths
  =
  let construction_checkpoint =
    Some
      (fun () ->
        let open Result.Let_syntax in
        let%bind () = check_construction construction_checkpoint in
        Delegation_authority.check_execution authority)
  in
  build_with_services
    ~ancestor_capabilities
    ~ancestor_sources
    ~selected_source
    ~construction_checkpoint
    ~legacy_construction
    ~startup
    ~agent_source
    ~native_registrations
    ~sw
    ~env
    ~inference_context
    ~inference_identity
    ~bind_inference_identity
    ~on_inference_attempt
    ~on_inference_completion
    ~on_inference_observation
    ~paths
    ~storage_paths
    ~source:(Captured_artifact { captured; script_preparation; authority })
    ~extension_services:services
;;

let build_product_generated
      ~ancestor_capabilities
      ~ancestor_sources
      ~selected_source
      ~construction_checkpoint
      ~services
      ~definition
      ~artifact_store
      ~parent_runtime
      ~inherited_managed
      ~authority
  =
  build_with_services
    ~ancestor_capabilities
    ~ancestor_sources
    ~selected_source
    ~construction_checkpoint
    ~legacy_construction:None
    ~startup:None
    ~agent_source:None
    ~native_registrations:[]
    ~source:
      (Generated
         { definition; artifact_store; parent_runtime; inherited_managed; authority })
    ~extension_services:(Some services)
;;

let build_product_authored_child
      ~ancestor_capabilities
      ~ancestor_sources
      ~selected_source
      ~construction_checkpoint
      ~services
      ~revision
      ~prepared
      ~code
      ~authority
      ~existing_history
  =
  build_with_services
    ~ancestor_capabilities
    ~ancestor_sources
    ~selected_source
    ~construction_checkpoint
    ~legacy_construction:None
    ~startup:None
    ~agent_source:None
    ~native_registrations:[]
    ~source:(Authored_child { revision; prepared; code; authority })
    ~existing_history
    ~extension_services:(Some services)
;;

let build
      ~construction_checkpoint
      ~startup
      ~legacy_construction
      ~agent_source
      ~root_invocation
      ~script_preparation
      ~sw
      ~env
      ~inference_context
      ~inference_identity
      ~bind_inference_identity
      ~on_inference_attempt
      ~on_inference_completion
      ~on_inference_observation
      ~paths
      ~storage_paths
      ~revision
      ~session_id
      ~history_namespace
      ~next_history_sequence
      ~existing_history
      ~existing_moderator_snapshot
      ~moderator_reservation_size
      ~manifest_authorizer
      ~approval_provider
      ~approval_store
      ~permission_profile
      ~review_permission
      ~schedule_services
      ~job_services
  =
  build_product
    ~ancestor_capabilities:None
    ~ancestor_sources:None
    ~selected_source:None
    ~construction_checkpoint
    ~startup
    ~legacy_construction
    ~agent_source
    ~root_invocation
    ~script_preparation
    ~sw
    ~env
    ~inference_context
    ~inference_identity
    ~bind_inference_identity
    ~on_inference_attempt
    ~on_inference_completion
    ~on_inference_observation
    ~paths
    ~storage_paths
    ~revision
    ~session_id
    ~history_namespace
    ~next_history_sequence
    ~existing_history
    ~existing_moderator_snapshot
    ~moderator_reservation_size
    ~manifest_authorizer
    ~approval_provider
    ~approval_store
    ~permission_profile
    ~review_permission
    ~schedule_services
    ~job_services
  |> Result.map ~f:(fun (_, binding) -> Product.Binding.runtime binding)
;;

let build_with_extensions
      ~construction_checkpoint
      ~startup
      ~legacy_construction
      ~agent_source
      ~root_invocation
      ~script_preparation
      ~native_registrations
      ~services
      ~sw
      ~env
      ~inference_context
      ~inference_identity
      ~bind_inference_identity
      ~on_inference_attempt
      ~on_inference_completion
      ~on_inference_observation
      ~paths
      ~storage_paths
      ~revision
      ~session_id
      ~history_namespace
      ~next_history_sequence
      ~existing_history
      ~existing_moderator_snapshot
      ~moderator_reservation_size
      ~manifest_authorizer
      ~approval_provider
      ~approval_store
      ~permission_profile
      ~review_permission
      ~schedule_services
      ~job_services
  =
  build_product_with_extensions
    ~ancestor_capabilities:None
    ~ancestor_sources:None
    ~selected_source:None
    ~construction_checkpoint
    ~startup
    ~legacy_construction
    ~agent_source
    ~root_invocation
    ~script_preparation
    ~native_registrations
    ~services
    ~sw
    ~env
    ~inference_context
    ~inference_identity
    ~bind_inference_identity
    ~on_inference_attempt
    ~on_inference_completion
    ~on_inference_observation
    ~paths
    ~storage_paths
    ~revision
    ~session_id
    ~history_namespace
    ~next_history_sequence
    ~existing_history
    ~existing_moderator_snapshot
    ~moderator_reservation_size
    ~manifest_authorizer
    ~approval_provider
    ~approval_store
    ~permission_profile
    ~review_permission
    ~schedule_services
    ~job_services
  |> Result.map ~f:(fun (_, binding) -> Product.Binding.runtime binding)
;;

let build_captured_artifact
      ~captured
      ~authority
      ~construction_checkpoint
      ~startup
      ~legacy_construction
      ~agent_source
      ~script_preparation
      ~native_registrations
      ~services
      ~sw
      ~env
      ~inference_context
      ~inference_identity
      ~bind_inference_identity
      ~on_inference_attempt
      ~on_inference_completion
      ~on_inference_observation
      ~paths
      ~storage_paths
      ~session_id
      ~history_namespace
      ~next_history_sequence
      ~existing_history
      ~existing_moderator_snapshot
      ~moderator_reservation_size
      ~manifest_authorizer
      ~approval_provider
      ~approval_store
      ~permission_profile
      ~review_permission
      ~schedule_services
      ~job_services
  =
  build_product_captured_artifact
    ~ancestor_capabilities:None
    ~ancestor_sources:None
    ~selected_source:None
    ~captured
    ~authority
    ~construction_checkpoint
    ~startup
    ~legacy_construction
    ~agent_source
    ~script_preparation
    ~native_registrations
    ~services
    ~sw
    ~env
    ~inference_context
    ~inference_identity
    ~bind_inference_identity
    ~on_inference_attempt
    ~on_inference_completion
    ~on_inference_observation
    ~paths
    ~storage_paths
    ~session_id
    ~history_namespace
    ~next_history_sequence
    ~existing_history
    ~existing_moderator_snapshot
    ~moderator_reservation_size
    ~manifest_authorizer
    ~approval_provider
    ~approval_store
    ~permission_profile
    ~review_permission
    ~schedule_services
    ~job_services
  |> Result.map ~f:(fun (_, binding) -> Product.Binding.runtime binding)
;;

let build_generated
      ~construction_checkpoint
      ~services
      ~definition
      ~artifact_store
      ~parent_runtime
      ~inherited_managed
      ~authority
      ~sw
      ~env
      ~inference_context
      ~inference_identity
      ~bind_inference_identity
      ~on_inference_attempt
      ~on_inference_completion
      ~on_inference_observation
      ~paths
      ~storage_paths
      ~session_id
      ~history_namespace
      ~next_history_sequence
      ~existing_history
      ~existing_moderator_snapshot
      ~moderator_reservation_size
      ~manifest_authorizer
      ~approval_provider
      ~approval_store
      ~permission_profile
      ~review_permission
      ~schedule_services
      ~job_services
  =
  build_product_generated
    ~ancestor_capabilities:None
    ~ancestor_sources:None
    ~selected_source:None
    ~construction_checkpoint
    ~services
    ~definition
    ~artifact_store
    ~parent_runtime
    ~inherited_managed
    ~authority
    ~sw
    ~env
    ~inference_context
    ~inference_identity
    ~bind_inference_identity
    ~on_inference_attempt
    ~on_inference_completion
    ~on_inference_observation
    ~paths
    ~storage_paths
    ~session_id
    ~history_namespace
    ~next_history_sequence
    ~existing_history
    ~existing_moderator_snapshot
    ~moderator_reservation_size
    ~manifest_authorizer
    ~approval_provider
    ~approval_store
    ~permission_profile
    ~review_permission
    ~schedule_services
    ~job_services
  |> Result.map ~f:(fun (_, binding) -> Product.Binding.runtime binding)
;;

let build_authored_child
      ~construction_checkpoint
      ~services
      ~revision
      ~prepared
      ~code
      ~authority
      ~existing_history
      ~sw
      ~env
      ~inference_context
      ~inference_identity
      ~bind_inference_identity
      ~on_inference_attempt
      ~on_inference_completion
      ~on_inference_observation
      ~paths
      ~storage_paths
      ~session_id
      ~history_namespace
      ~next_history_sequence
      ~existing_moderator_snapshot
      ~moderator_reservation_size
      ~manifest_authorizer
      ~approval_provider
      ~approval_store
      ~permission_profile
      ~review_permission
      ~schedule_services
      ~job_services
  =
  build_product_authored_child
    ~ancestor_capabilities:None
    ~ancestor_sources:None
    ~selected_source:None
    ~construction_checkpoint
    ~services
    ~revision
    ~prepared
    ~code
    ~authority
    ~existing_history
    ~sw
    ~env
    ~inference_context
    ~inference_identity
    ~bind_inference_identity
    ~on_inference_attempt
    ~on_inference_completion
    ~on_inference_observation
    ~paths
    ~storage_paths
    ~session_id
    ~history_namespace
    ~next_history_sequence
    ~existing_moderator_snapshot
    ~moderator_reservation_size
    ~manifest_authorizer
    ~approval_provider
    ~approval_store
    ~permission_profile
    ~review_permission
    ~schedule_services
    ~job_services
  |> Result.map ~f:(fun (_, binding) -> Product.Binding.runtime binding)
;;

let inference_execution t = t.inference_execution

let with_authored_source_lease (prepared : authored_resources) ~source_lease =
  let open Or_error.Let_syntax in
  let%map resources =
    with_resource_source_lease
      prepared.resources
      ~source_lease:(Prompt_source_lease.borrow source_lease)
  in
  { prepared with resources }
;;

let with_source_lease (runtime : t) ~source_lease =
  let open Or_error.Let_syntax in
  let%map () = admit_source_attachment runtime.source_lease source_lease in
  let close =
    Runtime_close.reported (fun () ->
      let physical = Runtime_close.run_reported runtime.close in
      let close_source () =
        Runtime_close.run_reported
          (Runtime_close.legacy (fun () -> Prompt_source_lease.close source_lease))
      in
      let source =
        match Close_observation.exit physical with
        | Returned () -> close_source ()
        | Raised _ -> Eio.Cancel.protect close_source
      in
      let selected =
        match Close_observation.exit physical with
        | Returned () -> source
        | Raised _ -> physical
      in
      Close_observation.create
        (Close_observation.exit selected)
        ~diagnostics:
          (Close_observation.diagnostics physical @ Close_observation.diagnostics source)
        ?primary:(Close_observation.primary selected)
        ())
  in
  { runtime with source_lease; close }
;;
