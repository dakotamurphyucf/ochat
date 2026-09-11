open! Core
module Agent_runtime = Chat_response.Agent_runtime
module Config = Chat_response.Config
module Converter = Chat_response.Converter
module Manager = Chat_response.Moderator_manager
module Moderation = Chat_response.Moderation
module Request = Openai.Responses.Request

type moderator_drain =
  { moderator_snapshot : Jsonaf.t option
  ; runtime_requests : Moderation.Runtime_request.t list
  ; notifications : string list
  ; remaining_events : bool
  }

type model_job_outcome =
  | Model_succeeded of Jsonaf.t
  | Model_failed of string

type model_post_stream = Chat_response.In_memory_stream.post_stream

type resources =
  { native : Agent_runtime.t
  ; definition : Chat_response.Extension_compiler.definition option
  ; managed : Chat_response.Managed_tool_registry.t option
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
  ; claim_lifecycle : event:Moderation.Event.t -> Moderator_event.claim
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
      -> operation_id:Agent_protocol.Id.Operation.t
      -> unit
      -> ( Chat_response.In_memory_stream.Safe_point_input.batch
           , Agent_protocol.Error.t )
           result
  ; initial_notification_input :
      source:Agent_protocol.Invocation.observer option
      -> tools:Script_tool_calls.t
      -> operation_id:Agent_protocol.Id.Operation.t
      -> unit
      -> ( Chat_response.In_memory_stream.Safe_point_input.batch
           , Agent_protocol.Error.t )
           result
  }

type moderator_activation =
  { pending : unit -> bool
  ; run : unit -> (bool, Agent_protocol.Error.t) result
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

type t =
  { worker : Operation_worker.t
  ; parse_user_content :
      id:History_entry.Id.t
      -> Agent_protocol.Session.Message_content.t
      -> (History_entry.t, Agent_protocol.Error.t) result
  ; initial_history : History_entry.t list
  ; initial_prompt_entry_count : int
  ; reserved_history_through : int
  ; mutable moderator_snapshot : Jsonaf.t option
  ; moderator_manager : Manager.t option
  ; moderator_tools : Request.Tool.t list
  ; idle_notifications : (unit -> (bool, Agent_protocol.Error.t) result) option
  ; moderator_script_tools : Script_tool_calls.t option
  ; standalone_completion :
      (Agent_protocol.Job.t -> (unit, Agent_protocol.Error.t) result) option
  ; background_executor : background_executor option
  ; moderator_activation : moderator_activation option
  ; automatic_turn_policy : Chat_response.Runtime_semantics.policy option
  ; check_execution : (unit -> (unit, Agent_protocol.Error.t) result) option
  ; ancestor_capabilities :
      (Agent_protocol.Id.Session.t
       -> (Chat_response.Tool_capability.t, Agent_protocol.Error.t) result)
        option
  ; activity : Runtime_activity.t option
  ; native_runtime : Agent_runtime.t option
  ; start_moderator : unit -> (Jsonaf.t option, Agent_protocol.Error.t) result
  ; enqueue_internal_event :
      ?prepare:prepare_enqueue
      -> Jsonaf.t
      -> (Jsonaf.t option, Agent_protocol.Error.t) result
  ; drain_internal_events :
      History_entry.t list -> (moderator_drain, Agent_protocol.Error.t) result
  ; execute_model_job :
      recipe:string
      -> payload:Jsonaf.t
      -> (model_job_outcome, Agent_protocol.Error.t) result
  ; enqueue_model_job_completion :
      ?prepare:prepare_enqueue
      -> Agent_protocol.Job.t
      -> (Jsonaf.t option, Agent_protocol.Error.t) result
  ; close : unit -> unit
  }

type schedule_services =
  { after_ms : delay_ms:int -> payload:Jsonaf.t -> (string, string) result
  ; cancel : id:string -> (unit, string) result
  }

type job_services =
  { spawn_model : recipe:string -> payload:Jsonaf.t -> (string, string) result
  ; call_model :
      recipe:string
      -> payload:Jsonaf.t
      -> execute:(unit -> (Moderation.Capabilities.model_call_result, string) result)
      -> (Moderation.Capabilities.model_call_result, string) result
  }

let failure message =
  Agent_protocol.Error.create Invalid_state ~message ~retryable:false ()
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

let context ~env paths cache =
  Chat_response.Ctx.create
    ~env
    ~dir:paths.Runtime_paths.prompt_dir
    ~tool_dir:paths.tool_dir
    ~cache
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
    ~resource_runner:(Sys.getenv "OCHAT_SHELL_RESOURCE_RUNNER")
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

let create_agent_runtime
      ~extensions
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

let declares_native elements name =
  List.exists elements ~f:(function
    | Prompt.Chat_markdown.Tool (Builtin declared) -> String.equal declared name
    | _ -> false)
;;

let native_registrations ~env ~elements ~one_off_policy ~authoring_validation_host =
  let registrations =
    match declares_native elements Run_chatml_tool.name, one_off_policy with
    | true, Some policy ->
      [ Run_chatml_tool.registration ~env ~policy ~services:(fun () ->
          let open Result.Let_syntax in
          let%bind script_tools = Script_tool_calls.current_native_services () in
          let%map moderation = Native_tool_moderation.current () in
          Run_chatml_tool.
            { script_tools
            ; observer = Native_tool_moderation.observer moderation
            ; now =
                (fun () ->
                  Eio.Time.now (Eio.Stdenv.clock env)
                  |> Time_ns.Span.of_sec
                  |> Time_ns.of_span_since_epoch
                  |> Agent_protocol.Timestamp.of_time_ns)
            ; moderate_tool =
                (fun _ call ->
                  Native_tool_moderation.prepare moderation call
                  |> Result.map ~f:(fun tool_moderation ->
                    Some { Moderation.Outcome.empty with tool_moderation }))
            ; prepare_outcome =
                (fun outcome ->
                  Agent_protocol.Invocation.validate_outcome outcome
                  |> Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message))
            })
      ]
    | _ -> []
  in
  let registrations =
    match declares_native elements Generated_session_tool.name, one_off_policy with
    | true, Some _ -> registrations @ [ Generated_session_tool.registration () ]
    | _ -> registrations
  in
  let registrations =
    match declares_native elements Managed_session_tool.status_name, one_off_policy with
    | true, Some _ -> registrations @ [ Managed_session_tool.status_registration () ]
    | _ -> registrations
  in
  let registrations =
    match declares_native elements Managed_send_tool.name, one_off_policy with
    | true, Some _ -> registrations @ [ Managed_send_tool.registration () ]
    | _ -> registrations
  in
  let registrations =
    match declares_native elements Managed_read_tool.name, one_off_policy with
    | true, Some _ -> registrations @ [ Managed_read_tool.registration () ]
    | _ -> registrations
  in
  let registrations =
    match declares_native elements Managed_wait_tool.name, one_off_policy with
    | true, Some _ -> registrations @ [ Managed_wait_tool.registration () ]
    | _ -> registrations
  in
  let registrations =
    match declares_native elements Managed_stop_tool.name, one_off_policy with
    | true, Some _ -> registrations @ [ Managed_stop_tool.registration () ]
    | _ -> registrations
  in
  match
    declares_native elements Authoring_validation_tool.name, authoring_validation_host
  with
  | false, _ -> Ok registrations
  | true, Some host ->
    Ok (registrations @ [ Authoring_validation_tool.registration ~env ~host ])
  | true, None ->
    Error
      (Agent_protocol.Error.create
         Invalid_state
         ~message:
           "authoring.unavailable: readonly validation needs an explicit host target"
         ~retryable:false
         ())
;;

let create_authored_resources
      ~native_service_revision
      ~env
      ~sw
      ~ctx
      ~host
      ~elements
      ~one_off_policy
      ~authoring_validation_host
      ~manifest_authorizer
      ~approval_provider
      ~approval_store
      ~response_dir
  =
  let open Result.Let_syntax in
  let%bind native_registrations =
    native_registrations ~env ~elements ~one_off_policy ~authoring_validation_host
  in
  let extensions =
    Option.is_some one_off_policy
    && (declares_native elements Run_chatml_tool.name
        || declares_native elements Generated_session_tool.name
        || declares_native elements Managed_session_tool.status_name
        || declares_native elements Managed_send_tool.name
        || declares_native elements Managed_read_tool.name
        || declares_native elements Managed_wait_tool.name
        || declares_native elements Managed_stop_tool.name
        || declares_native elements Authoring_validation_tool.name
        || List.exists elements ~f:(function
          | Prompt.Chat_markdown.Extension_script _ | Tool (Extension _) -> true
          | _ -> false))
  in
  let%map native, definition, managed =
    create_agent_runtime
      ~extensions
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
  { native; definition; managed }
;;

let prepare_resources
      ~native_service_revision
      ~env
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
  let ctx = context ~env paths (cache storage_paths) in
  let%bind host = host ~env ~paths ~session_id ~elements in
  create_authored_resources
    ~native_service_revision
    ~env
    ~sw
    ~ctx
    ~host
    ~elements
    ~one_off_policy:(Some one_off_policy)
    ~authoring_validation_host
    ~manifest_authorizer
    ~approval_provider
    ~approval_store
    ~response_dir
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
  { native
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
      Chat_response.Managed_tool_registry.delegate_standalone
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
;;

let initial_items ~ctx ~elements ~manifest_authorizer ~approval_provider ~response_dir =
  Converter.to_items
    ~ctx
    ~run_agent:(fun ?prompt_dir ?session_id ~ctx prompt items ->
      Chat_response.Driver.run_agent
        ~history_compaction:false
        ?prompt_dir
        ?session_id
        ~response_dir
        ~shell_manifest_authorizer:manifest_authorizer
        ~shell_approval_provider:approval_provider
        ~ctx
        prompt
        items)
    elements
;;

let allocate_initial ~namespace ~next_sequence items =
  let open Result.Let_syntax in
  let%bind allocator =
    History_entry.Allocator.create ~namespace ~next_sequence
    |> Result.map_error ~f:failure
  in
  let%map history =
    List.map items ~f:(History_entry.create ~allocator)
    |> Result.all
    |> Result.map_error ~f:failure
  in
  history, History_entry.Allocator.next_sequence allocator
;;

let model_config elements =
  let config = Config.of_elements elements in
  let model =
    Option.value_map config.model ~default:Request.Gpt4 ~f:Request.model_of_str_exn
  in
  let reasoning =
    Option.map config.reasoning_effort ~f:(fun effort ->
      Request.Reasoning.
        { effort = Some (Effort.of_str_exn effort); summary = Some Summary.Detailed })
  in
  config, model, reasoning
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

let create_moderator
      ~definition
      ~delegated
      ~runtime_policy
      ~sw
      ~env
      ~ctx
      ~session_id
      ~elements
      ~history
      ~tools
      ~allocator
      ~agent_runtime
      ~manifest_authorizer
      ~approval_provider
      ~response_dir
      ~schedule_services
      ~job_services
      ~snapshot
  =
  let open Result.Let_syntax in
  let legacy_artifact () =
    Manager.Registry.of_elements
      ~surface:Chatml.Chatml_builtin_surface.ui_moderator_surface
      Manager.Registry.empty
      elements
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
  match artifact with
  | None -> Ok (None, fun () -> Ok None)
  | Some artifact ->
    let executor =
      model_executor ~sw ~ctx ~manifest_authorizer ~approval_provider ~response_dir
    in
    let session_text = Agent_protocol.Id.Session.to_string session_id in
    let legacy_recipe =
      Chat_response.Model_executor.recipe_agent_prompt_v1
        executor
        ~session_id:session_text
    in
    let durable_recipe : Moderation.Capabilities.model_recipe =
      { call =
          (fun ~payload ->
            job_services.call_model
              ~recipe:Chat_response.Model_executor.agent_prompt_v1_name
              ~payload
              ~execute:(fun () -> legacy_recipe.call ~payload))
      ; spawn =
          (fun ~payload ->
            job_services.spawn_model
              ~recipe:Chat_response.Model_executor.agent_prompt_v1_name
              ~payload)
      }
    in
    let capabilities =
      { Moderation.Capabilities.default with
        model_recipes =
          (match delegated with
           | true -> Map.empty (module String)
           | false ->
             Map.singleton
               (module String)
               Chat_response.Model_executor.agent_prompt_v1_name
               durable_recipe)
      ; on_schedule_after_ms =
          (fun ~delay_ms ~payload ->
            let open Result.Let_syntax in
            let%bind payload = Chatml.Chatml_value_codec.Snapshot.of_value payload in
            let payload = Chatml.Chatml_value_codec.Snapshot.to_jsonaf payload in
            schedule_services.after_ms ~delay_ms ~payload)
      ; on_schedule_cancel = schedule_services.cancel
      }
    in
    let%bind manager =
      Manager.create_entries
        ~env
        ~artifact
        ~capabilities
        ~allocator
        ?on_process_run:(Agent_runtime.moderator_process_handler agent_runtime)
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
    let started = ref false in
    let current_snapshot () =
      Manager.identity_snapshot manager
      |> Result.map_error ~f:failure
      |> Result.map ~f:(fun snapshot ->
        Some
          (`Object
              [ ( "identity_snapshot_sexp"
                , `String
                    (Sexp.to_string_mach
                       ([%sexp_of: Session.Moderator_state.Identity_snapshot.t] snapshot))
                )
              ]))
    in
    let start () =
      if Option.is_some (Manager.extension_definition manager) || !started
      then current_snapshot ()
      else (
        let now_ms =
          Eio.Time.now (Eio.Stdenv.clock env) *. 1_000. |> Float.iround_nearest_exn
        in
        let open Result.Let_syntax in
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
    Ok (Some moderator_pair, start)
;;

let encode_moderator_snapshot snapshot =
  `Object
    [ ( "identity_snapshot_sexp"
      , `String
          (Sexp.to_string_mach
             ([%sexp_of: Session.Moderator_state.Identity_snapshot.t] snapshot)) )
    ]
;;

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
  let%bind snapshot =
    Chatml.Chatml_value_codec.Snapshot.of_jsonaf payload |> Result.map_error ~f:failure
  in
  let%bind value =
    Chatml.Chatml_value_codec.Snapshot.to_value snapshot |> Result.map_error ~f:failure
  in
  enqueue_internal_value ?prepare moderator value
;;

let execute_model_job moderator session_id ~recipe ~payload =
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
      in
      handler.call ~payload
      |> Result.map_error ~f:failure
      |> Result.map ~f:(function
        | Moderation.Capabilities.Model_ok value -> Model_succeeded value
        | Model_refused message | Model_error message -> Model_failed message))
;;

let job_recipe_and_result (job : Agent_protocol.Job.t) =
  match job.payload with
  | `Object fields ->
    (match List.Assoc.find fields "recipe" ~equal:String.equal with
     | Some (`String recipe) -> Ok (recipe, job.result)
     | Some _ | None -> Error (failure "model job payload has no recipe"))
  | _ -> Error (failure "model job payload must be an object")
;;

let model_job_event (job : Agent_protocol.Job.t) =
  let open Result.Let_syntax in
  let%bind recipe, result = job_recipe_and_result job in
  let job_id = Agent_protocol.Id.Job.to_string job.id in
  match job.status with
  | Agent_protocol.Job.Succeeded ->
    let%map result =
      result |> Result.of_option ~error:(failure "succeeded model job has no result")
    in
    Chatml.Chatml_lang.VVariant
      ( "Model_job_succeeded"
      , [ VString job_id
        ; VString recipe
        ; Chatml.Chatml_value_codec.jsonaf_to_value result
        ] )
  | Failed error ->
    Ok
      (Chatml.Chatml_lang.VVariant
         ("Model_job_failed", [ VString job_id; VString recipe; VString error.message ]))
  | Interrupted reason ->
    Ok
      (Chatml.Chatml_lang.VVariant
         ("Model_job_failed", [ VString job_id; VString recipe; VString reason ]))
  | Cancelled ->
    Ok
      (Chatml.Chatml_lang.VVariant
         ( "Model_job_failed"
         , [ VString job_id; VString recipe; VString "job was cancelled" ] ))
  | Queued | Running | Waiting_permission _ | Waiting_completion _ ->
    Error (failure "model job is not terminal")
;;

let enqueue_model_job_completion moderator ?prepare job =
  Result.bind (model_job_event job) ~f:(enqueue_internal_value ?prepare moderator)
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

let close_runtime cache paths session_id moderator =
  Option.iter moderator ~f:(fun (_, executor) ->
    Chat_response.Model_executor.unregister_session_wakeup
      executor
      ~session_id:(Agent_protocol.Id.Session.to_string session_id));
  Chat_response.Cache.save
    ~file:Eio.Path.(paths.Runtime_paths.cache_dir / "cache.bin")
    cache
;;

let plain_user_item text =
  Openai.Responses.Item.Input_message
    { role = User; content = [ Text { text; _type = "input_text" } ]; _type = "message" }
;;

let chatmd_user_item ~ctx ~run_agent paths text =
  let xml =
    if String.is_prefix (String.strip text) ~prefix:"<"
    then text
    else sprintf "<user>\n%s\n</user>" text
  in
  try
    Prompt.Chat_markdown.parse_chat_inputs ~dir:paths.Runtime_paths.workspace xml
    |> List.find_map ~f:(function
      | Prompt.Chat_markdown.User message ->
        Some (Converter.convert_user_msg ~ctx ~run_agent message)
      | _ -> None)
    |> Result.of_option ~error:(failure "ChatMD input does not contain a user message")
  with
  | exn -> Error (failure ("ChatMD input parse failed: " ^ Exn.to_string exn))
;;

let parse_user_content ~ctx ~manifest_authorizer ~approval_provider ~response_dir paths =
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
  fun ~id content ->
    let open Result.Let_syntax in
    let%map item =
      match content.Agent_protocol.Session.Message_content.kind with
      | Plain_text -> Ok (plain_user_item content.text)
      | Chatmd -> chatmd_user_item ~ctx ~run_agent:converter_runner paths content.text
    in
    History_entry.create_with_id ~id item
;;

type inherited_managed =
  { delegation : Chat_response.Managed_tool_registry.delegation
  ; current : unit -> Chat_response.Tool_capability.t
  }

type source =
  | Authored of Prompt_revision.t
  | Generated of
      { definition : Generated_definition.t
      ; artifact_store : Agent_store.Prompt_artifact_store.t
      ; parent_runtime : Agent_runtime.t
      ; inherited_managed : inherited_managed option
      ; authority : Delegation_authority.t
      }

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
  ; notification_input =
      (fun ~source ~tools ~operation_id () ->
        checked (fun () -> services.notification_input ~source ~tools ~operation_id ()))
  ; initial_notification_input =
      (fun ~source ~tools ~operation_id () ->
        checked (fun () ->
          services.initial_notification_input ~source ~tools ~operation_id ()))
  ; idle_notifications =
      (fun ~source ~tools () ->
        checked (fun () -> services.idle_notifications ~source ~tools ()))
  ; standalone_completion =
      (fun ~tools job -> checked (fun () -> services.standalone_completion ~tools job))
  }
;;

let build_with_services
      ~extension_services
      ~sw
      ~env
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
      ~model_post_stream
      ~review_permission
      ~schedule_services
      ~job_services
  =
  let open Result.Let_syntax in
  let%bind () =
    match source with
    | Authored _ -> Ok ()
    | Generated { definition; authority; _ } ->
      Delegation_authority.check_preparation
        authority
        ~session_id
        ~revision_id:(Generated_definition.artifact definition).revision_id
        ~manifest_sha256:(Generated_definition.artifact definition).manifest_sha256
        ~permission_profile
  in
  let extension_services =
    match source, extension_services with
    | Generated { authority; _ }, Some services ->
      Some (guarded_services authority services)
    | _, services -> services
  in
  let extension_services =
    Option.map extension_services ~f:(fun services ->
      let host =
        Option.map services.authoring_validation_host ~f:(fun host ->
          match source with
          | Authored _ -> host
          | Generated _ -> Chat_response.Authoring_validation.for_delegated host)
      in
      { services with
        authoring_validation_host = host
      ; script_tools =
          (fun native ->
            Script_tool_calls.with_authoring_validation_host
              (services.script_tools native)
              host)
      })
  in
  let elements, artifact, materialized_tree, delegated =
    match source with
    | Authored revision ->
      ( Prompt_revision.elements revision
      , Prompt_revision.artifact revision
      , Prompt_revision.materialized_tree revision
      , false )
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
  let%bind generation_config =
    match delegated with
    | false -> Ok (model_config elements)
    | true ->
      (try Ok (model_config elements) with
       | Failure _ ->
         Error (failure "unsupported generated model/reasoning configuration"))
  in
  let%bind () =
    Agent_store.Prompt_artifact_store.verify_tree ~root:materialized_tree artifact
    |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
  in
  let response_dir = response_dir storage_paths in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 response_dir;
  let cache = cache storage_paths in
  let ctx = context ~env paths cache in
  let declares_one_off = declares_native elements Run_chatml_tool.name in
  let%bind agent_runtime, definition, managed =
    match source with
    | Generated { definition; parent_runtime; inherited_managed; _ } ->
      let%map resources =
        inherit_prepared_resources
          ~inherited_managed:
            (Option.map inherited_managed ~f:(fun inherited -> inherited.delegation))
          ~parent_runtime
          ~definition
      in
      resources.native, resources.definition, resources.managed
    | Authored _ ->
      let%bind host = host ~env ~paths ~session_id ~elements in
      let%map resources =
        create_authored_resources
          ~native_service_revision:
            (Option.bind extension_services ~f:(fun services ->
               services.native_service_revision))
          ~env
          ~one_off_policy:
            (Option.map extension_services ~f:(fun services -> services.one_off_policy))
          ~authoring_validation_host:
            (Option.bind extension_services ~f:(fun services ->
               services.authoring_validation_host))
          ~sw
          ~ctx
          ~host
          ~elements
          ~manifest_authorizer
          ~approval_provider
          ~approval_store
          ~response_dir
      in
      resources.native, resources.definition, resources.managed
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
    | Authored _ -> None
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
  let%bind initial_history, initial_end =
    match existing_history with
    | Some history -> Ok (history, next_history_sequence)
    | None ->
      initial_items ~ctx ~elements ~manifest_authorizer ~approval_provider ~response_dir
      |> allocate_initial
           ~namespace:history_namespace
           ~next_sequence:next_history_sequence
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
  let%bind moderator, start_moderator_once =
    create_moderator
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
      ~elements
      ~history:initial_history
      ~tools
      ~allocator:moderator_allocator
      ~agent_runtime
      ~manifest_authorizer
      ~approval_provider
      ~response_dir
      ~schedule_services
      ~job_services
      ~snapshot:restored_moderator_snapshot
  in
  let%bind moderator_snapshot = moderator_snapshot moderator in
  let now () =
    Eio.Time.now (Eio.Stdenv.clock env)
    |> Time_ns.Span.of_sec
    |> Time_ns.of_span_since_epoch
    |> Agent_protocol.Timestamp.of_time_ns
  in
  let script_tools =
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
      Some
        (match moderator with
         | Some (moderator, _)
           when (not delegated)
                && Option.is_some (Manager.extension_definition moderator.manager) ->
           Script_tool_calls.with_moderator_dispatch
             tools_service
             ~dispatch:
               (Managed_moderator_dispatch.create
                  ~definition
                  ~manager:moderator.manager
                  ~history:services.history
                  ~available_tools:tools
                  ~session_meta:`Null
                  ~now)
         | _ -> tools_service)
    | None, Some services -> Some (services.script_tools agent_runtime)
    | _, None -> None
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
  let activate lifecycle ~claim ~history =
    Moderator_event.Lifecycle.run
      lifecycle
      ~claim
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
        ; run =
            (fun () ->
              activate lifecycle ~claim:services.claim_lifecycle ~history:services.history
              |> Result.map ~f:(function
                | Activated _ -> true
                | Unavailable | Already_active -> false))
        }
    | _ -> None
  in
  let moderator_events =
    match lifecycle, moderator with
    | Some lifecycle, Some (moderator, _) ->
      Some
        (fun ~input ~capabilities ->
          let%bind activation =
            activate
              lifecycle
              ~claim:(fun ~event ->
                capabilities.Operation_worker.Capabilities.with_moderator_event ~event)
              ~history:(fun () -> input.Operation_worker.Input.history)
          in
          match activation with
          | Unavailable ->
            Error (failure "foreground moderator activation was not admitted")
          | Activated _ | Already_active ->
            Moderator_event.foreground_handlers
              ?script_tools
              ~capabilities
              ~manager:moderator.manager
              ~session_meta:`Null
              ~now
              ())
    | _ -> None
  in
  let%bind advertised_dispatch =
    match script_tools with
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
        Option.map moderator ~f:(fun (moderator, _) ->
          Moderator_event.foreground_handlers
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
        | Some (Ok handlers), Some services ->
          handlers.handle
            ~history:(services.history ())
            ~available_tools:tools
            ~now_ms:
              (Agent_protocol.Timestamp.to_time_ns (now ())
               |> Time_ns.to_int_ns_since_epoch
               |> fun n -> n / 1_000_000)
            ~event:(Moderation.Event.Pre_tool_call call)
        | Some _, None -> Error "moderator services are not installed"
      in
      let native =
        Script_tool_calls.native_dispatch script_tools ~declared ~input ~capabilities
      in
      let native =
        { native with
          run =
            (fun request ~authorize ->
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
                (fun () -> native.run request ~authorize))
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
      |> Script_tool_calls.with_model_preparation script_tools ~selected:declared ~input)
  in
  let config, model, reasoning = generation_config in
  let notification_source =
    Option.bind moderator ~f:(fun (moderator, _) ->
      Manager.invocation_observer moderator.manager)
  in
  let notification_input =
    match extension_services, script_tools with
    | Some services, Some tools ->
      Some
        (fun ~input ->
          services.notification_input
            ~source:notification_source
            ~tools
            ~operation_id:input.Operation_worker.Input.operation.id)
    | _ -> None
  in
  let initial_notification_input =
    match extension_services, script_tools with
    | Some services, Some tools ->
      Some
        (fun ~input ->
          services.initial_notification_input
            ~source:notification_source
            ~tools
            ~operation_id:input.Operation_worker.Input.operation.id)
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
      ?runtime_policy:
        (Option.map extension_services ~f:(fun services -> services.runtime_policy))
      ?dispatch_tool
      ?moderator_events
      ?notification_input
      ?initial_notification_input
      { env
      ; response_dir
      ; tools
      ; tool_tbl
      ; temperature = config.temperature
      ; max_output_tokens = config.max_tokens
      ; reasoning
      ; moderator = Option.map moderator ~f:fst
      ; permission_profile
      ; review_permission
      ; history_compaction = false
      ; parallel_tool_calls = true
      ; model
      ; prompt_cache_key = Some (Agent_protocol.Id.Session.to_string session_id)
      ; prompt_cache_retention = None
      ; post_stream = model_post_stream
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
    match source with
    | Authored _ -> None
    | Generated { authority; _ } ->
      Some (fun () -> Delegation_authority.check_execution authority)
  in
  let worker =
    match check_execution with
    | None -> worker
    | Some check ->
      Operation_worker.create ~run:(fun ~sw ~input capabilities ->
        match check () with
        | Error error -> Operation_worker.Failed error
        | Ok () -> Operation_worker.run worker ~sw ~input capabilities)
  in
  let activity =
    match source with
    | Authored _ -> None
    | Generated _ -> Some (Runtime_activity.create ~sw)
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
      Operation_worker.create ~run:(fun ~sw:_ ~input capabilities ->
        Runtime_activity.with_switch activity (fun ~sw ->
          Operation_worker.run worker ~sw ~input capabilities))
  in
  let parse_user_content =
    match delegated with
    | false ->
      parse_user_content ~ctx ~manifest_authorizer ~approval_provider ~response_dir paths
    | true ->
      fun ~id content ->
        (match
           content.Agent_protocol.Session.Message_content.kind, content.attachments
         with
         | Plain_text, [] ->
           Ok (History_entry.create_with_id ~id (plain_user_item content.text))
         | Chatmd, _ | Plain_text, _ :: _ ->
           Error
             (failure
                "generated sessions accept plain_text messages without implicit resource \
                 loading"))
  in
  let rec runtime =
    { worker
    ; parse_user_content
    ; initial_history
    ; initial_prompt_entry_count = List.length initial_history
    ; reserved_history_through
    ; moderator_snapshot
    ; moderator_manager =
        Option.map moderator ~f:(fun (moderator, _) ->
          moderator.Chat_response.In_memory_stream.manager)
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
    ; ancestor_capabilities = None
    ; activity
    ; native_runtime =
        (match extension_services with
         | None -> None
         | Some _ -> Some agent_runtime)
    ; start_moderator =
        (fun () ->
          let open Result.Let_syntax in
          let%map snapshot = start_moderator_once () in
          runtime.moderator_snapshot <- snapshot;
          snapshot)
    ; enqueue_internal_event = enqueue_internal_event moderator
    ; drain_internal_events = drain_internal_events ~env ~session_id ~tools moderator
    ; execute_model_job =
        (match delegated with
         | false -> execute_model_job moderator session_id
         | true ->
           fun ~recipe:_ ~payload:_ ->
             Error
               (failure "generated moderators must use inherited tools for model work"))
    ; enqueue_model_job_completion = enqueue_model_job_completion moderator
    ; close = (fun () -> close_runtime cache storage_paths session_id moderator)
    }
  in
  Ok runtime
;;

let build ~sw ~env ~paths ~storage_paths ~revision =
  build_with_services
    ~sw
    ~env
    ~paths
    ~storage_paths
    ~source:(Authored revision)
    ~extension_services:None
;;

let build_with_extensions ~services ~sw ~env ~paths ~storage_paths ~revision =
  build_with_services
    ~sw
    ~env
    ~paths
    ~storage_paths
    ~source:(Authored revision)
    ~extension_services:(Some services)
;;

let build_generated
      ~services
      ~definition
      ~artifact_store
      ~parent_runtime
      ~inherited_managed
      ~authority
  =
  build_with_services
    ~source:
      (Generated
         { definition; artifact_store; parent_runtime; inherited_managed; authority })
    ~extension_services:(Some services)
;;
