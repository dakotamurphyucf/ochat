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
  ; moderator_script_tools : Script_tool_calls.t option
  ; start_moderator : unit -> (Jsonaf.t option, Agent_protocol.Error.t) result
  ; enqueue_internal_event : Jsonaf.t -> (Jsonaf.t option, Agent_protocol.Error.t) result
  ; drain_internal_events :
      History_entry.t list -> (moderator_drain, Agent_protocol.Error.t) result
  ; execute_model_job :
      recipe:string
      -> payload:Jsonaf.t
      -> (model_job_outcome, Agent_protocol.Error.t) result
  ; enqueue_model_job_completion :
      Agent_protocol.Job.t -> (Jsonaf.t option, Agent_protocol.Error.t) result
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
      ~sw
      ~ctx
      ~host
      ~elements
      ~manifest_authorizer
      ~approval_provider
      ~approval_store
      ~response_dir
  =
  Agent_runtime.create
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
  let%bind _, artifact =
    Manager.Registry.of_elements
      ~surface:Chatml.Chatml_builtin_surface.ui_moderator_surface
      Manager.Registry.empty
      elements
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
          Map.singleton
            (module String)
            Chat_response.Model_executor.agent_prompt_v1_name
            durable_recipe
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
        ; runtime_policy = Chat_response.Runtime_semantics.default_policy
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
      if !started
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

let decode_moderator_snapshot = function
  | None -> Ok None
  | Some (`Object fields) ->
    (match List.Assoc.find fields "identity_snapshot_sexp" ~equal:String.equal with
     | Some (`String encoded) ->
       (try
          Ok
            (Some
               ([%of_sexp: Session.Moderator_state.Identity_snapshot.t]
                  (Sexp.of_string encoded)))
        with
        | exn ->
          Error (failure ("moderator snapshot decode failed: " ^ Exn.to_string exn)))
     | _ -> Error (failure "moderator snapshot is missing identity state"))
  | Some _ -> Error (failure "moderator snapshot must be an object")
;;

let moderator_snapshot_has_queued_events snapshot =
  let open Result.Let_syntax in
  let%map snapshot = decode_moderator_snapshot snapshot in
  Option.value_map snapshot ~default:false ~f:(fun snapshot ->
    not
      (List.is_empty
         snapshot.Session.Moderator_state.Identity_snapshot.queued_internal_events))
;;

let moderator_snapshot_is_halted snapshot =
  let open Result.Let_syntax in
  let%map snapshot = decode_moderator_snapshot snapshot in
  Option.value_map snapshot ~default:false ~f:(fun snapshot ->
    snapshot.Session.Moderator_state.Identity_snapshot.halted)
;;

let moderator_snapshot_observer snapshot =
  let open Result.Let_syntax in
  let%map snapshot = decode_moderator_snapshot snapshot in
  Option.map snapshot ~f:(fun snapshot ->
    Agent_protocol.Invocation.
      { script_id = snapshot.Session.Moderator_state.Identity_snapshot.script_id
      ; source_sha256 = snapshot.script_source_hash
      })
;;

let enqueue_internal_value moderator value =
  match moderator with
  | None -> Error (failure "session prompt has no ChatML moderator")
  | Some ((moderator, _) as moderator_pair) ->
    let open Result.Let_syntax in
    let%bind () =
      Manager.enqueue_internal_event
        moderator.Chat_response.In_memory_stream.manager
        value
      |> Result.map_error ~f:failure
    in
    moderator_snapshot (Some moderator_pair)
;;

let enqueue_internal_event moderator payload =
  let open Result.Let_syntax in
  let%bind snapshot =
    Chatml.Chatml_value_codec.Snapshot.of_jsonaf payload |> Result.map_error ~f:failure
  in
  let%bind value =
    Chatml.Chatml_value_codec.Snapshot.to_value snapshot |> Result.map_error ~f:failure
  in
  enqueue_internal_value moderator value
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
  | Queued | Running | Waiting_permission _ -> Error (failure "model job is not terminal")
;;

let enqueue_model_job_completion moderator job =
  Result.bind (model_job_event job) ~f:(enqueue_internal_value moderator)
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

let build
      ~sw
      ~env
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
      ~model_post_stream
      ~review_permission
      ~schedule_services
      ~job_services
  =
  let open Result.Let_syntax in
  let elements = Prompt_revision.elements revision in
  let%bind () =
    Agent_store.Prompt_artifact_store.verify_tree
      ~root:(Prompt_revision.materialized_tree revision)
      (Prompt_revision.artifact revision)
    |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
  in
  let response_dir = response_dir storage_paths in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 response_dir;
  let cache = cache storage_paths in
  let ctx = context ~env paths cache in
  let%bind host = host ~env ~paths ~session_id ~elements in
  let%bind agent_runtime =
    create_agent_runtime
      ~sw
      ~ctx
      ~host
      ~elements
      ~manifest_authorizer
      ~approval_provider
      ~approval_store
      ~response_dir
  in
  let comp_tools, tool_tbl = Ochat_function.functions agent_runtime.functions in
  let tools = Chat_response.Tool.convert_tools comp_tools in
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
  let config, model, reasoning = model_config elements in
  let worker =
    Turn_worker.create
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
      ; delegated_permission_tools = agent_runtime.shell_tool_names
      ; redact_tool_payload =
          (fun ~name payload ->
            Option.value_map
              agent_runtime.shell_registry
              ~default:payload
              ~f:(fun registry ->
                Shell_runtime.Registry.redact_tool_input registry ~tool_name:name payload))
      }
  in
  let parse_user_content =
    parse_user_content ~ctx ~manifest_authorizer ~approval_provider ~response_dir paths
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
    ; moderator_script_tools = None
    ; start_moderator =
        (fun () ->
          let open Result.Let_syntax in
          let%map snapshot = start_moderator_once () in
          runtime.moderator_snapshot <- snapshot;
          snapshot)
    ; enqueue_internal_event = enqueue_internal_event moderator
    ; drain_internal_events = drain_internal_events ~env ~session_id ~tools moderator
    ; execute_model_job = execute_model_job moderator session_id
    ; enqueue_model_job_completion = enqueue_model_job_completion moderator
    ; close = (fun () -> close_runtime cache storage_paths session_id moderator)
    }
  in
  Ok runtime
;;
