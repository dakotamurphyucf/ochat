open! Core

module Config = struct
  type t =
    { env : Eio_unix.Stdenv.base
    ; response_dir : Eio.Fs.dir_ty Eio.Path.t
    ; tools : Openai.Responses.Request.Tool.t list
    ; tool_tbl : (string, Ochat_function.runner) Hashtbl.t
    ; temperature : float option
    ; max_output_tokens : int option
    ; reasoning : Openai.Responses.Request.Reasoning.t option
    ; moderator : Chat_response.In_memory_stream.moderator option
    ; permission_profile : Permission_policy.t
    ; review_permission :
        Permission_policy.invocation
        -> (Permission_reviewer.Decision.t, Permission_reviewer.Error.t) result
    ; history_compaction : bool
    ; parallel_tool_calls : bool
    ; model : Openai.Responses.Request.model
    ; prompt_cache_key : string option
    ; prompt_cache_retention : string option
    ; post_stream : Chat_response.In_memory_stream.post_stream option
    ; agent_page_classifications :
        (string * Chat_response.Tool_execution_event.agent_page_kind) list
    ; delegated_permission_tools : String.Set.t
    ; redact_tool_payload : name:string -> string -> string
    }
end

exception Worker_failure of Agent_protocol.Error.t

let now config =
  Eio.Time.now (Eio.Stdenv.clock config.Config.env)
  |> Time_ns.Span.of_sec
  |> Time_ns.of_span_since_epoch
  |> Agent_protocol.Timestamp.of_time_ns
;;

let sourced_payload event =
  Chat_response.Sourced_response_event.(
    `Object
      [ ( "entry_id"
        , Option.value_map event.entry_id ~default:`Null ~f:(fun id ->
            `String (History_entry.Id.to_string id)) )
      ; ( "invocation_id"
        , Option.value_map event.invocation_id ~default:`Null ~f:(fun value ->
            `String value) )
      ; ( "parent_call_id"
        , Option.value_map event.parent_call_id ~default:`Null ~f:(fun value ->
            `String value) )
      ; "event", Openai.Responses.Response_stream.jsonaf_of_t event.event
      ])
;;

let history_event_payload event =
  Chat_response.History_stream_event.(
    `Object
      [ "entry_id", `String (History_entry.Id.to_string event.entry_id)
      ; ( "source"
        , Option.value_map event.source ~default:`Null ~f:(fun value -> `String value) )
      ; "event", Openai.Responses.Response_stream.jsonaf_of_t event.event
      ])
;;

let tool_event_kind = function
  | Chat_response.Tool_execution_event.Started _ ->
    Agent_protocol.Event.Recoverable.Tool_started
  | Progress _ -> Tool_progress
  | Trace _ -> Tool_trace
  | Finished _ -> Tool_finished
;;

let progress_payload progress =
  let channel =
    match progress.Ochat_function.Progress.channel with
    | `Assistant -> "assistant"
    | `Reasoning -> "reasoning"
    | `Stdout -> "stdout"
    | `Stderr -> "stderr"
    | `Activity -> "activity"
  in
  let update_kind, text =
    match progress.update with
    | Append text -> "append", text
    | Replace text -> "replace", text
  in
  `Object
    [ "channel", `String channel; "update", `String update_kind; "text", `String text ]
;;

let outcome_text = function
  | Ochat_function.Trace.Returned -> "returned"
  | Raised -> "raised"
  | Cancelled -> "cancelled"
;;

let optional_output output =
  Option.value_map
    output
    ~default:`Null
    ~f:Openai.Responses.Tool_output.Output.jsonaf_of_t
;;

let execution_kind_name = function
  | `Function -> "function"
  | `Custom -> "custom"
;;

let trace_payload = function
  | Ochat_function.Trace.Tool_started { call_id; name; kind; payload } ->
    `Object
      [ "type", `String "tool_started"
      ; "call_id", `String call_id
      ; "name", `String name
      ; "kind", `String (execution_kind_name kind)
      ; "payload", `String payload
      ]
  | Tool_progress { call_id; progress } ->
    `Object
      [ "type", `String "tool_progress"
      ; "call_id", `String call_id
      ; "progress", progress_payload progress
      ]
  | Tool_finished { call_id; outcome; output } ->
    `Object
      [ "type", `String "tool_finished"
      ; "call_id", `String call_id
      ; "outcome", `String (outcome_text outcome)
      ; "output", optional_output output
      ]
;;

let agent_page_kind classifications name =
  List.Assoc.find classifications name ~equal:String.equal
  |> Option.map ~f:(function
    | Chat_response.Tool_execution_event.Subagent -> `String "subagent"
    | Shell_script -> `String "shell_script")
  |> Option.value ~default:`Null
;;

let tool_event_payload_unredacted classifications event =
  match event with
  | Chat_response.Tool_execution_event.Started { call_id; name; kind; payload } ->
    `Object
      [ "call_id", `String call_id
      ; "name", `String name
      ; "kind", `String (execution_kind_name kind)
      ; "payload", `String payload
      ; "agent_page_kind", agent_page_kind classifications name
      ]
  | Progress { call_id; progress } ->
    `Object [ "call_id", `String call_id; "progress", progress_payload progress ]
  | Finished { call_id; outcome; output } ->
    `Object
      [ "call_id", `String call_id
      ; "outcome", `String (outcome_text outcome)
      ; "output", optional_output output
      ]
  | Trace { call_id; trace } ->
    `Object [ "call_id", `String call_id; "trace", trace_payload trace ]
;;

let redacted_tool_event config = function
  | Chat_response.Tool_execution_event.Started event ->
    Chat_response.Tool_execution_event.Started
      { event with
        payload = config.Config.redact_tool_payload ~name:event.name event.payload
      }
  | Trace { call_id; trace = Ochat_function.Trace.Tool_started event } ->
    let trace =
      Ochat_function.Trace.Tool_started
        { event with
          payload = config.Config.redact_tool_payload ~name:event.name event.payload
        }
    in
    Trace { call_id; trace }
  | event -> event
;;

let tool_event_payload config event =
  tool_event_payload_unredacted
    config.Config.agent_page_classifications
    (redacted_tool_event config event)
;;

let require_ok = function
  | Ok value -> value
  | Error failure -> raise (Worker_failure failure)
;;

let safe_point_input capabilities =
  Chat_response.In_memory_stream.Safe_point_input.
    { consume_entries =
        (fun () ->
          capabilities.Operation_worker.Capabilities.consume_deferred ()
          |> require_ok
          |> user_entries)
    ; consume_compatibility_text = (fun () -> None)
    }
;;

let allocator capabilities =
  History_entry.Allocator.create
    ~namespace:
      (History_entry.Id_source.namespace
         capabilities.Operation_worker.Capabilities.id_source)
    ~next_sequence:0
  |> Result.ok_or_failwith
;;

let tool_kind_name = function
  | Chat_response.Tool_call.Kind.Function -> "function"
  | Custom -> "custom"
;;

let invocation ~kind ~name ~payload =
  let identity_digest =
    String.concat ~sep:"\000" [ tool_kind_name kind; name; payload ]
    |> Digestif.SHA256.digest_string
    |> Digestif.SHA256.to_hex
  in
  Permission_policy.
    { tool_name = name
    ; identity_digest
    ; invocation_display = name ^ "(<redacted>)"
    ; effects = [ "tool_invocation" ]
    }
;;

let permission_timeout profile =
  Option.map profile.Permission_policy.approval_timeout_ms ~f:(fun milliseconds ->
    Float.of_int milliseconds /. 1_000.)
;;

let permission_expiry config profile =
  Option.map profile.Permission_policy.approval_timeout_ms ~f:(fun milliseconds ->
    now config
    |> Agent_protocol.Timestamp.to_time_ns
    |> Fn.flip Time_ns.add (Time_ns.Span.of_ms (Float.of_int milliseconds))
    |> Agent_protocol.Timestamp.of_time_ns)
;;

let permission_request config input ~call_id invocation =
  Agent_protocol.Permission.
    { id = Agent_protocol.Id.Permission.create ()
    ; session_id = input.Operation_worker.Input.session_id
    ; generation = input.session_generation
    ; owner = Operation input.operation.id
    ; call_id
    ; tool_name = invocation.Permission_policy.tool_name
    ; runtime_identity = Some invocation.identity_digest
    ; invocation_display = invocation.invocation_display
    ; rationale = None
    ; effects = invocation.effects
    ; choices = [ Approve_once; Approve_session; Approve_prefix; Durable_exact; Deny ]
    ; created_at = now config
    ; expires_at = permission_expiry config config.Config.permission_profile
    ; state = Pending
    ; resolution = None
    }
;;

let fallback_choice profile =
  match profile.Permission_policy.fallback with
  | Fallback_allow -> Agent_protocol.Permission.Approve_once
  | Fallback_deny | Fallback_allow_if_policy | Fallback_reviewer _ -> Deny
;;

let timeout_reviewer config invocation =
  match config.Config.permission_profile.fallback with
  | Fallback_reviewer _ -> Some (fun () -> config.review_permission invocation)
  | Fallback_allow | Fallback_deny | Fallback_allow_if_policy -> None
;;

let deny_tool message =
  raise
    (Worker_failure
       (Agent_protocol.Error.create Permission_denied ~message ~retryable:false ()))
;;

let authorize_tool config input capabilities ~kind ~name ~payload ~call_id =
  if Set.mem config.Config.delegated_permission_tools name
  then ()
  else (
    let invocation = invocation ~kind ~name ~payload in
    if
      capabilities.Operation_worker.Capabilities.invocation_granted
        ~tool_name:invocation.tool_name
        ~identity_digest:invocation.identity_digest
    then ()
    else (
      match
        Permission_policy.decide
          config.Config.permission_profile
          ~responder_available:
            (capabilities.Operation_worker.Capabilities.responder_available ())
          invocation
      with
      | Allow_now -> ()
      | Deny_now reason -> deny_tool reason
      | Request_permission ->
        let permission = permission_request config input ~call_id invocation in
        let resolution =
          capabilities.request_permission
            ~permission
            ~timeout_seconds:(permission_timeout config.permission_profile)
            ~fallback:(fallback_choice config.permission_profile)
            ~review_on_timeout:(timeout_reviewer config invocation)
          |> require_ok
        in
        (match resolution.choice with
         | Deny -> deny_tool "tool invocation was denied"
         | Approve_once | Approve_session | Approve_prefix | Durable_exact -> ())
      | Request_review ->
        let permission =
          { (permission_request config input ~call_id invocation) with
            choices = [ Approve_once; Deny ]
          }
        in
        let resolution =
          capabilities.request_review ~permission ~review:(fun () ->
            config.review_permission invocation)
          |> require_ok
        in
        (match resolution.choice with
         | Approve_once -> ()
         | Deny -> deny_tool "tool invocation was denied by the configured reviewer"
         | Approve_session | Approve_prefix | Durable_exact ->
           deny_tool "permission reviewer returned an invalid grant scope")))
;;

let moderator_snapshot = function
  | None -> None
  | Some moderator ->
    (match
       Chat_response.Moderator_manager.identity_snapshot
         moderator.Chat_response.In_memory_stream.manager
     with
     | Error message ->
       raise
         (Worker_failure
            (Agent_protocol.Error.create Invalid_state ~message ~retryable:false ()))
     | Ok snapshot ->
       Some
         (`Object
             [ ( "identity_snapshot_sexp"
               , `String
                   (Sexp.to_string_mach
                      ([%sexp_of: Session.Moderator_state.Identity_snapshot.t] snapshot))
               )
             ]))
;;

let moderate_submission config input on_runtime_request =
  match input.Operation_worker.Input.operation.kind with
  | Turn User_submit ->
    Chat_response.In_memory_stream.handle_item_appended_entries
      ~moderator:config.Config.moderator
      ~on_runtime_request
      ~available_tools:config.tools
      ~now_ms:(Eio.Time.now (Eio.Stdenv.clock config.env) *. 1_000. |> Float.to_int)
      ~history:input.history
    |> Result.map_error ~f:(fun message ->
      Agent_protocol.Error.create Invalid_state ~message ~retryable:false ())
    |> require_ok
  | Turn (Moderator_request | Idle_followup | Recovery_retry | Administrative)
  | Compaction -> ()
;;

let run ?dispatch_tool ?moderator_events config ~sw ~input capabilities =
  let config =
    match config.Config.moderator, moderator_events with
    | Some moderator, Some make ->
      let handlers = make ~input ~capabilities |> require_ok in
      { config with moderator = Some { moderator with event_handlers = Some handlers } }
    | _, None -> config
    | None, Some _ ->
      raise
        (Worker_failure
           (Agent_protocol.Error.create
              Invalid_state
              ~message:"owned event routing requires a moderator"
              ~retryable:false
              ()))
  in
  let runtime_requests = ref [] in
  moderate_submission config input (fun request ->
    runtime_requests := request :: !runtime_requests);
  let publish_live = capabilities.Operation_worker.Capabilities.publish_live in
  let checkpoint_moderator () =
    capabilities.commit_moderator (moderator_snapshot config.moderator) |> require_ok
  in
  checkpoint_moderator ();
  let final_history =
    if
      Option.is_some
        (Chat_response.Runtime_semantics.should_end_session !runtime_requests)
    then input.Operation_worker.Input.history
    else
      Chat_response.In_memory_stream.run_completion_stream_in_memory_entries
        ~env:config.Config.env
        ~datadir:config.response_dir
        ~allocator:(allocator capabilities)
        ~id_source:capabilities.id_source
        ~history:input.Operation_worker.Input.history
        ~tools:(Some config.tools)
        ~tool_tbl:config.tool_tbl
        ?temperature:config.temperature
        ?max_output_tokens:config.max_output_tokens
        ?reasoning:config.reasoning
        ?moderator:config.moderator
        ~on_sourced_event:(fun event ->
          checkpoint_moderator ();
          publish_live ~kind:Sourced_stream ~payload:(sourced_payload event))
        ~on_history_event:(fun event ->
          publish_live
            ~kind:History_correlated_stream
            ~payload:(history_event_payload event))
        ~on_history_item_appended:(fun entry ->
          capabilities.commit_entry entry |> require_ok)
        ~on_tool_execution:(fun event ->
          checkpoint_moderator ();
          publish_live
            ~kind:(tool_event_kind event)
            ~payload:(tool_event_payload config event))
        ~authorize_tool:(authorize_tool config input capabilities)
        ?dispatch_tool:
          (Option.map dispatch_tool ~f:(fun make -> make ~input ~capabilities))
        ~redact_tool_payload:config.redact_tool_payload
        ~on_runtime_request:(fun request ->
          runtime_requests := request :: !runtime_requests)
        ~history_compaction:config.history_compaction
        ~parallel_tool_calls:config.parallel_tool_calls
        ~safe_point_input:(safe_point_input capabilities)
        ~model:config.model
        ?prompt_cache_key:config.prompt_cache_key
        ?prompt_cache_retention:config.prompt_cache_retention
        ?post_stream:config.post_stream
        ~sw
        ()
  in
  Operation_worker.Completed
    { final_history
    ; runtime_requests = List.rev !runtime_requests
    ; moderator_snapshot = moderator_snapshot config.moderator
    }
;;

let create ?dispatch_tool ?moderator_events config =
  Operation_worker.create ~run:(fun ~sw ~input capabilities ->
    match run ?dispatch_tool ?moderator_events config ~sw ~input capabilities with
    | outcome -> outcome
    | exception Worker_failure failure -> Operation_worker.Failed failure
    | exception Moderator_tool_dispatch.Dispatch_error failure ->
      Operation_worker.Failed failure
    | exception Chat_response.In_memory_stream.Post_tool_moderation_failed (entry, _) ->
      Operation_worker.Failed
        (Agent_protocol.Error.create
           Invalid_state
           ~message:"Post-tool moderation failed after the initial result was committed."
           ~retryable:false
           ~data:
             (`Object
                 [ "phase", `String "post_tool_response"
                 ; ( "output_entry_id"
                   , Agent_protocol.History.Id.to_json (History_entry.id entry) )
                 ])
           ()))
;;
