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

let tool_activity config ~(scope : Transcript.Scope.t) event =
  let open Result.Let_syntax in
  let module A = Agent_protocol.Activity in
  let parent =
    match scope.relation with
    | Root -> None
    | Nested parent ->
      Option.map parent.call_alias ~f:(fun call_alias ->
        A.Key.{ scope = parent.scope; call_alias })
  in
  let key ~parent call_alias = A.Key.create ~scope:scope.key ~call_alias ~parent in
  let progress (p : Ochat_function.Progress.t) =
    A.Progress.
      { channel =
          (match p.channel with
           | `Assistant -> Assistant
           | `Reasoning -> Reasoning
           | `Stdout -> Stdout
           | `Stderr -> Stderr
           | `Activity -> Activity)
      ; update =
          (match p.update with
           | Append s -> Append s
           | Replace s -> Replace s)
      }
  in
  let outcome = function
    | Ochat_function.Trace.Returned -> A.Tool.Returned
    | Raised -> A.Tool.Raised
    | Cancelled -> A.Tool.Cancelled
  in
  let started ~parent ~call_id ~name ~kind ~payload =
    let%bind key = key ~parent call_id in
    let classification =
      List.Assoc.find config.Config.agent_page_classifications name ~equal:String.equal
      |> Option.map ~f:(function
        | Chat_response.Tool_execution_event.Subagent -> A.Tool.Subagent
        | Shell_script -> A.Tool.Shell_script)
    in
    let%map descriptor =
      A.Tool.descriptor
        key
        ~call_entry_id:None
        ~name
        ~kind:
          (match kind with
           | `Function -> History_entry.Payload.Call_kind.Function
           | `Custom -> Custom)
        ~input:(config.redact_tool_payload ~name payload)
        ~classification
    in
    A.Tool.Started descriptor
  in
  let progressed ~parent ~call_id value =
    let%map key = key ~parent call_id in
    A.Tool.Progress { key; progress = progress value }
  in
  let finished ~parent ~call_id result output =
    let%map key = key ~parent call_id in
    (A.Tool.Finished
       { key
       ; outcome = outcome result
       ; output = Option.map output ~f:Chat_response.Tool_execution_event.neutral_output
       }
     : A.Tool.event)
  in
  match event with
  | Chat_response.Tool_execution_event.Started { call_id; name; kind; payload } ->
    started ~parent ~call_id ~name ~kind ~payload
  | Progress { call_id; progress } -> progressed ~parent ~call_id progress
  | Finished { call_id; outcome; output } -> finished ~parent ~call_id outcome output
  | Trace { call_id = parent; trace } ->
    let parent = Some A.Key.{ scope = scope.key; call_alias = parent } in
    (match trace with
     | Ochat_function.Trace.Tool_started { call_id; name; kind; payload } ->
       started ~parent ~call_id ~name ~kind ~payload
     | Tool_progress { call_id; progress } -> progressed ~parent ~call_id progress
     | Tool_finished { call_id; outcome; output } ->
       finished ~parent ~call_id outcome output)
;;

let require_ok = function
  | Ok value -> value
  | Error failure -> raise (Worker_failure failure)
;;

let safe_point_input ?notification_input capabilities =
  Chat_response.In_memory_stream.Safe_point_input.
    { consume_entries =
        (fun () ->
          capabilities.Operation_worker.Capabilities.consume_deferred ()
          |> require_ok
          |> user_entries
          |> fun users ->
          match notification_input with
          | None -> users
          | Some consume -> append users (consume () |> require_ok))
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
     | Ok snapshot -> Some (Moderator_checkpoint.encode snapshot))
;;

let moderate_appended_history config history on_runtime_request =
  Chat_response.In_memory_stream.handle_item_appended_entries
    ~moderator:config.Config.moderator
    ~on_runtime_request
    ~available_tools:config.tools
    ~now_ms:(Eio.Time.now (Eio.Stdenv.clock config.env) *. 1_000. |> Float.to_int)
    ~history
  |> Result.map_error ~f:(fun message ->
    Agent_protocol.Error.create Invalid_state ~message ~retryable:false ())
  |> require_ok
;;

let moderate_submission config input on_runtime_request =
  match input.Operation_worker.Input.operation.kind with
  | Turn User_submit -> moderate_appended_history config input.history on_runtime_request
  | Turn (Moderator_request | Idle_followup | Recovery_retry | Administrative)
  | Compaction -> ()
;;

let run
      ?runtime_policy
      ?authoring_context
      ?dispatch_tool
      ?moderator_events
      ?notification_input
      ?initial_notification_input
      config
      ~sw
      ~input
      capabilities
  =
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
  let ended () =
    Option.is_some (Chat_response.Runtime_semantics.should_end_session !runtime_requests)
  in
  let initial_entries =
    match ended (), initial_notification_input with
    | true, _ | _, None -> []
    | false, Some prepare ->
      (prepare ~input () |> require_ok)
        .Chat_response.In_memory_stream.Safe_point_input.entries
  in
  let history = input.Operation_worker.Input.history @ initial_entries in
  let rec notify seen = function
    | [] -> ()
    | _ when ended () -> ()
    | entry :: remaining ->
      let seen = seen @ [ entry ] in
      moderate_appended_history config seen (fun request ->
        runtime_requests := request :: !runtime_requests);
      notify seen remaining
  in
  notify input.history initial_entries;
  (match initial_entries with
   | [] -> ()
   | _ -> checkpoint_moderator ());
  let final_history =
    let prepare_model_input =
      Option.map authoring_context ~f:(fun make ->
        let materialization = make ~input |> require_ok in
        fun ~history ~effective ->
          checkpoint_moderator ();
          capabilities.prepare_authoring_input materialization ~history ~effective
          |> require_ok)
    in
    if
      Option.is_some
        (Chat_response.Runtime_semantics.should_end_session !runtime_requests)
    then history
    else
      Chat_response.In_memory_stream.run_completion_stream_in_memory_entries
        ~env:config.Config.env
        ~datadir:config.response_dir
        ~allocator:(allocator capabilities)
        ~id_source:capabilities.id_source
        ~history
        ~tools:(Some config.tools)
        ~tool_tbl:config.tool_tbl
        ?temperature:config.temperature
        ?max_output_tokens:config.max_output_tokens
        ?reasoning:config.reasoning
        ?moderator:config.moderator
        ?runtime_policy
        ~before_model_call:(fun () ->
          capabilities.admit_notification_turn () |> require_ok)
        ?prepare_model_input
          (* Transient observations can arrive while an invocation owns the
           moderator checkpoint. Persistence belongs to the explicit owner and
           turn boundaries; these callbacks only publish admitted live data. *)
        ~on_transcript_event:(fun event ->
          publish_live (Agent_protocol.Event.Recoverable.Transcript event))
        ~on_history_item_appended:(fun entry ->
          capabilities.commit_entry entry |> require_ok)
        ~on_scoped_tool_execution:(fun ~scope event ->
          let event = tool_activity config ~scope event |> require_ok in
          publish_live (Agent_protocol.Event.Recoverable.Tool_activity event))
        ~authorize_tool:(authorize_tool config input capabilities)
        ?dispatch_tool:
          (Option.map dispatch_tool ~f:(fun make -> make ~input ~capabilities))
        ~redact_tool_payload:config.redact_tool_payload
        ~on_runtime_request:(fun request ->
          runtime_requests := request :: !runtime_requests)
        ~history_compaction:config.history_compaction
        ~parallel_tool_calls:config.parallel_tool_calls
        ~safe_point_input:
          (safe_point_input
             ?notification_input:
               (Option.map notification_input ~f:(fun make -> make ~input))
             capabilities)
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

let create
      ?runtime_policy
      ?authoring_context
      ?dispatch_tool
      ?moderator_events
      ?notification_input
      ?initial_notification_input
      config
  =
  Operation_worker.create ~run:(fun ~sw ~input capabilities ->
    match
      run
        ?runtime_policy
        ?authoring_context
        ?dispatch_tool
        ?moderator_events
        ?notification_input
        ?initial_notification_input
        config
        ~sw
        ~input
        capabilities
    with
    | outcome -> outcome
    | exception Worker_failure failure -> Operation_worker.Failed failure
    | exception
        ( Moderator_tool_dispatch.Dispatch_error failure
        | Native_tool_dispatch.Dispatch_error failure
        | Standalone_tool_dispatch.Dispatch_error failure ) ->
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
