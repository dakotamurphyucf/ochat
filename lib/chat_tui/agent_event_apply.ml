open! Core

type t =
  { mutable revision : int64 option
  ; mutable event_sequence : int64 option
  ; mutable applied_live_events : int
  ; mutable tool_sequences : (Agent_protocol.Id.Operation.t, int64) Map.Poly.t
  ; mutable agent_operation : Agent_protocol.Id.Operation.t option
  }

let create () =
  { revision = None
  ; event_sequence = None
  ; applied_live_events = 0
  ; tool_sequences = Map.Poly.empty
  ; agent_operation = None
  }
;;

let invalid message =
  Agent_protocol.Error.create Invalid_state ~message ~retryable:false ()
;;

let optional_string fields name =
  match List.Assoc.find fields name ~equal:String.equal with
  | None | Some `Null -> Ok None
  | Some (`String value) -> Ok (Some value)
  | Some _ -> Error (invalid (Printf.sprintf "live-event field %S must be a string" name))
;;

let entry_id fields =
  let open Result.Let_syntax in
  let%bind encoded = optional_string fields "entry_id" in
  match encoded with
  | None -> Ok None
  | Some value ->
    History_entry.Id.of_string value
    |> Result.map ~f:Option.some
    |> Result.map_error ~f:invalid
;;

let stream_event fields =
  match List.Assoc.find fields "event" ~equal:String.equal with
  | None -> Error (invalid "live stream event payload is missing event")
  | Some json ->
    Result.try_with (fun () -> Openai.Responses.Response_stream.t_of_jsonaf json)
    |> Result.map_error ~f:(fun exn ->
      invalid ("invalid live stream event: " ^ Exn.to_string exn))
;;

let stream_patches model event =
  match event.Agent_protocol.Event.Recoverable.payload with
  | `Object fields ->
    let open Result.Let_syntax in
    let%bind entry_id = entry_id fields in
    let%bind parent_call_id = optional_string fields "parent_call_id" in
    let%map stream_event = stream_event fields in
    let committed =
      Option.exists entry_id ~f:(fun id ->
        List.exists (Model.history_items model) ~f:(fun entry ->
          History_entry.Id.equal (History_entry.id entry) id))
    in
    if committed
    then []
    else Stream.handle_event ~model ?entry_id ~parent_call_id stream_event
  | _ -> Error (invalid "live stream payload must be an object")
;;

let required_string fields name =
  match List.Assoc.find fields name ~equal:String.equal with
  | Some (`String value) -> Ok value
  | None -> Error (invalid (Printf.sprintf "live-event field %S is missing" name))
  | Some _ -> Error (invalid (Printf.sprintf "live-event field %S must be a string" name))
;;

let tool_kind fields =
  Result.bind (required_string fields "kind") ~f:(function
    | "function" -> Ok `Function
    | "custom" -> Ok `Custom
    | value -> Error (invalid ("unknown tool kind: " ^ value)))
;;

let agent_page_kind fields =
  match List.Assoc.find fields "agent_page_kind" ~equal:String.equal with
  | None | Some `Null -> Ok None
  | Some (`String "subagent") -> Ok (Some Chat_response.Tool_execution_event.Subagent)
  | Some (`String "shell_script") -> Ok (Some Shell_script)
  | Some (`String value) -> Error (invalid ("unknown Agent-page kind: " ^ value))
  | Some _ -> Error (invalid "Agent-page kind must be a string")
;;

let apply_tool_started model fields =
  let open Result.Let_syntax in
  let%bind call_id = required_string fields "call_id" in
  let%bind name = required_string fields "name" in
  let%bind kind = tool_kind fields in
  let%bind payload = required_string fields "payload" in
  let%map agent_page_kind = agent_page_kind fields in
  Option.iter agent_page_kind ~f:(fun agent_page_kind ->
    ignore
      (Model.agent_call_started model ~call_id ~name ~kind ~payload ~agent_page_kind
       : bool));
  Model.set_activity model (Some (Model.Assistant Model.Working))
;;

let progress_channel = function
  | "assistant" -> Ok `Assistant
  | "reasoning" -> Ok `Reasoning
  | "stdout" -> Ok `Stdout
  | "stderr" -> Ok `Stderr
  | "activity" -> Ok `Activity
  | value -> Error (invalid ("unknown tool progress channel: " ^ value))
;;

let progress_update update text =
  match update with
  | "append" -> Ok (Ochat_function.Progress.Append text)
  | "replace" -> Ok (Replace text)
  | value -> Error (invalid ("unknown tool progress update: " ^ value))
;;

let progress_of_json = function
  | `Object fields ->
    let open Result.Let_syntax in
    let%bind channel = required_string fields "channel" >>= progress_channel in
    let%bind update = required_string fields "update" in
    let%bind text = required_string fields "text" in
    let%map update = progress_update update text in
    Ochat_function.Progress.{ channel; update }
  | _ -> Error (invalid "tool progress must be an object")
;;

let apply_tool_progress model fields =
  let open Result.Let_syntax in
  let%bind call_id = required_string fields "call_id" in
  let%bind encoded =
    List.Assoc.find fields "progress" ~equal:String.equal
    |> Result.of_option ~error:(invalid "tool progress is missing")
  in
  let%map progress = progress_of_json encoded in
  ignore (Model.agent_call_progress model ~call_id progress : bool)
;;

let tool_outcome = function
  | "returned" -> Ok Ochat_function.Trace.Returned
  | "raised" -> Ok Raised
  | "cancelled" -> Ok Cancelled
  | value -> Error (invalid ("unknown tool outcome: " ^ value))
;;

let tool_output fields =
  match List.Assoc.find fields "output" ~equal:String.equal with
  | None | Some `Null -> Ok None
  | Some json ->
    Result.try_with (fun () -> Openai.Responses.Tool_output.Output.t_of_jsonaf json)
    |> Result.map ~f:Option.some
    |> Result.map_error ~f:(fun exn ->
      invalid ("invalid tool output: " ^ Exn.to_string exn))
;;

let apply_tool_finished model fields =
  let open Result.Let_syntax in
  let%bind call_id = required_string fields "call_id" in
  let%bind outcome = required_string fields "outcome" >>= tool_outcome in
  let%map output = tool_output fields in
  ignore (Model.agent_call_finished model ~call_id ~outcome ~output : bool);
  let running =
    Model.active_agent_calls model
    |> List.exists ~f:(fun call -> Option.is_none (Model.agent_call_outcome call))
  in
  Model.set_activity
    model
    (Some (Model.Assistant (if running then Working else Thinking)))
;;

let apply_tool_event model event apply =
  match event.Agent_protocol.Event.Recoverable.payload with
  | `Object fields -> apply model fields
  | _ -> Error (invalid "tool live-event payload must be an object")
;;

let apply_live_event model event =
  match event.Agent_protocol.Event.Recoverable.kind with
  | Sourced_stream ->
    Result.map (stream_patches model event) ~f:(fun patches ->
      ignore (Model.apply_patches model patches : Model.t))
  | Tool_started -> apply_tool_event model event apply_tool_started
  | Tool_progress -> apply_tool_event model event apply_tool_progress
  | Tool_finished -> apply_tool_event model event apply_tool_finished
  | Provider_stream
  | History_correlated_stream
  | Tool_trace
  | Agent_call_classified
  | Agent_call_progress
  | Activity
  | Compaction_progress -> Ok ()
;;

let replace_history model ~viewport_height projection =
  let history = Agent_projection.visible_history projection in
  let rows = Conversation.project_entries history |> Conversation.rows in
  Model.set_history_items model (Agent_projection.canonical_history projection);
  Model.rebuild_tool_output_index_for_items model history;
  Model.reconcile_projected_messages_with_damage
    model
    ~viewport_height
    ~rows
    ~messages:(Agent_projection.messages projection)
;;

let apply_once t model event =
  match event.Agent_protocol.Event.Recoverable.kind with
  | Tool_started | Tool_progress | Tool_finished ->
    let previous =
      Map.find t.tool_sequences event.operation_id |> Option.value ~default:0L
    in
    if Int64.(event.operation_sequence <= previous)
    then Ok ()
    else
      Result.map (apply_live_event model event) ~f:(fun () ->
        t.tool_sequences
        <- Map.set t.tool_sequences ~key:event.operation_id ~data:event.operation_sequence)
  | _ -> apply_live_event model event
;;

let apply_live_events t model events =
  t.tool_sequences
  <- Map.filter_keys t.tool_sequences ~f:(fun id ->
       List.exists events ~f:(fun event ->
         Agent_protocol.Id.Operation.compare
           id
           event.Agent_protocol.Event.Recoverable.operation_id
         = 0));
  let applied = Int.min t.applied_live_events (List.length events) in
  let pending = List.drop events applied in
  Result.map
    (Result.all_unit (List.map pending ~f:(apply_once t model)))
    ~f:(fun () -> t.applied_live_events <- List.length events)
;;

let replace_if_changed t model ~viewport_height projection =
  let revision = (Agent_projection.snapshot projection).revision in
  let sequence = (Agent_projection.snapshot projection).latest_event_sequence in
  if
    Option.exists t.revision ~f:(Int64.equal revision)
    && Option.exists t.event_sequence ~f:(Int64.equal sequence)
  then Model.No_damage
  else (
    t.revision <- Some revision;
    t.event_sequence <- Some sequence;
    t.applied_live_events <- 0;
    replace_history model ~viewport_height projection)
;;

let canonical_tool_output projection call_id =
  List.find_map (Agent_projection.canonical_history projection) ~f:(fun entry ->
    match History_entry.item entry with
    | Openai.Responses.Item.Function_call_output output
      when String.equal output.call_id call_id -> Some output.output
    | Custom_tool_call_output output when String.equal output.call_id call_id ->
      Some output.output
    | _ -> None)
;;

let sync_agent_operation t model projection =
  match (Agent_projection.snapshot projection).session.active_operation with
  | Some operation
    when not
           (Option.exists t.agent_operation ~f:(fun id ->
              Agent_protocol.Id.Operation.compare id operation.id = 0)) ->
    Model.clear_agent_calls model;
    t.tool_sequences <- Map.Poly.empty;
    t.applied_live_events <- 0;
    t.agent_operation <- Some operation.id
  | None
    when Option.is_some t.agent_operation
         && Option.is_none (Agent_projection.terminal_operation projection) ->
    Model.clear_agent_calls model;
    t.tool_sequences <- Map.Poly.empty;
    t.applied_live_events <- 0;
    t.agent_operation <- None
  | _ -> ()
;;

let finish_agent_operation model projection =
  Option.iter (Agent_projection.terminal_operation projection) ~f:(fun operation ->
    let outcome =
      match operation.state with
      | Agent_protocol.Operation.Completed -> Ochat_function.Trace.Returned
      | Failed _ -> Raised
      | Cancelled | Interrupted _ -> Cancelled
      | Starting | Running | Cancelling -> Raised
    in
    List.iter (Model.active_agent_calls model) ~f:(fun call ->
      let call_id = Model.agent_call_id call in
      let output = canonical_tool_output projection call_id in
      ignore (Model.agent_call_finished model ~call_id ~outcome ~output : bool)))
;;

let apply t ~model ~viewport_height projection =
  sync_agent_operation t model projection;
  let damage = replace_if_changed t model ~viewport_height projection in
  let open Result.Let_syntax in
  let%bind starts =
    Result.all
      (List.map
         (Agent_projection.snapshot projection).active_tool_calls
         ~f:Agent_protocol.Event.Recoverable.of_json)
  in
  let%bind () = Result.all_unit (List.map starts ~f:(apply_once t model)) in
  Result.map
    (apply_live_events t model (Agent_projection.live_events projection))
    ~f:(fun () ->
      finish_agent_operation model projection;
      damage)
;;
