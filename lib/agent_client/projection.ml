open Core

type t =
  { snapshot : Agent_protocol.Snapshot.t
  ; live_events : Agent_protocol.Event.Recoverable.t list
  ; operation_sequences : (Agent_protocol.Id.Operation.t, int64) Map.Poly.t
  ; terminal_operation : Agent_protocol.Operation.t option
  }

let install_snapshot snapshot =
  { snapshot
  ; live_events = []
  ; operation_sequences = Map.Poly.empty
  ; terminal_operation = None
  }
;;

let snapshot t = t.snapshot
let live_events t = t.live_events
let terminal_operation t = t.terminal_operation
let error code message = Agent_protocol.Error.create code ~message ~retryable:true ()
let expected_sequence t = Int64.(t.snapshot.latest_event_sequence + 1L)

let validate_event t event =
  if
    Agent_protocol.Id.Session.compare
      event.Agent_protocol.Event.Durable.session_id
      t.snapshot.session.id
    <> 0
  then Error (error Invalid_state "event belongs to another session")
  else if not Int64.(event.sequence = expected_sequence t)
  then Error (error Snapshot_required "durable event sequence is not contiguous")
  else if Int64.(event.revision < t.snapshot.revision)
  then Error (error Invalid_state "durable event revision regressed")
  else Ok ()
;;

let replace_by compare_id id value values ~id_of =
  value :: List.filter values ~f:(fun candidate -> compare_id (id_of candidate) id <> 0)
;;

let update_session
      (_snapshot : Agent_protocol.Snapshot.t)
      (event : Agent_protocol.Event.Durable.t)
      (session : Agent_protocol.Session.t)
  =
  { session with
    revision = event.Agent_protocol.Event.Durable.revision
  ; latest_event_sequence = event.sequence
  ; updated_at = event.timestamp
  }
;;

let apply_payload
      (snapshot : Agent_protocol.Snapshot.t)
      (event : Agent_protocol.Event.Durable.t)
  = function
  | Agent_protocol.Event.Durable.Payload.Session_created session | Session_updated session
    -> { snapshot with session }
  | Session_state_changed change ->
    let session =
      { snapshot.session with
        desired_state = change.desired_state
      ; observed_state = change.observed_state
      }
    in
    { snapshot with session }
  | Attachment_owner_changed _ -> snapshot
  | History_message_deferred entry ->
    { snapshot with deferred_entries = snapshot.deferred_entries @ [ entry ] }
  | History_appended entries ->
    { snapshot with
      canonical_history =
        { snapshot.canonical_history with
          entries = snapshot.canonical_history.entries @ entries
        }
    ; deferred_entries =
        List.filter snapshot.deferred_entries ~f:(fun deferred ->
          not
            (List.exists entries ~f:(fun entry ->
               History_entry.Id.equal deferred.id entry.id)))
    }
  | History_replaced canonical_history -> { snapshot with canonical_history }
  | Moderator_overlay_changed _ | Moderator_notification _ -> snapshot
  | Permission_requested permission | Permission_resolved permission ->
    { snapshot with
      permissions =
        replace_by
          Agent_protocol.Id.Permission.compare
          permission.id
          permission
          snapshot.permissions
          ~id_of:(fun value -> value.Agent_protocol.Permission.id)
    }
  | Grant_created grant | Grant_revoked grant ->
    { snapshot with
      grants =
        replace_by
          Agent_protocol.Id.Grant.compare
          grant.id
          grant
          snapshot.grants
          ~id_of:(fun value -> value.Agent_protocol.Grant.id)
    }
  | Operation_started operation ->
    { snapshot with
      session = { snapshot.session with active_operation = Some operation }
    }
  | Operation_completed _
  | Operation_failed _
  | Operation_cancelled _
  | Operation_interrupted _ ->
    { snapshot with
      session = { snapshot.session with active_operation = None }
    ; active_tool_calls = []
    ; active_agent_calls = []
    }
  | Job_state_changed job ->
    { snapshot with
      jobs =
        replace_by
          Agent_protocol.Id.Job.compare
          job.id
          job
          snapshot.jobs
          ~id_of:(fun value -> value.Agent_protocol.Job.id)
    }
  | Schedule_created schedule
  | Schedule_state_changed schedule
  | Schedule_cancelled schedule ->
    { snapshot with
      schedules =
        replace_by
          Agent_protocol.Id.Schedule.compare
          schedule.id
          schedule
          snapshot.schedules
          ~id_of:(fun value -> value.Agent_protocol.Schedule.id)
    }
  | Prompt_upgraded upgrade ->
    { snapshot with
      session = { snapshot.session with prompt_revision = Some upgrade.current_revision }
    }
  | Workspace_state_changed _ -> snapshot
  | Session_error failure -> { snapshot with failure = Some failure }
;;

let advance snapshot event =
  let session = update_session snapshot event snapshot.session in
  { snapshot with
    session
  ; revision = event.Agent_protocol.Event.Durable.revision
  ; latest_event_sequence = event.sequence
  }
;;

let terminal_operation_id = function
  | Agent_protocol.Event.Durable.Payload.Operation_completed operation
  | Operation_failed operation
  | Operation_cancelled operation
  | Operation_interrupted operation -> Some operation.Agent_protocol.Operation.id
  | Session_created _
  | Session_updated _
  | Session_state_changed _
  | Attachment_owner_changed _
  | History_message_deferred _
  | History_appended _
  | History_replaced _
  | Moderator_overlay_changed _
  | Moderator_notification _
  | Permission_requested _
  | Permission_resolved _
  | Grant_created _
  | Grant_revoked _
  | Operation_started _
  | Job_state_changed _
  | Schedule_created _
  | Schedule_state_changed _
  | Schedule_cancelled _
  | Prompt_upgraded _
  | Workspace_state_changed _
  | Session_error _ -> None
;;

let discard_terminal_live_events t payload =
  match terminal_operation_id payload with
  | None -> t.live_events, t.operation_sequences
  | Some operation_id ->
    ( List.filter t.live_events ~f:(fun event ->
        Agent_protocol.Id.Operation.compare event.operation_id operation_id <> 0)
    , Map.remove t.operation_sequences operation_id )
;;

let apply_moderator_projection (snapshot : Agent_protocol.Snapshot.t) = function
  | Some (Agent_protocol.Event.Durable.Payload.Moderator_overlay_changed json) ->
    let open Result.Let_syntax in
    let%bind fields = Agent_protocol.Json_codec.fields json in
    let%bind effective_history =
      Agent_protocol.Json_codec.optional_as
        fields
        "effective_history"
        Agent_protocol.History.Window.of_json
    in
    let%bind halted =
      Agent_protocol.Json_codec.required_as fields "halted" Agent_protocol.Json_codec.bool
    in
    let%map halt_reason =
      Agent_protocol.Json_codec.optional_as
        fields
        "halt_reason"
        Agent_protocol.Json_codec.string
    in
    { snapshot with effective_history; halted; halt_reason }
  | _ -> Ok snapshot
;;

let updated_terminal_operation t = function
  | Some
      ( Agent_protocol.Event.Durable.Payload.Operation_completed operation
      | Operation_failed operation
      | Operation_cancelled operation
      | Operation_interrupted operation ) -> Some operation
  | Some (Operation_started _) -> None
  | _ -> t.terminal_operation
;;

let apply_event t event =
  let open Result.Let_syntax in
  let%bind () = validate_event t event in
  let%bind replacement =
    if Agent_protocol.Event.Durable.equal_visibility event.visibility Hidden
    then Ok None
    else Agent_protocol.Event.Durable.replacement_snapshot event
  in
  let t = Option.value_map replacement ~default:t ~f:install_snapshot in
  let%bind snapshot, payload =
    match event.visibility with
    | Hidden -> Ok (t.snapshot, None)
    | Full | Redacted ->
      Agent_protocol.Event.Durable.Payload.of_json ~kind:event.kind event.payload
      |> Result.map ~f:(fun payload ->
        apply_payload t.snapshot event payload, Some payload)
  in
  let%bind snapshot = apply_moderator_projection snapshot payload in
  let live_events, operation_sequences =
    Option.value_map
      payload
      ~default:(t.live_events, t.operation_sequences)
      ~f:(discard_terminal_live_events t)
  in
  Ok
    { snapshot = advance snapshot event
    ; live_events
    ; operation_sequences
    ; terminal_operation = updated_terminal_operation t payload
    }
;;

let apply_live_event t (event : Agent_protocol.Event.Recoverable.t) =
  let session_matches =
    Agent_protocol.Id.Session.compare event.session_id t.snapshot.session.id = 0
  in
  let previous =
    Map.find t.operation_sequences event.operation_id |> Option.value ~default:0L
  in
  if not session_matches
  then Error (error Invalid_state "live event belongs to another session")
  else if Int64.(event.operation_sequence <= previous)
  then Error (error Invalid_state "live operation sequence did not advance")
  else (
    let snapshot =
      if Agent_protocol.Event.Recoverable.equal_kind event.kind Tool_finished
      then (
        let call_id payload =
          match payload with
          | `Object fields -> List.Assoc.find fields "call_id" ~equal:String.equal
          | _ -> None
        in
        let keep json =
          match Agent_protocol.Event.Recoverable.of_json json with
          | Error _ -> false
          | Ok started ->
            not
              (Agent_protocol.Id.Operation.compare started.operation_id event.operation_id
               = 0
               && Poly.equal (call_id started.payload) (call_id event.payload))
        in
        { t.snapshot with
          active_tool_calls = List.filter t.snapshot.active_tool_calls ~f:keep
        ; active_agent_calls = List.filter t.snapshot.active_agent_calls ~f:keep
        })
      else t.snapshot
    in
    Ok
      { t with
        snapshot
      ; live_events = t.live_events @ [ event ]
      ; operation_sequences =
          Map.set
            t.operation_sequences
            ~key:event.operation_id
            ~data:event.operation_sequence
      })
;;
