open! Core
module P = Agent_protocol
module Public = P.Public

type synchronization =
  | Current
  | Snapshot_required of P.Error.t

type t =
  { snapshot : Public.Snapshot.t
  ; live : Live_projection.t
  ; terminal_operation : P.Operation.t option
  ; synchronization : synchronization
  }

let install_snapshot ?live_limits snapshot =
  let live = Live_projection.empty ?limits:live_limits () in
  let fields = Public.Snapshot.fields snapshot in
  let admitted =
    match fields.session.active_operation with
    | None -> Ok live
    | Some operation ->
      Live_projection.seed_activity
        live
        ~operation_id:operation.id
        fields.active_tool_calls
  in
  let live, synchronization =
    match admitted with
    | Ok live -> live, Current
    | Error failure -> live, Snapshot_required failure
  in
  { snapshot; live; terminal_operation = None; synchronization }
;;

let snapshot t = t.snapshot
let live t = t.live
let terminal_operation t = t.terminal_operation
let synchronization t = t.synchronization
let error code message = P.Error.create code ~message ~retryable:true ()

let mark_stale t failure =
  { t with
    live = Live_projection.clear t.live
  ; synchronization = Snapshot_required failure
  }
;;

let validate_event t event =
  let fields = Public.Snapshot.fields t.snapshot in
  match t.synchronization with
  | Snapshot_required failure -> Error failure
  | Current ->
    if not (P.Id.Session.equal event.Public.Durable.session_id fields.session.id)
    then Error (error Invalid_state "event belongs to another session")
    else if Int64.equal fields.latest_event_sequence Int64.max_value
    then Error (error Snapshot_required "durable event sequence is exhausted")
    else if not Int64.(event.sequence = fields.latest_event_sequence + 1L)
    then Error (error Snapshot_required "durable event sequence is not contiguous")
    else if Int64.(event.revision < fields.revision)
    then Error (error Invalid_state "durable event revision regressed")
    else Ok ()
;;

let replace_by compare_id id value values ~id_of =
  value :: List.filter values ~f:(fun candidate -> compare_id (id_of candidate) id <> 0)
;;

let shared_payload (fields : Public.Snapshot.Fields.t) = function
  | P.Event.Durable.Payload.Session_created session | Session_updated session ->
    Ok { fields with session }
  | Session_state_changed change ->
    Ok
      { fields with
        session =
          { fields.session with
            desired_state = change.desired_state
          ; observed_state = change.observed_state
          }
      }
  | Attachment_owner_changed _ | Moderator_notification _ -> Ok fields
  | Permission_requested permission | Permission_resolved permission ->
    Ok
      { fields with
        permissions =
          replace_by
            P.Id.Permission.compare
            permission.id
            permission
            fields.permissions
            ~id_of:(fun permission -> permission.P.Permission.id)
      }
  | Grant_created grant | Grant_revoked grant ->
    Ok
      { fields with
        grants =
          replace_by P.Id.Grant.compare grant.id grant fields.grants ~id_of:(fun grant ->
            grant.P.Grant.id)
      }
  | Operation_started operation ->
    Ok { fields with session = { fields.session with active_operation = Some operation } }
  | Operation_completed _
  | Operation_failed _
  | Operation_cancelled _
  | Operation_interrupted _ ->
    Ok
      { fields with
        session = { fields.session with active_operation = None }
      ; active_tool_calls = []
      ; active_agent_calls = []
      }
  | Job_state_changed job ->
    Ok
      { fields with
        jobs =
          replace_by P.Id.Job.compare job.id job fields.jobs ~id_of:(fun job ->
            job.P.Job.id)
      }
  | Schedule_created schedule
  | Schedule_state_changed schedule
  | Schedule_cancelled schedule ->
    Ok
      { fields with
        schedules =
          replace_by
            P.Id.Schedule.compare
            schedule.id
            schedule
            fields.schedules
            ~id_of:(fun schedule -> schedule.P.Schedule.id)
      }
  | Prompt_upgraded upgrade ->
    Ok
      { fields with
        session = { fields.session with prompt_revision = Some upgrade.current_revision }
      }
  | Workspace_state_changed _ -> Ok fields
  | Session_error failure -> Ok { fields with failure = Some failure }
  | History_message_deferred _
  | History_appended _
  | History_replaced _
  | Moderator_overlay_changed _ ->
    Error (error Invalid_state "history payload crossed the shared event boundary")
;;

let payload (fields : Public.Snapshot.Fields.t) = function
  | Public.Durable.History_message_deferred entry ->
    Ok { fields with deferred_entries = fields.deferred_entries @ [ entry ] }
  | History_appended entries ->
    Ok
      { fields with
        canonical_history =
          { fields.canonical_history with
            entries = fields.canonical_history.entries @ entries
          }
      ; deferred_entries =
          List.filter fields.deferred_entries ~f:(fun deferred ->
            not
              (List.exists entries ~f:(fun entry ->
                 History_entry.Id.equal deferred.id entry.id)))
      }
  | History_replaced canonical_history -> Ok { fields with canonical_history }
  | Moderator_overlay_changed overlay ->
    Ok
      { fields with
        effective_history = overlay.effective_history
      ; halted = overlay.halted
      ; halt_reason = overlay.halt_reason
      }
  | Shared shared -> shared_payload fields (Public.Durable.Shared_payload.value shared)
;;

let observed_terminal previous = function
  | Some (Public.Durable.Shared shared) ->
    (match Public.Durable.Shared_payload.value shared with
     | P.Event.Durable.Payload.Operation_completed operation
     | Operation_failed operation
     | Operation_cancelled operation
     | Operation_interrupted operation -> Some operation
     | Operation_started _ -> None
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
     | Job_state_changed _
     | Schedule_created _
     | Schedule_state_changed _
     | Schedule_cancelled _
     | Prompt_upgraded _
     | Workspace_state_changed _
     | Session_error _ -> previous)
  | Some
      ( History_message_deferred _
      | History_appended _
      | History_replaced _
      | Moderator_overlay_changed _ )
  | None -> previous
;;

let advance (fields : Public.Snapshot.Fields.t) (event : Public.Durable.t) =
  { fields with
    session =
      { fields.session with
        revision = event.Public.Durable.revision
      ; latest_event_sequence = event.sequence
      ; updated_at = event.timestamp
      }
  ; lifecycle = None
  ; revision = event.revision
  ; latest_event_sequence = event.sequence
  }
;;

let activity_fields (fields : Public.Snapshot.Fields.t) live =
  let running =
    (match fields.session.active_operation with
     | None -> []
     | Some active ->
       Live_projection.operations live
       |> List.concat_map ~f:(fun operation ->
         if P.Id.Operation.equal active.id operation.operation_id
         then operation.activities
         else []))
    |> List.filter ~f:(fun summary ->
      match summary.P.Activity.Tool.state with
      | Running -> true
      | Finished _ -> false)
  in
  let agents =
    List.filter running ~f:(fun summary ->
      Option.exists summary.P.Activity.Tool.descriptor ~f:(fun descriptor ->
        Option.is_some descriptor.classification))
  in
  { fields with active_tool_calls = running; active_agent_calls = agents }
;;

let apply_event t event =
  let open Result.Let_syntax in
  let%bind () = validate_event t event in
  let previous = Public.Snapshot.fields t.snapshot in
  let fields =
    Option.value_map
      event.Public.Durable.replacement_snapshot
      ~default:previous
      ~f:Public.Snapshot.fields
  in
  let%bind fields, observed_payload =
    match event.body with
    | Hidden ->
      let fields =
        match event.kind with
        | Operation_completed
        | Operation_failed
        | Operation_cancelled
        | Operation_interrupted ->
          { fields with
            session = { fields.session with active_operation = None }
          ; active_tool_calls = []
          ; active_agent_calls = []
          }
        | Session_created
        | Session_state_changed
        | Session_updated
        | Attachment_owner_changed
        | History_message_deferred
        | History_appended
        | History_replaced
        | Moderator_overlay_changed
        | Moderator_notification
        | Permission_requested
        | Permission_resolved
        | Grant_created
        | Grant_revoked
        | Operation_started
        | Job_state_changed
        | Schedule_created
        | Schedule_state_changed
        | Schedule_cancelled
        | Prompt_upgraded
        | Workspace_state_changed
        | Session_error -> fields
      in
      Ok (fields, None)
    | Full value | Filtered value ->
      Result.map (payload fields value) ~f:(fun fields -> fields, Some value)
  in
  let fields =
    Option.value_map event.extension_status ~default:fields ~f:(fun extension_status ->
      { fields with extension_status })
    |> fun fields -> advance fields event
  in
  let%bind live =
    match event.replacement_snapshot with
    | None -> Ok t.live
    | Some snapshot ->
      let replacement = Public.Snapshot.fields snapshot in
      Live_projection.replace_snapshot
        t.live
        ~active_operation:replacement.session.active_operation
        replacement.active_tool_calls
  in
  let%bind live =
    Live_projection.advance_durable
      live
      event
      ~previous_operation:previous.session.active_operation
      ~active_operation:fields.session.active_operation
      ~canonical_history:fields.canonical_history.entries
  in
  let%map snapshot = Public.Snapshot.create (activity_fields fields live) in
  { snapshot
  ; live
  ; terminal_operation = observed_terminal t.terminal_operation observed_payload
  ; synchronization = Current
  }
;;

let apply_live_event t event =
  let open Result.Let_syntax in
  let fields = Public.Snapshot.fields t.snapshot in
  let%bind () =
    match t.synchronization with
    | Snapshot_required failure -> Error failure
    | Current ->
      if P.Id.Session.equal event.P.Event.Recoverable.session_id fields.session.id
      then Ok ()
      else Error (error Invalid_state "live event belongs to another session")
  in
  let%bind live =
    Live_projection.apply
      t.live
      ~durable_sequence:fields.latest_event_sequence
      ~active_operation:fields.session.active_operation
      ~canonical_history:fields.canonical_history.entries
      event
  in
  let%map snapshot = Public.Snapshot.create (activity_fields fields live) in
  { t with snapshot; live }
;;
