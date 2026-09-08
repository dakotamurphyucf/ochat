open! Core

let has = Agent_protocol.Principal.has_scope

let history_entry principal (entry : Agent_protocol.History.entry) =
  match entry.kind with
  | (Tool_call | Tool_output | Other) when not (has principal View_security_state) ->
    { entry with payload = `Object []; redacted = true }
  | _ -> entry
;;

let history principal (window : Agent_protocol.History.Window.t) =
  if not (has principal View_session_transcript)
  then
    Agent_protocol.History.Window.
      { entries = []
      ; reached_start = false
      ; reached_end = false
      ; structurally_complete = false
      ; previous_cursor = None
      ; next_cursor = None
      }
  else { window with entries = List.map window.entries ~f:(history_entry principal) }
;;

let snapshot principal (snapshot : Agent_protocol.Snapshot.t) =
  let security = has principal View_security_state in
  let writer = has principal Send_messages in
  { snapshot with
    archived_revisions =
      (if has principal View_session_transcript then snapshot.archived_revisions else [])
  ; canonical_history = history principal snapshot.canonical_history
  ; effective_history = Option.map snapshot.effective_history ~f:(history principal)
  ; deferred_entries =
      (if has principal View_session_transcript
       then List.map snapshot.deferred_entries ~f:(history_entry principal)
       else [])
  ; permissions = (if security then snapshot.permissions else [])
  ; grants = (if has principal Manage_grants then snapshot.grants else [])
  ; jobs = (if writer then snapshot.jobs else [])
  ; extension_status = (if security then snapshot.extension_status else [])
  ; schedules = (if writer then snapshot.schedules else [])
  ; active_tool_calls = (if security then snapshot.active_tool_calls else [])
  ; active_agent_calls = (if security then snapshot.active_agent_calls else [])
  }
;;

let visible principal = function
  | Agent_protocol.Event.Durable.Permission_requested | Permission_resolved ->
    has principal View_security_state
  | Grant_created | Grant_revoked -> has principal Manage_grants
  | Job_state_changed | Schedule_created | Schedule_state_changed | Schedule_cancelled ->
    has principal Send_messages
  | Moderator_notification -> has principal View_security_state
  | _ -> true
;;

let redact_payload principal = function
  | Agent_protocol.Event.Durable.Payload.History_appended entries ->
    Agent_protocol.Event.Durable.Payload.History_appended
      (List.map entries ~f:(history_entry principal))
  | History_message_deferred entry ->
    History_message_deferred (history_entry principal entry)
  | History_replaced window -> History_replaced (history principal window)
  | Moderator_overlay_changed (`Object fields) ->
    let fields =
      List.map fields ~f:(fun (name, value) ->
        if String.equal name "effective_history"
        then (
          match Agent_protocol.History.Window.of_json value with
          | Ok window ->
            name, Agent_protocol.History.Window.to_json (history principal window)
          | Error _ -> name, `Null)
        else name, value)
    in
    Moderator_overlay_changed (`Object fields)
  | payload -> payload
;;

let durable_payload principal (event : Agent_protocol.Event.Durable.t) =
  if not (visible principal event.kind)
  then { event with visibility = Hidden; payload = `Object [] }
  else if
    has principal View_security_state
    || Agent_protocol.Event.Durable.equal_visibility event.visibility Hidden
  then event
  else (
    match Agent_protocol.Event.Durable.Payload.of_json ~kind:event.kind event.payload with
    | Error _ -> { event with visibility = Hidden; payload = `Object [] }
    | Ok payload ->
      let payload =
        redact_payload principal payload |> Agent_protocol.Event.Durable.Payload.to_json
      in
      { event with visibility = Redacted; payload })
;;

let durable principal event =
  match Agent_protocol.Event.Durable.replacement_snapshot event with
  | Error _ -> { event with visibility = Hidden; payload = `Object [] }
  | Ok replacement ->
    let event = durable_payload principal event in
    if Agent_protocol.Event.Durable.equal_visibility event.visibility Hidden
    then event
    else
      Option.value_map replacement ~default:event ~f:(fun replacement ->
        Agent_protocol.Event.Durable.with_replacement_snapshot
          event
          (snapshot principal replacement))
;;

let recoverable principal event =
  if has principal View_security_state then Some event else None
;;

let attach principal (attached : Agent_protocol.Method_result.Attach.t) =
  let replay =
    match attached.replay with
    | Snapshot value ->
      Agent_protocol.Method_result.Attach.Snapshot (snapshot principal value)
    | Events events -> Events (List.map events ~f:(durable principal))
    | Current -> Current
  in
  { attached with replay }
;;

let result principal = function
  | Agent_protocol.Method_result.Session_get value ->
    Agent_protocol.Method_result.Session_get (snapshot principal value)
  | Session_attach value -> Session_attach (attach principal value)
  | Session_create value ->
    Session_create
      { value with attachment = Option.map value.attachment ~f:(attach principal) }
  | value -> value
;;

let scope_identity principal =
  Agent_protocol.Principal.to_json principal
  |> Jsonaf.to_string
  |> Digestif.SHA256.digest_string
  |> Digestif.SHA256.to_hex
;;

let export_use principal = "session_export:" ^ scope_identity principal

let can_read_blob principal (metadata : Agent_store.Blob_store.Metadata.t) =
  if String.is_prefix metadata.allowed_use ~prefix:"session_export:"
  then String.equal metadata.allowed_use (export_use principal)
  else if String.equal metadata.allowed_use "session_export"
  then
    has principal View_security_state
    && has principal Manage_grants
    && has principal Send_messages
  else true
;;
