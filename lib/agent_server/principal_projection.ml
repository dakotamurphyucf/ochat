open! Core
module P = Agent_protocol
module H = P.Public.History
module D = P.Public.Durable

let has = P.Principal.has_scope

let history_entry principal (entry : P.History.entry) =
  let open Result.Let_syntax in
  let%bind payload =
    History_entry.Payload.of_json entry.payload
    |> Result.map_error ~f:P.Error.invalid_request
  in
  if entry.redacted
  then Error (P.Error.invalid_request "internal history contains a projected placeholder")
  else if has principal View_security_state
  then
    H.full
      (History_entry.create_with_id ~id:entry.id payload)
      ~provenance:entry.provenance
  else (
    match H.Visible.of_semantic (History_entry.Payload.semantic payload) with
    | Some view -> H.visible entry.id ~provenance:entry.provenance view
    | None ->
      H.redacted
        entry.id
        ~provenance:entry.provenance
        (H.Redaction.create
           ~disclosed_header:
             (Some
                (Transcript.Header.of_semantic (History_entry.Payload.semantic payload)))))
;;

let history principal (window : P.History.Window.t) =
  let open Result.Let_syntax in
  if not (has principal View_session_transcript)
  then
    Ok
      H.Window.
        { entries = []
        ; previous_cursor = None
        ; next_cursor = None
        ; reached_start = false
        ; reached_end = false
        ; structurally_complete = false
        }
  else (
    let%map entries = Result.all (List.map window.entries ~f:(history_entry principal)) in
    H.Window.
      { entries
      ; previous_cursor = window.previous_cursor
      ; next_cursor = window.next_cursor
      ; reached_start = window.reached_start
      ; reached_end = window.reached_end
      ; structurally_complete = window.structurally_complete
      })
;;

let snapshot principal (snapshot : P.Snapshot.t) =
  let open Result.Let_syntax in
  let security = has principal View_security_state in
  let transcript = has principal View_session_transcript in
  let writer = has principal Send_messages in
  let%bind canonical_history = history principal snapshot.canonical_history in
  let%bind effective_history =
    match transcript, snapshot.effective_history with
    | true, Some window -> Result.map (history principal window) ~f:Option.some
    | false, _ | true, None -> Ok None
  in
  let%bind deferred_entries =
    if transcript
    then Result.all (List.map snapshot.deferred_entries ~f:(history_entry principal))
    else Ok []
  in
  let%bind active_tool_calls =
    if security && transcript
    then
      Result.all (List.map snapshot.active_tool_calls ~f:P.Activity.Tool.summary_of_json)
    else Ok []
  in
  let%bind active_agent_calls =
    if security && transcript
    then
      Result.all (List.map snapshot.active_agent_calls ~f:P.Activity.Tool.summary_of_json)
    else Ok []
  in
  P.Public.Snapshot.create
    { session = snapshot.session
    ; canonical_history
    ; effective_history
    ; deferred_entries
    ; archived_revisions = (if transcript then snapshot.archived_revisions else [])
    ; permissions = (if security then snapshot.permissions else [])
    ; grants = (if has principal Manage_grants then snapshot.grants else [])
    ; jobs = (if writer then snapshot.jobs else [])
    ; extension_status = (if security then snapshot.extension_status else [])
    ; schedules = (if writer then snapshot.schedules else [])
    ; active_tool_calls
    ; active_agent_calls
    ; halted = snapshot.halted
    ; halt_reason = snapshot.halt_reason
    ; failure = snapshot.failure
    ; revision = snapshot.revision
    ; latest_event_sequence = snapshot.latest_event_sequence
    }
;;

let visible principal = function
  | P.Event.Durable.Permission_requested | Permission_resolved ->
    has principal View_security_state
  | Grant_created | Grant_revoked -> has principal Manage_grants
  | Job_state_changed | Schedule_created | Schedule_state_changed | Schedule_cancelled ->
    has principal Send_messages
  | Moderator_notification -> has principal View_security_state
  | History_message_deferred | History_appended | History_replaced ->
    has principal View_session_transcript
  | Session_created
  | Session_state_changed
  | Session_updated
  | Attachment_owner_changed
  | Moderator_overlay_changed
  | Operation_started
  | Operation_completed
  | Operation_failed
  | Operation_cancelled
  | Operation_interrupted
  | Prompt_upgraded
  | Workspace_state_changed
  | Session_error -> true
;;

let overlay principal json =
  let open Result.Let_syntax in
  let%bind fields = P.Json_codec.fields json in
  let%bind original =
    P.Json_codec.optional_as fields "effective_history" P.History.Window.of_json
  in
  let%bind effective_history =
    match has principal View_session_transcript, original with
    | true, Some window -> Result.map (history principal window) ~f:Option.some
    | false, _ | true, None -> Ok None
  in
  let%bind halted = P.Json_codec.required_as fields "halted" P.Json_codec.bool in
  let%map halt_reason =
    P.Json_codec.optional_as fields "halt_reason" P.Json_codec.string
  in
  D.{ effective_history; halted; halt_reason }
;;

let durable principal (event : P.Event.Durable.t) =
  let open Result.Let_syntax in
  if
    (not (visible principal event.kind))
    || P.Event.Durable.equal_visibility event.visibility Hidden
  then
    D.of_internal_envelope
      event
      ~body:Hidden
      ~extension_status:None
      ~replacement_snapshot:None
  else (
    let%bind original = P.Event.Durable.Payload.of_json ~kind:event.kind event.payload in
    let%bind payload =
      match original with
      | History_message_deferred entry ->
        let%map entry = history_entry principal entry in
        D.History_message_deferred entry
      | History_appended entries ->
        let%map entries = Result.all (List.map entries ~f:(history_entry principal)) in
        D.History_appended entries
      | History_replaced window ->
        let%map window = history principal window in
        D.History_replaced window
      | Moderator_overlay_changed json ->
        let%map view = overlay principal json in
        D.Moderator_overlay_changed view
      | shared ->
        let%map shared = D.Shared_payload.of_internal shared in
        D.Shared shared
    in
    let%bind replacement = P.Event.Durable.replacement_snapshot event in
    let%bind replacement_snapshot =
      match replacement with
      | None -> Ok None
      | Some value -> Result.map (snapshot principal value) ~f:Option.some
    in
    let%bind extension_status =
      if has principal View_security_state
      then P.Event.Durable.extension_status event
      else Ok None
    in
    let body =
      if has principal View_security_state && has principal View_session_transcript
      then D.Full payload
      else D.Filtered payload
    in
    D.of_internal_envelope event ~body ~extension_status ~replacement_snapshot)
;;

let recoverable principal event =
  if has principal View_security_state && has principal View_session_transcript
  then Some event
  else None
;;

let attach principal (attached : P.Method_result.Attach.t) =
  let open Result.Let_syntax in
  let%map replay =
    match attached.replay with
    | Current -> Ok P.Public.Result.Attach.Current
    | Snapshot value ->
      Result.map (snapshot principal value) ~f:(fun value ->
        P.Public.Result.Attach.Snapshot value)
    | Events values ->
      Result.map
        (Result.all (List.map values ~f:(durable principal)))
        ~f:(fun values -> P.Public.Result.Attach.Events values)
  in
  P.Public.Result.Attach.
    { attachment = attached.attachment
    ; replay
    ; latest_event_sequence = attached.latest_event_sequence
    ; reclaim_token = attached.reclaim_token
    }
;;

let project_result principal = function
  | P.Method_result.Provider_login_challenge value ->
    if P.Principal.has_scope principal Provider_manage
    then Ok (P.Public.Result.Private_provider_challenge value)
    else
      Error
        (P.Error.create
           Permission_denied
           ~message:"private provider challenge requires provider.manage"
           ~retryable:false
           ())
  | P.Method_result.Session_get value ->
    Result.map (snapshot principal value) ~f:(fun value ->
      P.Public.Result.Session_get value)
  | Session_attach value ->
    Result.map (attach principal value) ~f:(fun value ->
      P.Public.Result.Session_attach value)
  | Session_create value ->
    let open Result.Let_syntax in
    let%map attachment =
      match value.attachment with
      | None -> Ok None
      | Some value -> Result.map (attach principal value) ~f:Option.some
    in
    P.Public.Result.Session_create
      { session = value.session; mutation = value.mutation; attachment }
  | (Session_configuration_get configuration | Session_configuration_update configuration)
    as value ->
    let open Result.Let_syntax in
    let%bind configuration =
      if has principal Diagnostics
      then Ok configuration
      else Agent_session.Configuration_transition.redact_identity configuration
    in
    let value =
      match value with
      | Session_configuration_get _ ->
        P.Method_result.Session_configuration_get configuration
      | _ -> P.Method_result.Session_configuration_update configuration
    in
    Result.map (P.Public.Result.Non_history.of_internal value) ~f:(fun value ->
      P.Public.Result.Non_history value)
  | value ->
    Result.map (P.Public.Result.Non_history.of_internal value) ~f:(fun value ->
      P.Public.Result.Non_history value)
;;

let result principal value =
  let open Result.Let_syntax in
  let%bind result = project_result principal value in
  let%map () = P.Public.Result.validate result in
  result
;;

let scope_identity principal =
  Agent_protocol.Principal.to_json principal
  |> Jsonaf.to_string
  |> Digestif.SHA256.digest_string
  |> Digestif.SHA256.to_hex
;;

let export_use principal = "session_export:" ^ scope_identity principal

let can_read_blob principal (metadata : Agent_store.Blob_store.Metadata.t) =
  if String.is_prefix metadata.allowed_use ~prefix:"job_result:"
  then has principal Send_messages
  else if String.is_prefix metadata.allowed_use ~prefix:"session_export:"
  then String.equal metadata.allowed_use (export_use principal)
  else if String.equal metadata.allowed_use "session_export"
  then
    has principal View_security_state
    && has principal Manage_grants
    && has principal Send_messages
  else true
;;
