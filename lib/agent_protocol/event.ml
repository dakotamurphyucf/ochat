open Core

let nonnegative_int64 = Json_codec.bounded_int64 ~min:Int64.zero ~max:Int64.max_value
let int64_to_json value = `Number (Int64.to_string value)

module Durable = struct
  type kind =
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
    | Operation_completed
    | Operation_failed
    | Operation_cancelled
    | Operation_interrupted
    | Job_state_changed
    | Schedule_created
    | Schedule_state_changed
    | Schedule_cancelled
    | Prompt_upgraded
    | Workspace_state_changed
    | Session_error
  [@@deriving compare, equal, sexp]

  type visibility =
    | Full
    | Redacted
    | Hidden
  [@@deriving compare, equal, sexp]

  type t =
    { session_id : Id.Session.t
    ; sequence : int64
    ; revision : int64
    ; timestamp : Timestamp.t
    ; kind : kind
    ; visibility : visibility
    ; payload : Jsonaf.t
    }
  [@@deriving sexp]

  let with_replacement_snapshot (event : t) (snapshot : Snapshot.t) =
    let session =
      { snapshot.session with
        revision = event.revision
      ; latest_event_sequence = event.sequence
      ; updated_at = event.timestamp
      }
    in
    let snapshot =
      { snapshot with
        session
      ; revision = event.revision
      ; latest_event_sequence = event.sequence
      }
    in
    match event.kind, event.payload with
    | Session_updated, `Object fields ->
      { event with
        payload =
          `Object
            (("replacement_snapshot", Snapshot.to_json snapshot)
             :: List.filter fields ~f:(fun (name, _) ->
               not (String.equal name "replacement_snapshot")))
      }
    | _ -> event
  ;;

  let replacement_snapshot (event : t) =
    let open Result.Let_syntax in
    match event.kind, event.payload with
    | Session_updated, `Object fields ->
      let%bind fields = Json_codec.fields (`Object fields) in
      let%bind snapshot =
        Json_codec.optional_as fields "replacement_snapshot" Snapshot.of_json
      in
      (match snapshot with
       | Some snapshot
         when Id.Session.compare snapshot.session.id event.session_id <> 0
              || (not (Int64.equal snapshot.revision event.revision))
              || (not (Int64.equal snapshot.latest_event_sequence event.sequence))
              || (not (Int64.equal snapshot.session.revision event.revision))
              || not (Int64.equal snapshot.session.latest_event_sequence event.sequence)
         ->
         Error
           (Protocol_error.invalid_request
              "replacement snapshot disagrees with event anchor")
       | _ -> Ok snapshot)
    | _ -> Ok None
  ;;

  (* An optional field on the existing session.updated event keeps older clients
     able to advance the durable cursor without adding an unknown event kind. *)
  let with_extension_status (event : t) statuses =
    match event.kind, event.payload with
    | Session_updated, `Object fields ->
      { event with
        payload =
          `Object
            (("extension_status", `Array (List.map statuses ~f:Extension_status.to_json))
             :: List.filter fields ~f:(fun (name, _) ->
               not (String.equal name "extension_status")))
      }
    | _ -> event
  ;;

  let extension_status (event : t) =
    let open Result.Let_syntax in
    match event.kind, event.payload with
    | Session_updated, `Object fields ->
      let%bind fields = Json_codec.fields (`Object fields) in
      let%bind statuses =
        Json_codec.optional_as fields "extension_status" Extension_status.list_of_json
      in
      (match statuses with
       | None -> Ok None
       | Some statuses ->
         let%bind session = Session.of_json event.payload in
         if
           Id.Session.compare session.id event.session_id <> 0
           || List.exists statuses ~f:(fun status ->
             status.Extension_status.generation > session.generation)
         then
           Error
             (Protocol_error.invalid_request
                "extension status belongs to another session or future generation")
         else Ok (Some statuses))
    | _ -> Ok None
  ;;

  let kind_values =
    [ "session.created", Session_created
    ; "session.state_changed", Session_state_changed
    ; "session.updated", Session_updated
    ; "attachment.owner_changed", Attachment_owner_changed
    ; "history.message_deferred", History_message_deferred
    ; "history.appended", History_appended
    ; "history.replaced", History_replaced
    ; "moderator.overlay_changed", Moderator_overlay_changed
    ; "moderator.notification", Moderator_notification
    ; "permission.requested", Permission_requested
    ; "permission.resolved", Permission_resolved
    ; "grant.created", Grant_created
    ; "grant.revoked", Grant_revoked
    ; "operation.started", Operation_started
    ; "operation.completed", Operation_completed
    ; "operation.failed", Operation_failed
    ; "operation.cancelled", Operation_cancelled
    ; "operation.interrupted", Operation_interrupted
    ; "job.state_changed", Job_state_changed
    ; "schedule.created", Schedule_created
    ; "schedule.state_changed", Schedule_state_changed
    ; "schedule.cancelled", Schedule_cancelled
    ; "prompt.upgraded", Prompt_upgraded
    ; "workspace.state_changed", Workspace_state_changed
    ; "session.error", Session_error
    ]
  ;;

  let kind_to_string kind =
    List.Assoc.find_exn
      (List.map kind_values ~f:(fun (name, kind) -> kind, name))
      kind
      ~equal:equal_kind
  ;;

  let kind_of_json = Json_codec.enum ~name:"durable event kind" kind_values

  let visibility_to_string = function
    | Full -> "full"
    | Redacted -> "redacted"
    | Hidden -> "hidden"
  ;;

  let visibility_of_json =
    Json_codec.enum
      ~name:"event visibility"
      [ "full", Full; "redacted", Redacted; "hidden", Hidden ]
  ;;

  module Payload = struct
    type lifecycle_change =
      { desired_state : Session.desired_state
      ; observed_state : Session.observed_state
      }
    [@@deriving sexp]

    type prompt_upgrade =
      { prompt_id : Id.Prompt_definition.t
      ; previous_revision : Id.Prompt_revision.t
      ; current_revision : Id.Prompt_revision.t
      }
    [@@deriving sexp]

    type t =
      | Session_created of Session.t
      | Session_state_changed of lifecycle_change
      | Session_updated of Session.t
      | Attachment_owner_changed of Session.Attachment.t option
      | History_message_deferred of History.entry
      | History_appended of History.entry list
      | History_replaced of History.Window.t
      | Moderator_overlay_changed of Jsonaf.t
      | Moderator_notification of Jsonaf.t
      | Permission_requested of Permission.t
      | Permission_resolved of Permission.t
      | Grant_created of Grant.t
      | Grant_revoked of Grant.t
      | Operation_started of Operation.t
      | Operation_completed of Operation.t
      | Operation_failed of Operation.t
      | Operation_cancelled of Operation.t
      | Operation_interrupted of Operation.t
      | Job_state_changed of Job.t
      | Schedule_created of Schedule.t
      | Schedule_state_changed of Schedule.t
      | Schedule_cancelled of Schedule.t
      | Prompt_upgraded of prompt_upgrade
      | Workspace_state_changed of Workspace.t
      | Session_error of Protocol_error.t
    [@@deriving sexp]

    let kind (payload : t) : kind =
      match payload with
      | Session_created _ -> Session_created
      | Session_state_changed _ -> Session_state_changed
      | Session_updated _ -> Session_updated
      | Attachment_owner_changed _ -> Attachment_owner_changed
      | History_message_deferred _ -> History_message_deferred
      | History_appended _ -> History_appended
      | History_replaced _ -> History_replaced
      | Moderator_overlay_changed _ -> Moderator_overlay_changed
      | Moderator_notification _ -> Moderator_notification
      | Permission_requested _ -> Permission_requested
      | Permission_resolved _ -> Permission_resolved
      | Grant_created _ -> Grant_created
      | Grant_revoked _ -> Grant_revoked
      | Operation_started _ -> Operation_started
      | Operation_completed _ -> Operation_completed
      | Operation_failed _ -> Operation_failed
      | Operation_cancelled _ -> Operation_cancelled
      | Operation_interrupted _ -> Operation_interrupted
      | Job_state_changed _ -> Job_state_changed
      | Schedule_created _ -> Schedule_created
      | Schedule_state_changed _ -> Schedule_state_changed
      | Schedule_cancelled _ -> Schedule_cancelled
      | Prompt_upgraded _ -> Prompt_upgraded
      | Workspace_state_changed _ -> Workspace_state_changed
      | Session_error _ -> Session_error
    ;;

    let lifecycle_to_json change =
      `Object
        [ "desired_state", `String (Session.desired_state_to_string change.desired_state)
        ; "observed_state", Session.observed_state_to_json change.observed_state
        ]
    ;;

    let lifecycle_of_json json =
      let open Result.Let_syntax in
      let%bind fields = Json_codec.fields json in
      let%bind desired_state =
        Json_codec.required_as fields "desired_state" Session.desired_state_of_json
      in
      let%map observed_state =
        Json_codec.required_as fields "observed_state" Session.observed_state_of_json
      in
      { desired_state; observed_state }
    ;;

    let prompt_upgrade_to_json upgrade =
      `Object
        [ "prompt_id", Id.Prompt_definition.to_json upgrade.prompt_id
        ; "previous_revision", Id.Prompt_revision.to_json upgrade.previous_revision
        ; "current_revision", Id.Prompt_revision.to_json upgrade.current_revision
        ]
    ;;

    let prompt_upgrade_of_json json =
      let open Result.Let_syntax in
      let%bind fields = Json_codec.fields json in
      let%bind prompt_id =
        Json_codec.required_as fields "prompt_id" Id.Prompt_definition.of_json
      in
      let%bind previous_revision =
        Json_codec.required_as fields "previous_revision" Id.Prompt_revision.of_json
      in
      let%map current_revision =
        Json_codec.required_as fields "current_revision" Id.Prompt_revision.of_json
      in
      { prompt_id; previous_revision; current_revision }
    ;;

    let to_json = function
      | Session_created value | Session_updated value -> Session.to_json value
      | Session_state_changed value -> lifecycle_to_json value
      | Attachment_owner_changed value ->
        Option.value_map value ~default:`Null ~f:Session.Attachment.to_json
      | History_message_deferred value -> History.entry_to_json value
      | History_appended values -> `Array (List.map values ~f:History.entry_to_json)
      | History_replaced value -> History.Window.to_json value
      | Moderator_overlay_changed value | Moderator_notification value -> value
      | Permission_requested value | Permission_resolved value -> Permission.to_json value
      | Grant_created value | Grant_revoked value -> Grant.to_json value
      | Operation_started value
      | Operation_completed value
      | Operation_failed value
      | Operation_cancelled value
      | Operation_interrupted value -> Operation.to_json value
      | Job_state_changed value -> Job.to_json value
      | Schedule_created value | Schedule_state_changed value | Schedule_cancelled value
        -> Schedule.to_json value
      | Prompt_upgraded value -> prompt_upgrade_to_json value
      | Workspace_state_changed value -> Workspace.to_json value
      | Session_error value -> Protocol_error.to_json value
    ;;

    let owner_of_json = function
      | `Null -> Ok None
      | json -> Result.map (Session.Attachment.of_json json) ~f:Option.some
    ;;

    let of_json ~(kind : kind) json =
      match kind with
      | Session_created ->
        Result.map (Session.of_json json) ~f:(fun x -> Session_created x)
      | Session_state_changed ->
        Result.map (lifecycle_of_json json) ~f:(fun x -> Session_state_changed x)
      | Session_updated ->
        Result.map (Session.of_json json) ~f:(fun x -> Session_updated x)
      | Attachment_owner_changed ->
        Result.map (owner_of_json json) ~f:(fun x -> Attachment_owner_changed x)
      | History_message_deferred ->
        Result.map (History.entry_of_json json) ~f:(fun x -> History_message_deferred x)
      | History_appended ->
        Result.map (Json_codec.list History.entry_of_json json) ~f:(fun x ->
          History_appended x)
      | History_replaced ->
        Result.map (History.Window.of_json json) ~f:(fun x -> History_replaced x)
      | Moderator_overlay_changed -> Ok (Moderator_overlay_changed json)
      | Moderator_notification -> Ok (Moderator_notification json)
      | Permission_requested ->
        Result.map (Permission.of_json json) ~f:(fun x -> Permission_requested x)
      | Permission_resolved ->
        Result.map (Permission.of_json json) ~f:(fun x -> Permission_resolved x)
      | Grant_created -> Result.map (Grant.of_json json) ~f:(fun x -> Grant_created x)
      | Grant_revoked -> Result.map (Grant.of_json json) ~f:(fun x -> Grant_revoked x)
      | Operation_started ->
        Result.map (Operation.of_json json) ~f:(fun x -> Operation_started x)
      | Operation_completed ->
        Result.map (Operation.of_json json) ~f:(fun x -> Operation_completed x)
      | Operation_failed ->
        Result.map (Operation.of_json json) ~f:(fun x -> Operation_failed x)
      | Operation_cancelled ->
        Result.map (Operation.of_json json) ~f:(fun x -> Operation_cancelled x)
      | Operation_interrupted ->
        Result.map (Operation.of_json json) ~f:(fun x -> Operation_interrupted x)
      | Job_state_changed ->
        Result.map (Job.of_json json) ~f:(fun x -> Job_state_changed x)
      | Schedule_created ->
        Result.map (Schedule.of_json json) ~f:(fun x -> Schedule_created x)
      | Schedule_state_changed ->
        Result.map (Schedule.of_json json) ~f:(fun x -> Schedule_state_changed x)
      | Schedule_cancelled ->
        Result.map (Schedule.of_json json) ~f:(fun x -> Schedule_cancelled x)
      | Prompt_upgraded ->
        Result.map (prompt_upgrade_of_json json) ~f:(fun x -> Prompt_upgraded x)
      | Workspace_state_changed ->
        Result.map (Workspace.of_json json) ~f:(fun x -> Workspace_state_changed x)
      | Session_error ->
        Result.map (Protocol_error.of_json json) ~f:(fun x -> Session_error x)
    ;;
  end

  let to_json t =
    let visibility =
      match t.visibility with
      | Full -> []
      | (Redacted | Hidden) as visibility ->
        [ "visibility", `String (visibility_to_string visibility) ]
    in
    `Object
      ([ "session_id", Id.Session.to_json t.session_id
       ; "sequence", int64_to_json t.sequence
       ; "revision", int64_to_json t.revision
       ; "timestamp", Timestamp.to_json t.timestamp
       ; "kind", `String (kind_to_string t.kind)
       ; "payload", t.payload
       ]
       @ visibility)
  ;;

  let decode_ordering fields =
    let open Result.Let_syntax in
    let%bind sequence = Json_codec.required_as fields "sequence" nonnegative_int64 in
    let%map revision = Json_codec.required_as fields "revision" nonnegative_int64 in
    sequence, revision
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind session_id = Json_codec.required_as fields "session_id" Id.Session.of_json in
    let%bind sequence, revision = decode_ordering fields in
    let%bind timestamp = Json_codec.required_as fields "timestamp" Timestamp.of_json in
    let%bind kind = Json_codec.required_as fields "kind" kind_of_json in
    let%bind visibility = Json_codec.optional_as fields "visibility" visibility_of_json in
    let%bind payload = Json_codec.required fields "payload" in
    let visibility = Option.value visibility ~default:Full in
    match visibility, payload with
    | Hidden, `Object [] ->
      Ok { session_id; sequence; revision; timestamp; kind; visibility; payload }
    | Hidden, _ ->
      Error (Protocol_error.invalid_request "hidden event payload must be empty")
    | (Full | Redacted), _ ->
      Ok { session_id; sequence; revision; timestamp; kind; visibility; payload }
  ;;

  let of_payload ~session_id ~sequence ~revision ~timestamp payload =
    { session_id
    ; sequence
    ; revision
    ; timestamp
    ; kind = Payload.kind payload
    ; visibility = Full
    ; payload = Payload.to_json payload
    }
  ;;

  let to_notification t =
    Envelope.notification ~method_:"session.event" ~params:(to_json t) ()
  ;;
end

module Recoverable = struct
  type kind =
    | Provider_stream
    | Sourced_stream
    | History_correlated_stream
    | Tool_started
    | Tool_progress
    | Tool_trace
    | Tool_finished
    | Agent_call_classified
    | Agent_call_progress
    | Activity
    | Compaction_progress
  [@@deriving compare, equal, sexp]

  type t =
    { session_id : Id.Session.t
    ; operation_id : Id.Operation.t
    ; operation_sequence : int64
    ; anchor_sequence : int64
    ; timestamp : Timestamp.t
    ; kind : kind
    ; payload : Jsonaf.t
    }
  [@@deriving sexp]

  let kind_values =
    [ "provider.stream", Provider_stream
    ; "sourced.stream", Sourced_stream
    ; "history.correlated_stream", History_correlated_stream
    ; "tool.started", Tool_started
    ; "tool.progress", Tool_progress
    ; "tool.trace", Tool_trace
    ; "tool.finished", Tool_finished
    ; "agent.call_classified", Agent_call_classified
    ; "agent.call_progress", Agent_call_progress
    ; "activity", Activity
    ; "compaction.progress", Compaction_progress
    ]
  ;;

  let kind_to_string kind =
    List.Assoc.find_exn
      (List.map kind_values ~f:(fun (name, kind) -> kind, name))
      kind
      ~equal:equal_kind
  ;;

  let kind_of_json = Json_codec.enum ~name:"recoverable event kind" kind_values

  let to_json t =
    `Object
      [ "session_id", Id.Session.to_json t.session_id
      ; "operation_id", Id.Operation.to_json t.operation_id
      ; "operation_sequence", int64_to_json t.operation_sequence
      ; "anchor_sequence", int64_to_json t.anchor_sequence
      ; "timestamp", Timestamp.to_json t.timestamp
      ; "kind", `String (kind_to_string t.kind)
      ; "payload", t.payload
      ]
  ;;

  let decode_ordering fields =
    let open Result.Let_syntax in
    let%bind operation_sequence =
      Json_codec.required_as fields "operation_sequence" nonnegative_int64
    in
    let%map anchor_sequence =
      Json_codec.required_as fields "anchor_sequence" nonnegative_int64
    in
    operation_sequence, anchor_sequence
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind session_id = Json_codec.required_as fields "session_id" Id.Session.of_json in
    let%bind operation_id =
      Json_codec.required_as fields "operation_id" Id.Operation.of_json
    in
    let%bind operation_sequence, anchor_sequence = decode_ordering fields in
    let%bind timestamp = Json_codec.required_as fields "timestamp" Timestamp.of_json in
    let%bind kind = Json_codec.required_as fields "kind" kind_of_json in
    let%map payload = Json_codec.required fields "payload" in
    { session_id
    ; operation_id
    ; operation_sequence
    ; anchor_sequence
    ; timestamp
    ; kind
    ; payload
    }
  ;;

  let to_notification t =
    Envelope.notification ~method_:"session.live_event" ~params:(to_json t) ()
  ;;
end
