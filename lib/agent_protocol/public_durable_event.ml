open Core
module D = Event.Durable
module H = Public_history
module J = Json_codec

module Shared_payload = struct
  type t = D.Payload.t [@@deriving sexp_of]

  let value t = t

  let of_internal = function
    | D.Payload.History_message_deferred _
    | History_appended _
    | History_replaced _
    | Moderator_overlay_changed _ ->
      Error
        (Protocol_error.invalid_request
           "history payload requires explicit public projection")
    | ( Session_created _
      | Session_state_changed _
      | Session_updated _
      | Attachment_owner_changed _
      | Moderator_notification _
      | Permission_requested _
      | Permission_resolved _
      | Grant_created _
      | Grant_revoked _
      | Operation_started _
      | Operation_completed _
      | Operation_failed _
      | Operation_cancelled _
      | Operation_interrupted _
      | Job_state_changed _
      | Schedule_created _
      | Schedule_state_changed _
      | Schedule_cancelled _
      | Prompt_upgraded _
      | Workspace_state_changed _
      | Session_error _ ) as payload ->
      let open Result.Let_syntax in
      let json = D.Payload.to_json payload in
      let%bind () = Projection_codec.validate json in
      let%map (_ : D.Payload.t) = D.Payload.of_json ~kind:(D.Payload.kind payload) json in
      payload
  ;;
end

type overlay =
  { effective_history : H.Window.t option
  ; halted : bool
  ; halt_reason : string option
  }
[@@deriving sexp_of]

type payload =
  | History_message_deferred of H.t
  | History_appended of H.t list
  | History_replaced of H.Window.t
  | Moderator_overlay_changed of overlay
  | Shared of Shared_payload.t
[@@deriving sexp_of]

type body =
  | Full of payload
  | Filtered of payload
  | Hidden
[@@deriving sexp_of]

type t =
  { session_id : Id.Session.t
  ; sequence : int64
  ; revision : int64
  ; timestamp : Timestamp.t
  ; kind : D.kind
  ; body : body
  ; extension_status : Extension_status.t list option
  ; replacement_snapshot : Public_snapshot.t option
  }
[@@deriving sexp_of]

let kind = function
  | History_message_deferred _ -> D.History_message_deferred
  | History_appended _ -> D.History_appended
  | History_replaced _ -> D.History_replaced
  | Moderator_overlay_changed _ -> D.Moderator_overlay_changed
  | Shared payload -> D.Payload.kind (Shared_payload.value payload)
;;

let payload_to_json = function
  | History_message_deferred entry -> H.to_json entry
  | History_appended entries ->
    `Object [ "entries", `Array (List.map entries ~f:H.to_json) ]
  | History_replaced window -> H.Window.to_json window
  | Moderator_overlay_changed { effective_history; halted; halt_reason } ->
    `Object
      ([ ("halted", if halted then `True else `False) ]
       @ Projection_codec.optional "effective_history" effective_history H.Window.to_json
       @ Projection_codec.optional "halt_reason" halt_reason (fun s -> `String s))
  | Shared payload -> D.Payload.to_json (Shared_payload.value payload)
;;

let payload_of_json ~kind json =
  let open Result.Let_syntax in
  match kind with
  | D.History_message_deferred ->
    let%map entry = H.of_json json in
    History_message_deferred entry
  | History_appended ->
    let%bind fields = J.fields json in
    let%map entries = J.required_as fields "entries" (J.list H.of_json) in
    History_appended entries
  | History_replaced ->
    let%map window = H.Window.of_json json in
    History_replaced window
  | Moderator_overlay_changed ->
    let%bind fields = J.fields json in
    let%bind effective_history =
      J.optional_as fields "effective_history" H.Window.of_json
    in
    let%bind halted = J.required_as fields "halted" J.bool in
    let%map halt_reason = J.optional_as fields "halt_reason" J.string in
    Moderator_overlay_changed { effective_history; halted; halt_reason }
  | Session_created
  | Session_state_changed
  | Session_updated
  | Attachment_owner_changed
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
  | Session_error ->
    let%bind payload = D.Payload.of_json ~kind json in
    let%map payload = Shared_payload.of_internal payload in
    Shared payload
;;

let to_json t =
  let visibility, payload =
    match t.body with
    | Full p -> D.Full, payload_to_json p
    | Filtered p -> D.Redacted, payload_to_json p
    | Hidden -> D.Hidden, `Object []
  in
  let payload =
    match payload with
    | `Object fields ->
      `Object
        (fields
         @ Projection_codec.optional "extension_status" t.extension_status (fun values ->
           `Array (List.map values ~f:Extension_status.to_json))
         @ Projection_codec.optional
             "replacement_snapshot"
             t.replacement_snapshot
             Public_snapshot.to_json)
    | value -> value
  in
  D.to_json
    { session_id = t.session_id
    ; sequence = t.sequence
    ; revision = t.revision
    ; timestamp = t.timestamp
    ; kind = t.kind
    ; visibility
    ; payload
    }
;;

let validate_owner session_id = function
  | History_message_deferred _
  | History_appended _
  | History_replaced _
  | Moderator_overlay_changed _ -> Ok ()
  | Shared shared ->
    let owner =
      match Shared_payload.value shared with
      | D.Payload.Session_created session | Session_updated session -> Some session.id
      | Permission_requested permission | Permission_resolved permission ->
        Some permission.session_id
      | Grant_created grant | Grant_revoked grant -> Some grant.session_id
      | Job_state_changed job -> Some job.session_id
      | Schedule_created schedule
      | Schedule_state_changed schedule
      | Schedule_cancelled schedule -> Some schedule.session_id
      | Session_state_changed _
      | Attachment_owner_changed _
      | Moderator_notification _
      | Operation_started _
      | Operation_completed _
      | Operation_failed _
      | Operation_cancelled _
      | Operation_interrupted _
      | Prompt_upgraded _
      | Workspace_state_changed _
      | Session_error _ -> None
      | History_message_deferred _
      | History_appended _
      | History_replaced _
      | Moderator_overlay_changed _ -> None
    in
    if Option.for_all owner ~f:(Id.Session.equal session_id)
    then Ok ()
    else
      Error (Protocol_error.invalid_request "durable payload belongs to another session")
;;

let validate_extension_status body = function
  | None -> Ok ()
  | Some statuses ->
    let open Result.Let_syntax in
    let%bind (_ : Extension_status.t list) =
      Extension_status.list_of_json
        (`Array (List.map statuses ~f:Extension_status.to_json))
    in
    (match body with
     | Full (Shared shared) | Filtered (Shared shared) ->
       (match Shared_payload.value shared with
        | D.Payload.Session_updated session ->
          if
            List.for_all statuses ~f:(fun status ->
              status.Extension_status.generation <= session.generation)
          then Ok ()
          else
            Error
              (Protocol_error.invalid_request "durable event has future extension status")
        | Session_created _
        | Session_state_changed _
        | Attachment_owner_changed _
        | Moderator_notification _
        | Permission_requested _
        | Permission_resolved _
        | Grant_created _
        | Grant_revoked _
        | Operation_started _
        | Operation_completed _
        | Operation_failed _
        | Operation_cancelled _
        | Operation_interrupted _
        | Job_state_changed _
        | Schedule_created _
        | Schedule_state_changed _
        | Schedule_cancelled _
        | Prompt_upgraded _
        | Workspace_state_changed _
        | Session_error _
        | History_message_deferred _
        | History_appended _
        | History_replaced _
        | Moderator_overlay_changed _ ->
          Error
            (Protocol_error.invalid_request "extension status requires a session update"))
     | Full
         ( History_message_deferred _
         | History_appended _
         | History_replaced _
         | Moderator_overlay_changed _ )
     | Filtered
         ( History_message_deferred _
         | History_appended _
         | History_replaced _
         | Moderator_overlay_changed _ )
     | Hidden ->
       Error
         (Protocol_error.invalid_request
            "extension status requires a visible session update"))
;;

let of_internal_envelope (event : D.t) ~body ~extension_status ~replacement_snapshot =
  let open Result.Let_syntax in
  let valid_kind =
    match body with
    | Hidden -> true
    | Full p | Filtered p -> D.equal_kind event.kind (kind p)
  in
  let hidden_extras =
    match body with
    | Hidden -> Option.is_some extension_status || Option.is_some replacement_snapshot
    | Full _ | Filtered _ -> false
  in
  let invalid_replacement =
    Option.exists replacement_snapshot ~f:(fun snapshot ->
      let fields = Public_snapshot.fields snapshot in
      not
        (Id.Session.equal fields.session.id event.session_id
         && Int64.equal fields.revision event.revision
         && Int64.equal fields.latest_event_sequence event.sequence))
  in
  if
    Int64.(event.sequence < zero || event.revision < zero)
    || (not valid_kind)
    || hidden_extras
    || invalid_replacement
  then Error (Protocol_error.invalid_request "invalid public durable event envelope")
  else if
    (Option.is_some extension_status || Option.is_some replacement_snapshot)
    && not (D.equal_kind event.kind D.Session_updated)
  then Error (Protocol_error.invalid_request "replacement extras require session.updated")
  else (
    let%bind () =
      match body with
      | Hidden -> Ok ()
      | Full payload | Filtered payload -> validate_owner event.session_id payload
    in
    let%bind () = validate_extension_status body extension_status in
    let t =
      { session_id = event.session_id
      ; sequence = event.sequence
      ; revision = event.revision
      ; timestamp = event.timestamp
      ; kind = event.kind
      ; body
      ; extension_status
      ; replacement_snapshot
      }
    in
    let%bind () = Projection_codec.validate (to_json t) in
    let%map () =
      match body with
      | Full (History_appended entries) | Filtered (History_appended entries) ->
        H.validate_unique_ids entries
      | Full (History_replaced window) | Filtered (History_replaced window) ->
        H.Window.validate window
      | Full (Moderator_overlay_changed overlay)
      | Filtered (Moderator_overlay_changed overlay) ->
        (match overlay.effective_history with
         | None -> Ok ()
         | Some window -> H.Window.validate window)
      | Full (History_message_deferred _ | Shared _)
      | Filtered (History_message_deferred _ | Shared _)
      | Hidden -> Ok ()
    in
    t)
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () = Projection_codec.validate json in
  let%bind event = D.of_json json in
  let%bind fields =
    J.fields
      (match event.payload with
       | `Object _ -> event.payload
       | _ -> `Object [])
  in
  let%bind extension_status =
    J.optional_as fields "extension_status" Extension_status.list_of_json
  in
  let%bind replacement_snapshot =
    J.optional_as fields "replacement_snapshot" Public_snapshot.of_json
  in
  let%bind body =
    match event.visibility with
    | D.Hidden ->
      if List.is_empty (J.to_alist fields)
      then Ok Hidden
      else Error (Protocol_error.invalid_request "hidden event contains payload")
    | Full ->
      let%map p = payload_of_json ~kind:event.kind event.payload in
      Full p
    | Redacted ->
      let%map p = payload_of_json ~kind:event.kind event.payload in
      Filtered p
  in
  of_internal_envelope event ~body ~extension_status ~replacement_snapshot
;;

let to_notification t =
  Envelope.notification ~method_:"session.event" ~params:(to_json t) ()
;;
