open! Core

let protocol_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Agent_protocol.Error.t)]
;;

let snapshot_to_json fields =
  Agent_protocol.Public.Snapshot.create fields
  |> protocol_ok
  |> Agent_protocol.Public.Snapshot.to_json
;;

let non_history = function
  | Agent_protocol.Public.Result.Non_history value ->
    Agent_protocol.Public.Result.Non_history.value value
  | Session_get _ | Session_attach _ | Session_create _ | Private_provider_challenge _ ->
    failwith "expected a response without inline history"
;;

let shared_payload (event : Agent_protocol.Public.Durable.t) =
  match event.body with
  | Full (Shared value) | Filtered (Shared value) ->
    Agent_protocol.Public.Durable.Shared_payload.value value
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
  | Hidden -> failwith "expected a visible shared durable payload"
;;

let history_text (entry : Agent_protocol.Public.History.t) =
  match entry.body with
  | Full payload ->
    (match
       History_entry.Payload.Semantic.view (History_entry.Payload.semantic payload)
     with
     | Message { content; _ } ->
       List.filter_map content ~f:(function
         | Text { text; _ } -> Some text
         | Refusal _ | Image _ | Unknown _ -> None)
     | Call _ | Result _ | Reasoning _ | Unknown _ -> [])
  | Visible (Message { content; _ }) ->
    List.filter_map content ~f:(function
      | Text text -> Some text
      | Refusal _ | Image _ | Redacted_part _ -> None)
  | Visible (Reasoning _) | Redacted _ -> []
;;

let full_payload (entry : Agent_protocol.Public.History.t) =
  match entry.body with
  | Full payload -> payload
  | Visible _ | Redacted _ -> failwith "assertion requires admitted Full history"
;;

let has_header entry header =
  Option.equal
    Transcript.Header.equal
    (Agent_protocol.Public.History.header entry)
    (Some header)
;;

let history_of_internal (entry : Agent_protocol.History.entry) =
  Agent_session.History_codec.of_canonical entry
  |> protocol_ok
  |> fun native ->
  Agent_protocol.Public.History.full native ~provenance:entry.provenance |> protocol_ok
;;

let payload (event : Agent_protocol.Public.Durable.t) =
  match event.body with
  | Full value | Filtered value -> Some value
  | Hidden -> None
;;

let visibility (event : Agent_protocol.Public.Durable.t) =
  match event.body with
  | Full _ -> Agent_protocol.Event.Durable.Full
  | Filtered _ -> Redacted
  | Hidden -> Hidden
;;

let shared_payload_opt event =
  match payload event with
  | Some (Shared value) -> Some (Agent_protocol.Public.Durable.Shared_payload.value value)
  | Some
      ( History_message_deferred _
      | History_appended _
      | History_replaced _
      | Moderator_overlay_changed _ )
  | None -> None
;;
