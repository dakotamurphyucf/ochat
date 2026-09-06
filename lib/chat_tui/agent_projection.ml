open! Core

type t =
  { snapshot : Agent_protocol.Snapshot.t
  ; canonical_history : History_entry.t list
  ; visible_history : History_entry.t list
  ; messages : Types.message list
  ; live_events : Agent_protocol.Event.Recoverable.t list
  ; terminal_operation : Agent_protocol.Operation.t option
  }

let invalid message =
  Agent_protocol.Error.create Invalid_state ~message ~retryable:false ()
;;

let decode_item payload =
  Result.try_with (fun () -> Openai.Responses.Item.t_of_jsonaf payload)
  |> Result.map_error ~f:(fun exn ->
    invalid ("invalid projected history payload: " ^ Exn.to_string exn))
;;

let decode_entry (entry : Agent_protocol.History.entry) =
  if entry.redacted
  then
    Ok
      (History_entry.create_with_id
         ~id:entry.id
         (Openai.Responses.Item.Input_message
            { role = Assistant
            ; _type = "message"
            ; content =
                [ Text { text = "[Tool content redacted]"; _type = "input_text" } ]
            }))
  else
    Result.map (decode_item entry.payload) ~f:(fun item ->
      History_entry.create_with_id ~id:entry.id item)
;;

let decode_window window =
  Result.all (List.map window.Agent_protocol.History.Window.entries ~f:decode_entry)
;;

let of_client_projection projection =
  let open Result.Let_syntax in
  let snapshot = Agent_client.Projection.snapshot projection in
  let%bind canonical_history = decode_window snapshot.canonical_history in
  let%map visible_history =
    match snapshot.effective_history with
    | None -> Ok canonical_history
    | Some history -> decode_window history
  in
  { snapshot
  ; canonical_history
  ; visible_history
  ; messages = Conversation.of_history (History_entry.items visible_history)
  ; live_events = Agent_client.Projection.live_events projection
  ; terminal_operation = Agent_client.Projection.terminal_operation projection
  }
;;

let snapshot t = t.snapshot
let canonical_history t = t.canonical_history
let visible_history t = t.visible_history
let messages t = t.messages
let live_events t = t.live_events
let terminal_operation t = t.terminal_operation
