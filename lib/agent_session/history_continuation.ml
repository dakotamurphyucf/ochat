open! Core
module P = Agent_protocol

type t =
  { deltas : Session_delta.t list
  ; payloads : P.Event.Durable.Payload.t list
  }

let prepare (state : Session_state.t) ~retiring_history =
  let open Result.Let_syntax in
  let first =
    Int64.max
      state.conversation.next_history_sequence
      state.conversation.reserved_history_through
  in
  let%bind first_sequence =
    match Int64.to_int first with
    | Some value -> Ok value
    | None ->
      Error
        (P.Error.create
           Invalid_state
           ~message:"history sequence exceeds platform allocation range"
           ~retryable:false
           ())
  in
  let%bind recovery =
    Invocation_recovery.plan_foreground
      ~state
      ~namespace:(P.Id.Session.to_string state.identity.session_id)
      ~first_sequence
      ~reason:"history continuation reconciles retained invocation outcomes"
  in
  let%bind () =
    if
      retiring_history
      && ((not (List.is_empty recovery.appended))
          || not (Int.equal first_sequence recovery.next_sequence))
    then
      Error
        (P.Error.create
           Conflict
           ~message:
             "save the edit first, then continue to reconcile retained invocation \
              outcomes"
           ~retryable:false
           ())
    else Ok ()
  in
  let deltas =
    (if retiring_history
     then []
     else [ Session_delta.History_block_reserved (Int64.of_int recovery.next_sequence) ])
    @ recovery.deltas
  in
  let payloads =
    if List.is_empty recovery.appended
    then []
    else [ P.Event.Durable.Payload.History_appended recovery.appended ]
  in
  Ok { deltas; payloads }
;;

let deltas t = t.deltas
let payloads t = t.payloads
