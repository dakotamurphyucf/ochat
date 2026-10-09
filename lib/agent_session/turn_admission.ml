open! Core
module P = Agent_protocol

type t =
  { operation : P.Operation.t
  ; deltas : Session_delta.t list
  ; payloads : P.Event.Durable.Payload.t list
  }

let create
      (state : Session_state.t)
      ~(operation : P.Operation.t)
      ~notification_wakes
      ~adopt_deferred
  =
  let open Result.Let_syntax in
  let%bind () =
    match operation.kind, operation.state with
    | Turn _, Starting when Int.equal operation.generation state.identity.generation ->
      Ok ()
    | _ ->
      Error
        (P.Error.invalid_request
           "turn admission requires a current-generation starting Turn")
  in
  let%bind () =
    if Option.is_none state.active_operation
    then Ok ()
    else
      Error
        (P.Error.create
           Conflict
           ~message:"a foreground operation is already admitted"
           ~retryable:false
           ())
  in
  let lifecycle : Session_state.Lifecycle.t =
    { desired = state.lifecycle.desired; observed = Running_turn operation.id }
  in
  let%map wakes =
    Result.all
      (List.map notification_wakes ~f:(fun wake ->
         P.Delivery.accept_wake wake ~operation_id:operation.id
         |> Result.map ~f:(fun wake -> Session_delta.Delivery_wake_changed wake)))
  in
  let deltas =
    [ Session_delta.Active_operation_changed (Some operation)
    ; Lifecycle_changed lifecycle
    ]
    @ wakes
    |> fun deltas ->
    if adopt_deferred then Session_delta.Deferred_entries_adopted :: deltas else deltas
  in
  let payloads =
    (if adopt_deferred && not (List.is_empty state.conversation.deferred_user_entries)
     then
       [ P.Event.Durable.Payload.History_appended state.conversation.deferred_user_entries
       ]
     else [])
    @ [ P.Event.Durable.Payload.Operation_started operation
      ; Session_state_changed
          { desired_state = lifecycle.desired; observed_state = lifecycle.observed }
      ]
  in
  { operation; deltas; payloads }
;;

let operation t = t.operation
let deltas t = t.deltas
let payloads t = t.payloads
