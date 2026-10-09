open! Core
module P = Agent_protocol

type t =
  { operation : P.Operation.t
  ; deltas : Session_delta.t list
  ; payloads : P.Event.Durable.Payload.t list
  }

let create
      ?(pending_retention = None)
      ?(runtime_admission_open = true)
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
  let%bind pending =
    if not adopt_deferred
    then Ok None
    else (
      let%bind retention =
        match pending_retention with
        | Some retention -> Ok retention
        | None ->
          Pending_disposition.Retention.create
            ~max_records:Staged_notifications.default_limits.max_retained
      in
      Pending_transition.prepare
        state
        ~change:(Adopt { boundary = Idle_start; runtime_admission_open })
        ~retention
        ~archive:None
        ~limits:Session_delta.native_limits
      |> Result.map ~f:Option.some)
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
    Option.value_map pending ~default:deltas ~f:(fun pending ->
      Pending_transition.delta pending :: deltas)
  in
  let payloads =
    Option.value_map pending ~default:[] ~f:(fun pending ->
      match Pending_plan.adopted_entries (Pending_transition.plan pending) with
      | [] -> []
      | entries -> [ P.Event.Durable.Payload.History_appended entries ])
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
