open! Core
module P = Agent_protocol

type t =
  { enqueue : Pending_transition.t
  ; adoption : Pending_transition.t
  ; entries : P.History.entry list
  ; accepted : bool
  ; deferred_payloads : P.Event.Durable.Payload.t list
  }

let prepare state ~enqueue ~(entry : P.History.entry) ~runtime_admission_open =
  let open Result.Let_syntax in
  let%bind queued = Pending_plan.apply (Pending_transition.plan enqueue) state in
  let%map adoption =
    Pending_transition.prepare
      queued
      ~change:(Adopt { boundary = Idle_start; runtime_admission_open })
      ~retention:Pending_disposition.Retention.default
      ~archive:None
      ~limits:Session_delta.native_limits
  in
  let entries = Pending_plan.adopted_entries (Pending_transition.plan adoption) in
  let accepted =
    List.exists entries ~f:(fun candidate -> P.History.Id.equal candidate.id entry.id)
  in
  { enqueue
  ; adoption
  ; entries
  ; accepted
  ; deferred_payloads =
      (if accepted then [] else [ P.Event.Durable.Payload.History_message_deferred entry ])
  }
;;

let entries t = t.entries
let accepted t = t.accepted
let enqueue_delta t = Pending_transition.delta t.enqueue

let turn_deltas t =
  [ Pending_transition.delta t.enqueue; Pending_transition.delta t.adoption ]
;;

let deferred_payloads t = t.deferred_payloads
