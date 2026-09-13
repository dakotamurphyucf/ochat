(** Add receipt reconciliation to the same durable transaction as input adoption,
    operation termination or administrative replacement. Replacement retains old
    identities as terminal receipts; it never makes an old key reusable. *)
val apply
  :  previous:Session_state.t
  -> state:Session_state.t
  -> delta:Session_delta.t
  -> payloads:Agent_protocol.Event.Durable.Payload.t list
  -> now:Agent_protocol.Timestamp.t
  -> (Session_state.t * Session_delta.t, Agent_protocol.Error.t) result
