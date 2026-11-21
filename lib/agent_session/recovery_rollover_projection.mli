(** Fixed-size public projections for the local operator control. *)
val receipt
  :  Recovery_rollover.Receipt.t
  -> (Agent_protocol.Recovery_rollover_control.Receipt.t, Agent_protocol.Error.t) result

val capacity
  :  Session_state.t
  -> Session_context_capacity.status
  -> Agent_protocol.Recovery_rollover_control.Capacity.t
