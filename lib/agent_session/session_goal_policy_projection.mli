(** Private adapter of actual State fields, no runtime authority. *)
val of_state
  :  Session_state.t
  -> (Goal_policy_ordered.Projection.t, Agent_protocol.Error.t) result

val begin_ : Session_state.t -> (Goal_policy_ordered.t, Agent_protocol.Error.t) result
