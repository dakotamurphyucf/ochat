(** Exact factual Invocation change owned by an already authenticated constructor
    callback. This validates one actual claim/result row and preserves every other
    retained row, including its order. It grants no admission or authority; the
    actor must separately own the physical callback and original control basis. *)
val validate
  :  previous:Session_state.t
  -> incoming:Session_state.t
  -> before:Agent_protocol.Invocation.t option
  -> after:Agent_protocol.Invocation.t
  -> (unit, Agent_protocol.Error.t) result
