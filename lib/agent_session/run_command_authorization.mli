(** Current original command authorization. The host policy must be non-yielding;
    it cannot replace the original proof or its current Send_messages scope.
    Both are checked before and after policy, including when policy revokes
    the original authentication before returning success. *)
val check
  :  Operator_authorization.t
  -> authorize:(Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> Session_state.t
  -> (unit, Agent_protocol.Error.t) result
