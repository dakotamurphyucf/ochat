(** Method-level authorization. Session actors perform a second current-state
    attachment check for mutations. *)

val authorize
  :  Agent_protocol.Principal.t
  -> Agent_protocol.Command.t
  -> (unit, Agent_protocol.Error.t) result
