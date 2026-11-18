(** Request-local target data and its nonyielding final check. No scheduler or
    Run is started. Callers separately verify the controller writer. *)
type t

val value : t -> Agent_session.Recurrence_target.t
val check_current : t -> (unit, Agent_protocol.Error.t) result

val capture
  :  Session_factory.t
  -> controller:Session_registry.entry
  -> session:Agent_protocol.Session_ref.t
  -> actor:Operator_authorization.t
  -> authorize:(Agent_session.Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> Agent_protocol.Recurrence_control.Capture.selector
  -> (t, Agent_protocol.Error.t) result

(** Recheck the existing definition's frozen target without changing its pins. *)
val check_frozen
  :  Session_factory.t
  -> controller:Session_registry.entry
  -> session:Agent_protocol.Session_ref.t
  -> actor:Operator_authorization.t
  -> authorize:(Agent_session.Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> Agent_session.Recurrence_target.t
  -> (t, Agent_protocol.Error.t) result
