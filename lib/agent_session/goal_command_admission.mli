(** Actual original operator proof and current non-yielding host policy checked
    before receipt disclosure or metadata planning. These values grant no Run
    custody. The sole actor commits the returned ordinary session delta. *)
type t =
  { receipt : Goal_command_receipt.t
  ; delta : Session_delta.t option
  }

val check_write
  :  Operator_authorization.t
  -> authorize:(Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> Session_state.t
  -> (unit, Agent_protocol.Error.t) result

val read
  :  Session_state.t
  -> actor:Operator_authorization.t
  -> authorize:(Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> (Agent_protocol.Goal.t list, Agent_protocol.Error.t) result

val prepare
  :  Session_state.t
  -> actor:Operator_authorization.t
  -> authorize:(Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> session:Agent_protocol.Session_ref.t
  -> request:Goal_command.t
  -> now:Agent_protocol.Timestamp.t
  -> (t, Agent_protocol.Error.t) result
