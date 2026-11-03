(** Original operator authorization remains independent of stored metadata.
    Preparation may issue an actual store lifecycle observation; the actor must
    repeat [check_write] and [check_current] after preparation and immediately
    before publishing a candidate. It durably reconciles stale births first. *)
type t =
  { receipt : Recurrence_command_receipt.t
  ; delta : Session_delta.t option
  ; current : Recurrence_controller.observation option
  }

val check_write
  :  Operator_authorization.t
  -> authorize:(Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> Session_state.t
  -> (unit, Agent_protocol.Error.t) result

val check_current : t -> (unit, Agent_protocol.Error.t) result

val prepare
  :  Session_state.t
  -> controller:Recurrence_controller.t
  -> actor:Operator_authorization.t
  -> authorize:(Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> request:Recurrence_command.t
  -> now:Agent_protocol.Timestamp.t
  -> (t, Agent_protocol.Error.t) result

(** Read-only effective retirement, including archived or old births, never
    claims reconciliation has persisted and never grants target authority. *)
val read
  :  Session_state.t
  -> controller:Recurrence_controller.t
  -> actor:Operator_authorization.t
  -> authorize:(Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> (Recurrence_view.t list, Agent_protocol.Error.t) result
