(** Actual genuine lifecycle observation for the current actor state. Durable
    reconciliation must commit before recurrence runtime/target activation;
    failed persistence keeps old births effectively fenced. Inert controller
    artifact decoding/reparsing need not be delayed. *)
type t =
  { delta : Session_delta.t option
  ; current : Recurrence_controller.observation
  }

val prepare
  :  Session_state.t
  -> controller:Recurrence_controller.t
  -> (t, Agent_protocol.Error.t) result

val check_current : t -> (unit, Agent_protocol.Error.t) result
