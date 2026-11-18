(** Retained facts from one target Run index. The target owner checks the caller,
    the current index and the lifecycle before using these facts as evidence. *)
type t = private
  { origin : Recurrence_run_origin.t
  ; run : Agent_protocol.Run.t
  ; admission : Agent_protocol.Run_receipt.t
  ; terminal : Recurrence_run_reference.t option
  }

val equal : t -> t -> bool

(** Look up the Admission for the checked request and validate its origin
    against the retained Run. A missing Admission returns [Ok None]. A
    terminal Run requires its exact terminal receipt. *)
val find
  :  Run_state.t
  -> intent:Recurrence_execution_intent.t
  -> attempt:Recurrence_execution_attempt.t
  -> (t option, Agent_protocol.Error.t) result
