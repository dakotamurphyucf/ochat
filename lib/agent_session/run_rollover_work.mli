(** Pure ownership planning for one exact foreground rollover successor.
    Durable work records constrain ownership without supplying live authority. *)

type owner =
  | Unowned
  | Unresolved of Agent_protocol.Run.t
  | Retired

val owner
  :  Session_state.t
  -> predecessor:Agent_protocol.Id.Operation.t
  -> (owner, Agent_protocol.Error.t) result

(** Return the same unresolved Run before dispatch as after work transfer.
    Known retired, relinquished and settled predecessors refuse dispatch. *)
val dispatch_owner
  :  Session_state.t
  -> predecessor:Agent_protocol.Id.Operation.t
  -> (Agent_protocol.Run.t option, Agent_protocol.Error.t) result

val prepare
  :  Session_state.t
  -> predecessor:Agent_protocol.Id.Operation.t
  -> successor:Agent_protocol.Id.Operation.t
  -> now:Agent_protocol.Timestamp.t
  -> ((Agent_protocol.Id.Run.t * Run_state.t) option, Agent_protocol.Error.t) result

val adds_operation_relinquishment : previous:Run_state.t option -> Run_state.t -> bool

(** Validate a proposed Run index against the exact pure transfer. Reuse the
    proposed update time under the ordinary monotonic Run transition rules. *)
val validate
  :  Session_state.t
  -> predecessor:Agent_protocol.Id.Operation.t
  -> successor:Agent_protocol.Id.Operation.t
  -> Run_state.t
  -> (unit, Agent_protocol.Error.t) result
