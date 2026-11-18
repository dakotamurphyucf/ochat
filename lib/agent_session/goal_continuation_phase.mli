(** The process-local phase of preparation for an existing Goal episode.
    These transitions carry no execution authority. The Actor establishes
    live custody, owns the writer and applies each result in its own lane. *)
module Cause : sig
  type t =
    | Resume_command
    | Pending_wake
    | Claimed_job
end

module Use : sig
  type t =
    | Fresh_effect
    | Registered_allocation
end

type 'binding t =
  | Preparing
  | Source_bound of 'binding
  | Writer_pending of 'binding
  | Ready of 'binding * Goal_command_receipt.t
  | Execution_ready of 'binding
  | Waiting of Goal_command_receipt.t * Agent_protocol.Run_wake.t
  | Closed of Goal_command_receipt.t option

val receipt : _ t -> Goal_command_receipt.t option

(** Close the preparation while retaining its known receipt, if any. The
    Actor separately closes the actual source and runtime custody. *)
val close : 'binding t -> 'binding t

(** Accept factual acknowledgement of the actual Resume writer. Only its
    same physical pending binding advances to Ready. A displaced phase keeps
    the receipt in Closed. The Actor separately checks live source custody. *)
val acknowledge
  :  'binding t
  -> binding:'binding
  -> receipt:Goal_command_receipt.t
  -> 'binding t

(** Check the phase against the current Goal state. Registered allocation is
    only for a previously admitted allocation selected by the Actor. In the
    claimed Job case the Actor must first verify the exact live Job scope. *)
val check_live
  :  _ t
  -> cause:Cause.t
  -> use:Use.t
  -> goal_state:Agent_protocol.Goal.State.t
  -> (unit, Agent_protocol.Error.t) result
