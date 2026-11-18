(** Private joint data candidate. The existing Frontier owner validates and
    materializes this whole value before publication. This module reads no clock,
    observes no target and grants no runtime authority. *)
type t =
  { state : Recurrence_frontier_records.State.t
  ; cursor : Recurrence_cursor.t
  ; result : Recurrence_settlement.t
  }

val import
  :  Recurrence_frontier_records.State.t
  -> definitions:Recurrence_definition.t list
  -> cursor:Recurrence_cursor.t
  -> origin:Recurrence_run_origin.t
  -> terminal:Recurrence_run_reference.t option
  -> automatic:Recurrence_settlement.Automatic.t
  -> (t, Agent_protocol.Error.t) result

(** Rearm only the exact definitive basis matching an awaiting cursor. The Entry
    owner restricts this operation to a genuine new enable from an ordinary
    operator/controller pause; duplicate enable and review holds never use it.
    The returned hold is data, not permission to initialize or launch work. *)
val resume
  :  Recurrence_frontier_records.State.t
  -> definition:Recurrence_definition.t
  -> cursor:Recurrence_cursor.t
  -> ( Recurrence_frontier_records.State.t
       * Recurrence_cursor.t
       * Recurrence_settlement.Next.t
       , Agent_protocol.Error.t )
       result
