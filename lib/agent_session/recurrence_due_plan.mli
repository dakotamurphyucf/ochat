open! Core

(** Pure ephemeral exact-basis due plan. No IO, clocks, root lookup, scheduler,
    callback, target launch or terminal evidence. Stored data cannot recreate
    source/lifecycle authority; the actor rechecks those after preparation. *)
type t

type decision =
  | Unchanged of Recurrence_phase.t option
  | Candidate of t

(** Live fixed-rate timing/grace uses the matching process-local phase. Missing
    phase means explicit restart/adoption wall reconciliation before seeding a
    new elapsed anchor. Calendar uses captured UTC wall cutoff and bounded
    Cursor.Calendar.next grants. Existing Ready rows are never demoted merely
    because a new batch is collecting. Missing Zone produces a bounded issue;
    coverage is never clipped and UTC is never substituted.

    Fixed-delay may select its initial occurrence and enter Awaiting_completion;
    there is no rearm path without the actual terminal owner. Due/grace equality
    is inclusive. Actual dispatch must revalidate grace, current source/control,
    lifecycle and target admission even after this plan's selection succeeds. *)
val prepare
  :  definition:Recurrence_definition.t
  -> cursor:Recurrence_cursor.t
  -> frontier:Recurrence_frontier.t
  -> phase:Recurrence_phase.t option
  -> snapshot:Recurrence_phase.Snapshot.t
  -> zone:Recurrence_rules.Zone.t option
  -> (decision, Agent_protocol.Error.t) result

val cursor : t -> Recurrence_cursor.t
val frontier : t -> Recurrence_frontier.t
val next_phase : t -> Recurrence_phase.t option

(** Exact old definition/cursor/frontier basis, not just scalar revisions. The
    owning entry/catalog uses this before accepting the pair as one transition. *)
val matches
  :  t
  -> definition:Recurrence_definition.t
  -> cursor:Recurrence_cursor.t
  -> frontier:Recurrence_frontier.t
  -> bool
