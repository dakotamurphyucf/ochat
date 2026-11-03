open! Core

(** Process-local interval phase tied to one exact definition and cursor. No IO,
    ambient clock, mutation, sexp/JSON decoder or surviving process origin. *)
type t

module Snapshot : sig
  type t

  val create
    :  wall:Time_ns.Alternate_sexp.t
    -> elapsed_ns:int64
    -> (t, Agent_protocol.Error.t) result

  val wall : t -> Time_ns.Alternate_sexp.t
  val elapsed_ns : t -> int64
end

(** Reanchor the retained wall deadline under a new process clock. Overdue missed
    slots/grace must first use explicit wall reconciliation in the due planner;
    this capture alone cannot reconstruct historical elapsed lateness. *)
val recover
  :  Recurrence_definition.t
  -> cursor:Recurrence_rules.Cursor.Interval.t
  -> snapshot:Snapshot.t
  -> (t, Agent_protocol.Error.t) result

val matches
  :  t
  -> definition:Recurrence_definition.t
  -> cursor:Recurrence_rules.Cursor.Interval.t
  -> bool

(** Fixed-rate count uses the captured period; fixed-delay reports at most one
    initial deadline. Callers cannot substitute another rule or period. *)
val due_count : t -> snapshot:Snapshot.t -> int64

val lateness_ns : t -> snapshot:Snapshot.t -> int64 option

(** Verify the consumed range/next cursor, then advance the same live elapsed
    phase. The actor installs the returned phase only after acknowledged save. *)
val advance : t -> count:int64 -> (t option, Agent_protocol.Error.t) result
