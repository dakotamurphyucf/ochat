open Core

(** Injected process elapsed nanoseconds. This module reads neither clock nor IO.
    Elapsed values are nonnegative and belong to one process origin. *)
type snapshot

type t

val snapshot : wall:Time_ns.t -> elapsed_ns:int64 -> (snapshot, Timing_error.t) Result.t

(** Live delays follow elapsed time despite wall corrections. [deadline] is an
    audit projection from capture, not a subsequently recomputed wall alarm. *)
val after : snapshot -> delay:Rule.Period.t -> (t, Timing_error.t) Result.t

(** Reboot/process recovery has no surviving monotonic origin. Reanchor the
    nonnegative remaining wall duration; overdue deadlines are immediately due.
    The caller separately applies recurrence misfire policy. *)
val recover : snapshot -> deadline:Time_ns.t -> (t, Timing_error.t) Result.t

val deadline : t -> Time_ns.t
val is_due : t -> snapshot -> bool

(** Elapsed nanoseconds since this live deadline, including zero at equality.
    Returns None before the deadline. This does not consult the wall projection;
    restart lateness must be reconciled explicitly from retained wall metadata. *)
val lateness_ns : t -> snapshot -> int64 option

(** Number of fixed-rate deadlines due at this elapsed snapshot, including the
    anchor itself. Uses arithmetic, independent of current wall time. Return zero
    before the anchor. Combine with Cursor.Interval.consume_count, then advance
    this same phase by the consumed count at the durable owner's commit. *)
val due_count : t -> period:Rule.Period.t -> snapshot -> int64

(** Advance a live fixed-rate phase by integral periods, never from callback
    completion or the corrected wall clock. *)
val advance : t -> period:Rule.Period.t -> count:int64 -> (t, Timing_error.t) Result.t
