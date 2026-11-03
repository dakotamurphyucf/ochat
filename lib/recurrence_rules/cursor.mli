open Core

(** Cursors are frontiers within an immutable rule revision, not globally unique
    recurrence identities or authority. The durable owner must bind each cursor to
    its definition revision and replace/reset it when that definition changes.
    Codecs validate local shape/range; they cannot validate that external binding. *)

module Interval : sig
  type t [@@deriving equal, sexp_of]

  type due_range =
    { first_slot : int64
    ; last_slot : int64
    ; count : int64
    ; next : t option
    }
  [@@deriving sexp_of]

  val create : first_due:Time_ns.Alternate_sexp.t -> t
  val slot : t -> int64
  val due_at : t -> Time_ns.Alternate_sexp.t

  (** Pure fixed-rate consumption plan for a positive count, for example from
      Clock.due_count on the live elapsed phase. No per-missed-slot loop. The owner
      commits cursor and clock-phase advance together. Range.next=None means the
      following endpoint/slot is exhausted; unrepresentable consumed endpoints
      return an error. Fixed_delay requires completion evidence instead. *)
  val consume_count
    :  t
    -> rule:Rule.Interval.t
    -> count:int64
    -> (due_range, Timing_error.t) Result.t

  (** Arithmetic fixed-rate range for restart wall reconciliation, including
      due_at=now. During live execution use Clock.due_count + consume_count so a
      wall correction cannot alter the elapsed phase. None means no slot due;
      range.next=None means timestamp/slot exhaustion. Fixed_delay is rejected. *)
  val due_through
    :  t
    -> rule:Rule.Interval.t
    -> now:Time_ns.Alternate_sexp.t
    -> (due_range option, Timing_error.t) Result.t

  (** Next fixed-delay slot is based on actual committed completion, including
      failed completion if the owning policy permits it. No completion is inferred
      by this pure helper. A fixed-rate rule is rejected. *)
  val after_completion
    :  t
    -> rule:Rule.Interval.t
    -> completed_at:Time_ns.Alternate_sexp.t
    -> (t, Timing_error.t) Result.t

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Timing_error.t) Result.t
end

module Calendar : sig
  type t [@@deriving equal, sexp_of]

  module Occurrence : sig
    type t = private
      { date : Date.t
      ; ofday : Time_ns.Ofday.t
      ; fold : int
      ; at : Time_ns.Alternate_sexp.t
      ; shifted_gap : bool
      ; zone : string
      ; provenance : Zone.Provenance.t
      }
    [@@deriving equal, sexp_of]
  end

  type search =
    | Found of Occurrence.t * t
    | Continue of t
    | End
  [@@deriving sexp_of]

  val create : Rule.Calendar.t -> t

  (** max_steps bounds newly examined dates; a retained speculative candidate is
      separately revalidated against the actual rule/zone. Search retains at most
      one earliest UTC candidate and emits it only after a lower bound excludes
      earlier candidates from all later dates. This handles folds interleaved
      across civil dates. Continue preserves that bounded search, not no-match.
      After emission search restarts at the earliest possibly relevant local date,
      filtering timestamps <=last_at. Equal UTC instants coalesce to the earlier
      local label. A changed zone provenance discards speculative lookahead and
      restarts from last_at; already emitted occurrence identities remain unchanged.
      Out_of_coverage is explicit. Cursors admit partially representable civil
      days and one exhausted-date sentinel beyond the final day. *)
  val next
    :  t
    -> rule:Rule.Calendar.t
    -> zone:Zone.t
    -> max_steps:int
    -> (search, Timing_error.t) Result.t

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Timing_error.t) Result.t
end
