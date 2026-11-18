open Core

module Period : sig
  type t [@@deriving equal, sexp_of]

  (** Positive integer milliseconds, with an exactly representable int64
      nanosecond duration. Timestamp endpoints are checked separately. *)
  val create_ms : int64 -> (t, Timing_error.t) Result.t

  val milliseconds : t -> int64
  val nanoseconds : t -> int64
end

module Interval : sig
  type mode =
    | Fixed_rate
    | Fixed_delay
  [@@deriving equal, sexp]

  type t [@@deriving equal, sexp_of]

  val create : mode:mode -> period:Period.t -> t
  val mode : t -> mode
  val period : t -> Period.t
end

module Calendar : sig
  type pattern =
    | Daily
    | Weekdays of Day_of_week.t list
    | Month_days of int list
  [@@deriving equal, sexp]

  type gap =
    | Skip
    | At_gap_end
  [@@deriving equal, sexp]

  type repeat =
    | Earlier
    | Later
    | Both
  [@@deriving equal, sexp]

  type t [@@deriving equal, sexp_of]

  (** Day/month sets must be nonempty and duplicate-free. Ofday is in [00:00,
      24:00), start/end dates are inclusive and ordered, and zone is explicit.
      Invalid monthly dates are skipped. Defaults skip gaps and select the earlier
      repeated time. These are OChat rules, not cron or RRULE syntax. *)
  val create
    :  ?gap:gap
    -> ?repeat:repeat
    -> ?end_date:Date.t
    -> pattern:pattern
    -> ofday:Time_ns.Ofday.t
    -> zone:string
    -> start_date:Date.t
    -> unit
    -> (t, Timing_error.t) Result.t

  val pattern : t -> pattern
  val ofday : t -> Time_ns.Ofday.t
  val zone : t -> string
  val start_date : t -> Date.t
  val end_date : t -> Date.t option
  val gap : t -> gap
  val repeat : t -> repeat
  val matches : t -> Date.t -> bool
end

type t =
  | Interval of Interval.t
  | Calendar of Calendar.t
[@@deriving equal, sexp_of]

(** Strict version-1 codec. Missing/duplicate/unknown fields and invalid domain
    values fail before admission. No generated nonvalidating sexp decoder. *)
val to_json : t -> Jsonaf.t

val of_json : Jsonaf.t -> (t, Timing_error.t) Result.t
