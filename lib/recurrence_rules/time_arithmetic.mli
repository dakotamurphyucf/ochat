open Core

(** Checked absolute arithmetic, without narrowing a duration to a signed Int63
    span. Endpoints must fit [Time_ns]; a difference may span its entire range. *)
val nanoseconds : Time_ns.t -> int64

val of_nanoseconds : int64 -> (Time_ns.t, Timing_error.t) Result.t
val add_ns : Time_ns.t -> int64 -> (Time_ns.t, Timing_error.t) Result.t
val add_ms : Time_ns.t -> int64 -> (Time_ns.t, Timing_error.t) Result.t
val positive_ms_to_ns : int64 -> (int64, Timing_error.t) Result.t
val local_scalar : Date.t -> Time_ns.Ofday.t -> (Time_ns.t, Timing_error.t) Result.t
val utc_parts : Time_ns.t -> Date.t * Time_ns.Ofday.t

(** Raw bounds of a civil day, permitting the first partially representable day
    through one exhausted-day sentinel after the last. These Int64 boundaries may
    lie outside Time_ns; they support bounded ordering proofs, not timestamps. *)
val day_bounds_ns : Date.t -> (int64 * int64, Timing_error.t) Result.t

(** Stable UTC-only textual codecs. No local zone is loaded or inferred. *)
val to_json : Time_ns.t -> Jsonaf.t

val of_json : Jsonaf.t -> (Time_ns.t, Timing_error.t) Result.t
