open Core

(** Private validated, bounded and self-contained future rule. Offsets are
    UTC-to-local seconds, not POSIX's reversed textual signs. *)
type t

val of_string
  :  string
  -> version:Tzif_reader.Version.t
  -> (t option, Zone_data_error.t) Result.t

(** Exact evaluation at any signed 64-bit POSIX second. Gregorian 400-year
    periodicity avoids overflowing seconds or narrowing arbitrary TZif times
    into Date/Time_ns. *)
val local_type : t -> at:int64 -> Tzif_reader.Local_type.t

(** Caller-derived finite year interval, within 1..9998 and at most 500 years.
    Includes designation/DST changes even when effective offsets do not change.
    Equal instants are coalesced with the right-continuous POSIX rule result. *)
val transitions
  :  t
  -> first_year:int
  -> last_year:int
  -> (Tzif_reader.Transition.t list, Zone_data_error.t) Result.t
