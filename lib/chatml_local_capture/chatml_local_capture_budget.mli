open Core

(** Invocation-owned host policy/accounting. Ordinary resource failures are
    Results; checkpoint callbacks never implement admission by raising. *)
module Limits : sig
  type t

  val create
    :  max_directory_entries:int
    -> max_sources:int
    -> max_source_bytes:int
    -> max_total_source_bytes:int
    -> max_path_components:int
    -> max_work_steps:int
    -> t Or_error.t

  val directory_entries : t -> int
  val sources : t -> int
  val source_bytes : t -> int
  val total_source_bytes : t -> int
  val path_components : t -> int
  val work_steps : t -> int
end

module Accounting : sig
  type t

  val directory_entries : t -> int
  val source_count : t -> int
  val source_bytes : t -> int
  val work_steps : t -> int
end

module Limit : sig
  type t =
    | Directory_entries
    | Source_count
    | Source_bytes
    | Total_source_bytes
    | Path_components
    | Work_steps
  [@@deriving equal, sexp_of]
end

type problem =
  | Exceeded of
      { limit : Limit.t
      ; maximum : int
      }
  | Overflow

type t

val create : Limits.t -> t
val remaining_entries : t -> int
val remaining_bytes : t -> int
val source_available : t -> bool
val read_allowance : t -> int
val check_source_slot : t -> (unit, problem) Result.t
val add_entries : t -> int -> (unit, problem) Result.t
val add_source : t -> bytes:int -> (unit, problem) Result.t

(** Nonnegative local stage units, checked before changing the owned counter.
    Reserved work is immutable in a published Accounting snapshot. *)
val reserve : t -> int -> (unit, problem) Result.t

val sum : int -> int -> (int, problem) Result.t

val reserve_ordering
  :  t
  -> count:int
  -> key_bytes:int
  -> passes:int
  -> (unit, problem) Result.t

val reserve_observation : t -> lookup_count:int -> (unit, problem) Result.t
val reserve_read : t -> byte_allowance:int -> components:int -> (unit, problem) Result.t
val reserve_scan : t -> bytes:int -> (unit, problem) Result.t
val snapshot : t -> Accounting.t
