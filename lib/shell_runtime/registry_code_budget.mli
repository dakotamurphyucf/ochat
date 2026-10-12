open Core

(** Invocation-owned logical work ledger. No wall/CPU/allocation proof is implied.
    A node, list cell or map admission costs one unit; byte fields are admitted and
    compared in chunks of at most 1024 bytes, charging before each chunk. *)
type t

module Error : sig
  type t =
    | Invalid_limit
    | Work_limit
  [@@deriving sexp, equal]
end

val run
  :  max_work_steps:int
  -> checkpoint:(unit -> unit)
  -> (t -> ('a, 'error) Result.t)
  -> (('a * int, 'error) Result.t, Error.t) Result.t

(** Charge before work, then poll the borrowed checkpoint and Eio cancellation.
    Ledger denials are invocation-affine; unrelated exceptions propagate with
    their original identity/backtrace. Neither the ledger nor callback escapes. *)
val work : t -> unit

val equal_string : t -> string -> string -> bool

(** Same chunked comparison for trusted construction helpers receiving the
    enclosing ledger's [work] callback. Charges the same base/chunk units and
    retains neither strings nor callback. This creates no independent ledger. *)
val equal_string_with_checkpoint : checkpoint:(unit -> unit) -> string -> string -> bool

(** Lexicographic ordering for invocation-scoped indexing. Common-prefix scans
    use the same charged/polled byte chunks as equality. *)
val compare_string : t -> string -> string -> int
