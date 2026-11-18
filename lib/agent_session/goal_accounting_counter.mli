(** Checked nonnegative lifetime counters. Constructing a number grants no
    accounting attribution or permission to reset an existing goal scope. *)
type t [@@deriving equal, sexp_of]

type error =
  | Negative
  | Exhausted
[@@deriving equal, sexp_of]

val zero : t
val of_int64 : int64 -> (t, error) result
val to_int64 : t -> int64
val add : t -> t -> (t, error) result
val succ : t -> (t, error) result
