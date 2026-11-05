(** Immutable native objective/progress intent. Session and principal are
    host-derived. Generation and full source are frozen expected-fence DATA from
    an authenticated get; fresh staging compares them with the actual selected
    execution before translating this data to Goal_command. No enable, accounting
    charge or execution permission is represented here. *)
module Mutation : sig
  type t =
    | Revise_objective of Goal.Objective.t
    | Record_progress of Goal.Progress.t
  [@@deriving sexp_of]
end

type t

(** Generation and counters are nonnegative; full source generation must match.
    The key and goal identifier use their existing
    validated constructors. Wire decoding must call this same constructor. *)
val create
  :  generation:int
  -> source:Run_source.t
  -> goal_id:Id.Goal.t
  -> expected_catalog_revision:int64
  -> expected_goal_revision:int64
  -> key:Idempotency_key.t
  -> mutation:Mutation.t
  -> (t, Error.t) result

val generation : t -> int
val source : t -> Run_source.t
val goal_id : t -> Id.Goal.t
val expected_catalog_revision : t -> int64
val expected_goal_revision : t -> int64
val key : t -> Idempotency_key.t
val mutation : t -> Mutation.t
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
