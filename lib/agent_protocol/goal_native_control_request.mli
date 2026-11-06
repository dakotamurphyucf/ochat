(** Native safe controls only. Identity/current actor are host-derived.
    This does not expose automatic/operator Hold reasons, budget relaxation,
    source adoption, an Enabled mutation, or achievement. *)
module Mutation : sig
  type t =
    | Hold of Goal.Reason.t
    | Abandon of Goal.Reason.t
end

type t

val create
  :  generation:int
  -> goal_id:Id.Goal.t
  -> expected_catalog_revision:int64
  -> expected_goal_revision:int64
  -> key:Idempotency_key.t
  -> mutation:Mutation.t
  -> (t, Error.t) result

val generation : t -> int
val goal_id : t -> Id.Goal.t
val expected_catalog_revision : t -> int64
val expected_goal_revision : t -> int64
val key : t -> Idempotency_key.t
val mutation : t -> Mutation.t
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
