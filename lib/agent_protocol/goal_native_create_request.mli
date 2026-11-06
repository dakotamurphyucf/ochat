(** Immutable metadata-create DATA. No existing Goal revision, creator,
    session identity or execution permission is supplied. The Actor derives its
    real session/principal and captures the actual installed source independently.
    This request never changes another Goal's budget/account or enables work. *)
type t

val create
  :  generation:int
  -> source:Run_source.t
  -> goal_id:Id.Goal.t
  -> expected_catalog_revision:int64
  -> objective:Goal.Objective.t
  -> budget:Goal.Budget.t
  -> key:Idempotency_key.t
  -> (t, Error.t) result

val generation : t -> int
val source : t -> Run_source.t
val goal_id : t -> Id.Goal.t
val expected_catalog_revision : t -> int64
val objective : t -> Goal.Objective.t
val budget : t -> Goal.Budget.t
val key : t -> Idempotency_key.t
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
