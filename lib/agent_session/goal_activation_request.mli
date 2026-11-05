(** Immutable command DATA. Neither a request nor its intent digest grants Goal
    or Run authority. Exact original Run identity and association intent are
    reconciled independently; adding/removing/replacing a Goal conflicts. *)
type t

val create
  :  goal_id:Agent_protocol.Id.Goal.t
  -> expected_goal_revision:int64
  -> expected_catalog_revision:int64
  -> request:Run_admission_request.Headless.t
  -> (t, Agent_protocol.Error.t) result

val request : t -> Run_admission_request.Headless.t
val intent : t -> Goal_episode_admission.intent
