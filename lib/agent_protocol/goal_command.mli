(** Bounded operator-command data. Decoded IDs and source claims grant
    no authority. The actor authenticates the original caller and current host
    policy independently. No command enables or schedules execution. *)
module Mutation : sig
  type t =
    | Create of
        { id : Id.Goal.t
        ; objective : Goal.Objective.t
        ; budget : Goal.Budget.t
        }
    | Revise_objective of Id.Goal.t * Goal.Objective.t
    | Record_progress of Id.Goal.t * Goal.Progress.t
    | Hold of Id.Goal.t * Goal.Hold.t
    | Change_budget of Id.Goal.t * Goal.Budget.t
    | Adopt_source of Id.Goal.t
    | Abandon of Id.Goal.t * Goal.Reason.t
  [@@deriving sexp_of]

  val goal_id : t -> Id.Goal.t
  val needs_source : t -> bool
end

type t = private
  { session_id : Id.Session.t
  ; generation : int
  ; source : Run_source.t option
  ; expected_catalog_revision : int64
  ; expected_goal_revision : int64 option
  ; key : Idempotency_key.t
  ; mutation : Mutation.t
  }

(** Source is required for create/edits/adoption and absent for safe Hold/Abandon.
    Safe controls therefore remain usable after moderator removal. Their generation
    and both applicable CAS frontiers still refer to the actual current actor.
    Abandon changes objective disposition; it does not cancel outstanding Run work. *)
val create
  :  session_id:Id.Session.t
  -> generation:int
  -> source:Run_source.t option
  -> expected_catalog_revision:int64
  -> expected_goal_revision:int64 option
  -> key:Idempotency_key.t
  -> mutation:Mutation.t
  -> (t, Error.t) result

val digest : t -> (string, Error.t) result
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
