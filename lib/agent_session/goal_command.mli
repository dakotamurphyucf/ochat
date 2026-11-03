(** Bounded internal operator-command data. Decoded IDs and source claims grant
    no authority. The actor authenticates the original caller and current host
    policy independently. No command enables or schedules execution. *)
module Mutation : sig
  type t =
    | Create of
        { id : Agent_protocol.Id.Goal.t
        ; objective : Agent_protocol.Goal.Objective.t
        ; budget : Agent_protocol.Goal.Budget.t
        }
    | Revise_objective of Agent_protocol.Id.Goal.t * Agent_protocol.Goal.Objective.t
    | Record_progress of Agent_protocol.Id.Goal.t * Agent_protocol.Goal.Progress.t
    | Hold of Agent_protocol.Id.Goal.t * Agent_protocol.Goal.Hold.t
    | Change_budget of Agent_protocol.Id.Goal.t * Agent_protocol.Goal.Budget.t
    | Adopt_source of Agent_protocol.Id.Goal.t
    | Abandon of Agent_protocol.Id.Goal.t * Agent_protocol.Goal.Reason.t
  [@@deriving sexp_of]

  val goal_id : t -> Agent_protocol.Id.Goal.t
  val needs_source : t -> bool
end

type t = private
  { session_id : Agent_protocol.Id.Session.t
  ; generation : int
  ; source : Agent_protocol.Run_source.t option
  ; expected_catalog_revision : int64
  ; expected_goal_revision : int64 option
  ; key : Agent_protocol.Idempotency_key.t
  ; mutation : Mutation.t
  }

(** Source is required for create/edits/adoption and absent for safe Hold/Abandon.
    Safe controls therefore remain usable after moderator removal. Their generation
    and both applicable CAS frontiers still refer to the actual current actor.
    Abandon changes objective disposition; it does not cancel outstanding Run work. *)
val create
  :  session_id:Agent_protocol.Id.Session.t
  -> generation:int
  -> source:Agent_protocol.Run_source.t option
  -> expected_catalog_revision:int64
  -> expected_goal_revision:int64 option
  -> key:Agent_protocol.Idempotency_key.t
  -> mutation:Mutation.t
  -> (t, Agent_protocol.Error.t) result

val digest : t -> (string, Agent_protocol.Error.t) result
