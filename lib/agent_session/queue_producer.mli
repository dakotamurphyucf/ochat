open! Core

(** Bounded data correlations, never execution or continuation authority.
    Job completion attempt zero records a truthful terminal disposition before
    the first claim, such as queued cancellation. Final carrier validation still
    requires the exact retained terminal Delivered Job and same attempt; this
    data creates no claim, execution or Run obligation. *)
type t =
  | Event of Agent_protocol.Id.Moderator_execution.t
  | Invocation of Agent_protocol.Id.Invocation.t
  | Job_completion of
      { id : Agent_protocol.Id.Job.t
      ; generation : int
      ; attempt : int
      }
  | Schedule_delivery of
      { id : Agent_protocol.Id.Schedule.t
      ; generation : int
      ; delivery_count : int
      }
  | Source_setup of { basis_revision : int64 }
[@@deriving equal, sexp]

val validate : t -> (unit, Agent_protocol.Error.t) result
val to_jsonaf : t -> Jsonaf.t
val of_jsonaf : Jsonaf.t -> (t, Agent_protocol.Error.t) result
val shape : Document_schema.Shape.t
