open! Core

(** Bounded data correlations, never execution or continuation authority. *)
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
