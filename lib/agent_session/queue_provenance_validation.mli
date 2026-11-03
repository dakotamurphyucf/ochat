open! Core

(** Complete durable facts, not a live owner or an execution permit. Absence of
    metadata on historical checkpoints and schedules remains unavailable. *)
type t =
  { session_id : Agent_protocol.Id.Session.t
  ; generation : int
  ; revision : int64
  ; moderator : Jsonaf.t option
  ; runs : Run_state.t option
  ; jobs : Agent_protocol.Job.t list
  ; schedules : Agent_protocol.Schedule.t list
  ; invocations : Agent_protocol.Invocation.t list
  ; events : Agent_protocol.Moderator_execution.t list
  }

(** Validate after all ordered deltas. Retained queue producer references must
    remain present, even after terminal disposition or ordinary pruning. *)
val validate : t -> (unit, Agent_protocol.Error.t) result

(** Validate immutable schedule captures and queue frontiers. A queue lifetime
    currently cannot be removed or replaced by a different source lifetime.
    Explicit source/reset carrier handling belongs to its live integration;
    observer strings and decoded installation claims never erase protection. *)
val validate_transition : previous:t -> t -> (unit, Agent_protocol.Error.t) result
