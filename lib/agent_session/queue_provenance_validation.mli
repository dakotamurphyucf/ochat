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

(** Validate after all ordered deltas or standalone snapshot decoding. Current
    lifetime Running/unretired failed captures name the exact head; completed or
    retired captures precede its frontier. Terminal history can outlive that
    lifetime only with a strictly later retained installation epoch, never by
    renewing permission. Retained queue producer references must remain present,
    even after terminal disposition or ordinary pruning. *)
val validate : t -> (unit, Agent_protocol.Error.t) result

(** Validate immutable schedule captures and queue frontiers. A queue lifetime
    currently cannot be removed or replaced by a different source lifetime.
    Explicit source/reset carrier handling belongs to its live integration;
    observer strings and decoded installation claims never erase protection. *)
val validate_transition : previous:t -> t -> (unit, Agent_protocol.Error.t) result

(** Validate a fresh positive Running claim at its ordered admission prefix.
    Uses the exact previous head (or unavailable structural bootstrap), never
    the receipt's asserted origin. The complete before/after validator rechecks
    against original facts after all leaves; this prefix grants no authority. *)
val validate_event_admission
  :  t
  -> Agent_protocol.Moderator_execution.t
  -> (unit, Agent_protocol.Error.t) result

(** Bound the complete optional occurrence child in an original/merged receipt,
    including unfamiliar fields. Other receipt semantics remain in its codec. *)
val validate_encoded_event : Jsonaf.t -> (unit, Agent_protocol.Error.t) result

(** Keep positive Running receipts and same-lifetime unretired failures until
    truthful termination/retirement. A failed terminal may follow ordinary
    retention only after actual retained installation progression makes its old
    occurrence unavailable. Settled terminal references remain validated by the
    final carrier. Also preserves any retained positive capture exactly.
    This narrow guard is shared by whole-state replacement and final admission. *)
val validate_event_retention : previous:t -> t -> (unit, Agent_protocol.Error.t) result
