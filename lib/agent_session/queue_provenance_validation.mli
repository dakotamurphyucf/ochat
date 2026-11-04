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
    even after terminal disposition or ordinary pruning. A Schedule's original
    Run attribution must name a retained same-session/full-source Run; absence of
    a Schedule work occurrence is allowed because attribution creates no work.
    Any actual retained Schedule owner must agree. Saved historical attribution
    need not match the actor's newer installation; delivery checks currentness. *)
val validate : t -> (unit, Agent_protocol.Error.t) result

(** Validate immutable schedule captures and queue frontiers. Retiring a queue
    requires its exact original installation, one validated epoch rotation, the
    same session and unchanged or exactly incremented generation. A destination
    queue starts at ordinal zero with the actual new full source and validated
    producer references. Prior Running queue occurrences cannot be retired here;
    truthful terminal history keeps its existing later-epoch retention rules.
    Complete raw checkpoint preservation is checked first: protected unfamiliar
    data cannot disappear or move to the new lifetime. These durable facts grant
    no live execution permission. *)
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
