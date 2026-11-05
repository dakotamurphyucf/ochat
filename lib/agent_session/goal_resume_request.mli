(** Immutable explicit operator-decision DATA for the current nonterminal Goal
    episode. It grants no Run, source, constructor or continuation authority and
    never describes a new Run admission. The actual Actor authenticates the
    retained physical binding and both original authorizations separately. *)
type t

(** All counters are nonnegative, [source] is validated and its generation must
    match [generation]. IDs and key use the same validation as their protocol
    constructors. These request fences remain exact across source preparation;
    later acknowledged bookkeeping does not rewrite the original request. *)
val create
  :  session_id:Agent_protocol.Id.Session.t
  -> generation:int
  -> goal_id:Agent_protocol.Id.Goal.t
  -> run_id:Agent_protocol.Id.Run.t
  -> source:Agent_protocol.Run_source.t
  -> expected_goal_revision:int64
  -> expected_catalog_revision:int64
  -> expected_run_revision:int64
  -> key:Agent_protocol.Idempotency_key.t
  -> (t, Agent_protocol.Error.t) result

val session_id : t -> Agent_protocol.Id.Session.t
val generation : t -> int
val goal_id : t -> Agent_protocol.Id.Goal.t
val run_id : t -> Agent_protocol.Id.Run.t
val source : t -> Agent_protocol.Run_source.t
val expected_goal_revision : t -> int64
val expected_catalog_revision : t -> int64
val expected_run_revision : t -> int64
val key : t -> Agent_protocol.Idempotency_key.t

(** Domain-separated SHA-256 of the complete canonical request. This is request
    identity only; no caller principal, receipt or live authority is inferred. *)
val digest : t -> (string, Agent_protocol.Error.t) result
