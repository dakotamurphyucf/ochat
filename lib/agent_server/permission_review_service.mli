open! Core

(** Durable execution boundary for configured model and external permission
    reviewers. *)

(** [review] redacts the invocation, commits and claims a session-owned job,
    invokes the pinned reviewer outside the actor, and commits a terminal job
    before returning its decision. Cancellation interrupts the claimed job and
    is re-raised. *)
val review
  :  now:Agent_protocol.Timestamp.t
  -> actor:Agent_session.Session_actor.t
  -> profile:Agent_session.Permission_policy.t
  -> Agent_session.Permission_policy.invocation
  -> ( Agent_session.Permission_reviewer.Decision.t
       , Agent_session.Permission_reviewer.Error.t )
       result

(** [resolve_timeout] applies the pinned profile's unattended fallback to an
    existing expired permission. Reviewer fallbacks use [review]; all choices
    are clamped to the permission's offered scopes, and the final resolution
    is committed through the actor compare-and-set path. *)
val resolve_timeout
  :  now:Agent_protocol.Timestamp.t
  -> actor:Agent_session.Session_actor.t
  -> profile:Agent_session.Permission_policy.t
  -> Agent_protocol.Permission.t
  -> (Agent_protocol.Permission.t, Agent_protocol.Error.t) result
