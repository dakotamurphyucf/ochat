open! Core

(** Direct Actor protocol fixture, not a Factory relation/reservation proof.
    One real submitted authenticated child worker retains this SAME Caller until
    [f] exits. The preparation names its committed canonical call and immutable
    native registration; the public child Actor currentness check validates it.
    Callback return/error/cancellation is followed by actual child cancellation,
    worker exit and terminal join, preserving the primary failure. *)
type t

val request : t -> Agent_session.Script_tool_calls.preparation
val delegation : t -> Agent_protocol.Moderator_execution.delegation
val event : t -> Chat_response.Moderation.Event.t
val check_current : t -> (unit, Agent_protocol.Error.t) result

(** Bind the actual controller-free child's SAME live Caller and request through
    the public governance checks. No fabricated association or Factory issuer. *)
val governance
  :  t
  -> (Agent_session.Session_actor.Causal_governance.t, Agent_protocol.Error.t) result

val with_request
  :  env:Eio_unix.Stdenv.base
  -> parent:Agent_session.Session_actor.t
  -> (t -> 'a)
  -> 'a
