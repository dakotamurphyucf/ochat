open! Core

type t =
  { state : Session_state.t
  ; delta : Session_delta.t
  ; events : Agent_protocol.Event.Durable.t list
  }

(** Apply one durable state change. If its effective conversation or halt state
    changes, append a rendering-neutral [moderator.overlay_changed] event at the
    same revision, after the supplied events. Public run evidence changes append
    one [session.updated] event unless the caller already supplied it, so existing
    session subscriptions can refresh their authorized run views. Private delivery
    frames and interpreter state are not published. *)
val apply
  :  now:Agent_protocol.Timestamp.t
  -> Session_state.t
  -> delta:Session_delta.t
  -> payloads:Agent_protocol.Event.Durable.Payload.t list
  -> (t, Agent_protocol.Error.t) result

val lifecycle
  :  now:Agent_protocol.Timestamp.t
  -> Session_state.t
  -> desired:Agent_protocol.Session.desired_state
  -> observed:Agent_protocol.Session.observed_state
  -> (t, Agent_protocol.Error.t) result
