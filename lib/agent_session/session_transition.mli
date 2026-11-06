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
    frames and interpreter state are not published.

    When the instruction receipt frontier changes, coalesce a final
    [Session_updated] and attach its body-free typed instruction summary. Exact
    retained retries do not call this transition owner.

    [inherited_turn] is pure receipt DATA consumed exactly once by the actual
    Host_turn_tracking Handle owner, at the same canonical edge and before final
    State validation. It grants no root reservation or worker-start authority. *)
val apply
  :  ?inherited_turn:Host_turn_tracking.Inherited.t
  -> now:Agent_protocol.Timestamp.t
  -> Session_state.t
  -> delta:Session_delta.t
  -> payloads:Agent_protocol.Event.Durable.Payload.t list
  -> (t, Agent_protocol.Error.t) result

(** Same single complete validation/event/counter path as apply. Retains the
    real tracking-owned edge only after all ordered, publication, State and event
    validation succeeds. Exact immediate ledgers may differ from the transaction
    original/final ledgers. A runtime owner must retain this exact edge with this
    same candidate and may acknowledge it only after actual canonical persistence
    and State installation, with the original writer-attempt/phase custody. *)
val apply_inherited
  :  inherited_turn:Host_turn_tracking.Inherited.t
  -> now:Agent_protocol.Timestamp.t
  -> Session_state.t
  -> delta:Session_delta.t
  -> payloads:Agent_protocol.Event.Durable.Payload.t list
  -> (t * Host_turn_tracking.Inherited_edge.t, Agent_protocol.Error.t) result

val lifecycle
  :  now:Agent_protocol.Timestamp.t
  -> Session_state.t
  -> desired:Agent_protocol.Session.desired_state
  -> observed:Agent_protocol.Session.observed_state
  -> (t, Agent_protocol.Error.t) result
