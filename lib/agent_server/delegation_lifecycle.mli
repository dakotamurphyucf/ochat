(** Owned descendant stop propagation shared by generated-session lifecycle
    hosts. This is an internal service, not a public session-ID authorization
    endpoint. The caller must hold the parent's creation/lifetime coordination
    and retain any borrowed parent runtime until this operation succeeds. *)

(** Resolve the current private relationship, require Owned or Invocation_owned lifetime, and
    atomically match the child's persisted reference before cancellation. Revoked
    and not-yet-linked admissions may still require cleanup; neither disposition
    prevents stopping their exact child. Independent children are not stopped.

    Uses the normal durable actor transition without attaching a synthetic client
    or granting approval rights. After acceptance, caller cancellation cannot
    bypass foreground/native/moderator/job cleanup. Waits outside actor/registry
    locks for quiescence, then cancels/joins child runtime leases and unloads it.
    Protects the initial ledger lookup too, so it can run from an already
    cancelled parent lease's cleanup. Data and history remain available; this
    does not delete or shut down the actor.

    The parent coordinator must exclude child start/reload through the whole
    operation, propagate failures, and must not release inherited resources after
    an error. Call outside the child's own retained runtime callback. *)
val stop_owned
  :  ?parent_stop_epoch:int64
  -> clock:_ Eio.Time.clock
  -> delegations:Agent_store.Delegation_store.t
  -> reference:Agent_store.Delegation_store.Reference.t
  -> actor:Agent_session.Session_actor.t
  -> runtime:Runtime_owner.t
  -> unit
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result
