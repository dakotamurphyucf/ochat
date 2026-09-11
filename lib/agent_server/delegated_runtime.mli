(** A generated runtime's construction/activity switch inside a retained parent
    runtime lease. The generated builder must install Runtime_activity using the
    supplied switch for every executable path. This supplies lifetime ownership,
    not admission or permission to invoke native functions. *)

(** Build while the parent lease is retained. Closing the returned runtime cancels
    and joins its switch before original runtime cleanup and parent lease release.
    Parent cancellation joins the same work and invokes [on_revoked] after the
    switch ends. That callback must use actor-only lifecycle operations, never
    acquire the child's runtime-owner mutex or join this scope itself.

    Preparation failures and caller cancellation do not publish a live runtime.
    Cleanup failures remain visible through close and the execution guard. *)
val prepare
  :  sw:Eio.Switch.t
  -> parent:Runtime_owner.t
  -> on_revoked:(unit -> (unit, Agent_protocol.Error.t) result)
  -> build:
       (sw:Eio.Switch.t
        -> Agent_session.Runtime_builder.t
        -> (Agent_session.Runtime_builder.t, Agent_protocol.Error.t) result)
  -> (Agent_session.Runtime_builder.t, Agent_protocol.Error.t) result
