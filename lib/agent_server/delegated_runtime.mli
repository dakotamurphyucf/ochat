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

(** Retain the exact parent resources across ordinary parent unload. The child
    activity switch has its own lifetime; closing the child or permanently closing
    the parent cancels and joins it before releasing those resources. Admission
    still requires a usable parent runtime. The host must separately authorize
    independent lifetime and install current delegation/revocation checks.
    This constructor does not implement stopped-ancestor restoration or widen
    supported inherited policy adapters. Same callback/cleanup rules as [prepare]. *)
val prepare_independent
  :  sw:Eio.Switch.t
  -> parent:Runtime_owner.t
  -> on_revoked:(unit -> (unit, Agent_protocol.Error.t) result)
  -> build:
       (sw:Eio.Switch.t
        -> Agent_session.Runtime_builder.t
        -> (Agent_session.Runtime_builder.t, Agent_protocol.Error.t) result)
  -> (Agent_session.Runtime_builder.t, Agent_protocol.Error.t) result

(** Build from independently reconstructed ancestor resources. [with_resources]
    must acquire the ancestors' resource-lifetime borrows and keep the resource
    construction switch alive through its callback and cleanup. The child uses
    the same activity/join/terminal-guard implementation as [prepare]. No ancestor
    execution runtime need be loaded. Current lifetime/delegation authorization,
    source verification and parent policy mediation remain the host's duty. *)
val prepare_resources
  :  with_resources:
       ((Agent_session.Runtime_builder.resources -> (unit, Agent_protocol.Error.t) result)
        -> (unit, Agent_protocol.Error.t) result)
  -> sw:Eio.Switch.t
  -> on_revoked:(unit -> (unit, Agent_protocol.Error.t) result)
  -> build:
       (sw:Eio.Switch.t
        -> Agent_session.Runtime_builder.resources
        -> (Agent_session.Runtime_builder.t, Agent_protocol.Error.t) result)
  -> (Agent_session.Runtime_builder.t, Agent_protocol.Error.t) result
