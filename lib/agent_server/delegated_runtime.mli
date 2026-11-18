(** Existing protocol-error projection used by the actual Driver closer.
    Constructing this exception grants no cleanup or disposal authority. *)
exception Cleanup_failed of Agent_protocol.Error.t

module Cleanup : sig
  (** One original driver invocation, supplied before fork. Creating-domain
      access only; reuse/foreign-domain use is an invariant violation. These
      read-only observations never stop work, retry cleanup or grant source
      authority. [join_original] separately uses the existing original stop/join. *)
  type t

  module Local : sig
    type t

    type status =
      | Joined
      | Failed
      | Incomplete
    [@@deriving equal, sexp_of]

    (** Actual child Switch release, original runtime close and revocation only.
        Work/build primary is independently retained in observation. Genuine
        never-entered local work owns no Switch report. Empty failures alone do
        not establish Joined; missing entered/close evidence is Incomplete. *)
    val status : t -> status

    val observation
      :  t
      -> (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t

    val switch_outcome
      :  t
      -> (unit, Agent_protocol.Error.t) result Eio.Switch.Outcome.t option

    val release : t -> Eio.Switch.Release_report.t option

    (** None is not by itself Not_created: use the exact Adoption fact. *)
    val runtime_close : t -> unit Agent_session.Close_observation.t option

    val revocation
      :  t
      -> (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t option

    (** One original Switch invocation contributes at most one coarse group.
        Its selected aggregate/result and supporting actual release failures are
        retained together, without parsing or deduplicating aggregate leaves.
        These are opaque context parts, not a unique leaf count or constituent
        provenance. Full raw outcomes and exact release traces remain separately
        in [release] and [switch_outcome]. *)
    val diagnostics : t -> Agent_session.Cleanup_diagnostic.t list
  end

  (** Original adopted runtime and SAME original close observation. The running
      case also denotes unavailable original close evidence after an exceptional
      finalizer escape; terminal control alone cannot supply that evidence. *)
  module Adoption : sig
    type t =
      | Original_running
      | Not_created
      | Closed_runtime of
          { runtime : Agent_session.Runtime_builder.t
          ; observation : unit Agent_session.Close_observation.t
          }
  end

  val create : unit -> t

  (** Local appears after its actual original finalizers. Terminal appears only
      after the original outer resource/Owner scopes exit. Previously returned
      values and carriers never change, including after factual ancestor repair. *)
  val local : t -> Local.t option

  val terminal
    :  t
    -> (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t option

  val adoption : t -> Adoption.t

  module Normalization : sig
    type t

    val original
      :  t
      -> (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t

    val selected : t -> Agent_session.Cleanup_diagnostic.t
  end

  module Original : sig
    type t

    val local : t -> Local.t

    val terminal
      :  t
      -> (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t

    (** Actual original outer invocation escape or unattributed negative, captured
        once at that boundary. Known callback/ancestry observations forward their
        own groups instead. Normalized outer cancellation is not a fixed failure. *)
    val outer_control
      :  t
      -> (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t option

    (** Some only at the actual original outer Cancelled-to-Local branch, retaining
        its pre-normalization observation and exact selected carrier. This is not
        cleanup/custody proof and never authorizes discarding Local, release,
        runtime-close, revocation or factual ancestry negatives. *)
    val normalization : t -> Normalization.t option
  end

  (** Use exactly the original stop signal and finished promise. A normal join
      returns Original DATA even when its frozen terminal is negative; an actual
      signal/await escape is an independently observed outer negative. Foreign
      creating-domain or not-started use returns a lifecycle Error before effects.
      This never creates a driver or replays its build, callbacks or finalizers.
      A partial constructor must first prove this original join cannot await its
      own running constructor; missing Local alone is no such proof. *)
  val join_original
    :  t
    -> (Original.t Agent_session.Close_observation.t, Agent_protocol.Error.t) result
end

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
    The actual child switch retains its queued-release report. A cancellation
    raised by a release hook, [on_revoked] or the original runtime's close cannot
    be normalized into successful close. The terminal outcome is frozen after
    original close; duplicate close observes that same outcome without repeating
    hooks or the original close. The execution guard rejects a terminal runtime.

    Work failures preserve the actual switch aggregate and its raw backtrace;
    callback cancellation induced by a worker never replaces that aggregate.
    Original close keeps [Core.Exn.protect] finalizer precedence: its failure
    overrides a returned result, and two raised failures retain [Core.Exn.Finally].
    The original close outcome is retained separately. These are child-runtime
    facts only: the ordinary parent lease projection does not certify its outer
    Owner finalizers or an Independent parent-resource graph/switch bracket. *)
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
    construction switch alive through its callback and cleanup. The resource
    bundle may retain the same borrow's currentness check alongside its resources;
    neither may escape that retained scope. The child uses
    the same activity/join/terminal-guard implementation as [prepare]. No ancestor
    execution runtime need be loaded. Current lifetime/delegation authorization,
    source verification and parent policy mediation remain the host's duty. *)
val prepare_resources
  :  with_resources:
       (('resources -> (unit, Agent_protocol.Error.t) result)
        -> (unit, Agent_protocol.Error.t) result)
  -> sw:Eio.Switch.t
  -> on_revoked:(unit -> (unit, Agent_protocol.Error.t) result)
  -> build:
       (sw:Eio.Switch.t
        -> 'resources
        -> (Agent_session.Runtime_builder.t, Agent_protocol.Error.t) result)
  -> (Agent_session.Runtime_builder.t, Agent_protocol.Error.t) result

(** Paired counterpart of prepare_resources through the SAME driver. Supply its
    fresh original holder before any fork or admission; Factory's early partial
    descriptor may read it after failure. Actual synchronous fork refusal records
    a genuine never-entered terminal, not a fabricated Switch report. A callback
    observation is forwarded before raw projection, retaining actual explicit
    selected origin, raw aggregate/Finally precedence and full release evidence.
    The resources and their checks remain scoped to with_resources' callback. *)
val prepare_resources_reported
  :  cleanup:Cleanup.t
  -> with_resources:
       (('resources
         -> (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t)
        -> (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t)
  -> sw:Eio.Switch.t
  -> on_revoked:(unit -> (unit, Agent_protocol.Error.t) result)
  -> build:
       (sw:Eio.Switch.t
        -> 'resources
        -> (Agent_session.Runtime_builder.t, Agent_protocol.Error.t) result)
  -> (Agent_session.Runtime_builder.t, Agent_protocol.Error.t) result

(** The legacy arbitrary with_resources CPS is an opaque raw callback boundary;
    it does not promise paired provenance. First-party observed composition uses
    prepare_resources_reported. Existing direct prepare/prepare_independent routes
    internally retain their exact original Owner leases through the same private
    ancestry helper; their legacy close still projects the frozen original driver
    result and does not gain authority to replay or repair arbitrary cleanup. *)
