open! Core

(** One daemon-owned fair dispatcher for conversation and physically attached
    developer jobs. Developer candidates are observations only: the existing
    Root/Worker path makes every claim with the shared capacity ledger.
    Conversation jobs retain typed Completion results and pending delivery;
    their event/notification adapter remains separate from legacy model events.
    A generic worker retains its completion and capacity lease while retrying a
    rejected save, with a cancellable 50ms-to-1s backoff. This never reruns tool
    effects or increments the attempt. Cancellation, generation/attempt replacement
    and an already-terminal job supersede the pending save. Shutdown leaves an
    unsaved running attempt for normal interrupted-job recovery.
    Generic admission rejections use independent workers, with at most one unsaved
    rejection per session, so persistence failure cannot block the shared scheduler.
    Published admission reservations transfer to workers without a second capacity
    charge. Staged reservations cannot run; reset/terminal/teardown paths retire
    unclaimed reservations. Removing an actor from the registry cancels its retained
    workers, including completion-save retries, before their leases are released. *)

type t

(** Pure ordering probe for the one owner-group chooser. A Host_job here is
    comparison data, never permission to claim or execute it. Groups must have
    unique owners; candidates retain their within-owner order. The returned
    anchor is the first owner considered, even if physical claim later fails.
    Missing anchors select the next typed owner and wrap. *)
module For_testing : sig
  val fair_order
    :  after:Agent_protocol.Execution_owner.t option
    -> (Agent_protocol.Execution_owner.t * Agent_protocol.Host_job.t list) list
    -> (Agent_protocol.Execution_owner.t * Agent_protocol.Host_job.t list) list
       * Agent_protocol.Execution_owner.t option

  (** Uses the production queued-row admission filter before owner ordering.
      Empty and terminal-only rows do not consume an owner turn. *)
  val fair_conversation_order
    :  after:Agent_protocol.Execution_owner.t option
    -> (Agent_protocol.Execution_owner.t * Agent_protocol.Job.t list) list
    -> Agent_protocol.Execution_owner.t list * Agent_protocol.Execution_owner.t option

  (** Read-only synchronization for an actual capacity-unavailable developer
      offer retained against this physical Root. Correctness must be asserted
      from actual handler/capacity/durable state. *)
  val developer_retry_pending : t -> root:Developer_root_owner.run -> bool

  (** The pending absolute deadline, in seconds on this scheduler's injected
      wall clock, of this exact dispatcher loop.
      [None] means that the loop is not in its post-process wait, is closed, or
      is observed from another Eio domain.
      This is a read-only synchronization observation for controlled-clock
      tests; it does not pause polling or expose candidates. *)
  val loop_wait_deadline : t -> float option
end

(** Recover complete private result preparations before durably interrupting the
    remaining running jobs. Count and byte budgets apply per session.
    Return the first actor/persistence failure so startup cannot clear an
    incomplete index-recovery marker. *)
val reconcile_recovered
  :  registry:Session_registry.t
  -> max_count:int
  -> max_total_bytes:int
  -> (unit, Agent_protocol.Error.t) result

(** Recover one actual uninstalled owner before selection admits it to schedulers.
    Same bounded interrupted-job/result recovery as eager startup; does not load
    other sessions or clear the Store's global recovery marker. *)
val reconcile_entry
  :  Session_registry.entry
  -> max_count:int
  -> max_total_bytes:int
  -> (unit, Agent_protocol.Error.t) Result.t

(** Internal composition: false constructs a stopped service and starts no fiber.
    [developer_registry] is the actual physical root registry, not an owner-ID
    resolver, and must share this exact [capacity] value. A mismatch raises
    [Invalid_argument] before starting a fiber (trusted host composition
    invariant). With no attached executor the poll does not read developer
    documents. Tool and Script packages attach to this ONE dispatcher; an
    unsupported kind or absent package stays queued without a claim.
    Conversation execution and delivery retain their actor/runtime path.
    Production daemon registry creation and installation belong to the
    developer-run host assembly. *)
val start_controlled
  :  enabled:bool
  -> sw:Eio.Switch.t
  -> clock:_ Eio.Time.clock
  -> registry:Session_registry.t
  -> developer_registry:Developer_root_owner.Registry.t option
  -> capacity:Job_capacity.t
  -> model_job_inference:
       (Session_registry.entry
        -> Agent_protocol.Job.t
        -> (Session_factory.model_job_inference, Agent_protocol.Error.t) Result.t)
  -> t

val start
  :  sw:Eio.Switch.t
  -> clock:_ Eio.Time.clock
  -> registry:Session_registry.t
  -> developer_registry:Developer_root_owner.Registry.t option
  -> capacity:Job_capacity.t
  -> model_job_inference:
       (Session_registry.entry
        -> Agent_protocol.Job.t
        -> (Session_factory.model_job_inference, Agent_protocol.Error.t) Result.t)
  -> t

(** Comparison only for trusted daemon installation: this scheduler was
    constructed with the exact physical developer Registry returned by the
    hosted service. A copied run ID or equal configuration is insufficient;
    false covers closed/disabled or uninstalled/foreign registry. This performs no I/O, claim or
    work offer and grants no execution authority. *)
val uses_developer_registry : t -> Developer_root_owner.Registry.t -> bool

(** Attach the physical Tool/Script package for [root]. The registry must be
    present and currently contain this same live Root instance, and [runner]
    must have been issued by it. An exact repeated attachment is idempotent;
    a different package cannot replace a live one. Tool/Script and Model
    packages may be installed in either order on the SAME Root attachment;
    neither creates another scan/offer loop. The bounded attachment
    table adds no execution authority or second Store/capacity owner. An
    unavailable registry, stopped/closed scheduler, foreign/stale Root or mismatched
    runner gives a finite typed error. No job is read or claimed here. *)
val attach_developer
  :  t
  -> root:Developer_root_owner.run
  -> runner:Developer_background_job_runner.t
  -> (unit, Agent_protocol.Error.t) result

(** Attach the physical Model_call package to that same Root attachment and
    existing fair scheduler loop. A Model-only Root needs no dummy Tool runner.
    Issuer/current-registry, exact repeated attachment, replacement, shutdown
    and no-Claim-on-install rules match [attach_developer]. The host binds one
    Root.Job_result owner before constructing either package, so both borrow
    the same result publisher and pending allowance. An uninstalled package
    leaves its supported queued jobs unclaimed until installation/wakeup. *)
val attach_developer_model
  :  t
  -> root:Developer_root_owner.run
  -> runner:Developer_model_job_runner.t
  -> (unit, Agent_protocol.Error.t) result

(** Coalesce a scan hint for the exact attached physical Root. Hints grant no
    claim, are dropped for stale/unattached roots, and cannot disable the
    finite periodic fallback scan. This call performs no Store read. *)
val wake_developer : t -> root:Developer_root_owner.run -> unit

(** Stop offering this exact Root. Cancel and join a scan or bounded offer
    tasks that
    borrowed it, discard its unclaimed batch, then remove only its physical
    attachment. The host calls this before replacing/closing Root. Entered
    Worker claims and result custody remain with Root/Worker and their own
    joined close; cancelling an offer uses their established cleanup. A
    foreign Root never removes an equal-ID attachment. A call from this
    scanner/offer rejects self-join before changing state; cancellation keeps
    its original exception identity. *)
val retire_developer
  :  t
  -> root:Developer_root_owner.run
  -> (unit, Agent_protocol.Error.t) result

(** After [close], join the one in-flight developer scan and any bounded
    developer offer tasks, then discard the unclaimed batch before physical
    Root/Store teardown. Entered Worker effects retain Root/Worker custody;
    this does not await conversation workers or deliveries. Calling before
    [close], or from one of these tasks, returns a typed error without
    mutating the scheduler. Cancellation/exception does not falsely report
    joined work. *)
val await_developer_scans_idle : t -> (unit, Agent_protocol.Error.t) result

(** Signal only the conversation worker owned by this physical actor and exact
    returned job owner/generation/attempt. The caller must first perform the
    authenticated actor-owned durable cancellation and pass that returned
    [job]. A bare ID cannot select a worker through this API; the job's typed
    coordinates are comparison evidence rather than standalone authority.
    Mismatch is a no-op, never a search for another owner with the same ID.
    Developer cancellation remains with its physical Root.Job_control.
    Workers retain their claimed attempt through cleanup, so stale cancellation
    and completion cannot affect a newer attempt. *)
val cancel_conversation
  :  t
  -> actor:Agent_session.Session_actor.t
  -> job:Agent_protocol.Job.t
  -> unit

(** Revoke new dispatch, cancel conversation workers and developer scan/offer
    tasks. Use [await_developer_scans_idle] before closing Root/Store; existing
    conversation delivery wait remains separate. Idempotent. The scheduler and
    its joined child bookkeeping belong to the creating Eio domain; calling
    [close] from another domain raises [Invalid_argument] before mutation. *)
val close : t -> unit

(** Stop this SAME scheduler, join its existing polling fiber and its
    developer scan/offer tasks before the developer Store or Root registry is
    closed. Conversation worker cancellation is requested by [close]; the
    daemon separately joins its Session_registry after this scheduler stops.
    Called only by the owning daemon on the creating Eio domain, never from the
    scheduler's own fiber. A foreign-domain call returns a typed error before
    mutation. *)
val close_and_wait : t -> (unit, Agent_protocol.Error.t) result

(** After [close], wait for already-dispatched completion delivery callbacks.
    Running jobs are still cancelled by [close]; this does not await their
    successful execution. The caller supplies a bounded, cancellable grace. *)
val await_deliveries_idle : t -> unit

val is_running : t -> bool
val running_count : t -> int
