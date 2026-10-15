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

(** Attach the one physical Tool/Script runner for [root]. The registry must be
    present and currently contain this same live Root instance, and [runner]
    must have been issued by it. An exact repeated attachment is idempotent;
    a different package cannot replace a live one. The bounded attachment
    table adds no execution authority or second Store/capacity owner. An
    unavailable registry, stopped/closed scheduler, foreign/stale Root or mismatched
    runner gives a finite typed error. No job is read or claimed here. *)
val attach_developer
  :  t
  -> root:Developer_root_owner.run
  -> runner:Developer_background_job_runner.t
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
    conversation delivery wait remains separate. Idempotent. *)
val close : t -> unit

(** After [close], wait for already-dispatched completion delivery callbacks.
    Running jobs are still cancelled by [close]; this does not await their
    successful execution. The caller supplies a bounded, cancellable grace. *)
val await_deliveries_idle : t -> unit

val is_running : t -> bool
val running_count : t -> int
