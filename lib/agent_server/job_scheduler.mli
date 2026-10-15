open! Core

(** One daemon-owned fair dispatcher for conversation jobs, with a bounded
    physical developer-candidate boundary ready for selected executors.
    Developer jobs remain queued until those executors are installed.
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
    resolver. Until the selected developer tool/model executor is installed,
    the 50-ms production poll does not repeatedly read developer documents:
    queued developer kinds stay untouched. The same bounded physical candidate
    projection and owner-ordering policy are independently testable with real
    Store/Root candidates now; the later executor attaches them to this ONE
    poll, without creating a second scheduler or claiming unsupported work.
    Conversation execution and delivery retain their existing actor/runtime
    path. Production daemon registry creation/installation belongs to the
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

val close : t -> unit

(** After [close], wait for already-dispatched completion delivery callbacks.
    Running jobs are still cancelled by [close]; this does not await their
    successful execution. The caller supplies a bounded, cancellable grace. *)
val await_deliveries_idle : t -> unit

val is_running : t -> bool
val running_count : t -> int
