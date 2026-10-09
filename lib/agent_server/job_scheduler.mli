open! Core

(** Daemon-owned dispatcher for actor-persisted model jobs and qualified Async_tool
    requests. Generic jobs retain typed Completion results and pending delivery;
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

(** Internal composition: false constructs a stopped service and starts no fiber. *)
val start_controlled
  :  enabled:bool
  -> sw:Eio.Switch.t
  -> clock:_ Eio.Time.clock
  -> registry:Session_registry.t
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
  -> capacity:Job_capacity.t
  -> model_job_inference:
       (Session_registry.entry
        -> Agent_protocol.Job.t
        -> (Session_factory.model_job_inference, Agent_protocol.Error.t) Result.t)
  -> t

(** [cancel t job_id] cooperatively cancels the running Eio worker, if any.
    Actor state remains authoritative for whether cancellation was accepted.
    Workers retain their claimed attempt; a retry waits for prior worker cleanup,
    and stale completion/cleanup cannot affect a newer attempt. *)
val cancel : t -> Agent_protocol.Id.Job.t -> unit

val close : t -> unit

(** After [close], wait for already-dispatched completion delivery callbacks.
    Running jobs are still cancelled by [close]; this does not await their
    successful execution. The caller supplies a bounded, cancellable grace. *)
val await_deliveries_idle : t -> unit

val is_running : t -> bool
val running_count : t -> int
