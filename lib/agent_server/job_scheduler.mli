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

(** [reconcile_recovered ~registry] durably interrupts recovered running jobs.
    Return the first actor/persistence failure so startup cannot clear an
    incomplete index-recovery marker. *)
val reconcile_recovered
  :  registry:Session_registry.t
  -> (unit, Agent_protocol.Error.t) result

val start
  :  sw:Eio.Switch.t
  -> clock:_ Eio.Time.clock
  -> registry:Session_registry.t
  -> capacity:Job_capacity.t
  -> t

(** [cancel t job_id] cooperatively cancels the running Eio worker, if any.
    Actor state remains authoritative for whether cancellation was accepted.
    Workers retain their claimed attempt; a retry waits for prior worker cleanup,
    and stale completion/cleanup cannot affect a newer attempt. *)
val cancel : t -> Agent_protocol.Id.Job.t -> unit

val close : t -> unit
val is_running : t -> bool
val running_count : t -> int
