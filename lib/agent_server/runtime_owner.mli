open! Core

(** Process-local runtime ownership for one durable session. *)

type t

val create
  :  actor:Agent_session.Session_actor.t
  -> initial:Agent_session.Runtime_builder.t option
  -> build:(unit -> (Agent_session.Runtime_builder.t, Agent_protocol.Error.t) result)
  -> t

val is_loaded : t -> bool
val ensure_loaded : t -> (unit, Agent_protocol.Error.t) result
val unload : t -> (unit, Agent_protocol.Error.t) result

(** Retain one loaded runtime for a background worker without holding the owner
    mutex while [f] executes. Independent workers may run concurrently or await
    each other's results. Loading remains serialized; temporary unload and
    administration return Conflict while any worker owns the runtime.

    The callback must use the actual actor/job invocation and moderator services;
    retaining this runtime grants no session or tool authority. Join all work
    within [f] and do not retain the runtime after return. Callback exceptions and caller
    cancellation release ownership without poisoning the mutex.

    [close] rejects new work and cancels callback contexts. Retirement waits for
    the final callback to unwind, so cleanup cannot close resources still in use.
    This is lifetime ownership, not generic scheduler dispatch or job admission. *)
val with_background_runtime
  :  t
  -> (Agent_session.Runtime_builder.t -> ('a, Agent_protocol.Error.t) result)
  -> ('a, Agent_protocol.Error.t) result

(** [with_administration t f] excludes concurrent runtime loading while [f]
    prepares and commits stopped state. Preserve the previous runtime on failure;
    retire it after success without turning an accepted commit into a failed
    response because of cleanup. Cancellation and exceptions do not poison the
    owner mutex. Do not call runtime-owner operations from [f]. *)
val with_administration
  :  t
  -> (unit -> ('a, Agent_protocol.Error.t) result)
  -> ('a, Agent_protocol.Error.t) result

val parse_user_content
  :  t
  -> id:History_entry.Id.t
  -> Agent_protocol.Session.Message_content.t
  -> (History_entry.t, Agent_protocol.Error.t) result

(** Load the pinned runtime and prepare one external event under the actor
    checkpoint gate. Persist the schedule delivery and queue checkpoint together
    before installing the live append. Rejection does not mutate the live queue. *)
val deliver_schedule
  :  t
  -> Agent_protocol.Schedule.t
  -> (unit, Agent_protocol.Error.t) result

(** [drain_idle_moderator] handles pending invocation observations and queued
    internal events when the actor can grant an idle moderator borrow. An installed
    runtime's deferred startup/resume activation is also polled while runnable,
    before observation/queued-event handling. It never activates a stopped actor;
    failed activation is not bypassed by later idle event work. Each
    observation and v1 event batch is bounded to 32 handlers; durable follow-up
    requests are consumed together before another event drain, even when no
    internal event or observation is queued. Only the installed v1 source can
    handle an observation. Failed handlers retain their separate failure and
    are never replayed. The result requests another idle probe when a batch
    exhausts its budget, queued events remain, or follow-up work was scheduled.
    When the runtime supplies script-tool services, every observation gets its
    own actor-bound native scope using the manager's exact admitted definition.
    Queued v1 events use the same scoped service through persisted event claims;
    the legacy drain is used only for managers without a v1 definition. Event
    requests are applied after the batch, with termination ending the batch early.
    Unretired failed/interrupted event claims suppress further queue polling for
    their source/generation, while saved native outcomes remain observable.
    Native child results join subsequent bounded observation work. A runtime
    without these services returns invocation.unavailable for Tool.call. A
    completed event also requests another probe for newly created observations.
    V1 handler cancellation releases the owner mutex. Cancellation confined to
    the owned event returns Interrupted, keeping the shared scheduler alive;
    cancellation of the caller still propagates. Later polling and administration
    remain usable. Runtime installation and legacy draining
    retain their protected lifecycle boundaries. Normal v1 declaration admission
    remains separate; this does not enable public features. *)
val drain_idle_moderator : t -> (bool, Agent_protocol.Error.t) result

(** [execute_model_job t ~recipe ~payload] executes nested model work while
    retaining runtime ownership. Cancellation interrupts the provider wait and
    releases the runtime mutex without poisoning subsequent completion delivery. *)
val execute_model_job
  :  t
  -> recipe:string
  -> payload:Jsonaf.t
  -> (Agent_session.Runtime_builder.model_job_outcome, Agent_protocol.Error.t) result

(** Atomically acknowledge a terminal model job and append its event using the
    same actor/checkpoint ownership as schedule delivery. *)
val deliver_model_job_completion
  :  t
  -> Agent_protocol.Job.t
  -> (unit, Agent_protocol.Error.t) result

(** [close] permanently prevents runtime reload and cancels background callbacks.
    With no background owners, detaches the operation worker and closes the loaded
    runtime immediately. Otherwise the final callback release retires it; [close]
    does not wait for callbacks and is safe to request from inside one. The actor
    must remain running through callback cleanup. *)
val close : t -> unit

module For_testing : sig
  (** Hold loaded runtime ownership while installing a deterministic actor-state
      fixture. Polling resumes after the callback. Do not re-enter owner methods. *)
  val with_loaded_runtime
    :  t
    -> (unit -> ('a, Agent_protocol.Error.t) result)
    -> ('a, Agent_protocol.Error.t) result
end
