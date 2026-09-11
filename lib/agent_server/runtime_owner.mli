open! Core

(** Process-local runtime ownership for one durable session. *)

type t

exception Cleanup_failed of Agent_protocol.Error.t

val create
  :  actor:Agent_session.Session_actor.t
  -> initial:Agent_session.Runtime_builder.t option
  -> build:(unit -> (Agent_session.Runtime_builder.t, Agent_protocol.Error.t) result)
  -> t

(** Construct an owner whose accepted stop retirement must first complete a host
    dependency barrier. The callback runs outside the owner mutex after new
    admissions are excluded, before cancelling/releasing existing leases. A
    returned error retains resources and is shared by concurrent unload callers.
    The callback must not unload this same owner. For these owners [close] only
    excludes new admissions; [close_and_wait] joins the dependency barrier before
    cancelling leases or retiring resources. [closing=true] distinguishes permanent
    owner closure from reusable unload/early stop preparation. The barrier must
    handle shutdown and explicit stop without confusing their durable intent. *)
val create_with_unload
  :  before_unload:(closing:bool -> (unit, Agent_protocol.Error.t) result)
  -> actor:Agent_session.Session_actor.t
  -> initial:Agent_session.Runtime_builder.t option
  -> build:(unit -> (Agent_session.Runtime_builder.t, Agent_protocol.Error.t) result)
  -> t

val is_loaded : t -> bool
val ensure_loaded : t -> (unit, Agent_protocol.Error.t) result
val unload : t -> (unit, Agent_protocol.Error.t) result

(** Begin and join dependency cancellation after a durable stop request, even
    while this owner's foreground operation is still cleaning up. Does not retire
    this runtime or acquire its mutex. Call outside actor commit callbacks and
    owner locks. The host barrier must tolerate repeated/concurrent calls;
    [unload_and_wait] rechecks it before retirement. *)
val prepare_dependency_stop : t -> (unit, Agent_protocol.Error.t) result

(** After committing a session stop, exclude new runtime admission, cancel and
    join existing background leases, then unload before workspace cleanup. Waits
    outside the owner mutex so worker finalizers can finish. Keep the actor alive
    through this call, and call from outside a retained background callback.
    Accepted-stop cleanup survives caller cancellation; the
    owner remains reusable for a later authorized session start. Concurrent stop
    cleanup requests join the same retirement and observe its success or failure;
    they do not race into a spurious Conflict or close the runtime twice. Cleanup
    remains callable after [close], including retry after a dependency failure;
    this never reopens runtime admission. *)
val unload_and_wait : t -> (unit, Agent_protocol.Error.t) result

(** Run maintenance only with no installed runtime or background lease, excluding
    reload until the callback finishes. [None] defers; this does not unload an
    active runtime. Acquire this owner before the actor checkpoint. Exceptions
    and cancellation release ownership without poisoning subsequent operations. *)
val with_unloaded
  :  t
  -> (unit -> ('a, Agent_protocol.Error.t) result)
  -> ('a option, Agent_protocol.Error.t) result

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

(** Read the installed immutable source while retaining its runtime. Legacy
    moderators reject. Check persisted halt state in the authority guard; querying
    the manager's mutable state here would reenter its execution lock during native
    policy effects. This only identifies availability; it does not approve a
    child call. Actual policy execution must use the actor-owned event handoff. *)
val moderation_source
  :  t
  -> (Agent_protocol.Invocation.observer option, Agent_protocol.Error.t) result

(** Evaluate an owned child candidate using this parent's live manager, actor
    event/native services and history. The host authorizer must validate private
    delegation and the still-active child owner, repeatedly after waits. The parent
    actor commits decision, checkpoint and UI notices together; runtime requests
    stay with the parent. Replays do not rerun policy effects or notices. *)
val prepare_delegated_tool
  :  t
  -> delegation:Agent_protocol.Moderator_execution.delegation
  -> event:Chat_response.Moderation.Event.t
  -> authorize:(unit -> (unit, Agent_protocol.Error.t) result)
  -> (Agent_protocol.Moderator_execution.Decision.t, Agent_protocol.Error.t) result

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

(** Producer identity is supplied by a trusted authenticated adapter, never taken
   from the helper payload. This bridge requires a qualified runtime and commits
   the receipt with its captured queue frame before installing the live queue. *)

val submit_ingress
  :  t
  -> producer:Agent_protocol.Id.Principal.t
  -> registration_id:Agent_protocol.Id.Capability.t
  -> namespace:string
  -> key:Agent_protocol.Idempotency_key.t
  -> payload:Jsonaf.t
  -> (Agent_session.External_ingress.receipt, Agent_protocol.Error.t) result

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

type background_result =
  | Completed of Agent_protocol.Completion.t
  | Pending of Agent_protocol.Job.dependency

(** Execute a qualified Async_tool request through retained runtime ownership and
    the actor's exact job-attempt scope. Queue/retry time counts against the stored
    execution budget from Job.created_at. Generic results retain Completion data;
    moderator pre-tool requests use their persisted event intent. Unconsumed
    managed-handler/native requests still fail explicitly until their owning
    integration is installed. Does not admit or finish the job. *)
val execute_background_job
  :  t
  -> Agent_protocol.Job.t
  -> (background_result, Agent_protocol.Error.t) result

(** Atomically acknowledge a terminal model job and append its event using the
    same actor/checkpoint ownership as schedule delivery. *)
val deliver_model_job_completion
  :  t
  -> Agent_protocol.Job.t
  -> (unit, Agent_protocol.Error.t) result

(** Save a source-bound generic terminal event and its job acknowledgement
    atomically. The moderator's current job selection is checked before projection. *)
val deliver_background_job_completion
  :  t
  -> Agent_protocol.Job.t
  -> (unit, Agent_protocol.Error.t) result

(** [close] permanently prevents runtime reload and is safe to request from a
    background callback. For owners with a host dependency barrier, it defers
    cancellation and retirement to [close_and_wait]. For ordinary owners it
    cancels background callbacks and retires after their cleanup, or immediately
    if no callback owns the runtime. An in-progress unload always retains control
    of retirement. The actor must remain running through cleanup. *)
val close : t -> unit

(** Close and join dependency/background cleanup before shutting down the actor or
    its persistence writer. Concurrent accepted-stop cleanup is joined. A typed
    dependency failure raises [Cleanup_failed], retaining resources for retry;
    other exceptions retain their original backtrace. Call from the external
    session lifecycle, never a retained callback (which would await itself). *)
val close_and_wait : t -> unit

module For_testing : sig
  (** Hold loaded runtime ownership while installing a deterministic actor-state
      fixture. Polling resumes after the callback. Do not re-enter owner methods. *)
  val with_loaded_runtime
    :  t
    -> (unit -> ('a, Agent_protocol.Error.t) result)
    -> ('a, Agent_protocol.Error.t) result
end
