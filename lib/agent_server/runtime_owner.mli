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

(** [enqueue_internal_event] loads the pinned runtime if necessary, appends one
    ChatML internal event, and returns the resulting serializable moderator
    snapshot. *)
val enqueue_internal_event
  :  t
  -> Jsonaf.t
  -> (Jsonaf.t option, Agent_protocol.Error.t) result

(** [drain_idle_moderator] handles pending invocation observations and queued
    internal events when the actor can grant an idle moderator borrow. Each
    observation batch is bounded; its durable follow-up requests are consumed
    before another event drain. Only the installed v1 source can handle an
    observation. Failed handlers retain their separate observation failure and
    are never replayed. The result requests another idle probe when a batch
    exhausts its budget, queued events remain, or follow-up work was scheduled.
    This does not install script tool-call authority or enable v1 declarations. *)
val drain_idle_moderator : t -> (bool, Agent_protocol.Error.t) result

(** [execute_model_job t ~recipe ~payload] executes nested model work while
    retaining runtime ownership. Cancellation interrupts the provider wait and
    releases the runtime mutex without poisoning subsequent completion delivery. *)
val execute_model_job
  :  t
  -> recipe:string
  -> payload:Jsonaf.t
  -> (Agent_session.Runtime_builder.model_job_outcome, Agent_protocol.Error.t) result

val enqueue_model_job_completion
  :  t
  -> Agent_protocol.Job.t
  -> (Jsonaf.t option, Agent_protocol.Error.t) result

(** [close] permanently prevents runtime reload, detaches the operation worker,
    and closes the loaded runtime. The actor must still be running. *)
val close : t -> unit
