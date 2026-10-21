(** Closed portable REPL client over one already initialized Connection.
    No UI, compiler, evaluator, transport or process ownership. *)
type t

val create : Connection.t -> (t, Agent_protocol.Error.t) result
val capabilities : t -> Agent_protocol.Developer_repl_wire.Capabilities.t

module Pending_create : sig
  type t

  type identity =
    { run_id : Agent_protocol.Id.Developer_run.t
    ; key : Agent_protocol.Idempotency_key.t
    }

  val identity : t -> identity
end

module Pending_submit : sig
  type t

  type identity =
    { instance : Agent_protocol.Developer_repl_wire.Wire_id.Instance.t
    ; epoch : Agent_protocol.Developer_repl_wire.Wire_id.Epoch.t
    ; expected_revision : Agent_protocol.Developer_repl_wire.Wire_id.Revision.t
    ; key : Agent_protocol.Idempotency_key.t
    }

  val identity : t -> identity
end

module Pending_task : sig
  type t

  type identity =
    { run_id : Agent_protocol.Id.Developer_run.t
    ; key : Agent_protocol.Idempotency_key.t
    ; fresh_epoch :
        (Agent_protocol.Developer_repl_wire.Wire_id.Instance.t
        * Agent_protocol.Developer_repl_wire.Wire_id.Epoch.t)
          option
    }

  val identity : t -> identity
end

module Pending_reset : sig
  type t

  type identity =
    { instance : Agent_protocol.Developer_repl_wire.Wire_id.Instance.t
    ; epoch : Agent_protocol.Developer_repl_wire.Wire_id.Epoch.t
    ; key : Agent_protocol.Idempotency_key.t
    }

  val identity : t -> identity
end

module Pending_close : sig
  type t

  type identity =
    { instance : Agent_protocol.Developer_repl_wire.Wire_id.Instance.t
    ; key : Agent_protocol.Idempotency_key.t
    }

  val identity : t -> identity
end

type pending =
  { creates : Pending_create.t list
  ; submits : Pending_submit.t list
  ; tasks : Pending_task.t list
  ; resets : Pending_reset.t list
  ; closes : Pending_close.t list
  }

val pending : t -> pending

(** Atomic in-memory adoption under the same server, principal and auth kind;
    at most 64 combined items and 16 MiB of original canonical params.
    Bounds and collisions are checked before any mutation. The snapshot is
    copied into the new owner without consuming custody from the old owner;
    adoption does not send or retry. *)
val adopt_pending : t -> pending -> (unit, Agent_protocol.Error.t) result

(** Each newly keyed action retains its exact original request before send.
    Calls on this client share the connection's serialized request lane. *)
val create_instance
  :  t
  -> Agent_protocol.Developer_repl_wire.Create.t
  -> (Agent_protocol.Developer_repl_wire.View.t, Agent_protocol.Error.t) result

val submit
  :  t
  -> Agent_protocol.Developer_repl_wire.Submit.t
  -> (Agent_protocol.Developer_repl_wire.Submission.t, Agent_protocol.Error.t) result

(** [view] must be the Create/Reset view obtained for this host and principal.
    Its instance and epoch must match the request's fresh epoch before custody
    or transport handoff. The host remains the authority for the Task reference. *)
val task_submit
  :  t
  -> view:Agent_protocol.Developer_repl_wire.View.t
  -> Agent_protocol.Developer_repl_wire.Task_submit.t
  -> (Agent_protocol.Developer_repl_wire.Task_view.t, Agent_protocol.Error.t) result

val reset_and_wait
  :  t
  -> Agent_protocol.Developer_repl_wire.Current_epoch.t
  -> (Agent_protocol.Developer_repl_wire.View.t, Agent_protocol.Error.t) result

val close_and_wait
  :  t
  -> Agent_protocol.Developer_repl_wire.Lookup.t
  -> (unit, Agent_protocol.Error.t) result

(** Explicit identical-key retry of an owned/adopted pending item. Failure,
    cancellation or a mismatched result retains it. No new source selection. *)
val retry_create
  :  t
  -> Pending_create.t
  -> (Agent_protocol.Developer_repl_wire.View.t, Agent_protocol.Error.t) result

val retry_submit
  :  t
  -> Pending_submit.t
  -> (Agent_protocol.Developer_repl_wire.Submission.t, Agent_protocol.Error.t) result

val retry_task
  :  t
  -> Pending_task.t
  -> (Agent_protocol.Developer_repl_wire.Task_view.t, Agent_protocol.Error.t) result

val retry_reset
  :  t
  -> Pending_reset.t
  -> (Agent_protocol.Developer_repl_wire.View.t, Agent_protocol.Error.t) result

val retry_close : t -> Pending_close.t -> (unit, Agent_protocol.Error.t) result

(** Historical reads are informative, never proof that the initial request
    executed or did not execute; even Some does not retire pending custody. *)
val observe_pending_submit
  :  t
  -> Pending_submit.t
  -> ( Agent_protocol.Developer_repl_wire.Submission.t option
       , Agent_protocol.Error.t )
       result

val observe_pending_task
  :  t
  -> Pending_task.t
  -> ( Agent_protocol.Developer_repl_wire.Task_view.t option
       , Agent_protocol.Error.t )
       result

(** Explicit local relinquishment after presenting uncertainty to the caller.
    Does not cancel, undo or infer an outcome. Foreign items are rejected. *)
val abandon_create : t -> Pending_create.t -> (unit, Agent_protocol.Error.t) result

val abandon_submit : t -> Pending_submit.t -> (unit, Agent_protocol.Error.t) result
val abandon_task : t -> Pending_task.t -> (unit, Agent_protocol.Error.t) result
val abandon_reset : t -> Pending_reset.t -> (unit, Agent_protocol.Error.t) result
val abandon_close : t -> Pending_close.t -> (unit, Agent_protocol.Error.t) result

(** Closed non-keyed reads and targeted controls; no generic Request.t escape.
    Cancel operations use the bounded control lane on this same physical
    connection, so they can overtake a blocked Submit or Task Await. They are
    reconciled through their exact Observe/Task_observe lookup and do not create
    a new idempotency key or retire uncertain pending custody by themselves. *)
val observe
  :  t
  -> Agent_protocol.Developer_repl_wire.Lookup.t
  -> ( Agent_protocol.Developer_repl_wire.Submission.t option
       , Agent_protocol.Error.t )
       result

val cancel_submission
  :  t
  -> Agent_protocol.Developer_repl_wire.Lookup.t
  -> (Agent_protocol.Developer_repl_wire.Submission.t, Agent_protocol.Error.t) result

val inspect_type
  :  t
  -> Agent_protocol.Developer_repl_wire.Inspect.t
  -> (Agent_protocol.Developer_repl_wire.Type_inspection.t, Agent_protocol.Error.t) result

val task_references_page
  :  t
  -> Agent_protocol.Developer_repl_wire.Reference_page_request.t
  -> (Agent_protocol.Developer_repl_wire.Reference_page.t, Agent_protocol.Error.t) result

val task_observe
  :  t
  -> Agent_protocol.Developer_common.Lookup.t
  -> ( Agent_protocol.Developer_repl_wire.Task_view.t option
       , Agent_protocol.Error.t )
       result

(** Await occupies the ordinary request lane; a targeted Task cancel can run on
    this client's bounded control lane while Await is blocked. *)
val task_await_terminal
  :  t
  -> Agent_protocol.Developer_common.Lookup.t
  -> (Agent_protocol.Developer_repl_wire.Task_view.t, Agent_protocol.Error.t) result

val task_cancel_and_wait
  :  t
  -> Agent_protocol.Developer_common.Lookup.t
  -> (Agent_protocol.Developer_repl_wire.Task_view.t, Agent_protocol.Error.t) result

(** Watch ack is only a hint subscription; immediately Observe the same key.
    Reconnect repeats Watch then Observe. Ordinary hints and Resync_required
    require an authoritative Observe; an existing subscription need not be
    registered again. Repeating Watch then Observe is also safe: refresh
    supersedes pre-ack hints, and the following Observe covers them without
    registration manufacturing another Resync_required. The caller claims
    exactly one Connection.notification_lease and passes its envelopes here. *)
val watch_and_observe_repl
  :  t
  -> Agent_protocol.Developer_repl_wire.Lookup.t
  -> ( Agent_protocol.Developer_repl_wire.Submission.t option
       , Agent_protocol.Error.t )
       result

val watch_and_observe_task
  :  t
  -> Agent_protocol.Developer_common.Lookup.t
  -> ( Agent_protocol.Developer_repl_wire.Task_view.t option
       , Agent_protocol.Error.t )
       result

val unwatch
  :  t
  -> Agent_protocol.Developer_repl_wire.Watch_key.t
  -> (unit, Agent_protocol.Error.t) result

val change_of_notification
  :  t
  -> Agent_protocol.Envelope.t
  -> (Agent_protocol.Developer_repl_wire.Change.t option, Agent_protocol.Error.t) result
