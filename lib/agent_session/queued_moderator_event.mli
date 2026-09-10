(** Whether the current generation has a running or unretired failed/interrupted
    internal event for this source. Polling uses the same guard as admission to
    avoid repeatedly attempting a blocked queue; other observations and retained
    scheduling requests may still be processed. *)
val has_unsettled_claim
  :  state:Session_state.t
  -> observer:Agent_protocol.Invocation.observer
  -> bool

(** IDs with a durable callback claim, including failed or retired callbacks.
    Lifecycle cancellation must not rewrite already-claimed timer delivery. *)
val claimed_timer_ids
  :  state:Session_state.t
  -> (Agent_protocol.Id.Schedule.t list, Agent_protocol.Error.t) result

(** Authorize a captured timer against its retained delivered record, moderator
    source, subscription epoch/deadline and prior claims. Ordinary internal data
    returns None. A reason requests retirement without executing script code.
    Evaluate before persisting a new receipt; recheck before atomic retirement. *)
val timer_retirement_reason
  :  state:Session_state.t
  -> observer:Agent_protocol.Invocation.observer
  -> event:Session.Snapshot.t
  -> now:Agent_protocol.Timestamp.t
  -> (string option, Agent_protocol.Error.t) result

(** Pure admission and completion checks for actor-owned queued moderator events.
    Failed/interrupted claims block queued execution for that source/generation,
    including after other handlers change the checkpoint. Explicit retirement is
    a separate host operation, not an implicit retry in this handoff.
    Successful consumption may emit an identical event. The actor persists the
    claim before invoking any script effects. *)
val claim
  :  state:Session_state.t
  -> id:Agent_protocol.Id.Moderator_execution.t
  -> snapshot:Session.Moderator_state.Identity_snapshot.t
  -> now:Agent_protocol.Timestamp.t
  -> ( Agent_protocol.Moderator_execution.t * Session.Snapshot.t
       , Agent_protocol.Error.t )
       result

(** Same queued-event validation with actual foreground operation provenance.
    The actor must establish operation ownership before invoking this helper. *)
val claim_foreground
  :  operation_id:Agent_protocol.Id.Operation.t
  -> state:Session_state.t
  -> id:Agent_protocol.Id.Moderator_execution.t
  -> snapshot:Session.Moderator_state.Identity_snapshot.t
  -> now:Agent_protocol.Timestamp.t
  -> ( Agent_protocol.Moderator_execution.t * Session.Snapshot.t
       , Agent_protocol.Error.t )
       result

(** Validate unchanged source and preservation of the old queue tail. The trusted
    engine supplies the prospective snapshot and runtime requests; the actor must
    atomically save both this receipt and that checkpoint while retaining its borrow. *)
val complete
  :  claimed:Agent_protocol.Moderator_execution.t
  -> before:Session.Moderator_state.Identity_snapshot.t
  -> snapshot:Session.Moderator_state.Identity_snapshot.t
  -> requests:Agent_protocol.Invocation.follow_up
  -> (Agent_protocol.Moderator_execution.t, Agent_protocol.Error.t) result

(** Capture an ordinary lifecycle/tool-boundary event against the exact installed
    checkpoint. Internal events require [claim]. The actor must establish the
    supplied operation ownership before calling this pure helper. An unsettled
    receipt for this exact source/generation/operation/phase/checkpoint/event
    prevents automatic replay of failed effects. Completed handlers do not block
    a later occurrence of the same event. *)
val claim_job
  :  job:Agent_protocol.Moderator_execution.job_attempt
  -> state:Session_state.t
  -> id:Agent_protocol.Id.Moderator_execution.t
  -> snapshot:Session.Moderator_state.Identity_snapshot.t
  -> event:Chat_response.Moderation.Event.t
  -> now:Agent_protocol.Timestamp.t
  -> ( Agent_protocol.Moderator_execution.t * Session.Snapshot.t
       , Agent_protocol.Error.t )
       result

val claim_ordinary
  :  state:Session_state.t
  -> id:Agent_protocol.Id.Moderator_execution.t
  -> snapshot:Session.Moderator_state.Identity_snapshot.t
  -> operation_id:Agent_protocol.Id.Operation.t option
  -> event:Chat_response.Moderation.Event.t
  -> now:Agent_protocol.Timestamp.t
  -> ( Agent_protocol.Moderator_execution.t * Session.Snapshot.t
       , Agent_protocol.Error.t )
       result

(** Ordinary events preserve the existing queue and may append emits. Their
    checkpoint/outcome/request intent must commit in one actor transaction. *)
val complete_ordinary
  :  claimed:Agent_protocol.Moderator_execution.t
  -> before:Session.Moderator_state.Identity_snapshot.t
  -> snapshot:Session.Moderator_state.Identity_snapshot.t
  -> requests:Agent_protocol.Invocation.follow_up
  -> (Agent_protocol.Moderator_execution.t, Agent_protocol.Error.t) result

(** Bind explicit retirement to a retained failed/interrupted receipt and its
    exact original checkpoint/head. Changed checkpoints require reconciliation;
    payload equality alone does not prove queue occurrence identity. *)
val claim_retirement
  :  state:Session_state.t
  -> id:Agent_protocol.Id.Moderator_execution.t
  -> snapshot:Session.Moderator_state.Identity_snapshot.t
  -> ( Agent_protocol.Moderator_execution.t * Session.Snapshot.t
       , Agent_protocol.Error.t )
       result

(** Prepare retirement paired with a checkpoint removing only that head. *)
val retire
  :  claimed:Agent_protocol.Moderator_execution.t
  -> before:Session.Moderator_state.Identity_snapshot.t
  -> snapshot:Session.Moderator_state.Identity_snapshot.t
  -> reason:string
  -> (Agent_protocol.Moderator_execution.t, Agent_protocol.Error.t) result
