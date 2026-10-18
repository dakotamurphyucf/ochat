open! Core

(** One opt-in, entry-based completion boundary for a moderator and its local
    Model executor. The caller has finished its foreground provider turn and
    holds the same live switch that owns the executor. It must release any
    short-lived source/content execution gate before this potentially blocking
    operation, and keep the switch live until the operation and its cleanup
    finish. Ordinary conversation turns do not call this module. *)

type stop =
  | Quiescent
  | Halted
  | End_session_requested of string
  | Waiting_for_ui of Moderator_manager.pending_ui_request
  | Paused
  | Event_limit_reached

type t =
  { outcomes : Moderation.Outcome.t list
  ; runtime_requests : Moderation.Runtime_request.t list
  ; processed_events : int
  ; spawned_jobs : int
  ; stop : stop
  }

(** Drain already queued moderator events before waiting for local jobs; a
    handler may release a job that would otherwise block. When the queue is
    empty, await the next executor delivery or spawn progress instead of
    joining behind a blocked sibling; the progress cursor is captured before
    queue observation. Once all jobs have completed, join them strictly,
    retry their queue delivery, and continue draining one event at a time so
    [End_session] stops before another queued handler runs. A handler may spawn
    another job, so
    repeat settlement and drain until [Quiescent], a terminal/pause state or
    the cumulative [moderator.runtime_policy.budget.max_internal_event_drain]
    is reached for this call. A caller initiating later follow-up turns retains
    its own cumulative turn/event accounting. The existing executor's spawned-job cap still
    applies. [Event_limit_reached] means at least one event remains; exact
    budget exhaustion with an empty queue is [Quiescent]. No new scheduler,
    provider turn or durable job state is created.

    This local completion path requires [moderator.event_handlers = None]. A
    host with transactional event handlers receives [Error] before settling or
    draining; it must use its authenticated handler instead of bypassing it.

    Halt, pending UI and [Pause_internal_event_drains] are checked before a
    potentially blocking job join and after each drain. [Paused] leaves local
    jobs and queued events under the enclosing live owner for explicit
    cancellation or later resumption. [now_ms] is a trusted nonyielding clock
    sampled after each asynchronous settlement, immediately before that round's
    manager drain.

    Every surfaced runtime request is returned in order. The optional
    [on_runtime_request] observes each request from one committed handler
    outcome immediately after that outcome, before another event handler or
    any potentially blocking job wait. It is an existing host policy callback,
    not a new turn owner: it may raise to reject an unsupported mutable request,
    preserving the exception and backtrace. An absent callback preserves the
    ordinary collection behavior. [Request_turn] and [Request_compaction]
    require the caller's existing policy/turn owner to handle or explicitly
    reject them; [End_session] stops this drain with [End_session_requested]
    even if the manager is not yet halted. Halt and
    pending UI return typed stop states rather than
    spinning. Manager errors can follow committed outcomes; the caller must
    reconcile from the manager's authoritative state. Escaped worker exceptions
    and cancellation propagate with their original identity and backtrace;
    the enclosing owner cancels and joins its switch. *)
val run_entries
  :  executor:Model_executor.t
  -> moderator:In_memory_stream.moderator
  -> now_ms:(unit -> int)
  -> history:History_entry.t list
  -> available_tools:Openai.Responses.Request.Tool.t list
  -> ?on_runtime_request:(Moderation.Runtime_request.t -> unit)
  -> unit
  -> (t, string) result
