open Core

(** Pure scheduling decisions over acknowledged observations and completed events.
    The host saves
    all returned records atomically with the selected scheduling/stop action.
    This planner neither executes callbacks nor authorizes native tools. *)
type action =
  | Checkpoint
  | Stop of string
  | Compact
  | Turn
[@@deriving sexp_of]

type t =
  { action : action
  ; invocations : Agent_protocol.Invocation.t list
  ; events : Agent_protocol.Moderator_execution.t list
  }

val pending : Agent_protocol.Invocation.t -> bool
val pending_handler : Agent_protocol.Invocation.t -> bool

(** Event equivalents retain original execution outcomes and exact compaction
    bindings. Only pending or waiting intents are settled. *)
val pending_event : Agent_protocol.Moderator_execution.t -> bool

val event_delta : Agent_protocol.Moderator_execution.t -> Session_delta.t

val discard_events
  :  Agent_protocol.Moderator_execution.t list
  -> reason:string
  -> (Agent_protocol.Moderator_execution.t list, Agent_protocol.Error.t) result

val discard_event_compaction
  :  Agent_protocol.Moderator_execution.t list
  -> operation_id:Agent_protocol.Id.Operation.t
  -> reason:string
  -> (Agent_protocol.Moderator_execution.t list, Agent_protocol.Error.t) result

(** Discard uses nonexecuting reconciliation so old-generation requests can be
    retired; accepting work always requires a current-generation change. *)
val delta : Agent_protocol.Invocation.t -> Session_delta.t

val discard
  :  Agent_protocol.Invocation.t list
  -> reason:string
  -> (Agent_protocol.Invocation.t list, Agent_protocol.Error.t) result

(** Retire turns dependent on this failed/cancelled/interrupted compaction.
    Other operations and independent pending requests are untouched. Unbound
    legacy compaction receipts are also retired rather than implicitly resumed. *)
val discard_compaction
  :  Agent_protocol.Invocation.t list
  -> operation_id:Agent_protocol.Id.Operation.t
  -> reason:string
  -> (Agent_protocol.Invocation.t list, Agent_protocol.Error.t) result

(** A foreground provider admission satisfies current turn-only requests. Mixed
    compaction/turn requests and waiting-compaction receipts stay pending for the
    actor scheduler. Save these changes before dispatching the provider request;
    a rejected save must prevent dispatch. *)
val admit_turn
  :  state:Session_state.t
  -> observer:Agent_protocol.Invocation.observer
  -> (t, Agent_protocol.Error.t) result

(** Settle requests the managed foreground loop did not admit, including budget
    rejection and disabled follow-up policy. Successful completion preserves
    compaction intent for the idle scheduler; failed/cancelled workers discard
    their unscheduled continuation work. End-session intent is settled separately
    with the actual halt. Save these changes with the worker's terminal state.
    Requests belonging to independent events/jobs and their native descendants
    remain available for their actual owner's scheduler. *)
val finish_foreground
  :  state:Session_state.t
  -> observer:Agent_protocol.Invocation.observer
  -> operation_id:Agent_protocol.Id.Operation.t
  -> failed:bool
  -> (t, Agent_protocol.Error.t) result

(* [compaction_operation_id] must identify the operation committed with [Compact]. *)

(** Coalesce event, observation and handler requests for the current source/generation
    into one action. End overrides other actions. Compaction is accepted before a
    requested turn; its receipt retains
    that turn without requesting compaction again. Obsolete owners and halted
    moderators have their pending actions discarded. Handler requests wait for
    the owning job to finish; cancelled/interrupted owners discard them. Native
    handlers without an observer do not require an installed moderator. Handler
    and observation updates on one invocation preserve each other's disposition.
    Applied means scheduling
    was accepted, not that execution succeeded. *)
val plan
  :  state:Session_state.t
  -> observer:Agent_protocol.Invocation.observer option
  -> halted:bool
  -> compaction_operation_id:Agent_protocol.Id.Operation.t
  -> (t, Agent_protocol.Error.t) result
