open Core

(** Pure scheduling decisions over acknowledged observations. The host saves
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
  }

val pending : Agent_protocol.Invocation.t -> bool

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

(* [compaction_operation_id] must identify the operation committed with [Compact]. *)

(** Coalesce requests for the current source/generation. End overrides other
    actions. Compaction is accepted before a requested turn; its receipt retains
    that turn without requesting compaction again. Obsolete owners and halted
    moderators have their pending actions discarded. Applied means scheduling
    was accepted, not that execution succeeded. *)
val plan
  :  state:Session_state.t
  -> observer:Agent_protocol.Invocation.observer option
  -> halted:bool
  -> compaction_operation_id:Agent_protocol.Id.Operation.t
  -> (t, Agent_protocol.Error.t) result
