(** Pure actor-side run/action composition. No I/O, authorization lookup, provider
    calls or callback execution. The actor establishes live scope/authority and
    passes a validated prospective state from its actual staged-work delta. New
    custody is captured even without an authored action. It persists the returned
    delta with its prospective moderator checkpoint and actual event receipt.

    An exact completed initial Single_turn callback with no action, owned work
    or pending turn/compaction request completes with no result and a terminal
    receipt in that same transaction. No input, continuation or work is fabricated.
    Unowned scheduling without a scoped decision rejects that startup checkpoint;
    explicit session-end disposition remains effective. Workflow is unchanged. *)
type t

module Lifecycle_selection : sig
  (** Exact retained Run selection DATA supplied by the Actor's live lifecycle
      owner. Neither case authenticates a caller or grants execution authority.
      The Actor must validate the same physical borrow and writer at commit. *)
  type t =
    | Authored_start of Agent_protocol.Run.t
    | Goal_resume of Agent_protocol.Run.t
end

(** [startup_run], when supplied by the actual actor, is the exact retained Run
    selected by its live held startup/Init owner. It defers quiescent terminal
    completion while real input/startup handoff remains pending. This optional
    pure DATA is not execution authority and must never be decoded as a live
    hold. Ordinary settlement without that owner remains unchanged. *)

(** [lifecycle_selection] identifies the exact retained Run from the Actor's
    actual lifecycle borrow. Authored_start retains the held Session_start law.
    Goal_resume requires a nonterminal Workflow with its immutable Goal episode
    link, unchanged source installation and actual Session_resume execution.
    Both exclude operation, Job and delegated contexts. Default generic callback
    selection remains unchanged. *)
val prepare
  :  ?lifecycle_selection:Lifecycle_selection.t
  -> ?startup_run:Agent_protocol.Run.t
  -> Session_state.t
  -> scope:Run_scope.t
  -> executing:Agent_protocol.Moderator_execution.t
  -> work_state:Session_state.t
  -> owned_work:Agent_protocol.Run_work.t list
  -> action:Agent_protocol.Run_action.t option
  -> now:Agent_protocol.Timestamp.t
  -> (t, Agent_protocol.Error.t) result

(** Immediate work admission while the same retained callback is still Running.
    The actor supplies a validated prospective state containing the actual new
    occurrences and commits both children together. Requires nonempty newly owned
    work; preserves prior terminal evidence and emits no action/terminal receipt.
    It does not complete the parent callback or change its staged action. *)
val add_owned_work
  :  ?lifecycle_selection:Lifecycle_selection.t
  -> Session_state.t
  -> scope:Run_scope.t
  -> executing:Agent_protocol.Moderator_execution.t
  -> work_state:Session_state.t
  -> owned_work:Agent_protocol.Run_work.t list
  -> now:Agent_protocol.Timestamp.t
  -> (t, Agent_protocol.Error.t) result

val run : t -> Agent_protocol.Run.t
val delta : t -> Session_delta.t
val payloads : t -> Agent_protocol.Event.Durable.Payload.t list

(** Join the actual root terminal transaction. Captures immutable owner evidence
    and resolves a pending Finish only after all retained owned work has settled.
    The same aggregate law is used when an actual Job is the final owned work,
    so child-before-root and child-after-root orderings agree. Waiting is never
    bypassed; successful Workflow still requires a pending Finish. This never
    starts another worker or stops the session. *)
val settle_operation
  :  ?startup_run:Agent_protocol.Run.t
  -> Session_state.t
  -> operation_id:Agent_protocol.Id.Operation.t
  -> outcome:Agent_protocol.Run_work.Terminal.outcome
  -> now:Agent_protocol.Timestamp.t
  -> (Session_delta.t list, Agent_protocol.Error.t) result

(** Retire a failed/interrupted actual callback in its enclosing actor transaction.
    The immutable scope must identify this execution and the current run source,
    principal and generation. A closed scope is accepted only for retirement;
    this operation grants no callback authority. Later revisions caused by owned
    work are preserved, and an already terminal winner remains unchanged. Other
    unresolved custody receives honest Unconfirmed evidence; no work is cancelled.
    Persist these deltas with [completed] in the same session transaction. *)
val fail_callback
  :  Session_state.t
  -> scope:Run_scope.t
  -> executing:Agent_protocol.Moderator_execution.t
  -> completed:Agent_protocol.Moderator_execution.t
  -> now:Agent_protocol.Timestamp.t
  -> (Session_delta.t list, Agent_protocol.Error.t) result

(** Pair existing turn admission with only its exact applied handler request or
    bound subscription-delivery wake. Consumes the retained action once and binds
    the actual operation; unrelated input never resumes a waiting run. *)
val admit_turn
  :  Session_state.t
  -> operation:Agent_protocol.Operation.t
  -> events:Agent_protocol.Moderator_execution.t list
  -> deliveries:Agent_protocol.Delivery.t list
  -> now:Agent_protocol.Timestamp.t
  -> (Agent_protocol.Id.Run.t list * Session_delta.t list, Agent_protocol.Error.t) result

(** Resume only the exact pending Wait matched by a host-validated private queue
    frame. Consumes it once and captures the actual callback execution as owned
    work. The actor validates current authority for every returned run ID, then
    commits these deltas with the same new moderator execution receipt. Retired,
    duplicate, unrelated and terminal occurrences must not invoke this path. *)
val claim_wake
  :  Session_state.t
  -> executing:Agent_protocol.Moderator_execution.t
  -> now:Agent_protocol.Timestamp.t
  -> (Agent_protocol.Id.Run.t list * Session_delta.t list, Agent_protocol.Error.t) result
