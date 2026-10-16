(** One daemon-owned, process-local REPL foundation over the existing developer
    source owner and OCH-401 typing/evaluator. It has no Store writer, Root,
    scheduler, native provider or Task interpreter. Task actions belong to the
    later Host adapter. A restart expires every instance and handle. *)

open! Core
module P = Agent_protocol
module Capture_host = Authoring_context_service.Developer
module Evaluator = Chatml_repl_execution

type t

module Original : sig
  type t

  (** Canonical capture-independent create identity. Route, selected Standard
      ABI, configured Prompt/Workspace selectors and requested execution/output
      ceilings are comparison data, not grants. This is a REPL action; it does
      not manufacture a Check/Live command kind or entrypoint. *)
  val create
    :  route:Developer_command_original.Route.t
    -> target:Chatml.Chatml_compiler_contract.Target.t
    -> prompt:P.Id.Prompt_definition.t
    -> workspace:P.Id.Workspace_definition.t
    -> requested_execution:Chatml_execution.limits option
    -> requested_max_output_bytes:int option
    -> checkpoint:(unit -> unit)
    -> (t, P.Error.t) result

  val fingerprint : t -> Chatml_source.Digest.t
end

module Instance : sig
  type t

  val equal : t -> t -> bool
end

module Epoch : sig
  type t

  val equal : t -> t -> bool
end

module Revision : sig
  type t

  val equal : t -> t -> bool
end

module Limits : sig
  type t

  (** All counts/bytes are positive and bounded by the installed source,
      typing and evaluator policies. Retained receipts and exact Task origins
      have independent caps. A bounded key ledger retains either the original
      result or a tombstone after detail eviction; known keys never execute
      again. At least two keys are required: one slot is reserved for an
      authenticated close even after submit/reset admission is exhausted.
      Exhaustion rejects fresh work keys before any effect. *)
  val create
    :  max_instances:int
    -> max_keys_per_instance:int
    -> max_submissions_per_epoch:int
    -> max_source_bytes:int
    -> max_receipt_bytes:int
    -> max_total_receipt_bytes:int
    -> max_task_origins:int
    -> t Or_error.t
end

module Configuration : sig
  type t

  (** Immutable REPL-only policy. This does not mint source, method or Task
      authority. The host binds it to its existing capture owner at install. *)
  val create
    :  scope_limits:Chatml_configured_lookup.Repl_scope.Limits.t
    -> root_limits:Chatml_dependency_session.Roots.Limits.t
    -> typing_limits:Chatml_repl_typing.Limits.t
    -> evaluator_limits:Evaluator.Limits.t
    -> execution_policy:Chatml_execution.policy
    -> limits:Limits.t
    -> t
end

module View : sig
  type t

  val instance : t -> Instance.t
  val epoch : t -> Epoch.t
  val revision : t -> Revision.t
  val run_id : t -> P.Id.Developer_run.t
  val capture_fingerprint : t -> Chatml_source.Digest.t
end

module Submission : sig
  type t

  module Diagnostic : sig
    type t

    val message : t -> string
    val location : t -> Chatml_source.Location.t option
  end

  type status =
    | Need_more of
        Chatml.Chatml_parse.Submission.Need_more.reason * Chatml_source.Location.t
    | Rejected of Diagnostic.t
    | Finished
    | Stopped of Diagnostic.t option

  type details =
    | Available
    | Output_limited

  val status : t -> status
  val details : t -> details
  val completed_statements : t -> int
  val revision : t -> Revision.t
  val statements : t -> Evaluator.Receipt.t list
  val task_handles : t -> Evaluator.Task_handle.t list
end

module Task_origin : sig
  type t

  (** Exact Candidate.source paired with the physical Task handle only after
      successful evaluator finalization; it may be an earlier snippet. Source
      identity and digest do not grant Task execution. *)
  val source : t -> Chatml_source.t

  (** Recheck the exact original/current actor and selected base source and
      contract after waits. A newly captured equal-looking graph is not this
      epoch's authority. *)
  val check_current
    :  t
    -> actor:Operator_authorization.t
    -> checkpoint:(unit -> unit)
    -> (unit, P.Error.t) result
end

(** Private host installation. Borrow the SAME capture host already installed
    by developer commands; no second source resolver or writer. [surface] is
    selected internally from the authentic Standard contract. One_off_v1 and
    Tool_v1 deliberately omit print and never touch the process-global print
    sink/stdout. No arbitrary caller-supplied runtime implementation is
    accepted. No language or native effect runs at installation. *)
val install
  :  sw:Eio.Switch.t
  -> capture_host:Capture_host.t
  -> env:Eio_unix.Stdenv.base
  -> clock:Eio.Time.Mono.ty Eio.Time.Mono.t
  -> configuration:Configuration.t
  -> checkpoint:(unit -> unit)
  -> t

(** Serialize this new instance's keyed create and exact physical source
    acquisition. The original actor token is retained; reconnect may provide
    a fresh current token only for the same principal ID and authentication
    kind, with both tokens current and the source policy approving each.
    Match route/Fresh_source before read, then use the shared
    Source_selection kernel; require its selected target to equal Original.
    Create the Repl_scope, Typing and sole evaluator from the same Prepared and
    contract. A duplicate key/payload returns the retained View without
    recapture; changed payload conflicts. No Task/initializer runs here. *)
val create
  :  t
  -> actor:Operator_authorization.t
  -> run_id:P.Id.Developer_run.t
  -> key:P.Idempotency_key.t
  -> original:Original.t
  -> fresh_source:Developer_command_original.Fresh_source.t option
  -> (View.t, P.Error.t) result

(** Expected-revision, keyed statement submission. The actor makes a fresh
    captured snippet ID/bytes under its bounded namespace, calls the existing
    Typing.admit/prepare_next and Evaluator.execute_next in order, and stores
    every truthful partial outcome. A syntax Need_more/Rejected result does not
    run code. Selected configured initializers execute only inside
    [execute_next]'s current execution scope. Before each
    code entry, repeat source/contract and original/current actor admission
    after waits. Standard One_off_v1/Tool_v1 have no print or eager native
    callbacks. Deferred Task constructors are inert here. Same key and
    identical action/payload return the exact prior result without re-executing
    effects; changed payload conflicts. If receipt detail was evicted but its
    bounded key tombstone remains, matching retry reports result-expired and
    never runs again. Fresh keys reject when the per-instance key ledger is
    full; reset does not clear old-key tombstones. Cancellation cannot leave a reserved
    typing candidate or detached evaluator callback. An unexpected callback
    exception is contained by the daemon owner: the original waiter receives
    the same exception/backtrace after join. If statement execution began, a
    bounded [Stopped] partial outcome remains; in every case the key is
    consumed and an authorized retry cannot execute it again. *)
val submit
  :  t
  -> actor:Operator_authorization.t
  -> instance:Instance.t
  -> epoch:Epoch.t
  -> expected_revision:Revision.t
  -> key:P.Idempotency_key.t
  -> source:string
  -> (Submission.t, P.Error.t) result

(** Read-only current process-local result, with current actor/source
    disclosure checks. A known evicted key reports result-expired rather than
    looking like a miss. Unknown or expired instances do not restart/replay. *)
val observe
  :  t
  -> actor:Operator_authorization.t
  -> instance:Instance.t
  -> key:P.Idempotency_key.t
  -> (Submission.t option, P.Error.t) result

(** Explicit current-actor control of the exact in-flight submission. This
    signal path does not wait for the serialized statement lock: it marks and
    cancels that physical callback first, then joins its finalization and
    returns the truthful retained outcome. A foreign/historical key cannot
    cancel a different statement. It does not clear idempotency evidence. *)
val cancel_and_wait
  :  t
  -> actor:Operator_authorization.t
  -> instance:Instance.t
  -> key:P.Idempotency_key.t
  -> (Submission.t, P.Error.t) result

type borrowed

(** Lexically lend the exact serialized actor/epoch to the later Task adapter.
    It must create a separate durable action receipt and fresh Root admission;
    this borrow itself never runs a Task. It expires on scope exit. *)
val with_current
  :  t
  -> actor:Operator_authorization.t
  -> instance:Instance.t
  -> epoch:Epoch.t
  -> f:(borrowed -> ('a, P.Error.t) result)
  -> ('a, P.Error.t) result

val evaluator : borrowed -> Evaluator.t
val check_now : borrowed -> (unit, P.Error.t) result
val task_origin : borrowed -> Evaluator.Task_handle.t -> (Task_origin.t, P.Error.t) result

(** Reset/close signal the actor's current statement through the independent
    control path, then join it before revoking
    scope/evaluator/handles. OCH-402 registers any background Task action with
    the same owner and joins it before these operations return. Same-key retry
    observes the original result; no restart recovery or replay exists. *)
val reset_and_wait
  :  t
  -> actor:Operator_authorization.t
  -> instance:Instance.t
  -> expected_epoch:Epoch.t
  -> key:P.Idempotency_key.t
  -> (View.t, P.Error.t) result

val close_and_wait
  :  t
  -> actor:Operator_authorization.t
  -> instance:Instance.t
  -> key:P.Idempotency_key.t
  -> (unit, P.Error.t) result

(** Trusted daemon destructor: stop admission, signal and join every owned
    statement callback, then close the evaluators. It does not rely on a
    request actor whose credential may have expired during shutdown. *)
val shutdown_and_wait : t -> (unit, P.Error.t) result
