(** One portable, actor-serialized ChatML REPL execution epoch. This owner
    composes the existing transactional typing owner, source-affine resolver,
    incremental configured-module runtime and ONE persistent execution runner.
    It neither selects a host source/contract nor grants native operations.
    Expression evaluation retains Task values inertly; only an explicit
    [drive_retained] call interprets one under caller-supplied operations and
    action budget. The caller supplies a trusted selected runtime surface and
    joins any execution before close/reset. *)

open Core
module Typing = Chatml_repl_typing
module Modules = Chatml_linked_runtime.Repl_modules
module Execution = Chatml_execution
module Surface = Chatml.Chatml_builtin_surface
module Snapshot = Chatml_typechecker.Type_snapshot

module Limits : sig
  type t

  (** Positive independent bounds on value previews, retained task count and
      diagnostic text. These supplement, never replace, Typing's frozen-type
      limits and the selected Execution policy. A preview truncates at its
      bound; it does not retry or re-evaluate an expression. *)
  val create
    :  max_preview_bytes:int
    -> max_preview_nodes:int
    -> max_preview_depth:int
    -> max_diagnostic_bytes:int
    -> max_retained_tasks:int
    -> t Or_error.t

  (** Comparison-only installed evaluator capacity. A host retaining a
      separate exact Task-origin table must admit every handle this owner can
      retain; this value does not grant execution or expose a handle. *)
  val max_retained_tasks : t -> int
end

module Error : sig
  type t =
    | Closed
    | Faulted_owner
    | Busy
    | Foreign_task_handle
    | Stale_task_handle
    | Typing of Typing.Error.t
    | Modules of Modules.Error.t
    | Lowering of Chatml_diagnostic.t
    | Budget of Execution.error
  [@@deriving sexp_of]
end

type t

(** [Typing.t] is this epoch's exact checked owner. [surface] must be its
    authentically selected runtime surface; Modules.create compares that with
    the pinned Typing contract before any initializer or expression. The owner
    creates the sole runner, supplies its persistent control to Modules.create,
    and installs precisely this surface's builtins in one initial environment.
    The supplied execution policy may narrow inherited budgets but cannot grant
    native authority. No compiler, module or task effect runs at creation.
    Callback exceptions preserve identity/backtrace. *)
val create
  :  typing:Typing.t
  -> surface:Surface.surface
  -> env:< mono_clock : _ Eio.Time.Mono.t ; .. >
  -> policy:Execution.policy
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) result

module Task_handle : sig
  (** Opaque comparison-only handle to a retained task value in this physical
      epoch. It is neither a language value nor permission to run that task.
      A later host adapter must separately authenticate an explicit action and
      current execution authority before borrowing/driving it. Close revokes
      every handle; a matching display coordinate from another epoch is foreign.
      A handle is current only while its issuing owner is live AND the exact
      physical handle remains in that owner's retained table. *)
  type t

  val equal : t -> t -> bool
end

module Diagnostic : sig
  (** Bounded immutable failure projection. [message] is limited by
      [Limits.max_diagnostic_bytes]. [location], when present, is the original
      validated source ID, digest and byte range of an owned ChatML diagnostic
      site. It may name an earlier closure or configured module, never a guessed
      location in the current submission. No captured source text or live
      runtime object is retained. *)
  type t

  val message : t -> string
  val location : t -> Chatml_source.Location.t option

  (** Length of the exact source retained by an authenticated diagnostic
      Site, paired with [location]. It is None precisely when the diagnostic
      has no owned site; no source text or replacement latest snippet is
      exposed. Hosts can form a bounded persisted location without guessing
      the source length from the span. *)
  val source_bytes : t -> int option
end

module Receipt : sig
  type failure_kind =
    | Initializer
    | Runtime
    | Budget
  [@@deriving equal, sexp_of]

  type outcome =
    | Succeeded
    | Failed of failure_kind
  [@@deriving equal, sexp_of]

  type task_retention =
    | Not_a_task
    | Retained of Task_handle.t
    | Limit_reached

  type t

  val outcome : t -> outcome

  (** This immutable bounded graph is from Typing's one finalization. Failure
      displays constrained BEFORE names; success displays AFTER names. A root
      returned by [expression_type] belongs to this same snapshot. *)
  val frozen_types : t -> Typing.Frozen_types.t

  val expression_type : t -> Snapshot.Id.t option

  (** The value was evaluated once; no live [Lang.value], mutable binding cell,
      environment or task escapes. Failure diagnostics are frozen and bounded
      before the receipt publishes. *)
  val value_preview : t -> string option

  val diagnostic : t -> Diagnostic.t option
  val task_retention : t -> task_retention
end

module Task_result : sig
  type failure_kind =
    | Runtime
    | Budget
  [@@deriving equal, sexp_of]

  type outcome =
    | Succeeded
    | Failed of failure_kind
  [@@deriving equal, sexp_of]

  type t

  val outcome : t -> outcome

  (** The Task is interpreted once per explicit call; no live language value,
      mutable cell, Task or host operation result escapes this immutable
      projection. The preview and owned diagnostic use this epoch's existing
      independent byte/node/depth limits. A successful Task need not return a
      value printable at full size. *)
  val value_preview : t -> string option

  val diagnostic : t -> Diagnostic.t option
end

(** Execute one exact current Typing candidate. Validate owner/epoch and lower
    ONLY its indexed checked statement before any effect. Then Modules.reserve
    lowers dependency artifacts and acquires the unique Typing reservation.
    In one [Execution.run_scoped] call, initialize exactly its selected direct
    value dependencies and evaluate the one statement into a copied top-level
    table using that statement's completed configured-module map. Type-only or
    configured inert statements do not call the evaluator.

    Even when lowering returns [None] for a type declaration or inert open,
    selected value dependencies still initialize before Typing finalizes AFTER;
    the existing environment remains unchanged. Once reserved, every normal
    failure, budget exhaustion or escaping
    cancellation/exception finalizes Typing exactly once before another
    submission/reset. On success, finalize AFTER and swap the staged table
    without yielding. On runtime failure, finalize constrained BEFORE and
    retain the old table; old shared-cell mutations, finished initializers and
    external effects are never rolled back or replayed. A cancellation keeps
    its original exception/backtrace after joined finalization. A typed
    post-reservation failure returns [Receipt.Failed] with bounded diagnostic;
    a pre-effect lowering/admission rejection returns [Error] and discards the
    candidate where the existing owner contract requires it. No callback
    executes a returned Task. A preview always truncates safely, and a full
    Task table reports successful [Limit_reached] without undoing effects.
    Bounded text/type projections are prepared before the nonyielding
    finalization/table swap. The actor serializes calls on [t]. *)
val execute_next
  :  t
  -> Typing.Candidate.t
  -> checkpoint:(unit -> unit)
  -> (Receipt.t, Error.t) result

(** Nonyielding comparison-only check. It cannot borrow or run the task. *)
val current_task_handle : t -> Task_handle.t -> bool

(** Borrow one exact retained Task and drive it through the existing interpreter
    on this epoch's single runner. [operations] are a trusted host-selected
    registry, not authority derived from the handle; the runtime filters them
    to its standalone effect families. The evaluator constructs the runtime
    configuration from its own pinned [surface]. [prepare_result] is a borrowed
    host transaction callback: it may inspect the raw result/surviving effects
    only during this call and returns an infallible installer. No raw Task or
    result is returned to the caller. A later hosted adapter must authenticate
    the action and current operation authority independently.

    [action_policy] opens an additional host budget around the SAME runner. It
    never resets the epoch runner or inherited ancestry. Interpreter fuel and
    task bounds use the stricter of the epoch and action policies. Handle owner,
    membership, closed/faulted state and serialization are checked before any
    operation. The same handle may be used by a separately admitted later
    action; catch/local rollback and earlier completed effects follow the
    existing task interpreter. Runtime/budget failures return a bounded
    [Task_result.Failed]; cancellation and unexpected callback exceptions
    propagate with their original backtraces after the busy loan ends. A host
    that reserves resources during an action still owns whole-execution abort
    and release when result preparation rejects, the action fails, or an
    exception/deadline interrupts it; catch rollback only releases effects
    discarded by that catch. The retained Task carries the exact captured
    source that returned it;
    owned sites within it may independently refer to earlier closures or
    modules. *)
val drive_retained
  :  t
  -> Task_handle.t
  -> operations:Chatml_host_runtime.op_def list
  -> ?scoped_async_operation:Chatml_host_runtime.scoped_async_operation
  -> ?prepare_result:
       (value:Chatml.Chatml_lang.value
        -> local_effects:Chatml.Chatml_lang.eff list
        -> (unit -> unit, string) result)
  -> action_policy:Execution.policy
  -> unit
  -> (Task_result.t, Error.t) result

(** Close only after the actor has joined any running statement; rejects Busy.
    Revoke all handles and close this owner's Modules and Typing. Old escaped
    closures/effects are not undone. A fresh epoch creates a fresh [t]. *)
val close : t -> (unit, Error.t) result
