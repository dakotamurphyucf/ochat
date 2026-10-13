(** One REPL epoch's incremental configured-module initialization.
    This is runtime state, not a compiler cache or an alternate resolver. *)

open Core
module Prepared = Chatml_prepared_program
module Contract = Chatml.Chatml_compiler_contract
module Surface = Chatml.Chatml_builtin_surface
module Lang = Chatml.Chatml_lang
module Typing = Chatml_repl_typing

module Error : sig
  type t =
    | Closed
    | Busy
    | Prepared_mismatch
    | Runtime_surface of Chatml_diagnostic.t
    | Invalid_linkage of Chatml_diagnostic.t
    | Lowering of Chatml_diagnostic.t
    | Typing of Typing.Error.t
    | Faulted of
        { module_id : Prepared.Module_id.t
        ; diagnostic : Chatml_diagnostic.t
        }
    | Runtime of
        { module_id : Prepared.Module_id.t
        ; diagnostic : Chatml_diagnostic.t
        }
    | Foreign_admission
    | Consumed_admission
    | Finalized_admission
  [@@deriving sexp_of]
end

(** One actor-owned mutable runtime registry, pinned to one Prepared
    graph/environment/contract and reset epoch. It retains only admitted
    immutable lowered unit artifacts and completed runtime module instances,
    never Checked_unit/inference cells, resolver callbacks, an executable
    project-main entry, filesystem/network capabilities or a new execution
    scheduler. The actor serializes operations and drops this owner on reset;
    old externally retained closures keep their original source/environment.

    The exact Typing.t owner is captured at creation, including its private
    epoch witness. Equal Prepared contents/digests from another typing owner
    cannot supply candidates or reuse runtime values. [control] MUST be the persistent proxy from the actor's
    Chatml_execution.runner_control; every effectful [initialize] runs inside
    that runner's [run_scoped]. It checks active lifetime/budgets and preserves
    source/control under copied module environments. The matching surface is
    still only metadata: native implementations and operation authority must
    be admitted by the caller's host. *)
type t

(** Validate the exact selected Typing.contract against Typing.prepared and
    compare the caller's actual runtime surface with that contract before any
    initializer. Start an empty, inert registry; no project main, catalog
    member or builtin callback is evaluated. Metadata/inventory work polls
    [checkpoint], whose exceptions propagate unchanged. *)
val create
  :  typing:Typing.t
  -> surface:Surface.surface
  -> control:Lang.execution_control
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

module Admission : sig
  type t

  (** The exact typing reservation acquired after all lowering and pure
      closure validation. The actor finalizes it non-yieldingly on every
      runtime outcome before admitting another submission or resetting. *)
  val typing_reservation : t -> Typing.Reservation.t

  (** Authentic direct Value_reference closure in dependency-before-owner
      order, excluding project main and type-only/open branches. Immutable
      metadata for trace/reporting; this is not a request to initialize twice. *)
  val selected_order : t -> Prepared.Module_id.t list
end

(** Pre-effect transaction for one successfully typed statement. Validate the
    candidate's exact pinned input fingerprint, graph/order/edges and direct
    reference ownership via Typing.validate_candidate on the captured owner;
    lower only newly checked canonical units with the existing source-affine
    Chatml_resolver.resolve_configured_program and checked visibility. Reuse
    previously lowered artifacts without rechecking or relabeling them.
    Validate the complete candidate cache, compute its direct value closure
    with Chatml_dependency_graph.runtime_order_for_roots, and prebuild the next
    artifact table under [checkpoint]. Never evaluate a module here.

    Only after ALL yielding work succeeds, call Typing.reserve_execution on
    the captured owner and SAME [candidate], then install the prebuilt artifact table by
    a non-yielding pointer swap and return an admission. This owner does not
    expose a separate install API, so a foreign/stale reservation cannot pair
    with another candidate. Lowering/validation failure calls
    Typing.discard_candidate if it is still current; typing reservation failure
    leaves the runtime registry untouched. Callback exceptions, including
    cancellation, preserve identity/backtrace; on interruption before
    reservation, no runtime state/effects publish. The actor uses the returned
    reservation for guaranteed typing finalization. The registry retains no
    raw Candidate or Checked_unit after reservation finalizes. *)
val reserve
  :  t
  -> Typing.Candidate.t
  -> checkpoint:(unit -> unit)
  -> (Admission.t, Error.t) Result.t

(** Initialize missing canonical units in [Admission.selected_order] exactly
    once, under the caller's current runner scope, then return only this
    statement's directly selected completed public modules. Each initializer
    sees its OWN direct completed Value_reference dependencies, not the whole
    registry; Open/Qualified_type edges never execute. Use the same controlled
    builtin environment, Eval.eval_captured_program, visibility projection,
    source-owned diagnostic and trace phase path as ordinary linked execution.
    The project main and submitted statement are never evaluated here.

    Set a unit to Initializing before its first effect; on runtime failure or
    cancellation, non-yieldingly mark that unit Faulted before leaving, retaining
    its original diagnostic/source when available and never retrying effects.
    Already completed predecessors remain initialized and retain mutable state.
    Preflight any previously faulted selected unit before new effects; unrelated
    modules and pure statements remain usable. A cancellation before a unit
    begins leaves that unit pending. Before any effect, [initialize] checks
    that this admission still belongs to the open registry and its exact typing
    reservation satisfies Typing.Reservation.pending_for the captured owner;
    external premature finalization or close rejects with Finalized_admission
    or Closed. [close] rejects while any admission's reservation is pending,
    including before the first initializer. This admission is single-use even when
    callback exceptions escape; exact client retries are actor receipts, not
    evaluator retries. The actor finalizes the typing reservation with
    constrained-before roots on any failure after reservation. *)
val initialize : t -> Admission.t -> (Lang.Configured_modules.t, Error.t) Result.t

module Status : sig
  type t =
    | Pending
    | Initializing
    | Initialized
    | Faulted of Chatml_diagnostic.t
  [@@deriving sexp_of]
end

(** Read-only actor-side inspection for a checked canonical unit. [None] means
    it was never checked/lowered in this epoch or the owner is closed. A
    reentrant native callback may observe [Initializing]; it cannot start a
    second initialization while the actor is busy. No public runtime value
    escapes. *)
val status : t -> Prepared.Module_id.t -> Status.t option

(** Release registry-owned roots after the actor has joined work and finalized
    any typing reservation. Reject reentrant/active close rather than racing
    a callback; host effects and externally retained closures are not undone. *)
val close : t -> (unit, Error.t) Result.t
