(** Compiler-domain REPL typing contract. *)

open Core
module Captured = Chatml_source
module Prepared = Chatml_prepared_program
module Lookup = Chatml_configured_lookup
module Parse = Chatml.Chatml_parse
module Session = Chatml_dependency_session
module Check = Chatml_configured_check
module Contract = Chatml.Chatml_compiler_contract
module Public = Chatml_typechecker.Configured_resolution.Public_module
module Root_copy = Chatml_typechecker.Repl_type_roots
module Snapshot = Chatml_typechecker.Type_snapshot

module Limits : sig
  type t

  (** Parser, detached graph-copy, frozen display and retained statement limits
      are separate. Root_copy owns the lexical/public root and generated-name
      caps; Roots.Limits owns canonical module/edge/depth caps. *)
  val create
    :  parse:Parse.Submission.Limits.t
    -> display:Snapshot.Limits.t
    -> max_display_name_bytes:int
    -> max_statements:int
    -> root_copy:Root_copy.Limits.t
    -> t Or_error.t
end

module Error : sig
  type t =
    | Lookup of Lookup.Repl_scope.Error.t
    | Closed
    | Prepared_mismatch
    | Contract_mismatch
    | Foreign_submission
    | Stale_submission
    | Aborted_submission
    | Statement_out_of_order
    | Statement_limit
    | Binding_limit
    | Root_copy of Root_copy.Error.t
    | Root_check of Session.Roots.Error.t
    | Display_limit
    | Expression_type_unavailable
    | Type_diagnostic of Chatml_diagnostic.t
    | Foreign_candidate
    | Stale_candidate
    | Consumed_candidate
    | Busy_executing
    | Conflicting_finalization
    | Accounting_overflow
  [@@deriving sexp_of]
end

(** The incremental root driver supplies on-demand selected canonical roots from the pinned
    Prepared input; no eager project-main/unused-module check is needed. One
    actor-owned compiler state for one REPL epoch. [create] verifies the scope
    fingerprint matches Prepared and the authentic selected contract fingerprint
    matches Prepared's contract, then starts an EMPTY Roots<Public.t> cache.
    A later statement copies its committed lexical schemes and cached public
    module schemes together in one memo. This core does no lazy fetch, graph
    discovery, project-main evaluation, runtime initialization or host call.
    Alias namespaces are persistent immutable values; candidate weak cells
    are always detached. *)
type t

type typing_owner = t

(** Immutable inputs pinned by this exact compiler owner. Returning them grants
    no new source or runtime authority; the runtime layer binds its registry to
    this [t] identity at creation and validates the selected runtime surface
    against the same contract. *)
val prepared : t -> Prepared.t

val contract : t -> Contract.t

val create
  :  prepared:Prepared.t
  -> scope:Lookup.Repl_scope.t
  -> contract:Contract.t
  -> root_limits:Session.Roots.Limits.t
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) result

module Submission : sig
  type ready

  type t =
    | Need_more of Parse.Submission.Need_more.t
    | Rejected of Parse.Submission.Rejection.t
    | Ready of ready

  val source : ready -> Captured.t
  val statement_count : ready -> int
end

module Frozen_types : sig
  (** One immutable snapshot with its exact active lexical binding names.
      Each ID belongs to this snapshot; [Snapshot.find] rejects a foreign ID.
      The ordered pairing is created in the same bounded freeze as the graph,
      including names introduced by [open] or shadowing. No raw scheme escapes. *)
  type t

  val snapshot : t -> Snapshot.t
  val bindings : t -> (string * Snapshot.Id.t) list

  (** Value bindings declared by this statement on success,
      in compiler order: pattern names depth-first left-to-right, recursive
      bindings in declaration order, and one name for a module declaration.
      Rebinding includes the new declaration even when its name already existed.
      Candidate snapshots expose these before execution; published runtime
      failures select the BEFORE snapshot with no declared bindings. Expression,
      type and open statements also have none. Roots belong to [snapshot], shared with [bindings];
      this projection never adds builtins or earlier statement names. *)
  val statement_bindings : t -> (string * Snapshot.Id.t) list

  (** Only a successful expression statement has this root. It belongs to the
      SAME bounded snapshot as [bindings] and was frozen before any runtime
      effect from the original checked inference, not reconstructed later. *)
  val expression_type : t -> Snapshot.Id.t option
end

(** Parse exact captured bytes as one complete unit BEFORE typing or effects.
    Need_more/Rejected do not consume a Repl_scope snippet ID or alter compiler
    state. Complete syntax then calls [Repl_scope.for_submission] once and pins
    its fresh current lookup. That context becoming current does not mutate
    committed inference cells, namespaces or public module schemes. Any later
    typing rejection leaves them unchanged, although this admitted source ID
    remains consumed and the old compiler lookup is stale. The actor handles
    exact request-ID retries, pending-buffer revisions and reset. Admission
    rejects while an execution reservation is open; the actor finalizes that
    reservation before accepting another source or resetting the epoch. *)
val admit : t -> Captured.t -> checkpoint:(unit -> unit) -> (Submission.t, Error.t) result

module Candidate : sig
  type t

  module Checked_dependency : sig
    (** Borrowed candidate-owned implementation/companion parse and checked
        annotations for the runtime registry pre-effect lowering. The exact canonical ID,
        captured source IDs/digests and checker owner are retained; no caller
        may insert these Checked_unit objects into a later typing cache. *)
    type t

    val id : t -> Prepared.Module_id.t
    val implementation : t -> Parse.Unit.t
    val interface : t -> Parse.Interface.t option
    val checked : t -> Check.Checked_unit.t
  end

  val source : t -> Captured.t
  val statement : t -> Chatml.Chatml_lang.stmt_node

  (** The exact parsed unit and selected statement coordinate retained by the
      admitted Ready submission. Resolver selection must use these together
      with [checked], after [validate_candidate] on this physical owner. *)
  val parsed : t -> Parse.Unit.t

  val statement_index : t -> int

  (** Per-statement checker annotations are source-bound to [source] and only
      cover this checked statement. the statement evaluator lowers via the existing resolver's
      source-affine single-statement adapter; future statements cannot read
      missing annotations. Raw checker values remain compiler-local. *)
  val checked : t -> Chatml_typechecker.checked_program

  (** Immutable bounded display projection created before publication. It is
      a checked candidate preview, not proof that a new runtime binding was
      published. No raw inference cell or callback escapes through this value. *)
  val frozen_types : t -> Frozen_types.t

  (** Pinned input identity even when no configured dependency/reference was
      selected. This digest alone is not owner/epoch authority: callers must
      also validate [t] against the exact live typing owner. *)
  val input_fingerprint : t -> Captured.Digest.t

  (** New canonical units in dependency-before-owner order, exactly the delta
      from the committed Roots<Public.t> cache. The runtime registry must fully lower these
      into immutable, source-affine runtime initializer artifacts BEFORE
      [reserve_execution]. Lowering may checkpoint/fail; failure calls
      [discard_candidate] and publishes no runtime registry or typing roots.
      Prior checked units live only as the runtime registry's already lowered artifacts; the
      next typing candidate never reads their old Checked_unit annotations or
      private scope. These borrowed raw units are never a service DTO. *)
  val new_checked_dependencies : t -> Checked_dependency.t list

  (** Authentic incremental Roots projection for the runtime registry's pure runtime closure
      selection. This is the complete candidate cache, including canonical
      units checked by earlier submissions, in dependency-before-owner order;
      it is not the project-main graph or a request to initialize every unit.
      Both projections retain their admitted Prepared input fingerprint and
      source-owned edges. They contain no raw checker value or runtime grant. *)
  val checked_dependency_order
    :  t
    -> checkpoint:(unit -> unit)
    -> Prepared.Module_id.t list

  val dependency_edges : t -> Chatml_dependency_graph.Edge.t list

  (** Genuine validated direct snippet selections, including distinct Open,
      Qualified_type and Value_reference events. Incremental root edges contain only
      canonical inter-module references; the runtime registry filters these direct events
      by reason for exact value-dependency initialization. *)
  val direct_references : t -> Lookup.Reference.t list
end

(** Check exactly the next source-owned statement against a one-memo detached
    copy of ALL committed lexical and configured-public roots. The copy preserves
    weak sharing/cycles and exact imported origins, but cannot point to an old
    committed cell. It reserves existing generated variable and recursive names
    before inference. [Check.check_repl_statement] selects this source-owned
    node from the complete parsed unit and runs the existing
    [infer_stmt_with_exports], alias elaboration, value restriction and
    configured resolution under the SAME Check.t engine used for new canonical
    roots. A Roots error inside the checker's Diagnostic-valued [require]
    callback is projected to a bounded diagnostic at the genuine use site;
    this owner retains its typed cause and reports it as [Root_check] after the
    checker returns. Callback exceptions retain identity/backtrace. The result
    privately holds constrained BEFORE lexical namespaces (old names, candidate
    cells) and AFTER lexical namespaces (including successful new names). BOTH
    outcomes carry the SAME candidate Roots<Public.t> cache, including newly
    checked canonical dependencies and their compatible weak-cell constraints.
    a separately prepared initializer registry is installed only after
    successful lowering and execution reservation; compiler cache membership
    never means a module initialized. No publication or initializer occurs
    during typechecking. Failure/cancellation discards the whole candidate and
    leaves committed cells, aliases, module roots and generation unchanged;
    this Ready submission is aborted so its later statements cannot execute.
    A subsequent submission may supersede this source; stale candidates fail
    at execution reservation. No statement may be prepared while another
    candidate has an open execution reservation. *)
val prepare_next
  :  t
  -> Submission.ready
  -> checkpoint:(unit -> unit)
  -> (Candidate.t, Error.t) result

(** Non-yielding read-only owner/epoch/current-source/generation check used by
    the runtime lowering adapter before touching a candidate's raw checked
    units. [reserve_execution] repeats the check after its final checkpoint.
    A matching Prepared fingerprint without this identity check is insufficient. *)
val validate_candidate : t -> Candidate.t -> (unit, Error.t) result

(** Reject a successfully typed candidate whose pre-effect runtime lowering
    failed or was cancelled. No reservation, initializer registration or
    inference publication occurred. The Ready submission is aborted; its
    admitted source ID remains consumed. This is not runtime failure and does
    not commit constrained-before roots. No callback or user effect. *)
val discard_candidate : t -> Candidate.t -> (unit, Error.t) result

module Published : sig
  type outcome =
    | After_success
    | Constrained_before_runtime_failure
  [@@deriving equal, sexp_of]

  type t

  val outcome : t -> outcome

  (* Success displays the frozen AFTER namespace. Runtime failure displays a
     separately frozen constrained BEFORE namespace, so no failed new lexical
     binding appears in the receipt. *)
  val frozen_types : t -> Frozen_types.t
end

module Reservation : sig
  type t

  (** True only while this reservation is still pending for the exact typing
      owner [owner]. False after finalization, which releases its candidate and
      owner. This read-only witness lets the runtime registry reject effects
      after external finalization/reset without retaining a raw candidate. *)
  val pending_for : t -> owner:typing_owner -> bool
end

(** Reserve BEFORE any initializer, statement evaluation, Task drive or other
    runtime effect. Validate candidate owner, epoch, exact current source and
    committed generation; perform the last yielding checkpoint and revalidate.
    A successful reservation consumes the candidate and blocks [admit],
    [prepare_next] and [close] until finalization. Rejection/cancellation here
    has no runtime effects and leaves committed inference untouched. The actor
    first requires the runtime registry lowering to have completed successfully; after this
    reservation it installs the precomputed runtime registry without a
    checkpoint, then permits effects. It must cancel/join execution and finalize
    before reset or another submission. This is an explicit execution phase,
    not a caller-invented cancellation shield. *)
val reserve_execution
  :  t
  -> Candidate.t
  -> checkpoint:(unit -> unit)
  -> (Reservation.t, Error.t) result

(** Guaranteed, non-yielding finalization AFTER the runtime outcome is known,
    including failure/cancellation after effects. No callback, allocation sized
    by input, parser, graph traversal or current-source check can fail here:
    the reservation captured a valid source/epoch before effects, and the actor
    must finalize it before revoking that epoch. Select precomputed AFTER roots
    on success, or constrained BEFORE roots on failure, retaining compatible
    old weak-cell constraints but NO failed new names. First finalization
    consumes exactly one private typing generation and returns a precomputed
    frozen receipt. In both outcomes, commit candidate public schemes and the
    newly checked canonical cache: a later selection reuses its checked module
    and its separately installed runtime artifact, rather than rechecking
    or accidentally replaying a partially effected initializer. The failed
    lexical names are excluded only from the BEFORE namespace. The runtime registry tracks
    pending/initialized/faulted state independently; a failed module cannot
    retry its external effects within that runtime registry. The REPL actor's
    broader fault/reset policy is separate. Exact same-outcome retry returns that receipt; conflicting
    outcome returns [Conflicting_finalization] without changing state. Failure
    also aborts the remaining statements in this Ready submission. The actor
    owns its separate externally visible revision/idempotency receipt.
    No API can reserve or finalize a failed typecheck. *)
val finalize_execution
  :  Reservation.t
  -> outcome:Published.outcome
  -> (Published.t, Error.t) result

(** Drop this compiler owner on reset/close only after any reserved execution is
    joined and finalized; [Busy_executing] otherwise. The actor separately
    revokes Repl_scope and manages runtime roots/Task handles; old lowered
    closures keep their original captured source and environment. *)
val close : t -> (unit, Error.t) result
