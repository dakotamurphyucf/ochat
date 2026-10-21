open Core
module Prepared = Chatml_prepared_program
module Lookup = Chatml_configured_lookup
module Reference = Lookup.Reference

module Unit : sig
  (** Canonical node with admitted implementation and optional companion contexts.
      No importer lexical environment or module cache is supplied to a unit. *)
  type t

  val id : t -> Prepared.Module_id.t
  val implementation : t -> Lookup.t
  val interface : t -> Lookup.t option
end

(** Completed compiler-domain result. Checked values remain compilation-owned,
    may share weak cells, and are not frozen semantic snapshots or a published
    host artifact. Contains no active traversal or checkpoint/require closure. *)
type 'checked t

module Roots : sig
  (** A completed set of canonical configured module roots. Unlike [Graph.t],
      this has no entry and need not be reachable from the project main. It
      retains only roots actually requested through owned compiler references,
      their transitive checked dependencies, and actual inter-module edges.
      Snippet owners are never graph nodes. Checked values may contain mutable
      weak cells: a cache is compiler-candidate-owned, not an isolated semantic
      artifact or a runtime initializer registry. *)
  type 'checked t

  module Limits : sig
    type t

    (** Positive module/depth ceilings and a nonnegative edge ceiling. The
        compiler host supplies these from its bounded admission policy; the
        caller's checkpoint also enforces its shared work budget. *)
    val create
      :  max_checked_modules:int
      -> max_edges:int
      -> max_dependency_depth:int
      -> t Core.Or_error.t
  end

  module Error : sig
    type t =
      | Invalid_reference of Lookup.Repl_scope.Error.t
      | Invalid_cache_seed
      | Different_input
      | Failed_check of Chatml_diagnostic.t
    [@@deriving sexp_of]
  end

  (** Start an empty candidate for the exact admitted Prepared graph. This
      does not check its entry or any catalog member. No source, alias or graph
      authority is derived from a workspace ID or the caller's module name. *)
  val empty : Prepared.t -> limits:Limits.t -> 'checked t

  (** Build the next exclusive candidate from a prior completed cache and
      values detached together with all lexical roots by the caller's ONE
      graph-copy memo. [checked] must contain exactly the prior cache's IDs in
      dependency order, once each; no lossy reconstitution, additional module,
      changed edge or changed Prepared input is admitted. The driver validates
      the seed before retaining any value, polling each element. This operation
      does not copy weak cells itself. The caller must never pass a published
      cache's raw values as a new mutable candidate or share one candidate's
      checked values across simultaneous checker machines. *)
  val rebind_checked
    :  'old t
    -> checked:(Prepared.Module_id.t * 'next) list
    -> checkpoint:(unit -> unit)
    -> ('next t, Error.t) Result.t

  (** Select one actual configured reference from the current REPL snippet.
      First validate the live scope/epoch, exact current source revision,
      selected binding and Prepared fingerprint via Repl_scope; then traverse
      the canonical target using the SAME unit/context/reference validation,
      cycle/depth rules and checker callback as [run]. A module already checked
      in this candidate is reused; a previously unused broken member remains
      untouched until selected. The nested [require] callback accepts only
      genuine references from the active canonical implementation/companion.

      Check module/edge caps before growing the cache (as bounded canonical
      diagnostics, including when reached through a nested [require]); charge
      stack, cache, edge and publication work through [checkpoint]. After all yielding work,
      revalidate the selected reference before returning. On diagnostic,
      invalidation, limit or checkpoint exception, no new cache is returned.
      The caller discards this whole candidate and checker machine on failure:
      preexisting checked values may have acquired weak constraints during the
      attempted check, so returning the old cache is NOT rollback. Callback
      exceptions, including cancellation, propagate unchanged. *)
  val require_selected
    :  'checked t
    -> scope:Lookup.Repl_scope.t
    -> source:Lookup.t
    -> Reference.t
    -> checkpoint:(unit -> unit)
    -> check:
         (Unit.t
          -> require:
               (source:Lookup.t
                -> Reference.t
                -> ('checked, Chatml_diagnostic.t) Result.t)
          -> ('checked, Chatml_diagnostic.t) Result.t)
    -> ('checked t * 'checked, Error.t) Result.t

  val input_fingerprint : 'checked t -> Chatml_source.Digest.t
  val checked_in_order : 'checked t -> (Prepared.Module_id.t * 'checked) list
  val edges : 'checked t -> Chatml_dependency_graph.Edge.t list
end

(** One invocation owns its active stack and completed-module cache. Uses only
    admitted bytes/metadata; no discovery, initializer, tool or provider calls.
    Load the entry as Module_id.of_implementation entry, even if it is also a
    catalog implementation: one node, one check, ordinary self-cycle rules.

    [check] is the real checker adapter, not a replacement syntax scanner. Its
    [require] callback consumes only actual resolved lexical-fallback events with
    the explicit source context of the currently checked file. BEFORE edge/cache
    mutation it checks context AND reference input fingerprints equal the run's
    Prepared fingerprint, owner equals the active canonical node, and reference
    source ID/digest/phase equal that context's admitted implementation/companion.
    Reject cross-input/cross-owner/cross-phase events even with identical offsets
    and source bytes. The checker passes its own current per-unit context, never
    importer context. Then add edge/reuse completed dependency/check on demand.
    An active target fails at the closing reference with bounded chronological related cycle
    edge sites, excluding the acyclic prefix. At most 8 closest preceding cycle
    sites are retained; longer witnesses state this truncation explicitly. No provisional interface is returned to make a cycle legal.

    [max_dependency_depth] must be positive and is supplied/mapped by the compiler
    host policy. Bound synchronous recursion before invoking another checker;
    finite node cardinality follows the admitted inventory. Poll stack/cache/edge
    walks and canonicalization. Callback exceptions, including domain collisions,
    propagate unchanged. Expected checker failures stay the canonical diagnostic.

    The checker owns one shared inference machine across callbacks and uses fresh
    per-unit annotations. It must not retain [require] or transfer checked values
    across domains. Failure/cancellation discards this invocation's graph/cache
    and inference state and publishes no partial result. Final graph construction
    and last checkpoint must succeed BEFORE constructing the completed result;
    no mutable cache/facts accessor exists during rejection or cancellation.
    Final dependency order
    is deterministic for fixed captured sources/scopes and deterministic checker
    visitation, independent of inventory/alias-map insertion order. *)
val run
  :  Prepared.t
  -> max_dependency_depth:int
  -> checkpoint:(unit -> unit)
  -> check:
       (Unit.t
        -> require:
             (source:Lookup.t -> Reference.t -> ('checked, Chatml_diagnostic.t) Result.t)
        -> ('checked, Chatml_diagnostic.t) Result.t)
  -> ('checked t, Chatml_diagnostic.t) Result.t

val graph : 'checked t -> Chatml_dependency_graph.t
val entry : 'checked t -> 'checked
val checked : 'checked t -> Prepared.Module_id.t -> 'checked option
