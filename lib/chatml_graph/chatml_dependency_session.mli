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
