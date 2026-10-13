open Core
module Prepared = Chatml_prepared_program
module Reference = Chatml_configured_lookup.Reference

module Edge : sig
  (** One actual owned reference; source/configuration locations and reasons are
      retained independently of canonical node identity. Availability is no edge.
      Repeated aliases coalesce nodes, not distinct located references. *)
  type t

  (** Pure projection of an already validated configured event. *)
  val of_reference : Reference.t -> t

  val owner : t -> Prepared.Module_id.t
  val dependency : t -> Prepared.Module_id.t
  val name : t -> string
  val reason : t -> Chatml_configured_lookup.Reason.t
  val phase : t -> Chatml_configured_lookup.Phase.t
  val site : t -> Chatml_diagnostic.Site.t
  val binding_site : t -> Chatml_diagnostic.Site.t
  val input_fingerprint : t -> Chatml_source.Digest.t
  val member_path : t -> string list

  (** Value_reference only; Open and Qualified_type do not select an initializer.
      Runtime selection is entry-rooted closure over these edges, not every
      checked node with an outgoing value edge. The caller owns execution. *)
  val is_runtime_dependency : t -> bool

  val compare : t -> t -> int
end

(** Immutable completed graph facts, with no inference cells, active traversal,
    callback, complete input inventory or execution-owned module registry. *)
type t

(** Internal session finalization seam. Checks actual edge membership and the
    supplied unique dependency-before-owner order ending in entry; every node
    must be entry-reachable through actual compiler edges of any reason; it does not re-admit source
    inventories or traverse catalog availability. Canonical edge ordering/dedup
    uses exact IDs, revision-owned ranges, names, reasons and phases. Every edge
    must have the supplied input fingerprint. Build only invocation-local data;
    failed validation or checkpoint interruption publishes no graph or cache.
    This explicit finalization seam is also usable by graph invariant tests. *)
val create
  :  input_fingerprint:Chatml_source.Digest.t
  -> entry:Prepared.Module_id.t
  -> dependency_order:Prepared.Module_id.t list
  -> edges:Edge.t list
  -> checkpoint:(unit -> unit)
  -> (t, Chatml_diagnostic.t) Result.t

val entry : t -> Prepared.Module_id.t
val nodes : t -> Prepared.Module_id.t list
val edges : t -> Edge.t list
val input_fingerprint : t -> Chatml_source.Digest.t

(** Compiler reachability/checking order, including type-only/open-only edges and
    entry exactly once.187 selects the accepted value-edge runtime closure. *)
val dependency_order : t -> Prepared.Module_id.t list

(** Entry-rooted transitive closure of Value_reference edges, filtered in compiler
    dependency order. Pure selection only; no initialization or runtime registry.
    Value edges in compiler-only checked subtrees do not enter this closure. *)
val runtime_dependency_order : t -> checkpoint:(unit -> unit) -> Prepared.Module_id.t list

(** Select the transitive Value_reference closure of zero or more authentic
    direct roots, filtered in the supplied canonical dependency-before-owner
    order. This is the same pure selection used by [runtime_dependency_order],
    without inventing a project-main entry for a REPL. Open/Qualified_type
    edges never select an initializer, including value edges below a branch
    reached only for types. Empty roots select nothing.

    The caller supplies the incremental root driver's completed Roots order/edges and the pinned
    Prepared fingerprint; this function validates unique ordered nodes, exact
    edge input identity/membership/order, and root membership before selection.
    Every list/edge/closure step polls [checkpoint]. The caller must bound input
    counts with its admitted Roots policy and shared work budget. On invalid
    input it returns a bounded diagnostic; callback exceptions propagate. *)
val runtime_order_for_roots
  :  input_fingerprint:Chatml_source.Digest.t
  -> checked_order:Prepared.Module_id.t list
  -> edges:Edge.t list
  -> roots:Prepared.Module_id.t list
  -> checkpoint:(unit -> unit)
  -> (Prepared.Module_id.t list, Chatml_diagnostic.t) Result.t
