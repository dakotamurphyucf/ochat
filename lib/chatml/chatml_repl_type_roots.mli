open Core
module Public = Chatml_configured_resolution.Public_module
module Roots = Chatml_dependency_session.Roots
module Scope = Chatml_value_scope

(** Compiler-domain OCH-269 bridge over OCH-268's private graph copier. A REPL
    candidate detaches its committed lexical and configured-public schemes in
    ONE memo before invoking the actual checker. This module neither checks a
    configured root nor owns a REPL actor, parser, runtime or service. *)

module Limits : sig
  type t

  (** Independent nonnegative binding/root/type/cell/edge/work/name-byte caps;
      copy depth is at most the private copier's finite stack ceiling. Zero
      admits the corresponding empty dimension. The caller's checkpoint adds
      its shared compiler work budget. Exact retained names include generic,
      free and recursive-variable names from lexical AND public roots. *)
  val create
    :  max_bindings:int
    -> max_type_roots:int
    -> max_type_nodes:int
    -> max_variable_cells:int
    -> max_type_edges:int
    -> max_copy_work:int
    -> max_copy_depth:int
    -> max_name_work:int
    -> max_name_bytes:int
    -> t Or_error.t

  (** The same ceiling bounds retained AFTER lexical names before publication,
      including new bindings not present in the captured BEFORE root vector. *)
  val max_bindings : t -> int
end

module Error : sig
  type copy_dimension =
    | Roots
    | Type_nodes
    | Variable_cells
    | Edges
    | Work
    | Depth
  [@@deriving compare, equal, sexp_of]

  type t =
    | Binding_limit
    | Copy_limit of copy_dimension
    | Name_limit
    | Name_counter_overflow
    | Invalid_module_scheme
    | Invalid_cache_seed of Roots.Error.t
  [@@deriving sexp_of]
end

module Name_seed : sig
  (** Precomputed fresh-name counters that cannot collide with generated-form
      free/generic/recursive names reachable from copied lexical/public roots.
      The root walk that computes these counters is bounded and checkpointed;
      applying them to a just-created empty checker machine is constant time,
      non-yielding and does not inspect an old inference graph. The configured
      checker adapter owns that application so this supporting library module
      does not depend on the library's main Chatml_typechecker module. *)
  type t

  val next_current_id : t -> int
  val next_mu_id : t -> int
end

module Copy : sig
  type t

  val lexical : t -> Scope.t
  val modules : t -> Public.t Roots.t
  val name_seed : t -> Name_seed.t
end

(** Capture active lexical bindings and every cached public module in exact
    dependency order; check counts before traversal. Pass ALL schemes as one
    root vector to [Chatml_type_graph_copy.copy_detached]. Preserve physical
    sharing/cycles, free-variable levels/equality, open records/variants,
    aliases and value restriction. Rebuild the lexical scope with its original
    names/origins/semantic metadata, and each public module with its original
    closed aliases/member provenance and a copied scheme. Rebind the exact
    canonical module IDs/order through Roots.rebind_checked. Reserve all
    generated-form variable and recursive names reachable from both namespaces
    before returning. No old mutable cell is reachable from [Copy] through a
    type root. Errors/cancellation leave input roots untouched and return no
    partial candidate; caller exclusively owns the originals during copying. *)
val copy_detached
  :  lexical:Scope.t
  -> modules:Public.t Roots.t
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (Copy.t, Error.t) Result.t
