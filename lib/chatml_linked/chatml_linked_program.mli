open Core
module Prepared = Chatml_prepared_program
module Captured = Chatml_source
module Graph = Chatml_dependency_graph
module Lang = Chatml.Chatml_lang
module Visibility = Chatml.Chatml_module_visibility
module Contract = Chatml.Chatml_compiler_contract

module Stage : sig
  type t =
    | Parse
    | Typecheck
    | Link
  [@@deriving equal, sexp_of]
end

module Failure : sig
  (** One canonical diagnostic, stage only. No pre-rendered text, source inventory,
      compiler cell or retained callback. Host adapters render at most16KiB. *)
  type t

  val stage : t -> Stage.t
  val diagnostic : t -> Chatml_diagnostic.t
end

module Unit : sig
  (** Frozen unit: exact captures, resolved immutable AST and checked runtime
      visibility. No raw typ, infer_state, alias callback, cells, runtime value,
      Frame_env.frame, control or initialized module registry. *)
  type t

  val id : t -> Prepared.Module_id.t
  val implementation : t -> Captured.t
  val interface : t -> Captured.t option
  val program : t -> Lang.resolved_program
  val visibility : t -> Visibility.t
end

module Entrypoint : sig
  type t

  val name : t -> string
  val target : t -> Chatml.Chatml_module_reference.Binding_target.t
end

(** Immutable joined-domain linkage output. Retains the admitted immutable input
    once for exact scopes/environment/provenance and custody, plus only actually
    checked units and deterministic graph/runtime selection. Keeping Prepared
    here is intentional artifact ownership; individual errors/sites do not.
    Abstract: no public unvalidated constructor or wire reader. *)
type t

(** Pure non-executing compiler. Same authoritative185 dependency driver and
    shared inference engine; parse/check companions and entry contracts inside
    active node callbacks, then resolve ALL checked units after inference settles.
    This avoids freezing stale slot tags before later imports specialize weak
    cells. Canonical target membership is established by construction: actual
    checker Value_read/host-origin creation succeeds only after Session.require
    validates input/source/phase and completes the target; final Graph.create
    validates every recorded edge's node membership before lowering. This function
    accepts no external raw checker/annotation/require result. Owned resolver
    checks exact source/program pairing; no second AST scan invents graph facts.
    Final checkpoint precedes publication.
    Select entry-rooted VALUE closure only. Type/open-only checked subtrees are
    retained compiler facts but do not select initializers.

    Contract is an authentic static target/surface/entrypoint selection. This
    function computes its actual fingerprint and rejects disagreement with
    Prepared.compiler_contract before parsing. No independently supplied surface
    and caller-asserted matching digest enter compilation.
    It does not invent target/config discovery or authority. Depth is explicit
    positive host policy, not a process-global default. Failures/cancellation
    discard engine/cache/resolution output; no initializer, filesystem, network,
    provider or tool execution. No independent per-file compiler namespaces. *)
val compile
  :  Prepared.t
  -> contract:Contract.t
  -> max_dependency_depth:int
  -> checkpoint:(unit -> unit)
  -> (t, Failure.t) Result.t

val prepared : t -> Prepared.t
val static_contract : t -> Contract.t
val graph : t -> Graph.t
val entry : t -> Unit.t
val find_unit : t -> Prepared.Module_id.t -> Unit.t option
val units : t -> Unit.t list
val runtime_order : t -> Prepared.Module_id.t list
val entrypoints : t -> Entrypoint.t list

(** Prepared execution187 invokes only these explicitly compiled entrypoint
    descriptors. A compile-only graph with no requirements stays inert.
    Unselected configured names yield typed missing/unavailable errors; runtime
    lookup cannot infer types or expand initialization. Legacy compiled_script's
    arbitrary-name compatibility is retained by its separate adapter. *)
