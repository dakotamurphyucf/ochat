open Core
module Captured = Chatml_source
module Prepared = Chatml_prepared_program
module Contract = Chatml.Chatml_compiler_contract
module Frozen = Chatml_typechecker.Type_snapshot
module Sites = Chatml.Chatml_syntax_sites

(** One non-executing public analysis library; implementation
    modules for publication, context indexes and partial syntax are private.
    No editor protocol, recovery parser, source loader or runtime authority. *)
module Identity : sig
  type t

  val input_fingerprint : t -> Captured.Digest.t
  val required_contract : t -> Captured.Digest.t
  val selected_contract : t -> Captured.Digest.t
  val environment : t -> Prepared.Environment.t
  val entry : t -> Captured.Id.t
  val semantic_format : t -> string
end

module Snapshot : sig
  module Symbol_id : sig
    type t [@@deriving equal, sexp_of]
  end

  module Namespace : sig
    type t =
      | Value
      | Type
      | Type_parameter
    [@@deriving equal, sexp_of]
  end

  module Unavailable : sig
    type t =
      | Not_checked
      | No_recorded_context
      | No_recorded_type
      | No_nominal_members
    [@@deriving sexp_of]
  end

  (** Ambiguity is an observed set, never a guessed first match. Available None
      is an empty successful lookup; unavailable describes missing proof scope. *)
  type 'a page =
    { items : 'a list
    ; truncated : bool
    }

  type 'a fact =
    | Available of 'a
    | Unavailable of Unavailable.t

  type 'a selection =
    | Selected of 'a
    | Missing of Unavailable.t
    | Ambiguous of 'a page

  module Origin : sig
    type t =
      | Authored of Captured.Location.t
      | Implicit_specification_parameter of Captured.Location.t
      | Host of
          { contract : Captured.Digest.t
          ; member_path : string list
          }
  end

  module Symbol : sig
    type kind =
      | Value
      | Parameter
      | Module
      | Type_alias
      | Type_parameter

    type t

    val id : t -> Symbol_id.t
    val name : t -> string
    val kind : t -> kind
    val origin : t -> Origin.t

    (** Principal definition root. Public binding views below can select a
        specification root without overwriting this principal declaration. *)
    val scheme : t -> Frozen.Id.t option

    val specifications : t -> Symbol_id.t list
  end

  module Binding : sig
    (** Actual lexical/export view of a symbol, including a restricted companion
        scheme/member selection. Never replace it with the symbol's principal
        root when showing a public interface. *)
    type t

    val symbol : t -> Symbol_id.t
    val scheme : t -> Frozen.Id.t option
    val values : t -> t list fact
    val types : t -> t list fact
  end

  module Alias : sig
    type t

    val symbol : t -> Symbol_id.t
    val parameters : t -> Symbol_id.t list
    val declaration : t -> Chatml.Chatml_type_expr.Declaration.t

    (** Original validated whole template; no sampled application or unbound
        Rec_var subtree. Parameter roots retain their actual grouped sharing. *)
    val template : t -> Frozen.Id.t
  end

  module Reference : sig
    type target =
      | Symbol of Symbol_id.t
      | Configured_module of Prepared.Module_id.t
      | Structural_label
      | Unlocated_module_member
      | Language_type of string

    type t

    val selection : t -> Captured.Location.t
    val target : t -> target
    val inferred_type : t -> Frozen.Id.t option

    (** Every authored qualified component is its own actual recorded reference;
        no fabricated common occurrence span or adjacency-based component group. *)
  end

  module Point : sig
    (** Snapshot-owner witness plus a ZERO-WIDTH validated source byte location.
        Every byte offset including EOF is legal; no Unicode/editor conversion.
        Same bytes and input fingerprint do not make another snapshot's point
        interchangeable. Retains no snapshot/inventory/callback. *)
    type t

    val location : t -> Captured.Location.t
  end

  module Context : sig
    (** Snapshot-affine immutable actual lexical namespace state. Root/Enter/
        Define/Join are retained from checker facts and traversed by bounded
        queries, never reconstructed from syntax.
        Type parameters remain separate from same-spelling type aliases. *)
    type t

    val source : t -> Captured.Id.t
  end

  module Query_limits : sig
    type t

    val create : max_results:int -> max_work_steps:int -> t Or_error.t
  end

  module Error : sig
    type t =
      | Foreign_point
      | Foreign_symbol
      | Foreign_context
      | Unknown_source
      | Unknown_module
      | Invalid_location
      | Nonzero_point_range
      | Query_work_limit
    [@@deriving sexp_of]
  end

  module Exports : sig
    type t

    val source : t -> Captured.Id.t
    val values : t -> Binding.t list
    val types : t -> Binding.t list
  end

  module Configured_module : sig
    type t

    val name : t -> string
    val module_id : t -> Prepared.Module_id.t
    val declared_at : t -> Captured.Location.t
    val exports : t -> Exports.t fact
    val shadowed_by : t -> Symbol_id.t option

    (** Separate exact-file module availability, not a type alias/value binder or
        actual edge. Present lexical value roots block configured fallback;
        unchecked modules have no invented exports/type. *)
  end

  module Source_status : sig
    type t =
      | Checked_implementation
      | Checked_interface
      | Unchecked
      | Metadata
    [@@deriving equal, sexp_of]
  end

  module Host_requirement : sig
    type t

    val name : t -> string
    val binding : t -> Symbol_id.t
    val required : t -> Frozen.Id.t
    val contract : t -> Captured.Digest.t
  end

  type t

  val identity : t -> Identity.t
  val sources : t -> Captured.t list
  val source_status : t -> Captured.Id.t -> Source_status.t option
  val scope : t -> Captured.Id.t -> Prepared.Scope.t option
  val types : t -> Frozen.t
  val graph : t -> Chatml_dependency_graph.t
  val runtime_order : t -> Prepared.Module_id.t list
  val diagnostics : t -> Chatml_diagnostic.t list
  val host_requirements : t -> Host_requirement.t list
  val point : t -> Captured.Location.t -> (Point.t, Error.t) Result.t
  val symbol : t -> Symbol_id.t -> (Symbol.t, Error.t) Result.t
  val alias : t -> Symbol_id.t -> (Alias.t option, Error.t) Result.t
  val exports : t -> Prepared.Module_id.t -> (Exports.t fact, Error.t) Result.t

  (** Exact positive parser selections take precedence for reference hits.
      Type lookup uses minimal containing recorded expression regions;
      conflicting equally specific roots stay Ambiguous. Context lookup uses
      actual reference selections, expression STARTS and zero-width
      statement/unit anchors carrying actual contexts, not inferred ranges.
      Conflicting established contexts remain Ambiguous. Merely containing an
      expression range does not certify lexical visibility at an inner gap.
      Arbitrary gaps with no actual site/anchor remain unavailable; do not extend
      an inner scope into whitespace by nearest-offset guessing. *)
  val references_at
    :  t
    -> Point.t
    -> limits:Query_limits.t
    -> checkpoint:(unit -> unit)
    -> (Reference.t page fact, Error.t) Result.t

  val definitions_at
    :  t
    -> Point.t
    -> limits:Query_limits.t
    -> checkpoint:(unit -> unit)
    -> (Symbol.t page fact, Error.t) Result.t

  val type_at
    :  t
    -> Point.t
    -> limits:Query_limits.t
    -> checkpoint:(unit -> unit)
    -> (Frozen.Id.t selection, Error.t) Result.t

  val context_at
    :  t
    -> Point.t
    -> limits:Query_limits.t
    -> checkpoint:(unit -> unit)
    -> (Context.t selection, Error.t) Result.t

  val candidates
    :  t
    -> Context.t
    -> namespace:Namespace.t
    -> limits:Query_limits.t
    -> checkpoint:(unit -> unit)
    -> (Binding.t page, Error.t) Result.t

  val configured_modules
    :  t
    -> Context.t
    -> limits:Query_limits.t
    -> checkpoint:(unit -> unit)
    -> (Configured_module.t page, Error.t) Result.t

  (** Deterministic byte-name order, explicit truncation, bounded query work and
      checkpoint identity preservation. No retained callback, source load,
      inference, runtime state, tool/provider use or global query cache. Value
      candidates are actual lexical values/modules; Type candidates actual bare
      aliases; Type_parameter candidates actual parameter-map bindings only.
      Configured qualifiers are queried separately, never invented type aliases. *)
end

module Partial : sig
  (** Failed compiler analysis ONLY. Immutable syntax sites from units actually
      parsed during the SAME check run; no raw types, certified references,
      exports, contexts or dependency/initialization graph. A failing parse has
      a diagnostic but no recovery syntax table. Earlier parsed units survive an
      implementation, companion, dependency or host-contract rejection. *)
  type t

  module Invalidity : sig
    type t =
      | Parse of
          { source : Captured.Id.t
          ; phase : Chatml_configured_lookup.Phase.t
          }
      | Implementation of Captured.Id.t
      | Companion of Captured.Id.t
      | Host_requirements
      | Dependency
    [@@deriving sexp_of]
  end

  module Syntax : sig
    type t

    val source : t -> Captured.t
    val owner : t -> Prepared.Module_id.t
    val phase : t -> Chatml_configured_lookup.Phase.t
    val sites : t -> Sites.t
  end

  val identity : t -> Identity.t
  val invalidity : t -> Invalidity.t
  val sources : t -> Captured.t list
  val parsed : t -> Syntax.t list
  val diagnostics : t -> Chatml_diagnostic.t list
end

module Limits : sig
  (** One plan and one admitted work/deadline ledger across all phases/modules.
      Existing recorder and freezer capacities remain their documented aggregate
      structural bounds. Publication additionally admits retained facts, member/
      context/index edges and label bytes before allocation; total retained
      capacity is the explicit sum of these phase capacities plus admitted input,
      never a reset per file. Source inventory bytes/metadata use Prepared's
      authentic accounting; sources/sites/labels are counted, not assumed free. *)
  type t

  val create
    :  max_input_bytes:int
    -> recording:Chatml_typechecker.Semantic.Limits.t
    -> type_graph:Frozen.Limits.t
    -> max_published_facts:int
    -> max_published_edges:int
    -> max_published_label_bytes:int
    -> max_work_steps:int
    -> t Or_error.t
end

module Error : sig
  type t =
    | Input_limit
    | Incompatible_contract
    | Invalid_dependency_depth
    | Invalid_input of Chatml_diagnostic.t
    | Work_limit
    | Sites of Sites.Error.t
    | Recording of Chatml_typechecker.Semantic.Error.t
    | Type_graph of Frozen.Error.t
    | Publication_fact_limit
    | Publication_edge_limit
    | Publication_label_byte_limit
    | Accounting_overflow
  [@@deriving sexp_of]
end

type t =
  | Complete of Snapshot.t
  | Failed of Partial.t

val analyze
  :  Prepared.t
  -> contract:Contract.t
  -> max_dependency_depth:int
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

(** One real Checked_graph invocation, then freeze ALL actual scheme groups
    together after graph/companion/importer/host settlement and build immutable
    indexes. Expected admission/resource failures are Error; invalid programs are
    Failed. Unexpected exceptions and checkpoints preserve exception identity and
    raw backtrace. No initializer, native surface implementation or mutable cell
    crosses publication. Final checkpoint precedes publishing either result. *)
