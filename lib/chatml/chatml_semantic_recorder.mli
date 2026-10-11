open Core
module Captured = Chatml_source
module Prepared = Chatml_prepared_program
module Inferred = Chatml_inferred_type
module Binding = Chatml_semantic_binding
module Sites = Chatml.Chatml_syntax_sites
module Syntax = Chatml.Chatml_type_expr
module Frozen = Chatml_type_snapshot

module Limits : sig
  (** Positive aggregate limits. Facts include bindings/roots/groups/contexts,
      regions/references/links/export members; edges include retained context and
      member references. Labels and member paths charge retained byte lengths.
      Work counts actual bounded staging/copy/identity-search poll boundaries. *)
  type t

  val create
    :  max_facts:int
    -> max_edges:int
    -> max_label_bytes:int
    -> max_work_steps:int
    -> t Or_error.t
end

module Error : sig
  type t =
    | Fact_limit
    | Edge_limit
    | Label_byte_limit
    | Work_limit
    | Accounting_overflow
    | Foreign_binding
    | Foreign_context
    | Foreign_site
    | Missing_sites
    | Ambiguous_site
    | Failed_invocation
    | Already_settled
    | Invalid_location
    | Foreign_unit
    | Foreign_signature_stage
    | Signature_already_committed
    | Foreign_group_stage
    | Group_already_committed
    | Unfinished_group
  [@@deriving sexp_of]
end

(** Invocation-local compiler staging. Holds borrowed raw roots until success,
    not inference machinery, runtime values or a whole source inventory. There is
    no optional recorder creation in ordinary compilation. Every operation borrows
    the current protected compiler checkpoint; t retains no callback. *)
type t

(** Retains only this admitted input fingerprint, not the inventory. *)
val create : Prepared.t -> limits:Limits.t -> t

(** The admitted static contract identity. The common driver computes/verifies
    the actual Contract fingerprint before creating host definition facts. *)
val compiler_contract : t -> Captured.Digest.t

(** Constant-time discard on any compiler failure, including a returned
    parse/type/host error. No failed invocation can later settle. *)
val discard : t -> unit

(** Admit one metadata traversal checkpoint before its next step. Used by
    stack-safe host row enumeration and bounded duplicate-name comparisons;
    this neither charges compiler inference nor retains a callback. *)
val poll : t -> checkpoint:(unit -> unit) -> (unit, Error.t) Result.t

(** Remaining aggregate metadata allowance for the real parser. Each admitted
    table is charged by [implementation]/[interface] before retaining its Unit. *)
val site_limits : t -> checkpoint:(unit -> unit) -> (Sites.Limits.t, Error.t) Result.t

(** Fresh exception-origin boundary for compiler helpers that borrow staging.
    Resource failures abort and poison this invocation. Caller checkpoint and
    other exceptions escape unchanged. A poisoned recorder cannot settle. *)
val with_recording
  :  t
  -> checkpoint:(unit -> unit)
  -> f:(checkpoint:(unit -> unit) -> 'a)
  -> ('a, Error.t) Result.t

(** Compiler-internal conversion under [with_recording]. *)
val take_exn : ('a, Error.t) Result.t -> 'a

module Unit : sig
  (** Exact owned parser sites plus active compiler source phase/input affinity.
      Construction rejects missing/foreign tables before inference. Frame copies
      for nested scopes retain immutable contexts, never mutable current-origin. *)
  type t
end

val implementation
  :  t
  -> Chatml_configured_lookup.t
  -> Chatml.Chatml_parse.Unit.t
  -> checkpoint:(unit -> unit)
  -> (Unit.t, Error.t) Result.t

val interface
  :  t
  -> Chatml_configured_lookup.t
  -> Chatml.Chatml_parse.Interface.t
  -> checkpoint:(unit -> unit)
  -> (Unit.t, Error.t) Result.t

module Frame : sig
  (** Borrowed compiler invocation frame, never retained by lexical namespaces
      or alias definitions. Contains no checkpoint or mutable inference owner. *)
  type recorder = t

  type t

  val recorder : t -> recorder
  val unit : t -> Unit.t
end

val frame : t -> Unit.t -> checkpoint:(unit -> unit) -> (Frame.t, Error.t) Result.t
val source : Unit.t -> Captured.t

(** Direct syntax-name site lookup. Missing metadata stays unlocated; foreign
    IDs or an unexpected authored role reject rather than acquiring ownership. *)
val named
  :  t
  -> Unit.t
  -> role:Sites.Role.t
  -> Chatml.Chatml_syntax_name.t
  -> checkpoint:(unit -> unit)
  -> (Captured.Location.t option, Error.t) Result.t

val authored
  :  t
  -> Unit.t
  -> role:Sites.Role.t
  -> span:Source.span
  -> checkpoint:(unit -> unit)
  -> (Captured.Location.t option, Error.t) Result.t

module Context : sig
  (** Persistent parent-plus-delta lexical context. Value and type namespaces
      are separate; defining a name replaces only that namespace. Availability
      from the admitted configured scope is separate from this lexical context. *)
  type t
end

val enter_context
  :  t
  -> Unit.t
  -> parent:Context.t option
  -> checkpoint:(unit -> unit)
  -> (Context.t, Error.t) Result.t

val define_context
  :  t
  -> Unit.t
  -> Context.t
  -> namespace:Binding.Namespace.t
  -> name:string
  -> binding:Binding.t
  -> checkpoint:(unit -> unit)
  -> (Context.t, Error.t) Result.t

val members
  :  t
  -> values:(string * Binding.t) list
  -> types:(string * Binding.t) list
  -> checkpoint:(unit -> unit)
  -> (Binding.Members.t, Error.t) Result.t

val context_id : Context.t -> Binding.Context_id.t

(** Two actual independently evolving namespaces. Cached owned parent pairs;
    never reconstructed from syntax or a mutable current-scope pointer. *)
val join_context
  :  t
  -> Unit.t
  -> values:Context.t option
  -> types:Context.t option
  -> checkpoint:(unit -> unit)
  -> (Context.t, Error.t) Result.t

(** The existing generalization decision supplies the raw roots. A whole-RHS
    tuple projection group has several roots; independent recursive generalise
    calls have different groups. Opens/imports reuse existing groups. Updating a
    recursive placeholder or validated whole alias template preserves its symbol but installs its finalized scheme
    at the actual generalization boundary; it does not freeze an earlier type. *)
module Group_stage : sig
  (** One actual alias/specification/scheme scope. Completed syntax roots can
      be admitted during real conversion before the final whole root exists.
      Commit preserves their deliberate quantifier sharing in one Input_group. *)
  type t

  val id : t -> Binding.Group_id.t
end

val stage_group : t -> checkpoint:(unit -> unit) -> (Group_stage.t, Error.t) Result.t

val stage_root
  :  t
  -> Group_stage.t
  -> Inferred.typ
  -> checkpoint:(unit -> unit)
  -> (Binding.Root_id.t, Error.t) Result.t

val commit_group
  :  t
  -> Group_stage.t
  -> checkpoint:(unit -> unit)
  -> (unit, Error.t) Result.t

val group
  :  t
  -> Inferred.typ list
  -> checkpoint:(unit -> unit)
  -> (Binding.Group_id.t * Binding.Root_id.t list, Error.t) Result.t

val define
  :  t
  -> Unit.t
  -> name:string
  -> kind:Binding.Kind.t
  -> definition:Binding.Definition.t
  -> scheme:(Binding.Group_id.t * Binding.Root_id.t) option
  -> members:Binding.Members.t option
  -> checkpoint:(unit -> unit)
  -> (Binding.t, Error.t) Result.t

val finish_scheme
  :  t
  -> Binding.t
  -> scheme:Binding.Group_id.t * Binding.Root_id.t
  -> checkpoint:(unit -> unit)
  -> (Binding.t, Error.t) Result.t

(** Host-only path admission before forward-path copy. Reversed segments are
    borrowed from the iterative static module traversal, not a retained callback.
    Same validation/accounting as [define], without an uncharged path append. *)
val define_host
  :  t
  -> Unit.t
  -> name:string
  -> kind:Binding.Kind.t
  -> member_path_rev:string list
  -> scheme:Binding.Group_id.t * Binding.Root_id.t
  -> checkpoint:(unit -> unit)
  -> (Binding.t, Error.t) Result.t

(** Complete a previously admitted module definition after its children; preserves
    identity/scheme and updates the recorder's definition projection. *)
val finish_members
  :  t
  -> Binding.t
  -> Binding.Members.t
  -> checkpoint:(unit -> unit)
  -> (Binding.t, Error.t) Result.t

(** Public signature projection retains the implementation definition identity,
    selecting the specification's actual scheme group and restricted nested
    members. It does not replace the implementation's principal declaration root
    in Settled.bindings; export/member facts retain this scoped projection. *)
val public_binding
  :  t
  -> implementation:Binding.t
  -> specification:Binding.t
  -> members:Binding.Members.t option
  -> checkpoint:(unit -> unit)
  -> (Binding.t, Error.t) Result.t

module Target : sig
  type t =
    | Binding of Binding.Symbol_id.t
    | Configured_module of Prepared.Module_id.t
    | Structural_label
    | Unlocated_module_member
    | Language_type of string
end

(** The checker supplies the result of its actual lookup before instantiation.
    Use-root is the instantiated result later constrained by inference. Source
    selections are validated parser IDs or already authoritative node spans; no
    spelling matcher/source scanner. Generated task sugar has no authored
    reference/binder site and therefore creates no such fact. Structural labels have no nominal target. *)
val reference
  :  t
  -> Unit.t
  -> context:Context.t
  -> location:Captured.Location.t
  -> target:Target.t
  -> inferred:Inferred.typ option
  -> checkpoint:(unit -> unit)
  -> (unit, Error.t) Result.t

(** Compiler AST expression ranges, including desugared task nodes. These are
    not asserted to be authored identifier tokens or unique semantic candidates;
    authored name facts use the positive parser site table. OCH212 must preserve
    ambiguity/origin distinctions when selecting overlapping regions. *)
val expression
  :  t
  -> Unit.t
  -> context:Context.t
  -> span:Source.span
  -> inferred:Inferred.typ
  -> checkpoint:(unit -> unit)
  -> (unit, Error.t) Result.t

(** Already admitted root in its true alias/specification quantification group. *)
val expression_root
  :  t
  -> Unit.t
  -> context:Context.t
  -> span:Source.span
  -> root:Binding.Root_id.t
  -> checkpoint:(unit -> unit)
  -> (unit, Error.t) Result.t

(** Record visibility at actual statement boundaries: zero-width source points
    immediately before/after successful inference carry the actual pre/post
    contexts and no inferred root. The compiler passes persistent contexts;
    the recorder does not resolve names or infer scopes from syntax. *)
val region
  :  t
  -> Unit.t
  -> context:Context.t
  -> span:Source.span
  -> checkpoint:(unit -> unit)
  -> (unit, Error.t) Result.t

(** The original whole validated alias template and parameter roots may share
    an alias group. A naked subtree containing an open Rec_var is never staged
    as a standalone inferred type; original syntax plus lexical targets preserve
    alias navigation without invented Mu wrapping or sampled instantiation.
    Original immutable alias declaration, parameters and definition-time lexical
    resolutions. Parameterized aliases are never represented by one sampled
    application. No resolver callback/defining type namespace enters the facts.
    Private alias expansion invokes no source-reference recorder. *)
val alias
  :  t
  -> Unit.t
  -> binding:Binding.t
  -> parameters:Binding.t list
  -> declaration:Syntax.Declaration.t
  -> checkpoint:(unit -> unit)
  -> (unit, Error.t) Result.t

module Signature_stage : sig
  (** All selected metadata/group roots/specification links are staged BEFORE
      Probe.commit, with its validated complete signature selection. Commit only
      activates this already allocated token. Tokens are invocation-affine and
      one-shot. Validate before Probe.commit and AFTER each borrowed commit
      checkpoint (especially its final callback). The trusted success path then
      performs only nonallocating activation; no callback/poll afterward. *)
  type t
end

val stage_signature
  :  t
  -> implementation:Binding.Members.t
  -> specification:Binding.Members.t
  -> checkpoint:(unit -> unit)
  -> (Signature_stage.t, Error.t) Result.t

val validate_signature_stage : t -> Signature_stage.t -> (unit, Error.t) Result.t

(** Compiler-internal trusted operation after successful validation and probe
    commit. Invalid/duplicate tokens are invariant violations, not admission
    errors that may occur after weak-cell mutation. *)
val commit_signature_exn : t -> Signature_stage.t -> unit

module Fact : sig
  (** All rows below are compiler-domain metadata. Roots identify actual raw
      inputs in [Settled.groups], never rendered type strings. Source ownership
      and input/phase affinity were validated when the row was admitted. *)
  type transition =
    | Root
    | Enter of Binding.Context_id.t
    | Join of
        { values : Binding.Context_id.t option
        ; types : Binding.Context_id.t option
        }
    | Define of
        { parent : Binding.Context_id.t
        ; namespace : Binding.Namespace.t
        ; binding : Binding.t
        }

  type context =
    { id : Binding.Context_id.t
    ; source : Captured.Id.t
    ; transition : transition
    }

  type reference =
    { context : Binding.Context_id.t
    ; location : Captured.Location.t
    ; target : Target.t
    ; inferred : Binding.Root_id.t option
    }

  type region =
    { context : Binding.Context_id.t
    ; location : Captured.Location.t
    ; inferred : Binding.Root_id.t option
    }

  type alias =
    { binding : Binding.Symbol_id.t
    ; parameters : Binding.Symbol_id.t list
    ; declaration : Syntax.Declaration.t
    }

  type signature_link =
    { implementation : Binding.Symbol_id.t
    ; specification : Binding.Symbol_id.t
    }

  type exports =
    { owner : Prepared.Module_id.t
    ; source : Captured.Id.t
    ; members : Binding.Members.t
    }

  type host_requirement =
    { contract : Captured.Digest.t
    ; name : string
    ; binding : Binding.Symbol_id.t
    ; required : Binding.Root_id.t
    }
end

(** Record actual selected exports and authentic selected host constraints.
    These do not fabricate source references for host-driven lookups. *)
val exports
  :  t
  -> Unit.t
  -> Binding.Members.t
  -> checkpoint:(unit -> unit)
  -> (unit, Error.t) Result.t

val host_requirement
  :  t
  -> Unit.t
  -> contract:Captured.Digest.t
  -> name:string
  -> binding:Binding.t
  -> required:Binding.Root_id.t
  -> checkpoint:(unit -> unit)
  -> (unit, Error.t) Result.t

module Settled : sig
  (** Private compiler-domain data only. Still contains borrowed raw roots and is
      NOT a immutable/domain-transfer/public semantic snapshot. Only the common
      driver may obtain this after graph/companion/host checks all succeed.
      Failed invocations expose no accessor. OCH212 freezes then publishes. *)
  type t

  module Group : sig
    type t

    val id : t -> Binding.Group_id.t
    val roots : t -> Binding.Root_id.t list
    val input : t -> Frozen.Input_group.t
  end

  (** Group/root order is stable within this successful invocation, allowing
      OCH212 to map every private Root_id to its frozen graph root directly. *)
  val groups : t -> Group.t list

  val bindings : t -> Binding.t list
  val contexts : t -> Fact.context list
  val references : t -> Fact.reference list
  val regions : t -> Fact.region list
  val aliases : t -> Fact.alias list
  val signature_links : t -> Fact.signature_link list
  val exports : t -> Fact.exports list
  val host_requirements : t -> Fact.host_requirement list
end

(** Private shared checking driver's success path, after all imported and host
    constraints settle. Validate affinity and stage final export/region arrays
    before publishing. Limits/cancellation return no certified settled facts. *)
val settle : t -> checkpoint:(unit -> unit) -> (Settled.t, Error.t) Result.t
