open Core
module Captured = Chatml_source
module Prepared = Chatml_prepared_program

(** Compiler-only configured lookup over an already admitted immutable inventory.
    Caller first exhausts the existing lexical lookup; this module neither scans
    syntax nor adds configured names to ordinary lexical environments. *)

module Reason : sig
  type t =
    | Value_reference
    | Open
    | Qualified_type
  [@@deriving compare, equal, sexp_of]
end

module Phase : sig
  type t =
    | Implementation
    | Interface
  [@@deriving compare, equal, sexp_of]
end

module Origin : sig
  (** Immutable provenance of a configured root or an imported member binding.
      Retains canonical target, exact relative member path, input/scope affinity
      and original configuration site, never an inference/runtime/callback owner.
      REPL imports also retain a private epoch witness; a later use may name a
      different snippet only under that same live witness and pinned scope.
      Used in persistent lexical bindings so rebinding/closure declaration order
      cannot retarget an earlier checked use. *)
  type t

  val dependency : t -> Prepared.Module_id.t
  val member_path : t -> string list
  val binding_site : t -> Chatml_diagnostic.Site.t

  (** Source that first introduced this imported binding. It remains snippet A
      when an admitted later snippet B uses that binding; the configured
      declaration [binding_site] also remains unchanged. *)
  val import_source : t -> Captured.Id.t * Captured.Digest.t

  (** Append a field name selected by the existing compiler syntax/lookup path;
      never generalize/copy a scheme. The checker must validate that the public
      member exists before successful graph publication. Type lookup can request
      a completed namespace before that final member validation. *)
  val member : t -> name:string -> t
end

(** One immutable selected scope and captured revision. Ordinary [create]
    contexts correspond to a canonical Prepared implementation/companion node;
    [Repl_scope.for_submission] contexts have a distinct snippet owner that is
    never inserted as a Prepared graph node. A context holds source/scope
    metadata, not a checker, callback or runtime authority. Retains the exact
    admitted Prepared fingerprint as input affinity, including selected scopes
    and configuration provenance, not just this source ID/content digest. *)
type t

(** Association lookup only, not another Prepared admission: source must be this
    node's implementation/companion, or its entry when no definition is present.
    An entry also in the catalog has the same node and ordinary companion.
    Checkpoints escape unchanged. *)
val create
  :  Prepared.t
  -> owner:Prepared.Module_id.t
  -> source:Captured.Id.t
  -> checkpoint:(unit -> unit)
  -> (t, Chatml_diagnostic.t) Result.t

val owner : t -> Prepared.Module_id.t
val source : t -> Captured.t
val phase : t -> Phase.t
val input_fingerprint : t -> Captured.Digest.t

module Reference : sig
  (** An actual configured resolution event. Reference/configuration sites retain
      only needed immutable captures. Qualified type references in a companion
      have Phase.Interface, not an assumed runtime initialization requirement. *)
  type t

  (** Current source owner for this resolution event, distinct from the origin's
      importing owner for a later REPL use. A snippet owner is not a graph node. *)
  val owner : t -> Prepared.Module_id.t

  val dependency : t -> Prepared.Module_id.t
  val name : t -> string
  val reason : t -> Reason.t
  val phase : t -> Phase.t
  val input_fingerprint : t -> Captured.Digest.t
  val site : t -> Chatml_diagnostic.Site.t
  val binding_site : t -> Chatml_diagnostic.Site.t
  val member_path : t -> string list
  val origin : t -> Origin.t

  (** Current use coordinates. For ordinary files these agree with [origin]'s
      source/owner. For an imported REPL binding, these name snippet B while
      [origin] and [binding_site] still describe its authentic introduction and
      selected configured declaration. [site] is source-owned by these bytes. *)
  val use_source : t -> Captured.Id.t * Captured.Digest.t
end

(** Called on a genuine lexical miss at its original byte span. Consult this
    exact file's selected scope. Missing/out-of-scope failures carry the use site
    and available selected-scope/configuration site, not guessed token offsets.
    Resolve aliases to the admitted Module_id; do not load/check any module.
    A missing [Value_reference] whose name is not a UIDENT reports an unknown
    value; missing module names and explicit module/type lookup report an unknown
    configured module. This wording never changes selected-scope resolution.
    Validate sites only for this real lookup. Reference copies this context's
    input affinity; another input with identical source bytes but different
    bindings cannot supply an event to the current graph. Retain no callback. *)
val resolve
  :  t
  -> name:string
  -> reason:Reason.t
  -> span:Source.span
  -> checkpoint:(unit -> unit)
  -> (Reference.t, Chatml_diagnostic.t) Result.t

(** Actual use of a lexical binding imported by configured open (or its imported
    nested module). Preserve root/configuration provenance and append the owned
    current use site/reason. Ordinary file contexts require exact same input,
    owner, source revision and phase. REPL contexts permit another snippet only
    under the same live private epoch witness, pinned graph and selected scope;
    the returned Reference reports the current owner/site separately from the
    unchanged introduction Origin. Value_reference is a runtime dependency even
    inside an uncalled function. Open/Qualified_type are compiler dependencies
    only. No initialized instance is required to introduce or inspect bindings. *)
val at_use
  :  t
  -> Origin.t
  -> reason:Reason.t
  -> span:Source.span
  -> checkpoint:(unit -> unit)
  -> (Reference.t, Chatml_diagnostic.t) Result.t

module Repl_scope : sig
  (** A compiler-domain selected scope for one REPL epoch. This is not a
      Prepared node, resolver, source grant, runtime capability or project-main
      execution. [create] verifies an actual implementation anchor through the
      ordinary [create] above and pins its exact Prepared fingerprint and
      selected bindings. Each call creates a fresh private epoch witness; reset
      must create a new scope and revoke the old one. The caller serializes
      access under its REPL actor; no global registry is used. *)
  type context = t

  type t

  module Limits : sig
    type t

    (** Positive ceilings for admitted snippet IDs and aggregate retained ID
        bytes (namespace plus relative path). Exact source bytes remain owned
        by the returned context, not the scope ledger. *)
    val create : max_snippets:int -> max_identity_bytes:int -> t Core.Or_error.t
  end

  module Error : sig
    type t =
      | Invalid_anchor of Chatml_diagnostic.t
      | Closed
      | Catalog_source_collision of Captured.Id.t
      | Reused_source of Captured.Id.t
      | Stale_source_revision of Captured.Id.t
      | Foreign_context
      | Stale_context
      | Foreign_reference
      | Snippet_limit
      | Identity_byte_limit
      | Accounting_overflow
    [@@deriving sexp_of]
  end

  val create
    :  Prepared.t
    -> anchor_owner:Prepared.Module_id.t
    -> anchor_implementation:Captured.Id.t
    -> limits:Limits.t
    -> checkpoint:(unit -> unit)
    -> (t, Error.t) Result.t

  (** Derive a lookup for exact, fresh captured snippet bytes under the same
      selected scope and graph fingerprint. The snippet ID must not occur in
      Prepared or have been used earlier in this epoch, even with equal bytes;
      an attempted different digest is reported as [Stale_source_revision].
      Its owner is a distinct snippet identity, never inserted as a graph node.
      Admission checks caps and cancellation before recording the ID; failed
      or cancelled attempts leave the ledger and current lookup unchanged.
      If a checkpoint reenters and admits a newer snippet before this attempt's
      final publication, this attempt returns [Stale_context] without replacing
      the newer ledger or current lookup. The owning actor still serializes
      ordinary submissions.
      Successful admission makes prior snippet lookups stale for new compiler
      resolution, while their retained origins remain usable through the new
      current lookup. Exact retries reuse the prior context at the REPL actor
      boundary rather than re-admitting the same ID. *)
  val for_submission
    :  t
    -> Captured.t
    -> checkpoint:(unit -> unit)
    -> (context, Error.t) Result.t

  (** Validate a pure statement before typing even if it has no configured
      reference. Requires this epoch's exact current context/source revision and
      Prepared fingerprint. Revocation, newer admission and foreign epochs fail
      closed; the last checkpoint is followed by currentness revalidation.
      This is compiler affinity, not runtime or file authority. *)
  val validate_context
    :  t
    -> context
    -> checkpoint:(unit -> unit)
    -> (unit, Error.t) Result.t

  (** The REPL require adapter must call this before selecting a checked module
      or recording a dependency. Validate the event against this live private
      epoch, the exact CURRENT derived lookup/source ID/digest, use owner/phase,
      pinned Prepared fingerprint and selected binding. A reference from another
      epoch fails even when its source ID, digest and graph fingerprint match.
      This does not make the snippet owner a Prepared node or authorize runtime
      initialization. Callback exceptions propagate unchanged. *)
  val validate_reference
    :  t
    -> context:context
    -> Reference.t
    -> checkpoint:(unit -> unit)
    -> (unit, Error.t) Result.t

  (** Invalidates all compiler lookups created by this epoch, including retained
      old contexts. [resolve], [at_use] and [validate_reference] then reject even
      a same-source event from the former current snippet. Already lowered old
      closures keep their original source owner; no runtime value or captured
      bytes are rewritten. Idempotent. *)
  val revoke : t -> unit

  val input_fingerprint : t -> Captured.Digest.t
end
