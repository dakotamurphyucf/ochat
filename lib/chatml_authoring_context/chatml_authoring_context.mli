open Core

(** Pure immutable state for host-admitted authoring contexts. The host owns the
    current store value, mints identities, checks actor/source grants and joins
    work. Values here grant no read, write, analysis or execution authority.
    No Eio, transport, filesystem, compiler checking or resolver runs here. *)

module Identity : sig
  (** Host-issued comparison labels, not bearer capabilities. A principal label,
      host incarnation, connection lease and context nonce are all required.
      The host derives the principal from its current actor and never decodes a
      client string into an admitted identity. Reconnect and refresh mint a new
      nonce, so tokens from the prior context cannot match. *)
  type t [@@deriving equal, compare, sexp_of]

  include Comparator.S with type t := t

  (** All labels must be nonempty and individually bounded by 256 bytes. They
      are compared as exact bytes, never interpreted as paths or permissions.
      The trusted host ensures nonce uniqueness within its incarnation. *)
  val create
    :  principal:string
    -> host_incarnation:string
    -> connection_lease:string
    -> nonce:string
    -> t Or_error.t

  val principal : t -> string
  val host_incarnation : t -> string
  val connection_lease : t -> string
end

module Limits : sig
  type t

  (** Positive limits selected by the host. [max_contexts] and
      [max_total_source_bytes] govern the current store. Other limits apply to
      one context/generation. Source-byte charge counts the immutable base
      prepared inventory plus each open overlay's exact bytes, even when those
      bytes share an OCaml string. It is deliberately not a heap-size claim and
      cannot bound snapshots retained outside the host-owned current store.
      [max_effective_input_bytes] bounds selected source bytes plus the base
      prepared metadata; OCH-264 separately bounds changed metadata after
      complete closure validation. *)
  val create
    :  max_contexts:int
    -> max_sources:int
    -> max_open_documents:int
    -> max_document_bytes:int
    -> max_effective_input_bytes:int
    -> max_context_source_bytes:int
    -> max_total_source_bytes:int
    -> t Or_error.t
end

module Generation : sig
  type t [@@deriving compare, equal, sexp_of]

  val to_int : t -> int
end

module Editor_revision : sig
  (** Positive client revision, strictly increasing for a source while this
      context remains open, including after an overlay is closed. Refresh opens
      a new context identity and resets document revision history explicitly. *)
  type t [@@deriving compare, equal, sexp_of]

  val create : int -> t Or_error.t
  val to_int : t -> int
end

module Error : sig
  type t =
    | Unknown_context
    | Duplicate_context
    | Unknown_source of Chatml_source.Id.t
    | Read_only_source of Chatml_source.Id.t
    | Duplicate_editable_source of Chatml_source.Id.t
    | Stale_generation
    | Stale_document
    | Revision_not_increasing
    | Document_not_open
    | Open_documents
    | Incompatible_environment
    | Invalid_refresh_identity
    | Unsupported_basis
    | Static_selection_unavailable
    | Invalid_requested_environment
    | Limit_exceeded
    | Generation_overflow
    | Capture of Core.Error.t
  [@@deriving sexp_of]
end

module Version : sig
  (** Exact compare-and-swap token for one source in one context generation.
      It binds context identity, source ID/digest, last editor revision and
      open/closed state. It does not authorize an operation. *)
  type t [@@deriving equal, sexp_of]

  val identity : t -> Identity.t
  val generation : t -> Generation.t
  val source : t -> Chatml_source.Id.t
  val digest : t -> Chatml_source.Digest.t
  val editor_revision : t -> Editor_revision.t option
  val overlay_open : t -> bool
end

module View : sig
  (** A bounded summary of a project-basis context. Source graph,
      environment and contract come from the admitted base build, never client
      identity labels. [base_graph] changes only on explicit refresh, which
      returns a new context identity. *)
  type t

  val identity : t -> Identity.t
  val generation : t -> Generation.t
  val base_graph : t -> Chatml_source.Digest.t
  val environment : t -> string
  val compiler_contract : t -> Chatml_source.Digest.t
  val source_count : t -> int
  val editable_sources : t -> Chatml_source.Id.t list
  val open_document_count : t -> int
  val charged_source_bytes : t -> int
end

module Document : sig
  module View : sig
    (** A document-basis summary. The source is the exact current base/overlay
        revision. [requested_environment] is an inert user preference;
        [selected_environment] describes one optional host-admitted default
        static build. A ChatMD file may contain scripts selecting different
        environments; assistance validates each script against its own
        authentic selected catalog later. Neither label grants source or
        runtime access. *)
    type t

    val identity : t -> Identity.t
    val generation : t -> Generation.t
    val source : t -> Chatml_source.t
    val requested_environment : t -> string option
    val selected_environment : t -> string option
    val compiler_contract : t -> Chatml_source.Digest.t option
    val static_graph : t -> Chatml_source.Digest.t option
    val open_document_count : t -> int
    val charged_source_bytes : t -> int
  end

  module Inputs : sig
    (** Exact selected document bytes, its immutable original capture, and an
        optional separately admitted default static build. A build is real
        selected data, not a synthetic graph for the document; it does not
        claim to cover all embedded scripts or authorize imports/execution.
        Each script's selection and separate source/catalog grants are checked
        by its assistance owner. *)
    type t

    val identity : t -> Identity.t
    val generation : t -> Generation.t
    val source : t -> Chatml_source.t
    val base : t -> Chatml_source.t
    val static_build : t -> Chatml_project_build.t option
    val effective_input_bytes : t -> int
  end
end

module Inputs : sig
  (** Exact selected bytes of one immutable generation. [base] carries the
      original complete admitted graph and lock. If [changed_sources] is empty,
      those bytes remain its exact prepared graph. Otherwise this is pending
      source data only: OCH-264 must decode/catalog/compose the whole effective
      closure before OCH-258 publishes certified semantic facts or run admission
      occurs. Never use [base]'s prepared value for changed overlay bytes. *)
  type t

  val identity : t -> Identity.t
  val generation : t -> Generation.t
  val base : t -> Chatml_project_build.t
  val sources : t -> Chatml_source.t list
  val changed_sources : t -> Chatml_source.Id.t list
  val effective_input_bytes : t -> int
end

module Analysis_key : sig
  (** One validation/publication stamp, not a semantic result or proof.
      OCH-258 owns the one actual immutable payload and its real semantic
      structural limits. OCH-278 stores only this key, clearing it on every
      edit, document close, refresh or context close. *)
  type t [@@deriving equal, sexp_of]

  val identity : t -> Identity.t
  val generation : t -> Generation.t
  val validated_graph : t -> Chatml_source.Digest.t
end

type t

(** An empty immutable ledger. The host retains only the latest [t], serializes
    updates and drops old values after any in-flight response releases them.
    This ledger bounds current context slots and charged source bytes, not
    arbitrary immutable values retained by callers or exact OCaml heap use. *)
val create : limits:Limits.t -> t

val context_count : t -> int
val charged_source_bytes : t -> int
val find : t -> identity:Identity.t -> View.t option

(** Distinguish an unknown ID from a current project/document basis before
    dispatching basis-specific requests. This is an identity comparison only. *)
module Basis : sig
  type t =
    | Project
    | Document
  [@@deriving equal, sexp_of]
end

val basis : t -> identity:Identity.t -> Basis.t option

(** [find] above projects only Project; a live document ID returns [None] there
    and [Some] here. The host checks actor/connection affinity independently. *)
val find_document : t -> identity:Identity.t -> Document.View.t option

val version : t -> identity:Identity.t -> source:Chatml_source.Id.t -> Version.t option

(** Project-only semantic publication key. A document basis always returns
    [None], including with an optional static build: raw document bytes are not
    a validated Prepared closure and cannot claim certified graph analysis. *)
val analysis_key : t -> identity:Identity.t -> Analysis_key.t option

(** Admit one host-captured base build under a fresh host-issued identity. The
    trusted host supplies the exact editable source IDs derived from its grant
    and the selected project origins; the list must be unique, present in the
    base inventory and exclude the generated lock source. The host excludes
    captured Git packages and immutable upload members without edit rights;
    mere presence in the graph grants none. Check the full base inventory and
    aggregate ledger before retaining it. This pure
    call cannot establish whether the supplied build was authorized; the host
    must obtain it from OCH-277 or an admitted OCH-264 bundle receipt. *)
val admit
  :  t
  -> identity:Identity.t
  -> base:Chatml_project_build.t
  -> editable_sources:Chatml_source.Id.t list
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

(** Admit one host-captured source as a document basis, including an empty or
    malformed ChatMD file. No Build, Prepared graph, parse, source-root grant or
    static environment is required. A trusted host mints the source ID and
    authorizes either its exact root/member read or supplied bytes separately;
    the pure ledger cannot prove that provenance. [requested_environment] is
    nonempty when present, at most 256 bytes, and inert intent, never a
    selected catalog. Invalid names reject [Invalid_requested_environment]
    before retaining an identity. Charge the base source in
    the same context/aggregate ledger as project contexts and make this source
    the sole editable member. Failed admission retains no identity/bytes. *)
val admit_document
  :  t
  -> identity:Identity.t
  -> base:Chatml_source.t
  -> requested_environment:string option
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

(** Attach an actual separately host-admitted DEFAULT static build to a live
    document at [expected] generation. An unavailable or mismatched requested
    environment rejects this optional step and leaves editing usable. The host
    must authorize that catalog and its source roots independently of the
    document's origin; this pure call grants neither. Recheck all source/byte
    limits including retained catalog inventory before publication. Advance
    generation so prior document CAS and analysis results cannot be reused.
    The build is immutable until explicit detach/refresh, and no document
    source is inserted into its Prepared graph. Embedded scripts may select
    different environments; this default does not resolve them automatically.
    No Analysis_key is published; later document assistance must own its own
    source-bound result. *)
val attach_document_static
  :  t
  -> identity:Identity.t
  -> expected:Generation.t
  -> build:Chatml_project_build.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

(** Drop optional static data without closing or revising the document bytes.
    Advance the context generation, invalidating prior CAS and any analysis
    publication. Pure lexical/syntax help remains available. *)
val detach_document_static
  :  t
  -> identity:Identity.t
  -> expected:Generation.t
  -> (t, Error.t) Result.t

(** Full-text replacement of an existing captured source only. Check exact
    [expected] against the current context, generation and source before work;
    [next_revision] must exceed its last accepted revision. Preflight per-call,
    context and aggregate byte charges before hashing/index allocation. Capture
    exact bytes under the same source ID without UTF-8 or newline repair. The
    result is an unvalidated pending generation, never a prepared graph. *)
val replace
  :  t
  -> expected:Version.t
  -> next_revision:Editor_revision.t
  -> contents:string
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

(** Discard an open overlay, restore its exact base bytes and advance generation.
    Retain its last editor revision to prevent reuse within this context.
    Restoration can fail without changing the context if another open edit makes
    the restored effective input exceed its cap. *)
val close_document : t -> expected:Version.t -> (t, Error.t) Result.t

module Refresh : sig
  type policy =
    | Reject_open_documents
    | Discard_open_documents
  [@@deriving equal, sexp_of]
end

(** Project-only: a document basis returns [Unsupported_basis]. Explicitly
    replace one context with a fresh host-captured base and NEW
    identity. Reject stale generation or open overlays unless the caller selects
    [Discard_open_documents]. The new base must select the same environment and
    authentic compiler contract; a selection change opens a separate context.
    The host rechecks source/actor authority before this call. No overlay merge,
    filesystem write, Git fetch or implicit retry. *)
val refresh
  :  t
  -> identity:Identity.t
  -> expected:Generation.t
  -> new_identity:Identity.t
  -> base:Chatml_project_build.t
  -> editable_sources:Chatml_source.Id.t list
  -> policy:Refresh.policy
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

(** Explicitly replace a host-document base capture under a NEW context ID.
    The host re-reads only its retained authorized root/member, then supplies
    that exact source with the same source ID; the pure ledger checks ID and
    generation, open-overlay policy and quotas. Optional static selection is
    dropped and must be separately re-admitted. A supplied document has no
    refresh read authority: the host must reject that origin and open a new
    context for different supplied base bytes. This pure function never reads. *)
val refresh_document
  :  t
  -> identity:Identity.t
  -> expected:Generation.t
  -> new_identity:Identity.t
  -> base:Chatml_source.t
  -> policy:Refresh.policy
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

(** Project-only: a document basis returns [Unsupported_basis]. Snapshot the
    exact selected source list for one current generation. This
    may be retained only within the host's independently bounded response/task
    lifetime; the ledger cannot account for arbitrary caller-held old values. *)
val inputs
  :  t
  -> identity:Identity.t
  -> checkpoint:(unit -> unit)
  -> (Inputs.t, Error.t) Result.t

(** Return exact selected document bytes at one generation. Project-basis IDs
    reject [Unsupported_basis]; no graph validation or semantic certification. *)
val document_inputs
  :  t
  -> identity:Identity.t
  -> checkpoint:(unit -> unit)
  -> (Document.Inputs.t, Error.t) Result.t

(** Project-only: a document basis returns [Unsupported_basis]. Record only
    the key of a separately validated and published OCH-258 result.
    The host must compare OCH-264's complete closure receipt and result identity
    before calling. This cannot certify an arbitrary digest or store a payload.
    Exactly one current key exists per context; a newer publication replaces it. *)
val note_analysis
  :  t
  -> identity:Identity.t
  -> expected:Generation.t
  -> validated_graph:Chatml_source.Digest.t
  -> (t, Error.t) Result.t

(** Remove one context and its source charge. The host first cancels/joins its
    activity; reconnect uses a newly minted identity and re-admits source data.
    Old versions and analysis keys cannot address the new context. *)
val close_context : t -> identity:Identity.t -> (t, Error.t) Result.t
