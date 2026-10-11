open Core
module Captured = Chatml_source
module Prepared = Chatml_prepared_program
module Build = Chatml_project_build
module Contract = Chatml.Chatml_compiler_contract
module Storage = Agent_store.Prompt_program_bundle

(** Parser-owned exact authored/effective bytes, roles and affine ranges. *)
module Parser_input = Chatmd_parser_input

(** Immutable parser and program custody owner. All operations are computations over supplied
    immutable bytes. No source loading, native directory, compiler, initialized
    module state, tool capability or runtime callback is retained. *)

module Limits : sig
  type t

  (** Explicit host ceilings; no defaults. The artifact's existing physical file,
      byte and complete manifest ceilings additionally apply, jointly with the
      authored ChatMD inventory. Logical source revisions (ID/digest/length) and program count are
      separately bounded even when many identities share one physical payload.
      [max_total_input_bytes] bounds the sum of Prepared canonical input bytes
      across programs, deliberately counting a shared source in each program.
      [project_work_steps] is one total allowance for selected receipt restores;
      it is never reset for each selected program. Other codec work is bounded by
      explicit cardinality, bytes and document ceilings and polls checkpoint. *)
  val create
    :  max_programs:int
    -> max_parser_inputs:int
    -> max_logical_sources:int
    -> max_total_input_bytes:int
    -> project_work_steps:int
    -> document:Document_schema.Limits.t
    -> t Or_error.t
end

module Binding : sig
  (** Exact declaration ownership plus one qualified script identity and the
      parser's exact extracted ChatML body digest. Declaration coordinates are
      half-open byte offsets in its exact effective Parser_input, not the inline
      body or original authored bytes when preprocessing changed them. Retain
      registration root: restart replays its saved effective root input and raw
      captured imports to reproduce namespace context. No native dirs. *)
  type t [@@deriving compare, equal, sexp_of]

  (** Admit one actual declaration belonging to [parsed.declarations]. Its Range
      input must belong to the same Parsed inventory. Retain that exact range,
      qualified ID and hash the parser's actual extracted body; callers cannot
      supply replacement offsets, ID or claimed body digest. The exact Parsed
      Root input (including role and authored/effective revisions) identifies the
      registration context. Logical paths must satisfy the portable-path domain.
      Input-sized membership/body work polls checkpoint. *)
  val of_declaration
    :  Prompt.Chat_markdown.Parsed.t
    -> Prompt.Chat_markdown.Declaration.t
    -> checkpoint:(unit -> unit)
    -> t Or_error.t

  val declaration_path : t -> Source.Portable_path.t
  val registration_input : t -> Parser_input.t
  val declaration_digest : t -> Captured.Digest.t
  val parser_input : t -> Parser_input.t
  val start_byte : t -> int
  val end_byte : t -> int
  val script_id : t -> string
  val script_digest : t -> Captured.Digest.t
end

module Input : sig
  type t =
    | Provided of Prepared.t
    | Selected of Build.t
end

module Error : sig
  type t =
    | Document of Document_schema.Error.t
    | Storage of Agent_store.Store_error.t
    | Provided of Chatml_program_document.Error.t
    | Selected of Build.Error.t
    | Invalid_binding of string
    | Duplicate_program of Binding.t * Captured.Digest.t
    | Conflicting_source of Captured.Id.t
    | Missing_contract of Binding.t
    | Unexpected_contract of Binding.t
    | Contract_mismatch of Binding.t
    | Script_mismatch of Binding.t
    | Invalid_inventory of string
    | Limit_exceeded
end

(** Retains the original admitted named document and exact immutable private
    payload inventory. Admission does not imply graph/compiler/runtime authority. *)
type t

(** Capture all programs against these same authored bytes and actual parser
    contexts. [root] is an exact Root from the supplied contexts and selects the
    current durable registration context, including the zero-program case. Each
    binding must belong to a supplied context; each active binding must belong to
    this selected Root. Derive the complete parser-input union from those contexts;
    reject conflicting role/path revisions. Actual constructors and source checks
    admit every supplied program at initial capture, including inactive variants.
    Provided metadata
    delegates to its existing codec. Selected metadata records only the actual
    exact lock source ID, expected Prepared fingerprint and expected contract;
    it never substitutes a provided graph for selected receipt restoration.

    Program identity is the binding PLUS exact compiler contract digest. A single
    declaration may have separately prepared authentic moderator/delegated roles;
    duplicate identity pairs reject. This never silently retargets one graph.
    One outer table stores source revisions (ID/digest/length); each program
    references its own exact revisions. IDs are graph-scoped: independent programs
    may use different bytes at the same ID. Each program rejects duplicate IDs;
    globally equal revisions must also have exact equal bytes. Distinct IDs and
    revisions remain distinct even when physical bodies match. The physical SHA256 pool deduplicates equal bodies
    and rejects a digest collision with different bodies. Every selected lock body
    belongs to that pool; no canonical lock re-rendering occurs.

    Preflight program/logical inventory counts, opaque ID expansion and cumulative
    metadata allowances before projection/hex/JSON construction; use existing
    document measurement for scalar encoding. Per-codec limits consume the
    remaining outer allowance, never the initial ceiling repeatedly. Check the
    authored-plus-private physical count/bytes before creating storage payloads.
    The final Artifact constructor still bounds its complete outer manifest.
    Failure returns no partially captured document/payload list. *)
val capture
  :  Chatmd_source_bundle.t
  -> parsed_contexts:Prompt.Chat_markdown.Parsed.t list
  -> root:Parser_input.t
  -> programs:(Binding.t * Input.t) list
  -> active_programs:(Binding.t * Captured.Digest.t) list
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

(** Check an incoming prepared custody against a first-admission parser capture:
    exact current Root and complete actual parser-input set, with every original
    binding owned by one supplied Parsed. No preprocessing provenance is inferred
    from hashes. Graph authenticity remains the host's preparation responsibility. *)
val validate_capture
  :  t
  -> parsed_contexts:Prompt.Chat_markdown.Parsed.t list
  -> root:Parser_input.t
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (unit, Error.t) Result.t

(** Check the durable active mappings against the actual replayed current Root,
    without graph restoration or inventing authentic contracts. *)
val validate_selection
  :  t
  -> parsed:Prompt.Chat_markdown.Parsed.t
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (unit, Error.t) Result.t

val to_storage : t -> Storage.t

(** Bounded immutable original program identity inventory, available before graph
    restoration so the host can reproduce each retained registration-root parser
    context and supply the correct authentic contract. No source bodies or live
    native authority. *)
val bindings : t -> (Binding.t * Captured.Digest.t) list

(** Exact admitted effective parser bytes and original authored associations.
    Restart uses these inputs with Fn.id; it never repeats preprocessing/provider
    work. Equal effective/authored bodies use an explicit authored reference;
    transformed bodies reside in the same bounded private SHA256 payload pool. *)
val parser_inputs : t -> Parser_input.t list

(** Exact current Root selection; replay it without preprocessing before restoring
    its active program selection. Other retained Root inputs remain available for
    child selection without consulting a parent registry. *)
val root_input : t -> Parser_input.t

(** Durable retained-child selection. Source inventory and original program
    records remain unchanged. Retain explicit original-program -> current child
    binding mappings, with selected contract digest, under this child root path
    in the named metadata before artifact publication. Validate exact declaration
    path/digest/range/body preservation against the supplied child bundle. Only
    qualified IDs and registration context may change; one-to-one selections
    reject ambiguous aliases. [parsed] is the actual child Root context, already
    retained in the parser inventory or added through [extend]. Mapped current
    bindings must belong to this context. A raw Import cannot substitute for a
    separately preprocessed child Root with changed effective bytes. Rerooting
    changes durable selection, not semantic admission: the host must successfully
    restore the new active selection with authentic contracts before admitting or
    publishing its runtime. An empty selection still retains this exact child Root. The child artifact retains the complete authored closure and private
    payload pool; restart can read its own mappings without a parent registry.
    Standard extension ownership applies to this known selection projection;
    unsafe unknown-field deletion returns a conflict.

    [programs] selects existing original program identities. A different target
    requires a separately prepared graph, never a changed stored digest. *)
val reroot
  :  t
  -> authored:Chatmd_source_bundle.t
  -> parsed:Prompt.Chat_markdown.Parsed.t
  -> programs:(Binding.t * Captured.Digest.t * Binding.t) list
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

(** Add missing exact-source, authentic-role variants during child preparation,
    before its immutable artifact publication. Existing records and unknown
    metadata survive; conflicts reject. The host supplies these Input values
    from legitimate preparation with captured input and the actual target owner.
    A selected role variant must be an actual Build.t; restore does not retarget
    an existing selected lock. This operation performs no preparation/acquisition
    and gives no permission to do so. [parsed_contexts] must include the actual
    current Root context as well as contexts for the added bindings; an empty or
    new-context-only list cannot substitute for that retained parser ownership. *)
val extend
  :  t
  -> authored:Chatmd_source_bundle.t
  -> parsed_contexts:Prompt.Chat_markdown.Parsed.t list
  -> programs:(Binding.t * Input.t) list
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

(** Adopt a complete incoming child/role projection without dropping previous
    immutable program records, exact logical sources, parser inputs or unknown
    fields. Validate the supplied authored closure before transferring extension
    ownership. Conflicting or unsafe unknown-field changes reject. *)
val adopt
  :  t
  -> previous:t
  -> authored:Chatmd_source_bundle.t
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

(** Current named document admission with original envelope/unknown fields
    retained. Reconcile every binding against the exact supplied authored bundle;
    pre-bound all lists/expanded IDs before maps/walks. Require precisely the union
    of referenced logical sources/effective parser inputs and their exact physical inventory,
    rejecting duplicate revision identities, duplicate IDs within each program, unreferenced logical entries and surplus payloads.
    Source bodies exist only in private verified Storage.Payload values.
    Unknown required semantics/conversions fail through Document_schema. Validate
    every inactive record's full metadata schema and source membership too; no
    malformed record is dropped. This structural admission does not restore an
    inactive graph with an invented contract. Initial [capture]/[extend] receive
    actual admitted graphs; later storage admission has no historical proof of
    their prior semantic admission. *)
val of_storage
  :  Storage.t
  -> authored:Chatmd_source_bundle.t
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

module Restored : sig
  module Program : sig
    type t

    val binding : t -> Binding.t

    (** Preserve the actual admitted restoration kind. Selected returns its real
        Build value, allowing captured-only recomposition; never construct a fake
        Build from the Prepared projection. *)
    val input : t -> Input.t

    val prepared : t -> Prepared.t
    val compiler_contract : t -> Captured.Digest.t
  end

  (** Complete immutable restored graphs, with original admitted metadata retained
      separately. No linkage, initializer, session, filesystem or grant. *)
  type t

  (** Current durable root selection only. Inactive custody records are never
      exposed as executable Prepared values; they remain in the custody value for
      later selection and authentic restoration. *)
  val programs : t -> Program.t list

  val find : t -> Binding.t -> compiler_contract:Captured.Digest.t -> Program.t option
end

(** Supply precisely one authentic current static contract per active selected
    binding/contract pair, derived from the actual replayed declaration registry
    and separately selected invocation role. [parsed] must replay [root_input]
    exactly and its current bindings must match the durable selection. Authenticate
    declaration membership/body again; metadata assertions alone are insufficient.
    Reject missing, duplicate or surplus active contracts. Inactive original
    variants need no parent-only host contract and are not semantically restored.
    A future reroot must authenticate and restore its own new active selection.

    Standard Contract.for_target derives pure current role metadata without live
    tool authority. Custom/native metadata must still be supplied by its actual
    host owner. Contract restoration grants neither implementation nor capability.

    Admit union Captured sources once using their exact IDs/expected digests.
    Provided restore delegates to Chatml_program_document.restore. Selected
    restore uses EXACT retained lock bytes through Build.restore, excluding that
    positional lock capture from its source argument only. Compare restored
    fingerprint and contract, and the entry source digest with Binding's exact
    parser-selected script body. Do not reconstruct selected scopes separately.

    Each graph receives remaining aggregate input capacity; charge actual
    Prepared.Accounting.input_bytes after success. Each selected restore receives
    remaining project work and charges its actual Build.Accounting.work_steps.
    Abort on any error, with no partial publication, failed receipt refund or
    per-program allowance reset. All descendant checkpoint exceptions propagate
    unchanged. Compilation/authority/initialization remain later host owners. *)
val restore
  :  t
  -> parsed:Prompt.Chat_markdown.Parsed.t
  -> contracts:(Binding.t * Contract.t) list
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (Restored.t, Error.t) Result.t
