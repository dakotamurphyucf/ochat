open Core

(** Pure versioned project declarations and selected scope intent. No directory is
    scanned, repository fetched, compiler invoked or initializer executed. Exact
    captured declaration bytes own every source location. Native authority and
    executable bindings remain with host admission. *)

module Declared_at : sig
  type t

  (** Whole exact declaration range plus decoded JSON field path. The JSON codec
      does not supply token positions; this never claims a field-byte range. *)
  val create : Chatml_source.t -> field_path:string list -> checkpoint:(unit -> unit) -> t

  val source : t -> Chatml_source.t
  val location : t -> Chatml_source.Location.t
  val field_path : t -> string list
end

module Error : sig
  (** Existing schema/domain validation error plus its exact captured declaration.
      No renderer, excerpt store or second diagnostic system. Host presentation
      adapts details/field paths to the shared bounded ChatML diagnostic. Retains
      the supplied immutable capture even if decode rejects its size; capture
      admission belongs to the caller. No parsed tree or checkpoint is retained. *)
  type t

  (** Records supplied immutable ownership and schema details without scanning the
      source, validating capture size, or inventing a metadata-derived location. *)
  val create
    :  Chatml_source.t
    -> details:Document_schema.Error.t
    -> field_path:string list
    -> t

  val source : t -> Chatml_source.t
  val details : t -> Document_schema.Error.t
  val field_path : t -> string list
end

module Git_pin : sig
  type object_format =
    | Sha1
    | Sha256
  [@@deriving equal, sexp_of]

  (** Exact full lowercase hexadecimal commit identity, 40/64 bytes for the
      explicitly selected object format. No ref resolution or format inference. *)
  type t [@@deriving equal, sexp_of]

  val create : object_format:object_format -> commit:string -> t Or_error.t
  val object_format : t -> object_format
  val commit : t -> string
end

module Dependency : sig
  type source =
    | Local of { directory : string }
    | Git of
        { repository : string
        ; pin : Git_pin.t
        }

  (** Local directory can be relative to the declaring project or absolute on the
      selected runtime host, under explicit host authority. It is not a portable
      source member ID or granted root. Repository locator is untrusted intent;
      the Git host adapter validates supported transport/authentication. Neither
      locator is resolved here. Namespaces never derive from locator/name/hash. *)
  type t

  val name : t -> string
  val source : t -> source
  val project : t -> Source.Portable_path.t
  val environment : t -> string option
  val namespace_override : t -> string option
  val declared_at : t -> Declared_at.t
end

module Module_ref : sig
  type catalog =
    | Self
    | Dependency of string
  [@@deriving equal, sexp_of]

  type t [@@deriving equal, sexp_of]

  val catalog : t -> catalog
  val name : t -> string
end

module Source_dir : sig
  type directory =
    | Root
    | Relative of Source.Portable_path.t

  (** Explicit nonrecursive source directory. Root is authored as '.', without
      weakening Source.Portable_path's file/member invariants. *)
  type t

  val directory : t -> directory
  val declared_at : t -> Declared_at.t
end

module Module_override : sig
  type interface_selection =
    | Companion
    | Explicit of Source.Portable_path.t

  (** Omitted interface selects an optional conventional companion for the chosen
      implementation during host preparation. Explicit interface must exist.
      Config null is invalid; no invented interface-only module or disable mode. *)
  type t

  val name : t -> string
  val implementation : t -> Source.Portable_path.t
  val interface_selection : t -> interface_selection
  val declared_at : t -> Declared_at.t
end

module Compiler_requirement : sig
  (** Compiler-owned family/target names, checked by the existing target adapter.
      No independent compiler version registry or native-surface union. Optional
      exact digest is an advanced constraint, not routine author bookkeeping. *)
  type t

  val compiler_family : t -> string option
  val allowed_targets : t -> string list option
  val exact_contract : t -> Chatml_source.Digest.t option
  val declared_at : t -> Declared_at.t option
end

module Scope_spec : sig
  type binding = private
    { name : string
    ; module_ref : Module_ref.t
    ; declared_at : Declared_at.t
    }

  (** None means the selected package's own complete catalog. Some bindings is
      the entire explicit selected set, including a deliberately empty set.
      Dependency catalogs never inherit root/importer-local names. *)
  type t

  val name : t -> string
  val bindings : t -> binding list option
  val compiler_requirement : t -> Compiler_requirement.t
  val declared_at : t -> Declared_at.t option
end

module Entry : sig
  type t

  val name : t -> string
  val source : t -> Source.Portable_path.t
  val target_name : t -> string
  val declared_at : t -> Declared_at.t
end

module Selection : sig
  type request =
    | Entry of
        { name : string
        ; environment : string option
        }
    | Library of { environment : string option }

  type t

  val entry : t -> Entry.t option
  val environment : t -> Scope_spec.t

  (** Exact-file overrides select another whole environment of the SAME package.
      Implementation and interface paths select independently. All referenced
      environment names are validated before publication, so lookup is total. *)
  val scope_for : t -> source:Source.Portable_path.t -> Scope_spec.t
end

type t

(** Flat schema_version=1 JSON. Duplicate keys/unknown semantic fields/null
    selectors reject. Reuse Document_schema JSON validation and presence rules.
    Poll before/after its bounded decode stage and during domain conversion.
    Existing JSON/Jsonaf parsing itself has no checkpoint API and is a bounded
    non-interruptible stage; this is cooperative cancellation, not a deadline.
    Exceptions from checkpoints propagate unchanged; no callback is retained. *)
val decode
  :  Chatml_source.t
  -> limits:Document_schema.Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

val source : t -> Chatml_source.t

(** Already validated root context of the exact decoded declaration. Reuses its
    immutable whole-source location and empty JSON field path without rescanning. *)
val declared_at : t -> Declared_at.t

val namespace : t -> string
val source_dirs : t -> Source_dir.t list
val exclusions : t -> (Source.Portable_path.t * Declared_at.t) list
val module_overrides : t -> Module_override.t list
val dependencies : t -> Dependency.t list

(** Exact host tool names requested against the existing effective registry:
    Tool_capability.select already performs selection without grants/invocation.
    No synthetic capability IDs, live references, callbacks or implementations.
    Host adaptation/revalidation remains required; fixed target builtins unchanged.
    These requirements do not define per-file native permission profiles. *)
val required_tools : t -> (string * Declared_at.t) list

(** Explicit selector, entry's declared selector or package-owned default; never
    propagation of another package's environment name. Missing explicit/default
    selections fail precisely, never selecting the first map item. *)
val select
  :  t
  -> request:Selection.request
  -> checkpoint:(unit -> unit)
  -> (Selection.t, Error.t) Result.t

module Lock : sig
  (** Pure selected-build receipt of expected catalog/config/source/interface/scopes
      and compiler compatibility, never proof of captured bytes, an installer,
      credential or executable grant. OCH192 reconciles expected digest/byte length,
      origin and selected environments against actual captured declarations before
      complete183 admission;193 consumes and qualifies that result.

      The serialized inventory excludes the selected lock's OWN ID/bytes/digest and
      final Prepared fingerprint. Host composition encodes the receipt first, captures
      it, then admits the complete183 input. Error ownership retained by this codec is
      separate from the serialized expected source inventory. *)

  module Error = Error

  module Source_entry : sig
    type t

    (** Nonnegative EXPECTED byte length and validated identity/digest syntax. This
        metadata does not verify any captured source. No bytes or live-file lookup
        are retained;192 must compare it against actual immutable captures. *)
    val create
      :  Chatml_source.Id.t
      -> digest:Chatml_source.Digest.t
      -> byte_length:int
      -> t Or_error.t

    val id : t -> Chatml_source.Id.t
    val digest : t -> Chatml_source.Digest.t
    val byte_length : t -> int
  end

  module Entry : sig
    type selection =
      | Declared of string
      | Captured

    (** Exact entry facts, never a live source lookup. Declared names are reconciled
        against the root declaration; Captured uses the root selected environment
        without inventing a file override or containing-document source location. *)
    type t

    val create : Source_entry.t -> selection:selection -> t Or_error.t
    val source : t -> Source_entry.t
    val selection : t -> selection
  end

  module Module_selection : sig
    type t

    (** Exact association, not companion discovery. Distinct implementation/interface
        IDs required; module identity derives from implementation as in183. *)
    val create
      :  implementation:Chatml_source.Id.t
      -> interface:Chatml_source.Id.t option
      -> t Or_error.t

    val implementation : t -> Chatml_source.Id.t
    val interface : t -> Chatml_source.Id.t option
  end

  module File_scope : sig
    type binding =
      { name : string
      ; module_id : Chatml_prepared_program.Module_id.t
      }

    type t

    (** Configured UIDENT names, duplicate names rejected; aliases to one module
        identity permitted. Environment is the selected own-package NAME intent;192 verifies membership
        in the actual captured declaration.
        No raw Source.span deserialization is promoted to a validated location. *)
    val create
      :  Chatml_source.Id.t
      -> environment:string
      -> bindings:binding list
      -> checkpoint:(unit -> unit)
      -> t Or_error.t

    val source : t -> Chatml_source.Id.t
    val environment : t -> string
    val bindings : t -> binding list
  end

  module Dependency_selection : sig
    (** The containing catalog owns this named dependency association. Locator,
        project member, namespace and environment intent remain in its exact
        captured declaration. Distinct names may select one declaration. *)
    type t

    val create
      :  name:string
      -> declaration:Chatml_source.Id.t
      -> checkpoint:(unit -> unit)
      -> t Or_error.t

    val name : t -> string
    val declaration : t -> Chatml_source.Id.t
  end

  module Catalog : sig
    type origin =
      | Local
      | Git of
          { repository : string
          ; pin : Git_pin.t
          }

    type t

    (** Validated wire/domain metadata, not a second Prepared_program admission.
        Nonempty namespace/selected-environment names and exact record-key duplicates
        are checked. Declaration, source entries, implementation/interface IDs and
        file-scope OWNER IDs must have this catalog's namespace. Scope binding TARGET
        Module_ids may cross catalog namespaces. These checks do not establish source
        membership or that an environment exists in a captured declaration.

        Complete portable-collision/inventory/association validation remains with183
        after192 reconciles the receipt against actual captured bytes and selections.
        Aliases live in scopes, not duplicated inventory entries. Local/Git locators
        remain in incoming captured declarations; Git repository/pin is validated
        provenance intent, never a granted capability or ephemeral native identity.
      Dependency names are canonical owner-local keys; their targets may cross
      namespaces. No second graph validator or placeholder captures. *)
    val create
      :  declared_at:Declared_at.t
      -> namespace:string
      -> origin:origin
      -> declaration:Chatml_source.Id.t
      -> selected_environment:string
      -> dependencies:Dependency_selection.t list
      -> sources:Source_entry.t list
      -> modules:Module_selection.t list
      -> scopes:File_scope.t list
      -> checkpoint:(unit -> unit)
      -> (t, Error.t) Result.t

    val namespace : t -> string
    val origin : t -> origin
    val declaration : t -> Chatml_source.Id.t
    val selected_environment : t -> string
    val dependencies : t -> Dependency_selection.t list
    val sources : t -> Source_entry.t list
    val modules : t -> Module_selection.t list
    val scopes : t -> File_scope.t list
  end

  (** Immutable receipt plus ONE encoding-error ownership context: create retains
      the supplied declared_at; decode retains the exact decoded lock source and its
      whole-declaration origin. This context is excluded from serialized receipt
      identity. No parsed JSON tree, callback or capability is retained. *)
  type t

  (** Validate primitive fields/duplicate keys and canonicalize typed metadata.
      Catalog namespaces must be unique. Complete source/scopes/binding association
      and portable-collision validation is183's responsibility after192 reconciles
      expected bytes/origins/own-package environment selections against captures.
      Host composition deduplicates genuinely equal aliases before construction and
      MUST exclude the future selected lock ID from the serialized source inventory;
      this constructor receives no selected lock ID and cannot prove that exclusion.
      Root identifies the selected project declaration; Entry records both exact
    source metadata and declared-name versus explicitly captured-host selection.
    Entry may also be a module implementation with the SAME Source.Id; no silent
      role exclusion or initialization here.

      Target/family names and required_tools are intent/compatibility metadata only.
      Actual compiler target/contract and live tool admission must still be checked
      by the host adapter. Tool names never restore a saved live reference/grant.
      All input traversal polls checkpoint, checks overflow and validates its domain.
      No actual source-cycle analysis, compiler invocation or publication. *)
  val create
    :  declared_at:Declared_at.t
    -> root:Chatml_source.Id.t
    -> entry:Entry.t
    -> environment:string
    -> compiler_family:string
    -> target_name:string
    -> compiler_contract:Chatml_source.Digest.t
    -> required_tools:string list
    -> catalogs:Catalog.t list
    -> checkpoint:(unit -> unit)
    -> unit
    -> (t, Error.t) Result.t

  (** Flat schema_version=1 JSON with existing limits/presence validation. Duplicate
      keys, unknown semantic fields and null required/optional selectors reject.
      Reject the decoded lock's own source ID in its serialized source inventory.
      Retain the exact lock source as the ONE context for honest decode/encoding
      errors; successful metadata remains expected, not verified against live files.
      Poll around bounded non-interruptible JSON parsing and during conversion. *)
  val decode
    :  Chatml_source.t
    -> limits:Document_schema.Limits.t
    -> checkpoint:(unit -> unit)
    -> (t, Error.t) Result.t

  (** Bounded canonical output: fixed compact object fields; catalogs by namespace,
      sources/scopes by typed Source.Id, module selections by implementation Source.Id,
      bindings by exact name and required_tools as an exact sorted set. Preserve
      namespace/path as separate fields and distinct aliases sharing one Module_id.
      Reject duplicate keys during canonical sorting and measure escaped JSON size before
      rendering. Overflow/output-limit errors use t's retained ownership context;
      checkpoint exceptions escape unchanged. No own lock digest/final Prepared
      fingerprint or encoding-error context is embedded in this output. *)
  val to_string
    :  t
    -> limits:Document_schema.Limits.t
    -> checkpoint:(unit -> unit)
    -> (string, Error.t) Result.t

  val root : t -> Chatml_source.Id.t
  val entry : t -> Chatml_source.Id.t
  val entry_selection : t -> Entry.t
  val environment : t -> string
  val compiler_family : t -> string
  val target_name : t -> string
  val compiler_contract : t -> Chatml_source.Digest.t
  val required_tools : t -> string list
  val catalogs : t -> Catalog.t list
end
