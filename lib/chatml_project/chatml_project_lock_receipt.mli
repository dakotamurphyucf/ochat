open Core

(** Pure selected-build receipt of expected catalog/config/source/interface/scopes
    and compiler compatibility, never proof of captured bytes, an installer,
    credential or executable grant. OCH192 reconciles expected digest/byte length,
    origin and selected environments against actual captured declarations before
    complete183 admission;193 consumes and qualifies that result.

    The serialized inventory excludes the selected lock's OWN ID/bytes/digest and
    final Prepared fingerprint. Host composition encodes the receipt first, captures
    it, then admits the complete183 input. Error ownership retained by this codec is
    separate from the serialized expected source inventory. *)

module Error = Chatml_project_declaration.Error

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
        ; pin : Chatml_project_declaration.Git_pin.t
        ; package_root : Source.Portable_path.t option
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
      provenance intent, never a granted capability or ephemeral native identity. Git package_root is a portable repository prefix (None means repository root); source IDs remain package-relative. The exact repository declaration member is prefix plus declaration relative path.
      Dependency names are canonical owner-local keys; their targets may cross
      namespaces. No second graph validator or placeholder captures. *)
  val create
    :  declared_at:Chatml_project_declaration.Declared_at.t
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

val declared_at : t -> Chatml_project_declaration.Declared_at.t

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
  :  declared_at:Chatml_project_declaration.Declared_at.t
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

val root : t -> Chatml_source.Id.t
val entry : t -> Chatml_source.Id.t
val entry_selection : t -> Entry.t
val environment : t -> string
val compiler_family : t -> string
val target_name : t -> string
val compiler_contract : t -> Chatml_source.Digest.t
val required_tools : t -> string list
val catalogs : t -> Catalog.t list
