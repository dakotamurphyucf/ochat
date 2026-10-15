open Core
module Captured = Chatml_source

(** Immutable admitted source/catalog/file
    scopes. No parsing, actual edges, source preparation or execution. Existing
    Chatml_compilation target/policy/contract types remain with their owner. *)

module Module_id : sig
  (** Implementation source identity. Scoped aliases with this ID share later
      checking/runtime identity; content revisions change digests, not this ID. *)
  type t [@@deriving compare, equal, hash, sexp_of]

  include Comparator.S with type t := t

  val of_implementation : Captured.Id.t -> t
  val implementation : t -> Captured.Id.t
end

module Environment : sig
  (** Named environment and exact required compiler contract digest. OCH-124
      owns compatibility decoding and mapping to Chatml_compilation.contract.
      No new language/library registry, target variant or config syntax. *)
  type t

  val create
    :  name:string
    -> required_contract:Captured.Digest.t
    -> declared_at:Captured.Location.t option
    -> t Or_error.t

  val implicit : required_contract:Captured.Digest.t -> t
  val name : t -> string
  val required_contract : t -> Captured.Digest.t
  val declared_at : t -> Captured.Location.t option
end

module Provenance : sig
  (** Captured identity only. OCH-124 decides when a lock is required and admits
      its schema/pins; OCH-126 owns custody. Omitted lock means no selected lock,
      never an invented empty document. No URL/installer/tool authority here. *)
  type t =
    | Implicit
    | Project of
        { declaration : Captured.Id.t
        ; lock : Captured.Id.t option
        }
end

module Definition : sig
  (** Explicit implementation/optional interface; no extension guessing. ID is
      derived from implementation. Reject equal implementation/interface IDs. *)
  type t

  val create
    :  implementation:Captured.Id.t
    -> interface:Captured.Id.t option
    -> declared_at:Captured.Location.t
    -> t Or_error.t

  val id : t -> Module_id.t
  val implementation : t -> Captured.Id.t
  val interface : t -> Captured.Id.t option
  val declared_at : t -> Captured.Location.t
end

module Scope : sig
  (** One exact file's selected bindings, including interface files. OCH-124
      applies config selection. Names follow existing lexer UIDENT (uppercase
      ASCII first, then ASCII letters/digits/underscores). Duplicate configured
      names reject; distinct names may reference one ID. Availability is no edge.
      Native builtin shadowing is OCH-185/186 lexical policy, not admission. *)
  type binding =
    { name : string
    ; module_id : Module_id.t
    ; declared_at : Captured.Location.t
    }

  type t

  val create
    :  Captured.Id.t
    -> bindings:binding list
    -> selected_at:Captured.Location.t option
    -> checkpoint:(unit -> unit)
    -> t Or_error.t

  val source : t -> Captured.Id.t
  val bindings : t -> binding list
  val selected_at : t -> Captured.Location.t option
  val find : t -> name:string -> binding option
end

module Accounting : sig
  (** Entire inventory, including unused catalog/config/lock bytes. Duplicate IDs
      reject. Metadata is canonical encoded IDs/associations/scopes/locations,
      excluding source contents. All sums check overflow. *)
  type t

  val source_count : t -> int
  val source_bytes : t -> int
  val metadata_bytes : t -> int
  val input_bytes : t -> int
end

module Error : sig
  type problem =
    | Duplicate_source of Captured.Id.t
    | Portable_source_collision of Captured.Id.t * Captured.Id.t
    | Missing_source of Captured.Id.t
    | Duplicate_module of Module_id.t
    | Missing_module of Module_id.t
    | Duplicate_scope of Captured.Id.t
    | Missing_scope of Captured.Id.t
    | Invalid_location of Captured.Location.t
    | Incompatible_contract of
        { required : Captured.Digest.t
        ; selected : Captured.Digest.t
        }
    | Input_limit of
        { limit_bytes : int
        ; observed_bytes : int
        }
    | Invalid_input_limit of int
    | Accounting_overflow
  [@@deriving sexp_of]

  (** Message bounded to the current compiler's 16 KiB convention. *)
  type t = private
    { problem : problem
    ; locations : Captured.Location.t list
    ; message : string
    }
  [@@deriving sexp_of]
end

(** Immutable; no inference cells, runtime registry, retained checkpoint closure,
    Eio capability, installer, credential or mutable global source cache. *)
type t

(** Validate associations and revision-owned source locations; every entry,
    implementation and interface has an explicit scope. Config/lock-only sources
    need no executable scope. Unknown references/duplicates/portable collisions
    reject. Canonicalize with typed comparisons. Required/selected contract
    digests must agree. The supplied digest grants no authority: OCH-186 checks
    it against the actual target's current contract before typechecking.

    One admission byte cap, no new limits framework: [Some n] requires positive n
    and bounds exact source bytes PLUS canonical metadata. Every encoded record
    or binding consumes positive bytes, bounding cardinality too. [None] removes
    the ceiling. Both modes check overflow and poll checkpoint during inventory
    walks, comparisons and bounded hashing/encoding chunks. Exceptions propagate;
    no partial result or global cache mutation. No clock/default/global ceiling.
    The single-file adapter below uses [None]; callers must retain their existing
    submitted-string check. Existing compilation wrappers are unchanged. Graph
    mapping from existing host policy is reviewed in
    the host/compiler adapter, not encoded into a new declaration syntax.

    SHA-256 fingerprint: version tag, selected contract, environment, entry,
    provenance, sorted exact source ID/digest/byte lengths, definitions, scopes,
    bindings and revision-owned locations. Encode unambiguous canonical fields.
    Changed captured bytes/selected associations change identity subject to SHA
    collision assumptions. Ordering and admission cap do not; cache admission
    may separately include host policy. No actual cycle checking, interface
    inference, Git/config/lock interpretation or initializer evaluation. *)
val create
  :  max_input_bytes:int option
  -> compiler_contract:Captured.Digest.t
  -> environment:Environment.t
  -> provenance:Provenance.t
  -> entry:Captured.Id.t
  -> sources:Captured.t list
  -> definitions:Definition.t list
  -> scopes:Scope.t list
  -> checkpoint:(unit -> unit)
  -> unit
  -> (t, Error.t) Result.t

(** Admit one ALREADY captured host source with its original Source.Id, digest
    and exact bytes. The implicit environment/provenance and one empty scope
    are the same as [single_file]; there is no project, catalog or resolver.
    [max_input_bytes] is positive and bounds source bytes PLUS canonical
    Prepared metadata through the existing [create] admission. The host still
    owns source/read and target-contract authority. Checkpoint exceptions,
    including cancellation, retain their identity. *)
val single_captured
  :  compiler_contract:Captured.Digest.t
  -> source:Captured.t
  -> max_input_bytes:int
  -> checkpoint:(unit -> unit)
  -> unit
  -> (t, Error.t) Result.t

(** Fixed in-memory entry identity, implicit environment/provenance, empty catalog
    and one empty scope. Its historical unconstrained source-string behavior
    remains; it shares the internal one-source construction algorithm with
    [single_captured] but no host source identity or bounded-capture claim.
    Host checks its existing source policy first; no metadata allowance
    deduction, compiler-target extraction or ambient project lookup. *)
val single_file
  :  compiler_contract:Captured.Digest.t
  -> source:string
  -> checkpoint:(unit -> unit)
  -> unit
  -> (t, Error.t) Result.t

val entry : t -> Captured.Id.t
val compiler_contract : t -> Captured.Digest.t
val environment : t -> Environment.t
val provenance : t -> Provenance.t
val sources : t -> Captured.t list
val definitions : t -> Definition.t list
val scopes : t -> Scope.t list
val find_source : t -> Captured.Id.t -> Captured.t option
val find_definition : t -> Module_id.t -> Definition.t option
val find_scope : t -> Captured.Id.t -> Scope.t option
val accounting : t -> Accounting.t
val fingerprint : t -> Captured.Digest.t
