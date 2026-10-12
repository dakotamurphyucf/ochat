open Core

(** Pure composition and
    capture-only restoration; no filesystem, Git transport, discovery callback,
    execution, grant or compiler-inference state. All completed results retain
    exact immutable captured sources, not acquisition identities or callbacks. *)

module Limits : sig
  type t

  val create
    :  max_work_steps:int
    -> max_input_bytes:int option
    -> document:Document_schema.Limits.t
    -> t Or_error.t

  val max_work_steps : t -> int
  val max_input_bytes : t -> int option
  val document : t -> Document_schema.Limits.t
end

module Error : sig
  type problem =
    | Missing_catalog of Chatml_source.Id.t
    | Unexpected_catalog of Chatml_source.Id.t
    | Namespace_conflict of string
    | Invalid_repository_member of Chatml_source.Id.t
    | Invalid_acquisition_key
    | Invalid_dependency of string
    | Missing_module of Chatml_project.Module_ref.t
    | Compiler_requirement
    | Invalid_entry_selection
    | Invalid_lock_identity
    | Lock_mismatch
    | Missing_capture of Chatml_source.Id.t
    | Duplicate_capture of Chatml_source.Id.t
    | Needs_update
    | Limit_exceeded
    | Accounting_overflow
    | Declaration of Chatml_project.Error.t
    | Catalog of Chatml_catalog.Error.t
    | Prepared of Chatml_prepared_program.Error.t

  type t

  val problem : t -> problem
  val declared_at : t -> Chatml_project.Declared_at.t option
end

module Catalog_input : sig
  (** Immutable acquisition facts for this composition attempt, not admitted
      compiler input. The host supplies one canonical admitted acquisition key;
      equality additionally requires exact captured bytes, own selection,
      definitions, bindings, origin and owner-local dependency associations.
      The key proves no authority and does not survive completed composition. *)
  type t

  (** A nonempty acquisition key and unique owner-local dependency names are
      checked during composition, together with the complete selected closure.
      Construction alone does not imply graph admission or native provenance. *)
  val create
    :  Chatml_catalog.t
    -> origin:Chatml_project.Lock.Catalog.origin
    -> acquisition_key:string
    -> dependencies:Chatml_project.Lock.Dependency_selection.t list
    -> t

  val declaration : t -> Chatml_source.Id.t
  val declared_at : t -> Chatml_project.Declared_at.t
end

module Entry : sig
  type t =
    | Declared
    | Captured of Chatml_source.t

  (** Declared consumes the root catalog's selected declared entry. Captured
      requires a root Library selection and gets its own selected environment,
      without a filename override or invented file origin. Entry/catalog overlap
      is one source only when exact revision and source-owned scope agree. *)
end

module Operation : sig
  (** [Build_without_previous] admits only Local roots and selected Local
      dependencies. Use it for an authoritative new script slot or a known prior
      Provided input, never as recovery for missing/corrupt historical metadata.
      Every Git selection requires explicit Update when no prior lock exists.
      Source authorization remains the host's responsibility. *)
  type t =
    | Initial_prepare
    | Build_without_previous
    | Build of { previous_lock : Chatml_source.t }
    | Update of { previous_lock : Chatml_source.t option }
end

module Accounting : sig
  type t

  (** Observed cooperative work for one pure gate, projection, composition or
      restoration call. Complete admitted
      input counts and bytes are Prepared_program.Accounting, including sources,
      metadata and the exact selected lock capture. Not an acquisition receipt. *)
  val work_steps : t -> int
end

module Selection_gate : sig
  (** Immutable decoded previous-selection policy. It retains no checkpoint,
      mutable budget, limits, acquisition key or live authority. Construct once before
      acquisition and pass this same policy to composition. *)
  type t

  module Owner : sig
    (** Prior catalog association affine to this exact immutable gate instance.
        Constructed only by root validation and named prior owner-local edge
        traversal. Namespace is configuration, never its lookup identity. *)
    type t

    val declaration : t -> Chatml_source.Id.t
  end

  (** Return decode work with the immutable policy. Failure has no partial
      receipt: abort the host attempt rather than resetting its allowance. *)
  val create
    :  Operation.t
    -> limits:Limits.t
    -> checkpoint:(unit -> unit)
    -> (t * Accounting.t, Error.t) Result.t

  val previous_lock : t -> Chatml_project.Lock.t option
  val preserves_git_selections : t -> bool

  (** Check requested root code intent before transport. Build preserves the exact
      previous Git repository/format/full pin/package root/project member. Local
      refresh remains allowed; changing source kind requires update. No namespace
      is invented before declaration capture. Build_without_previous admits only
      Local and returns no prior owner. Initial/Update admit authored intent.
      Every call uses explicit remaining limits and returns observed work; on Error
      abort the host attempt, since no partial failure receipt/retry budget exists. *)
  val check_root
    :  t
    -> origin:Chatml_project.Lock.Catalog.origin
    -> project:Source.Portable_path.t
    -> limits:Limits.t
    -> checkpoint:(unit -> unit)
    -> (Owner.t option * Accounting.t, Error.t) Result.t

  (** Check the owner's authored selected dependency before the host acquires
      code. Relative Local references under a Git owner resolve within its exact
      pinned repository and package root. Build requires a previous association
      and the same selected Git code; Initial/Update admit explicit authored full
      pins. Native Local admission remains host-owned. Full closure repeats this
      check and reconciles actual supplied captures; it is not an acquisition
      callback. Each call consumes supplied remaining limits and returns observed work. Abort
      the whole host attempt on Error; no failed partial receipt authorizes retry.
      Checkpoint exceptions propagate unchanged.

      Environment/namespace selections may change on Build when code intent stays
      fixed; complete composition validates their actual declarations and scopes.
      Pass the prior owner returned by root/incoming edge; None denotes no prior
      selected association. Foreign gate owners reject. Return the prior target
      association for the next package; Initial/Update return None. Hosts check
      every incoming prior route when current aliases converge, not an arbitrary
      first route. No global namespace/basename search identifies an owner. *)
  val check_dependency
    :  t
    -> previous_owner:Owner.t option
    -> owner:Chatml_project.t
    -> owner_origin:Chatml_project.Lock.Catalog.origin
    -> Chatml_project.Dependency.t
    -> limits:Limits.t
    -> checkpoint:(unit -> unit)
    -> (Owner.t option * Accounting.t, Error.t) Result.t
end

(** Selected available declaration edges only: the dependency aliases referenced
    by the selected own environment and actual captured member/interface scopes.
    Omitted bindings select own catalog, not every declared dependency. Sorted by
    authored dependency name. No executable edges or acquisition. On failure abort
    the host attempt; do not retry with a fresh allowance. *)
val selected_dependencies
  :  Chatml_catalog.t
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (Chatml_project.Dependency.t list * Accounting.t, Error.t) Result.t

module Dependency : sig
  (** Pure owner-relative resolved intent. No native identity or granted authority. *)
  type t

  val resolve
    :  Chatml_project.Dependency.t
    -> owner_origin:Chatml_project.Lock.Catalog.origin
    -> owner_declaration:Chatml_source.Id.t
    -> limits:Limits.t
    -> checkpoint:(unit -> unit)
    -> (t * Accounting.t, Error.t) Result.t

  val origin : t -> Chatml_project.Lock.Catalog.origin
  val project : t -> Source.Portable_path.t

  (** Translate a package-relative member to raw repository lookup. Logical
      source IDs remain package-relative; None is repository root. Failures lack
      a source span when no actual captured owner is supplied. Same explicit
      remaining-limit/receipt/abort-on-error contract as selected_dependencies. *)
  val repository_member
    :  package_root:Source.Portable_path.t option
    -> member:Source.Portable_path.t
    -> limits:Limits.t
    -> checkpoint:(unit -> unit)
    -> (Source.Portable_path.t * Accounting.t, Error.t) Result.t
end

(** Completed immutable compiler inventory and selected receipt. No callback,
    inference cells, native values, ephemeral acquisition keys or executable
    initialization state. Compilation and runtime remain separate operations. *)
type t

val prepared : t -> Chatml_prepared_program.t
val lock : t -> Chatml_project.Lock.t
val lock_source : t -> Chatml_source.t
val accounting : t -> Accounting.t

(** Canonicalize equal acquisition aliases, validate the exact selected available
    declaration closure, materialize each catalog's own scopes, authentic compiler
    requirement agreement and tool-name union, reconcile entry overlap, exclude
    the selected lock ID from serialized source inventory, then admit the complete
    Prepared_program. Reject conflicting namespaces and unused supplied catalogs.
    Declaration cycles may be coherent; actual executable edges are compiler-owned.
    Successful lock bytes are canonical, with no self-hash or final fingerprint.

    All phases share one bounded budget, including authentic contract hashing,
    receipt rendering and final admission. Result failures expose no partial
    output; checkpoint exceptions/backtraces propagate unchanged. No initializer,
    transport, filesystem or grant operation occurs. *)
val compose
  :  Catalog_input.t
  -> catalogs:Catalog_input.t list
  -> entry:Entry.t
  -> selection_gate:Selection_gate.t
  -> contract:Chatml.Chatml_compiler_contract.t
  -> lock_id:Chatml_source.Id.t
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

(** Reconstruct only from the exact supplied lock capture and immutable source
    captures. [sources] contains every receipt catalog source and the selected
    entry exactly once, but excludes the positional lock capture. Reject duplicates
    even when equal, surplus captures, absent sources and changed digest/length.
    Decode actual declarations; reuse the existing catalog planner on actual
    captured own-package members and exact recorded own selections. Independently
    derive definitions/scopes/dependency closure and compare the complete selected
    receipt before final Prepared admission with the authentic contract.

    Retain this exact lock source, including accepted noncanonical JSON formatting;
    canonical receipt equality does not rewrite owned bytes. No fake capture or
    ephemeral acquisition key is synthesized. Missing sources never acquire an
    invented source location; real lock/declaration ownership may locate the
    expectation. Coherence of a supplied inventory does not prove historical
    enumeration, uncaptured absence, native path identity, Git provenance, live
    tool admission or restored grants. No filesystem discovery or initialization.

    The same bounded/checkpoint/atomic-result guarantees as [compose] apply. *)
val restore
  :  Chatml_source.t
  -> sources:Chatml_source.t list
  -> contract:Chatml.Chatml_compiler_contract.t
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

(** Recompose an admitted captured-entry library for an authentic compiler
    contract, retaining its exact entry, sources, catalogs, scopes, environments
    and dependency selections. Declared entry builds are rejected. Authored target
    and compiler requirements are checked normally against the new contract.

    [lock_id] must differ from the receiver's lock and every retained source ID.
    The result owns a new canonical lock and fingerprint; the receiver remains
    unchanged. No source acquisition, discovery, preprocessing, compiler inference,
    initializer or tool execution occurs. All reconstruction and admission use
    one invocation-local work/input budget. Checkpoint exceptions propagate
    unchanged; runtime authority is still admitted by the execution host. *)
val recompose
  :  t
  -> contract:Chatml.Chatml_compiler_contract.t
  -> lock_id:Chatml_source.Id.t
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t
