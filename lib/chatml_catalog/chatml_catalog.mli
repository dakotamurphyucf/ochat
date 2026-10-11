open Core

(** Pure shared configured catalog planning/captured facts for two acquisition
    owners: bounded local descriptors and Git commit/tree/blob objects.
    No Eio/native/process/cache/auth types, generic acquisition callbacks, parser,
    dependency resolution, compiler or runtime. Host composition owns complete Prepared_program admission. *)
module Error : sig
  type problem =
    | Declaration of Chatml_project.Error.t
    | Invalid_candidate_name of { basename_prefix : string }
    | Invalid_candidate_path of { path_prefix : string }
    | Undeclared_directory
    | Foreign_observation
    | Orphan_interface of Source.Portable_path.t
    | Ambiguous_module of
        { name : string
        ; first : Source.Portable_path.t
        ; second : Source.Portable_path.t
        }
    | Portable_collision of Source.Portable_path.t * Source.Portable_path.t
    | Conflicting_interfaces of
        { implementation : Source.Portable_path.t
        ; first : Source.Portable_path.t option
        ; second : Source.Portable_path.t option
        }
    | Duplicate_capture of Chatml_source.Id.t
    | Conflicting_capture of Chatml_source.Id.t
    | Wrong_namespace of Chatml_source.Id.t
    | Missing_capture of Source.Portable_path.t
    | Unexpected_capture of Chatml_source.Id.t
    | Invalid_association of Source.Portable_path.t
    | Accounting_overflow
  [@@deriving sexp_of]

  type t

  val problem : t -> problem
  val declared_at : t -> Chatml_project.Declared_at.t
  val related : t -> Chatml_project.Declared_at.t option
end

module Plan : sig
  module Observation : sig
    (** Filtered pure candidate fact; not a claim of a regular file, existing
        source, optional-interface availability or admitted source bytes.
        Affine to the producing Discovery invocation via one private identity
        token, never mutated/global; not semantic path/source equality. *)
    type t
  end

  module Member : sig
    type requirement =
      | Required
      | Optional
    [@@deriving equal, sexp_of]

    type t

    val path : t -> Source.Portable_path.t
    val requirement : t -> requirement
    val declared_at : t -> Chatml_project.Declared_at.t
  end

  module Discovery : sig
    (** Immutable own declaration/selection and lookup sets. Construct once before
        enumeration; no retained checkpoint. Explicit overrides/exclusions precede
        conventional admission. All raw entries still count in the backend even
        when observe returns None. No discovery into nested directories. Reject
      undeclared directory inputs and use stored own declaration context, never
      an input Source_dir's foreign context. observe checks basename byte length
      <=1024 before scanning; it rejects slash/NUL/dot pseudoentries and applies
      portable component/path validation to suffix-relevant selections. No UTF-8
      repair or hidden normalization. Error prefixes are <=256 bytes. *)
    type t

    val create
      :  Chatml_project.t
      -> request:Chatml_project.Selection.request
      -> checkpoint:(unit -> unit)
      -> (t, Error.t) Result.t

    val observe
      :  t
      -> directory:Chatml_project.Source_dir.t
      -> name:string
      -> (Observation.t option, Error.t) Result.t
  end

  (** Canonical prospective member/association plan. Explicit names override all
      conventional candidates before orphan/ambiguity checks. Companion matching
      is exact basename; exclusions suppress discovered/implicit selections only.
      Explicit members still apply. Reject observations from any different
    Discovery invocation, preserving the supplied Discovery's own context.
    Optional means absence may be admitted by the
      backend's exact leaf/object Missing contract, not by other read failures. *)
  type t

  val create
    :  Discovery.t
    -> observations:Observation.t list
    -> checkpoint:(unit -> unit)
    -> (t, Error.t) Result.t

  val project : t -> Chatml_project.t
  val selection : t -> Chatml_project.Selection.t
  val members : t -> Member.t list
end

module Binding : sig
  type t

  val name : t -> string
  val module_id : t -> Chatml_prepared_program.Module_id.t
  val declared_at : t -> Chatml_project.Declared_at.t
end

module Accounting : sig
  (** Only validated immutable source facts. Enumeration/work/transport budget
      receipts stay with each acquisition owner, outside this pure artifact. *)
  type t

  val source_count : t -> int
  val source_bytes : t -> int
end

(** Immutable captured own-package catalog, never full Prepared input. No live
    paths, authority, mutable caches, callbacks or acquisition handles retained. *)
type t

(** Validate immutable capture facts against the plan.
    Include exact project declaration capture once plus each required planned
    member and whichever optional members were acquired. Captures must own the
    effective namespace from Project.source's Source.Id. Reject duplicate IDs
    (including equal duplicates), conflicting revisions, unexpected/missing IDs,
    changed declaration revision, portable collisions and same-implementation
    conflicting interface associations. Optional absence is represented only by
    omission; backend guarantees the actual Missing observation. No digest value
    or plan proves filesystem/Git provenance. Distinct paths remain distinct IDs,
    even equal contents/inodes. Aliases share one definition/Module_id. Entry
    overlapping catalog stays one source/node; no hidden discovery exclusion.

    Build canonical definitions/bindings from the validated plan/captures, with
    declaration-owned locations. Compute source count/bytes with checked sums;
    no host policy/caps are inferred. Check required source presence, namespace and distinct implementation/interface
    identities, including alias association agreement. Host composition still
    resolves imported targets/scopes, compiler compatibility, selected lock metadata
    and complete Prepared_program admission. Compiler source-role eligibility is
    not checked here. Poll walks/sorts and long identity bytes;
    ordinary failures Result, checkpoint exceptions unchanged. No effects. *)
val create
  :  Plan.t
  -> sources:Chatml_source.t list
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

val project : t -> Chatml_project.t
val selection : t -> Chatml_project.Selection.t
val sources : t -> Chatml_source.t list
val entry : t -> Chatml_source.Id.t option
val definitions : t -> Chatml_prepared_program.Definition.t list
val bindings : t -> Binding.t list
val accounting : t -> Accounting.t
val find_source : t -> Chatml_source.Id.t -> Chatml_source.t option

(** Resolve a captured planned member's whole environment. Poll the member
    search and long namespace/path comparisons when [checkpoint] is supplied;
    callback exceptions propagate unchanged and the callback is not retained.
    Foreign, absent or unplanned sources return [None]. *)
val scope_for
  :  ?checkpoint:(unit -> unit)
  -> t
  -> source:Chatml_source.Id.t
  -> Chatml_project.Scope_spec.t option
