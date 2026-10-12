open Core
module Error = Chatml_catalog_error

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

(** Private validated association facts consumed by the captured factory. *)
module Implementation : sig
  type interface =
    | Absent
    | Optional of Source.Portable_path.t
    | Required of Source.Portable_path.t

  type t

  val name : t -> string
  val source : t -> Source.Portable_path.t
  val interface : t -> interface
  val declared_at : t -> Chatml_project.Declared_at.t
end

val implementations : t -> Implementation.t list
val origin : t -> Chatml_project.Declared_at.t

(** Cooperates for admitted opaque identity/config-name bytes; used by the pure
    captured factory as well as this planner. No retained callback. *)
val compare_string : string -> string -> checkpoint:(unit -> unit) -> int
