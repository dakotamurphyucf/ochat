open! Core

(** Immutable installed ordinary source trees, embedded from their canonical
    reviewed files. No checkout, filesystem lookup, preparation, execution, tool
    registration or permission grant occurs. This is not a project resolver. *)
module Error : sig
  type code =
    | Invalid_id
    | Invalid_limit
    | Limit_exceeded
    | Invalid_inventory
    | Unknown_resource
    | Unknown_member
  [@@deriving equal, sexp_of]

  type t

  val code : t -> code
  val message : t -> string
  val sexp_of_t : t -> Sexp.t
end

module Id : sig
  (** Exact ASCII [a-z][a-z0-9_-]*, between 1 and 64 bytes. *)
  type t [@@deriving equal, compare, sexp_of]

  val of_string : string -> (t, Error.t) Result.t
  val to_string : t -> string
end

module Limits : sig
  type t

  (** Positive byte/count/work ceilings. Counts and encoded metadata are charged
      before retaining output. Limits do not change the installed resource set. *)
  val create
    :  max_resources:int
    -> max_members:int
    -> max_member_bytes:int
    -> max_total_bytes:int
    -> max_work_steps:int
    -> (t, Error.t) Result.t

  val default : t
end

module Member : sig
  type t

  val path : t -> Source.Portable_path.t
  val contents : t -> string
  val digest : t -> Chatml_source.Digest.t
  val byte_length : t -> int
end

module Project_summary : sig
  (** Requirements decoded from the exact included declaration. No source graph
      is prepared and no implementation is selected or authorized. *)
  type t

  val path : t -> Source.Portable_path.t
  val namespace : t -> string
  val required_tools : t -> string list
  val environments : t -> (string * Chatml_project.Compiler_requirement.t) list
end

module Resource : sig
  type t

  val id : t -> Id.t
  val title : t -> string
  val entry : t -> Source.Portable_path.t
  val digest : t -> Chatml_source.Digest.t
  val members : t -> Member.t list
  val projects : t -> Project_summary.t list
  val byte_length : t -> int

  (** Exact member lookup, without normalization, IO or fallback. *)
  val member : t -> Source.Portable_path.t -> (Member.t, Error.t) Result.t

  (** Bounded structural metadata, excluding source bodies. Derived from the
      same admitted immutable members and actual project declarations. *)
  val manifest : t -> Jsonaf.t
end

type t

(** Admit the complete embedded inventory. Invalid paths, duplicate IDs, member
    collisions, declarations or exhausted limits fail the whole operation;
    resources are never silently omitted. Exact paths retain canonical order.
    Hashing and projection poll the caller checkpoint; exceptions/cancellation
    propagate unchanged. No callback is retained and no global mutable cache. *)
val installed : limits:Limits.t -> checkpoint:(unit -> unit) -> (t, Error.t) Result.t

(** A nonnegative exact enumeration bound. Zero succeeds only for no resources;
    an insufficient bound returns Limit_exceeded without partial output. *)
val list
  :  t
  -> max_items:int
  -> checkpoint:(unit -> unit)
  -> (Resource.t list, Error.t) Result.t

val find : t -> Id.t -> (Resource.t, Error.t) Result.t
