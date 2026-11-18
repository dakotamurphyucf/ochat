open! Core

(** Immutable instruction bytes and provenance. No value in this library grants
    source access, tools, activation or provider-context insertion. *)
module Root_id : sig
  type t [@@deriving compare, equal, sexp_of]

  (** Administrative label: 1–128 ASCII letters/digits/underscore/hyphen,
      starting with a letter or digit. No path interpretation or authority. *)
  val of_string : string -> t Or_error.t

  val to_string : t -> string
end

module Skill_slug : sig
  (** 1–64 ASCII lowercase letters/digits and single internal hyphens.
      No leading/trailing/consecutive hyphen. Shared by metadata and discovery. *)
  type t [@@deriving equal, sexp_of]

  val of_string : string -> t Or_error.t
  val to_string : t -> string
end

module Kind : sig
  type t =
    | Project_guidance
    | Skill
  [@@deriving equal, sexp_of]
end

module Scope : sig
  type t

  val root : t
  val of_path : Source.Portable_path.t -> t
  val path : t -> Source.Portable_path.t option
end

module Id : sig
  type t [@@deriving compare, equal, sexp_of]

  val root : t -> Root_id.t
  val member : t -> Source.Portable_path.t
end

module Limits : sig
  type t

  (** Positive finite bounds, no wider than 256 files, 1 MiB/file, 8 MiB total
      and one million cooperative steps. Metadata additionally has fixed bounds:
      8 KiB/128 lines/32 fields, 4 KiB/string. Counts precede projections. *)
  val create
    :  max_files:int
    -> max_file_bytes:int
    -> max_total_bytes:int
    -> max_work_steps:int
    -> t Or_error.t

  val standard : t
  val files : t -> int
  val file_bytes : t -> int
  val total_bytes : t -> int
  val work_steps : t -> int
end

module Warning : sig
  type t =
    | Headerless_skill
    | Unknown_field of string
  [@@deriving equal, sexp_of]
end

module Metadata : sig
  type t

  val name : t -> string option
  val description : t -> string option
  val annotations : t -> (string * string) list
  val warnings : t -> Warning.t list
end

module Metadata_error : sig
  type reason =
    | Unsupported_syntax
    | Duplicate_field
    | Missing_field
    | Invalid_value
    | Name_mismatch
    | Limit_exceeded
  [@@deriving equal, sexp_of]

  type t = private
    { line : int
    ; field : string option
    ; reason : reason
    }
  [@@deriving sexp_of]
end

module Error : sig
  type t =
    | Invalid_identity
    | Invalid_encoding
    | Invalid_metadata of Metadata_error.t
    | Stale of
        { expected : Chatml_source.Digest.t
        ; observed : Chatml_source.Digest.t
        }
    | Limit_exceeded
    | Document of Document_schema.Error.t
    | Storage of Agent_store.Store_error.t
    | Invalid_capture of string
  [@@deriving sexp_of]
end

module Observed : sig
  (** Exact qualified member and observed content revision; no body or grant. *)
  type t

  val id : t -> Id.t
  val kind : t -> Kind.t
  val digest : t -> Chatml_source.Digest.t
  val byte_length : t -> int
  val metadata : t -> Metadata.t
end

module Loaded : sig
  type t

  val observed : t -> Observed.t
  val contents : t -> string

  (** Byte offset after the exact closing header line, or zero without a header.
      Original frontmatter, line endings and complete original file are retained. *)
  val body_start_byte : t -> int
end

(** Pure consistency checks, not proof of a historical authorized filesystem
    read. Hosts use the actual authenticated source receipt. Invalid UTF-8, NUL
    and BOM reject; project guidance is always plain text. Skill headers use the
    documented bounded string profile, not full YAML. Checkpoint exceptions,
    including cancellation, propagate unchanged. *)
val observe
  :  Chatml_source.t
  -> root:Root_id.t
  -> kind:Kind.t
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (Observed.t, Error.t) Result.t

val load
  :  Observed.t
  -> source:Chatml_source.t
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (Loaded.t, Error.t) Result.t

module Snapshot : sig
  (** Ordered immutable loaded content over the existing universal document and
      content-addressed payload container. No filesystem/store owner or activation.
      Compatible unknown document fields survive exact to_storage round trips. *)
  type t

  val capture
    :  Loaded.t list
    -> limits:Limits.t
    -> checkpoint:(unit -> unit)
    -> (t, Error.t) Result.t

  val loaded : t -> Loaded.t list
  val to_storage : t -> Agent_store.Prompt_program_bundle.t

  (** Revalidate exact payload membership, hashes, IDs and derived metadata.
      Missing/corrupt content fails; never consult current files. *)
  val of_storage
    :  Agent_store.Prompt_program_bundle.t
    -> limits:Limits.t
    -> checkpoint:(unit -> unit)
    -> (t, Error.t) Result.t
end
