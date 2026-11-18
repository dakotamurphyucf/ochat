open! Core

(** Immutable source-origin selectors shared by captured recurrence and existing
    authored delegation. No source loan, native capability or execution authority. *)
module Authored_tool : sig
  (** Representation remains constructible for compatibility with the existing
      Delegation_record.Admission.authored_tool record. All construction/decoding/
      persistence boundaries validate it; matching fields alone grants nothing. *)
  type t =
    { name : string
    ; source_sha256 : string
    }
  [@@deriving equal, sexp]

  val create : name:string -> source_sha256:string -> (t, Document_schema.Error.t) result

  (** Existing semantics: nonempty name <=1024 bytes, no ASCII control/DEL;
      exactly 64 lowercase hex SHA256. No normalization or case folding. *)
  val validate : t -> (unit, Document_schema.Error.t) result

  (** Complete selector JSON is bounded to 256 KiB/depth32 before decoding.
      The containing carrier preserves compatible unknown selector fields. *)
  val of_json : Jsonaf.t -> (t, Document_schema.Error.t) result

  val to_json : t -> (Jsonaf.t, Document_schema.Error.t) result
  val shape : Document_schema.Shape.t
end

(** Absence of the new authored-source field means the existing Artifact case.
    Decoders reject explicit null and require the source-specific semantic marker
    for Authored_tool. Outer carriers own compatible selector-field extensions. *)
type t =
  | Artifact
  | Authored_tool of Authored_tool.t
[@@deriving equal, sexp_of]
