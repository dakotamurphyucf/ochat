(** Current manifest fields. Values require admission by the document owner. *)
module Source : sig
  type t =
    { relative_path : string
    ; sha256 : string
    }
  [@@deriving equal, sexp_of]
end

type t =
  { version : int
  ; revision_id : Agent_protocol.Id.Prompt_revision.t
  ; prompt_definition_id : Agent_protocol.Id.Prompt_definition.t option
  ; source_receipt : Prompt_source_receipt.t option
  ; canonical_source : string option
  ; root_relative_path : string
  ; root_sha256 : string
  ; sources : Source.t list
  ; parser_schema_version : int
  ; runtime_schema_version : int
  ; shell_manifest_sha256 : string option
  ; program_bundle : Prompt_program_bundle.Manifest.t option
  ; content_bundle : Prompt_program_bundle.Manifest.t option
  ; created_at : Agent_protocol.Timestamp.t
  }
[@@deriving equal, sexp_of]

val valid_relative_path : string -> bool
val validate : t -> (unit, Document_schema.Error.t) result
