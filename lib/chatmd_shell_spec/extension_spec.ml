open Core
open Jsonaf.Export

(** Additive declaration types keep existing ChatMD binary record layouts intact. *)
type schema =
  { path : string
  ; source_text : string
  ; source_sha256 : string
  ; source_ref : Source_ref.t
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type implementation =
  | Moderator of string
  | Standalone of
      { script : string
      ; entrypoint : string
      }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type tool =
  { version : int
  ; name : string
  ; description : string option
  ; implementation : implementation
  ; input_schema : schema
  ; output_schema : schema
  ; completion_schema : schema option
  ; uses : string list
  ; source_ref : Source_ref.t
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type script_kind =
  | Moderator_script
  | Tool_script
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type script =
  { version : int
  ; id : string
  ; kind : script_kind
  ; source : Chatmd_script_spec.source
  ; source_sha256 : string
  ; source_ref : Source_ref.t
  ; limits : Chatmd_script_spec.limits
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type policy =
  | Auto
  | Manual
  | Preload of string list
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type authoring_context =
  { version : int
  ; policy : policy
  ; source_ref : Source_ref.t
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

let script_text script =
  match script.source with
  | Inline text | Src { source_text = text; _ } -> text
;;

let validate_schema schema =
  if not (String.equal (Source_ref.digest schema.source_text) schema.source_sha256)
  then
    Error
      [ Diagnostic.error
          ~source:schema.source_ref
          ~code:"chatmd.schema_digest_mismatch"
          "schema bytes do not match retained digest"
      ]
  else
    Tool_schema.of_string schema.source_text
    |> Result.map_error
         ~f:
           (List.map ~f:(fun error ->
              Diagnostic.error
                ~source:schema.source_ref
                ~path:error.Tool_schema.path
                ~code:error.code
                error.message))
;;
