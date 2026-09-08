open Core

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

val script_text : script -> string

(** Recompile pinned schema bytes, checking their retained digest first. *)
val validate_schema : schema -> (Tool_schema.t, Diagnostic.t list) result
