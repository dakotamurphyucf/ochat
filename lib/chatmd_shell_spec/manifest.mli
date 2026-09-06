open! Core

(** Canonical, inspectable shell runtime manifest. *)

type extension_script =
  { id : string
  ; kind : Chatmd_script_spec.kind
  ; source_sha256 : string
  ; surface_version : string
  ; entrypoint : string
  ; limits : Chatmd_script_spec.limits
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type edge_kind =
  | Extends
  | Tool_runtime
  | Moderator_runtime
  | Chatml_extension
  | Executable_extension
  | Worker_runtime
  | External_backend
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type dependency =
  { from_id : string
  ; to_id : string
  ; kind : edge_kind
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type payload =
  { encoding_version : string
  ; platform : Shell_spec.platform
  ; runtimes : Shell_spec.t list
  ; tools : Shell_tool_spec.t list
  ; moderator_runtime : string option
  ; extension_scripts : extension_script list
  ; dependencies : dependency list
  ; required_features : Feature.t list
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type t =
  { payload : payload
  ; canonical_json : string
  ; sha256 : string
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

(** [create payload] sorts JSON object keys, serializes [payload], and hashes
    the exact versioned encoding. Lists retain their supplied order. *)
val create : payload -> t
