open! Core
open Jsonaf.Export

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

let rec canonical_jsonaf = function
  | `Object fields ->
    `Object
      (List.map fields ~f:(fun (name, value) -> name, canonical_jsonaf value)
       |> List.sort ~compare:(fun (left, _) (right, _) -> String.compare left right))
  | `Array values -> `Array (List.map values ~f:canonical_jsonaf)
  | (`Null | `True | `False | `Number _ | `String _) as value -> value
;;

let sha256 value = Digestif.SHA256.(to_hex (digest_string value))

let logical_source (source : Source_ref.t) =
  { source with
    source_dir = Filename.dirname source.file
  ; prompt_dir = "."
  }
;;

let canonical_payload payload =
  { payload with
    runtimes =
      List.map payload.runtimes ~f:(fun runtime ->
        { runtime with Shell_spec.source = logical_source runtime.source })
  ; tools =
      List.map payload.tools ~f:(fun tool ->
        { tool with Shell_tool_spec.source = logical_source tool.source })
  }
;;

let create payload =
  let canonical_json =
    canonical_payload payload |> jsonaf_of_payload |> canonical_jsonaf |> Jsonaf.to_string
  in
  { payload; canonical_json; sha256 = sha256 canonical_json }
;;
