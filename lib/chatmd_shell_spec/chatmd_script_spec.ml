open! Core
open Jsonaf.Export

type kind =
  | Moderator
  | Shell_matcher
  | Shell_reviewer
  | Shell_before_interceptor
  | Shell_after_interceptor
  | Shell_effect_analyzer
  | Shell_audit_filter
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type source =
  | Inline of string
  | Src of
      { path : string
      ; source_text : string
      }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type limits =
  { wall_time : Duration.t
  ; fuel : int
  ; max_tasks : int
  ; max_value_bytes : Duration.bytes
  ; max_output_bytes : Duration.bytes
  ; max_array_items : int
  ; max_depth : int
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type t =
  { id : string
  ; language : string
  ; kind : kind
  ; source : source
  ; source_ref : Source_ref.t
  ; source_sha256 : string
  ; limits : limits
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

let default_limits =
  { wall_time = Duration.parse "2s" |> Result.ok_or_failwith
  ; fuel = 100_000
  ; max_tasks = 256
  ; max_value_bytes = Duration.parse_bytes "256KiB" |> Result.ok_or_failwith
  ; max_output_bytes = Duration.parse_bytes "16KiB" |> Result.ok_or_failwith
  ; max_array_items = 256
  ; max_depth = 32
  }
;;

let kinds =
  [ "moderator", Moderator
  ; "shell_matcher", Shell_matcher
  ; "shell_reviewer", Shell_reviewer
  ; "shell_before_interceptor", Shell_before_interceptor
  ; "shell_after_interceptor", Shell_after_interceptor
  ; "shell_effect_analyzer", Shell_effect_analyzer
  ; "shell_audit_filter", Shell_audit_filter
  ]
;;

let kind_to_string kind =
  List.find_map_exn kinds ~f:(fun (name, candidate) ->
    Option.some_if (equal_kind kind candidate) name)
;;

let kind_of_string source value =
  match List.Assoc.find kinds value ~equal:String.equal with
  | Some kind -> Ok kind
  | None ->
    Error
      (Diagnostic.error
         ~source
         ~path:[ "script"; "kind" ]
         ~code:"chatmd.script_invalid_kind"
         ("unsupported ChatML script kind: " ^ value))
;;

let source_text t =
  match t.source with
  | Inline source_text | Src { source_text; _ } -> source_text
;;

let qualify t = { t with id = Source_ref.qualify t.source_ref t.id }
