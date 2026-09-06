open! Core
open Jsonaf.Export

type severity =
  | Error
  | Warning
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type t =
  { code : string
  ; severity : severity
  ; path : string list
  ; source : Source_ref.t option
  ; message : string
  ; hints : string list
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

let create ?(path = []) ?source ?(hints = []) ~severity ~code message =
  { code; severity; path; source; message; hints }
;;

let error ?path ?source ?hints ~code message =
  create ?path ?source ?hints ~severity:Error ~code message
;;

let warning ?path ?source ?hints ~code message =
  create ?path ?source ?hints ~severity:Warning ~code message
;;

let severity_name = function
  | Error -> "error"
  | Warning -> "warning"
;;

let to_string t =
  let path =
    match t.path with
    | [] -> ""
    | path -> " at " ^ String.concat ~sep:"." path
  in
  sprintf "%s[%s]%s: %s" (severity_name t.severity) t.code path t.message
;;
