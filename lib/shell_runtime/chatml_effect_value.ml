open! Core
module C = Chatml_codec
module L = Chatml.Chatml_lang

type t =
  | Add of string list
  | Replace of string list
[@@deriving sexp, compare, equal]

let encode = function
  | Add effects -> C.encode_record [ "version", L.VString "shell-effect-v1"; "action", VString "add"; "effects", C.encode_strings effects ]
  | Replace effects -> C.encode_record [ "version", L.VString "shell-effect-v1"; "action", VString "replace"; "effects", C.encode_strings effects ]
;;

let decode value =
  let path = [ "effect" ] in
  let names = [ "version"; "action"; "effects" ] in
  let open Result.Let_syntax in
  let%bind fields = C.record ~path ~allowed:names ~required:names value in
  let%bind version = C.field ~path fields "version" >>= C.string ~path:(path @ [ "version" ]) in
  let%bind action = C.field ~path fields "action" >>= C.string ~path:(path @ [ "action" ]) in
  let%bind effects = C.field ~path fields "effects" >>= C.strings ~path:(path @ [ "effects" ]) in
  if not (String.equal version "shell-effect-v1")
  then Error (Chatmd_shell_spec.Diagnostic.error ~path:(path @ [ "version" ]) ~code:"shell.chatml_codec" "unsupported version")
  else
    match action with
    | "add" -> Ok (Add effects)
    | "replace" -> Ok (Replace effects)
    | _ -> Error (Chatmd_shell_spec.Diagnostic.error ~path:(path @ [ "action" ]) ~code:"shell.chatml_codec" "unknown effect action")
;;
