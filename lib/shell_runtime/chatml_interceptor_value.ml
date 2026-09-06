open! Core
module C = Chatml_codec
module L = Chatml.Chatml_lang

type t =
  | Continue
  | Rewrite of string list
  | Respond of
      { stdout : string
      ; stderr : string
      }
  | Reject of string
[@@deriving sexp, compare, equal]

let encode = function
  | Continue -> C.encode_record [ "version", L.VString "shell-interceptor-v1"; "action", VString "continue" ]
  | Rewrite argv -> C.encode_record [ "version", L.VString "shell-interceptor-v1"; "action", VString "rewrite"; "argv", C.encode_strings argv ]
  | Respond { stdout; stderr } -> C.encode_record [ "version", L.VString "shell-interceptor-v1"; "action", VString "respond"; "stdout", VString stdout; "stderr", VString stderr ]
  | Reject reason -> C.encode_record [ "version", L.VString "shell-interceptor-v1"; "action", VString "reject"; "reason", VString reason ]
;;

let decode value =
  let path = [ "interceptor" ] in
  let allowed = [ "version"; "action"; "argv"; "stdout"; "stderr"; "reason" ] in
  let open Result.Let_syntax in
  let%bind fields = C.record ~path ~allowed ~required:[ "version"; "action" ] value in
  let%bind version = C.field ~path fields "version" >>= C.string ~path:(path @ [ "version" ]) in
  let%bind action = C.field ~path fields "action" >>= C.string ~path:(path @ [ "action" ]) in
  if not (String.equal version "shell-interceptor-v1")
  then Error (Chatmd_shell_spec.Diagnostic.error ~path:(path @ [ "version" ]) ~code:"shell.chatml_codec" "unsupported version")
  else
    match action with
    | "continue" -> Ok Continue
    | "rewrite" -> C.field ~path fields "argv" >>= C.strings ~path:(path @ [ "argv" ]) |> Result.map ~f:(fun argv -> Rewrite argv)
    | "respond" ->
      let%bind stdout = C.field ~path fields "stdout" >>= C.string ~path:(path @ [ "stdout" ]) in
      let%map stderr = C.field ~path fields "stderr" >>= C.string ~path:(path @ [ "stderr" ]) in
      Respond { stdout; stderr }
    | "reject" -> C.field ~path fields "reason" >>= C.string ~path:(path @ [ "reason" ]) |> Result.map ~f:(fun reason -> Reject reason)
    | _ -> Error (Chatmd_shell_spec.Diagnostic.error ~path:(path @ [ "action" ]) ~code:"shell.chatml_codec" "unknown interceptor action")
;;
