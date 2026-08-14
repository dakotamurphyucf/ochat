open! Core
module C = Chatml_codec
module L = Chatml.Chatml_lang

type response =
  | Approve
  | Approve_for of string
  | Deny of string
  | Rewrite of string list
  | Defer
[@@deriving sexp, compare, equal]

let encode_request (request : Shell_access.Approval.request) =
  Chatml_context_value.of_context request.Shell_access.Approval.context
  |> fun context -> Chatml_context_value.with_policy context request.policy
  |> fun context -> Chatml_context_value.with_phase context "shell_reviewer"
  |> Chatml_context_value.encode
;;

let fields action argument =
  C.encode_record
    [ "version", L.VString "shell-approval-v1"
    ; "action", VString action
    ; "argument", C.encode_option (fun value -> L.VString value) argument
    ]
;;

let encode_response = function
  | Approve -> fields "approve" None
  | Approve_for scope -> fields "approve_for" (Some scope)
  | Deny reason -> fields "deny" (Some reason)
  | Rewrite argv -> C.encode_record [ "version", L.VString "shell-approval-v1"; "action", VString "rewrite"; "argument", C.encode_option Fn.id (Some (C.encode_strings argv)) ]
  | Defer -> fields "defer" None
;;

let decode_argument path fields =
  C.field ~path fields "argument" |> Result.bind ~f:(C.option ~path:(path @ [ "argument" ]) C.string)
;;

let require_argument path = function
  | Some value -> Ok value
  | None ->
    Error
      (Chatmd_shell_spec.Diagnostic.error
         ~path:(path @ [ "argument" ])
         ~code:"shell.chatml_codec"
         "action requires an argument")
;;

let decode_response value =
  let path = [ "approval" ] in
  let names = [ "version"; "action"; "argument" ] in
  let open Result.Let_syntax in
  let%bind fields = C.record ~path ~allowed:names ~required:names value in
  let%bind version = C.field ~path fields "version" >>= C.string ~path:(path @ [ "version" ]) in
  let%bind action = C.field ~path fields "action" >>= C.string ~path:(path @ [ "action" ]) in
  if not (String.equal version "shell-approval-v1")
  then Error (Chatmd_shell_spec.Diagnostic.error ~path:(path @ [ "version" ]) ~code:"shell.chatml_codec" "unsupported version")
  else
    match action with
    | "approve" -> Ok Approve
    | "defer" -> Ok Defer
    | "approve_for" ->
      decode_argument path fields
      >>= require_argument path
      |> Result.map ~f:(fun value -> Approve_for value)
    | "deny" ->
      decode_argument path fields
      >>= require_argument path
      |> Result.map ~f:(fun value -> Deny value)
    | "rewrite" ->
      C.field ~path fields "argument"
      >>= C.option ~path:(path @ [ "argument" ]) C.strings
      >>= require_argument path
      |> Result.map ~f:(fun value -> Rewrite value)
    | _ -> Error (Chatmd_shell_spec.Diagnostic.error ~path:(path @ [ "action" ]) ~code:"shell.chatml_codec" "unknown approval action")
;;
