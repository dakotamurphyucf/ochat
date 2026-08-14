open! Core

(** Versioned reviewer requests and responses. *)

type response =
  | Approve
  | Approve_for of string
  | Deny of string
  | Rewrite of string list
  | Defer
[@@deriving sexp, compare, equal]

val encode_request : Shell_access.Approval.request -> Chatml.Chatml_lang.value
val encode_response : response -> Chatml.Chatml_lang.value
val decode_response
  :  Chatml.Chatml_lang.value
  -> (response, Chatmd_shell_spec.Diagnostic.t) result
