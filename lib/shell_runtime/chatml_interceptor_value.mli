open! Core

(** Versioned before-interceptor responses. *)

type t =
  | Continue
  | Rewrite of string list
  | Respond of
      { stdout : string
      ; stderr : string
      }
  | Reject of string
[@@deriving sexp, compare, equal]

val encode : t -> Chatml.Chatml_lang.value
val decode : Chatml.Chatml_lang.value -> (t, Chatmd_shell_spec.Diagnostic.t) result
