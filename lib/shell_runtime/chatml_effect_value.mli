open! Core

(** Versioned effect analyzer responses. *)

type t =
  | Add of string list
  | Replace of string list
[@@deriving sexp, compare, equal]

val encode : t -> Chatml.Chatml_lang.value
val decode : Chatml.Chatml_lang.value -> (t, Chatmd_shell_spec.Diagnostic.t) result
