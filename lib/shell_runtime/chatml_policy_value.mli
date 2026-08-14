open! Core

(** Versioned policy decision values exposed to shell ChatML hooks. *)

type match_ =
  { rule_id : string
  ; action : Shell_access.Policy.action
  ; reason : string option
  }

type t =
  { action : Shell_access.Policy.action
  ; reason : string
  ; matches : match_ list
  }

val of_decision : Shell_access.Policy.decision -> t
val encode : t -> Chatml.Chatml_lang.value
val decode : Chatml.Chatml_lang.value -> (t, Chatmd_shell_spec.Diagnostic.t) result
