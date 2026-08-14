open! Core

module Context_value = Chatml_context_value
module Policy_value = Chatml_policy_value
module Approval_value = Chatml_approval_value
module Interceptor_value = Chatml_interceptor_value
module Effect_value = Chatml_effect_value
module Result_value = Chatml_result_value
module Audit_value = Chatml_audit_value

(** Bounded, secret-free values passed between shell execution and ChatML
    extension scripts. *)

val context : Shell_access.Context.t -> Chatml.Chatml_lang.value

val event
  :  phase:string
  -> fields:(string * Chatml.Chatml_lang.value) list
  -> Chatml.Chatml_lang.value

val strings : string list -> Chatml.Chatml_lang.value
