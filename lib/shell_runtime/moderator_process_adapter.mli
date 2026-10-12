open! Core

(** Routes ChatML moderator process operations through an authorized shell
    runtime. *)

(** [handler ~registry ~runtime_id session ~command ~args] executes one
    structured argv request. [args] must be a ChatML array containing only
    strings. The moderator session value is accepted for handler
    compatibility and is not used as ambient authority. Resolve the actual
    admitted [Call_context] services/adapter before execution, preserving current
    caller session, approval store/provider and checks. An expired scope rejects
    without reverting to registration services. An unbound owner retains its
    original services. Result identity follows the actual executed config. *)
val handler
  :  registry:Registry.t
  -> runtime_id:string
  -> Chatml_moderator_runtime.session
  -> command:string
  -> args:Chatml.Chatml_lang.value
  -> (string, string) result
