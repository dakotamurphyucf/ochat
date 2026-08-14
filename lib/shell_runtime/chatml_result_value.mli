open! Core

(** Versioned finalized command results exposed to after interceptors. *)

type t =
  { phase : string
  ; argv : string list
  ; status_kind : string
  ; status_code : int
  ; stdout : string
  ; stderr : string
  ; stdout_truncated : bool
  ; stderr_truncated : bool
  ; intercepted_by : string option
  ; untrusted_output : bool
  }
[@@deriving sexp, compare, equal]

val of_result : Shell_access.Interceptor.command_result -> t
val encode : t -> Chatml.Chatml_lang.value
val decode : Chatml.Chatml_lang.value -> (t, Chatmd_shell_spec.Diagnostic.t) result
