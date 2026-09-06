open! Core

(** Versioned, redacted shell context values. Environment values are never
    represented by this type. *)

type executable =
  { requested : string
  ; path : string
  ; canonical_path : string
  ; trusted : bool
  ; sha256 : string
  }

type capabilities =
  { read_roots : string list
  ; write_roots : string list
  ; network : bool
  ; child_processes : bool
  ; arbitrary_code : bool
  ; privilege_change : bool
  ; sandbox : string
  }

type t =
  { phase : string
  ; request_id : string
  ; runtime_id : string
  ; manifest_sha256 : string
  ; argv : string list
  ; executable : executable
  ; cwd : string
  ; origin : string
  ; request_kind : string
  ; stdin_kind : string
  ; stdin_sha256 : string option
  ; stdin_bytes : int
  ; script_sha256 : string option
  ; script_preview : string option
  ; effects : string list
  ; capabilities : capabilities
  ; session_id : string option
  ; policy : Chatml_policy_value.t option
  }

val of_context : Shell_access.Context.t -> t
val with_phase : t -> string -> t
val with_policy : t -> Shell_access.Policy.decision -> t
val encode : t -> Chatml.Chatml_lang.value
val decode : Chatml.Chatml_lang.value -> (t, Chatmd_shell_spec.Diagnostic.t) result
