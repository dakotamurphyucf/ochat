open! Core

(** Converts one instantiated ChatMD shell tool into an [Ochat_function]. *)

type error =
  { code : string
  ; message : string
  }
[@@deriving sexp, compare, equal]

(** [create registry tool] creates a shell function for any supported mode,
    bound to the exact authorized registry manifest. Script files are loaded
    and fingerprinted before the function is published.

    The published model description always includes a mode-aware input,
    result, and runtime-security contract. A non-empty ChatMD [description]
    is appended as additional tool guidance.

    Expected invocation failures return JSON tool output containing stable
    [error.code] and safe [error.message] fields. Cancellation and unexpected
    host exceptions retain their normal exception semantics. *)
val create
  :  Shell_runtime.Registry.t
  -> Chatmd_shell_spec.Shell_tool_spec.t
  -> (Ochat_function.t, error) result
