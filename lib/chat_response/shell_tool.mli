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

    [stream="finalized"] emits no process progress and retains the existing
    finalized result contract. [stream="sanitized"] uses the executor's live
    safe-prefix path. Registration fails with [shell.tool_stream_unsupported]
    for any after-interceptor or a secret/replacement configuration rejected by
    {!Shell_access.Sanitized_stream.support}; it never silently buffers instead.
    Progress is transient, bounded, valid UTF-8, and separately sanitized from
    the unchanged canonical result. Stdout and stderr progress use a single
    combined [Stdout] append stream, with a final cross-channel disclosure filter.
    Pending tails are discarded on failure; final flushing shares the deadline.

    Expected invocation failures return JSON tool output containing stable
    [error.code] and safe [error.message] fields. Cancellation and unexpected
    host exceptions retain their normal exception semantics. *)
val create
  :  Shell_runtime.Registry.t
  -> Chatmd_shell_spec.Shell_tool_spec.t
  -> (Ochat_function.t, error) result
