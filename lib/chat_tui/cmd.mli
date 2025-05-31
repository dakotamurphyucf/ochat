(** Effectful commands executed by the Chat-TUI runtime.

    The pure controller / stream logic emits values of [Types.cmd].  This
    module is responsible for interpreting those commands and performing the
    actual side-effects (file IO, network requests, spawning background
    fibres, …).

    In refactoring step 7 we introduce a first, minimal command –
    [Persist_session] – which persists the current conversation transcript to
    disk.  Future steps will extend the open variant in {!Chat_tui.Types} with
    further constructors (e.g. [Send_request], [Abort_request], …).
*)

open Types

(** Execute a single command.

    The function is deliberately generic – every constructor is free to carry
    the data it needs, including closures.  This keeps the API minimal and
    avoids layering violations.
*)
val run : cmd -> unit

(** Convenience wrapper to execute a list of commands sequentially. *)
val run_all : cmd list -> unit
