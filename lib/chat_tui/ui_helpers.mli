(** Terminal-side helpers that are **outside** the Notty event loop.  They
    are intended to be called **after** the TUI has shut down, e.g. from
    {!Chat_tui.App.run_chat}'s teardown logic.  Currently the module
    exposes a single helper – {!prompt_archive}. *)

(** [prompt_archive ?timeout_s ?default ()] displays the question

    {v
      Archive conversation to ChatMarkdown file? [y/N] 
    v}

    on [stdout] and waits for a reply on [stdin].  The reply is parsed
    case-insensitively:

    • ["y"] or ["yes"] → return [true]  
    • ["n"] or ["no"]  → return [false]  
    • any other input or no input before the timeout → return
      [?default] (defaults to [false]).

    Timing behaviour:

    • If [timeout_s] is **positive** (default ≈ 10 s) the call blocks
      for at most the given number of seconds.  
    • If [timeout_s] is **zero or negative** the function returns
      immediately with the default value.

    Internally the function uses [Core_unix.select] to implement the
    timeout and therefore only works on Unix platforms.  The prompt is
    flushed explicitly so that it becomes visible even when a Notty
    terminal has just been released.

    @return [true] if the user confirmed with “yes”, [false] otherwise. *)
val prompt_archive : ?timeout_s:float -> ?default:bool -> unit -> bool
