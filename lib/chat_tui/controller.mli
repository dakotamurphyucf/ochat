(** Controller – translate raw terminal key events into UI state updates.

    This module is entirely side-effect-free wrt IO.  It operates on the
    mutable fields contained inside {!Chat_tui.Model.t} and decides whether
    a redraw of the Notty viewport is necessary.  Complex operations such as
    sending a request to OpenAI are still handled in [bin/chat_tui.ml] or via
    {!Chat_tui.Cmd}; the controller focuses on purely local editing and
    scrolling interactions. *)

type reaction =
  | Redraw (** The event modified the visible state – caller should refresh. *)
  | Submit_input (** User pressed Meta+Enter to submit the prompt. *)
  | Cancel_or_quit (** ESC – cancel running request or quit. *)
  | Quit (** Immediate quit (Ctrl-C / q). *)
  | Unhandled (** Controller didn’t deal with the event. *)

(** [handle_key ~model ~term ev] inspects [ev] and performs the corresponding
    in-memory updates (editing the input buffer, scrolling the history, …).
    A value of {!reaction} indicates whether the caller needs to redraw the
    screen or pass the event on for further processing. *)
val handle_key
  :  model:Model.t
  -> term:Notty_eio.Term.t
  -> Notty.Unescape.event
  -> reaction
