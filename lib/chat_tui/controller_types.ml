(** Reaction values produced by controller modules.

    A *controller* (see {!Chat_tui.Controller} and its sub-modules) inspects
    low-level terminal input and performs *pure* in-memory updates on the
    current {!Chat_tui.Model.t}.  After handling the input it returns a value
    of type {!reaction} telling the caller – usually the main event-loop in
    {!Chat_tui.App} – what to do next.

    Separating the declaration of {!reaction} into its own compilation unit
    removes cyclic dependencies between {!Chat_tui.Controller} and the
    various specialised controller modules (normal mode, command-line mode,
    etc.).  No other values are exposed here. *)

type reaction =
  | Redraw
  (** Visible state changed – caller must redraw the Notty viewport before
        waiting for the next event. *)
  | Submit_input
  (** User finalised the prompt – typically pressing [Meta+Enter].  The
        caller should
        package the current input buffer into an OpenAI request and append a
        pending entry to the conversation view. *)
  | Cancel_or_quit
  (** Escape key pressed.

        - If a request is in flight, cancel it (by failing the associated
          {!Eio.Switch.t}).
        - Otherwise treat the event as {!Quit}. *)
  | Quit
  (** Immediate termination request (e.g. Ctrl-C).

        The caller should cleanly shut down all resources and return from the
        main loop. *)
  | Unhandled
  (** The controller ignored the event; propagate it to the next handler
        (e.g. a global key-binding layer). *)

(*  The module is a *type-shell* only – do **not** add helper functions here.
    Doing so would pull additional dependencies into the compilation unit and
    negate the whole purpose of having a minimal, dependency-free anchor type
    shared across the controller hierarchy. *)
