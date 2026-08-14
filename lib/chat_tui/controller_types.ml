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

type chat_destination =
  | Earlier_conversation
  | Search_result of Projected_message.Id.t
  | Latest_conversation

type reaction =
  | Redraw
  (** Visible state changed – caller must redraw the Notty viewport before
        waiting for the next event. *)
  | Refresh_messages
  (** Canonical history changed – caller must rebuild the effective Chat
      projection before redrawing. *)
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
  | Compact_context
  (** Trigger conversation compaction via {!Context_compaction.Compactor}.
        The main loop should summarise history and redraw. *)
  | Quit
  (** Immediate termination request (e.g. Ctrl-C).

        The caller should cleanly shut down all resources and return from the
        main loop. *)
  | Chat_scrolled of bool
  (** A conversation-history scroll was consumed. The payload is [true] when
      the visible viewport moved and therefore requires a redraw. *)
  | Prepare_chat_destination of chat_destination
  (** A nonlocal history destination requires asynchronous exact corridor
      preparation before it can be shown. *)
  | Shell_approval_response of
      string * Shell_runtime.Approval_broker.ui_response
  | Shell_grant_revoke_requested of int * string
  | Shell_management_refresh_requested of int
  | Moderator_input_response of string
  | Unhandled
  (** The controller ignored the event; propagate it to the next handler
        (e.g. a global key-binding layer). *)

(*  The module is a *type-shell* only – do **not** add helper functions here.
    Doing so would pull additional dependencies into the compilation unit and
    negate the whole purpose of having a minimal, dependency-free anchor type
    shared across the controller hierarchy. *)
