(** Reaction values produced by controller modules.

    A *controller* – see {!Chat_tui.Controller} and its sub-modules – takes a
    low-level {!Notty.Unescape.event}, performs a {e pure} in-memory update on
    the current {!Chat_tui.Model.t}, and returns a value of type
    {!reaction} describing {b what the caller must do next}.

    Keeping the variant definition in its own compilation unit breaks cyclic
    dependencies between the top-level dispatcher and the specialised
    controller implementations (insert-mode, normal-mode, command-line, …).

    The variant is intentionally {e closed}; future extensions must be
    handled explicitly and cannot be pattern-matched against implicitly. *)

type chat_destination =
  | Earlier_conversation
  | Search_result of Projected_message.Id.t
  | Latest_conversation

type reaction =
  | Redraw
  (** Visible state changed – redraw the Notty viewport before waiting for
      the next event. *)
  | Refresh_messages
  (** Canonical history changed – rebuild the effective Chat projection
      before redrawing. *)
  | Submit_input
  (** User finalised the prompt (Meta + Enter) – assemble an OpenAI request
      from the input buffer and append a {i pending} entry to the
      conversation view. *)
  | Cancel_or_quit
  (** Escape pressed.

      • If a request is in flight, cancel it via {!Eio.Switch.fail} and
        return to idle.
      • Otherwise fall back to {!Quit}. *)
  | Compact_context
  (** Trigger conversation compaction via
      {!Context_compaction.Compactor}.  The main loop should summarise the
      earlier history, replace the elided messages with the summary, and
      then issue {!Redraw}. *)
  | Quit
  (** Immediate termination request (Ctrl-C / `q`).  The caller should shut
      down resources and exit the main loop. *)
  | Chat_scrolled of bool
  (** A conversation-history scroll was consumed. The payload is [true] when
      the visible viewport moved and therefore requires a redraw. *)
  | Prepare_chat_destination of chat_destination
  (** A nonlocal history destination requires asynchronous exact corridor
      preparation before it can be shown. *)
  | Shell_approval_response of
      string * Shell_runtime.Approval_broker.ui_response
  (** Resolve the active shell approval modal without touching the composer. *)
  | Shell_grant_revoke_requested of int * string
  (** Confirm revocation of the selected persisted grant. *)
  | Shell_management_refresh_requested of int
  (** Open Shell Security and refresh its durable management data. *)
  | Moderator_input_response of string
  (** Resolve the active moderator interaction without touching the composer. *)
  | Unhandled
  (** The controller chose not to handle the event – propagate it to the
      next layer (e.g. a global key-binding handler). *)

(*  Do {b not} add helper functions here.  The unit serves purely as a
    dependency-free anchor type shared across the controller hierarchy. *)
