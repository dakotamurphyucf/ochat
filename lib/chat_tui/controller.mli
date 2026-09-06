(** Event controller for the Chat-TUI.

    [`Controller`] is the {b single entry-point} that the main event loop uses
    to translate a raw {!Notty.Unescape.event} into an in-memory mutation of
    the {!Chat_tui.Model.t}.  All logic in the file is {e pure with respect to
    IO} – it only edits the mutable record fields of the given [model] and
    returns a {!reaction} value that tells the caller what to do next.

    Chat-editor key maps are split into four modes:

    • {b Insert}  – free-form text editing (default)  
    • {b Normal}  – modal navigation & message manipulation (see
      {!Chat_tui.Controller_normal})  
    • {b Cmdline} – ':' command prompt (see {!Chat_tui.Controller_cmdline})
    • {b Search} – '/' or '?' history-search prompt

    [`Controller.handle_key`] is therefore a {e dispatcher}: it selects a
    key-handler based on [model.mode] and forwards the event.  Insert-mode is
    implemented locally in this compilation unit, whereas Normal- and
    Command-line modes live in their respective sub-modules.

    {1 Insert-mode shortcuts}
    The implementation provides a pragmatic subset of desktop-editor
    shortcuts.  Selected examples:

    {ul
    {- Left / Right arrow keys – move the caret by one character}
    {- Meta+Up/Down or Shift+Up/Down – move the caret by one visual line;
       plain and Ctrl+Up/Down scroll conversation history}
    {- Meta+←/→ or Ctrl+←/→ and Meta+{b b} / Meta+{b f} – word-wise
       movement}
    {- Ctrl-A / Ctrl-E – beginning / end of the current line; Ctrl+Home /
       Ctrl+End – beginning / end of the entire prompt}
    {- Backspace – delete the previous character}
    {- Ctrl-K / Ctrl-U / Ctrl-W or Meta+Backspace – kill to EOL / BOL /
       previous word; Ctrl-Y – yank the last killed text}
    {- Meta+v (or Alt+{b s}) – toggle a selection anchor; Ctrl-C /
       Ctrl-X operate on the active selection (copy / cut)}
    {- Up / Down arrows, PageUp / PageDown, Home / End – scroll conversation
       history while keeping the editor in Insert mode; this disables
       {!Chat_tui.Model.auto_follow} until the viewport reaches the bottom
       again}
    {- Meta+Enter – submit the current prompt ([Submit_input])}
    {- Type-ahead (when a completion is available and relevant): Tab accepts
       the full completion, Shift+Tab accepts a single line, Ctrl+Space
       toggles the preview popup; while the preview is open, Ctrl+Shift+Up/Down
       scrolls it by one line (with PageUp/PageDown as a fallback on terminals
       that cannot encode the Ctrl+Shift combinations); bare ESC closes the
       preview (if open), otherwise dismisses the completion (if present),
       otherwise switches to Normal mode}
    {- ESC with modifiers bubbles up as {!Cancel_or_quit}}
    }

    In Normal mode, [u] undoes draft edits and Ctrl-R redoes them. Ctrl-R in
    Insert mode toggles raw XML instead. Both bindings accept lowercase and
    uppercase Ctrl-modified events and the raw DC2 byte (0x12); Notty decodes
    terminal DC2 as uppercase Ctrl-R.

    Bare Escape in Normal mode clears an active Visual selection and pending
    operator/count state, returns [Redraw], and remains Normal without changing
    the draft or activity. Without a selection it returns [Cancel_or_quit].
    Shell interactions and non-Chat pages retain priority over editor keys.

    The set of bindings is intentionally conservative; unsupported keys are
    returned as {!Unhandled} so that outer layers may implement fallbacks.

    {1 Reactions}
    All controller variants share the same {!reaction} type (defined in
    {!Chat_tui.Controller_types}):

{[
| Redraw         – visible state changed; re-render the UI
| Refresh_messages – canonical history changed; rebuild the effective projection
| Delete_history – authorize and commit canonical occurrence deletion in the host
| Submit_input   – draft is ready; send it to the assistant
| Cancel_or_quit – ESC; cancel streaming or quit if idle
| Compact_context – user requested context compaction
| Quit           – explicit quit request (:q or Ctrl-C without selection)
| Chat_scrolled  – history scroll consumed; payload reports visible movement
| Prepare_chat_destination – asynchronously prepare a nonlocal exact viewport
| Unhandled      – event not recognised; try other handlers
]}

    Grapheme segmentation scans the input buffer. Rendering, persistence and
    network work belong to the host, not this controller. *)

type reaction = Controller_types.reaction =
  | Redraw (** The event modified the visible state – caller should refresh. *)
  | Refresh_messages
  | Delete_history of History_entry.Id.t
  (** Canonical history changed – caller should rebuild the effective Chat
      projection before rendering. *)
  | Submit_input (** User pressed Meta+Enter to submit the prompt. *)
  | Cancel_or_quit
  (** ESC request after editor selection handling. The host cancels active
      work or quits when idle; agent hosts use their authorized session API. *)
  | Compact_context
  (** Trigger conversation compaction via {!Context_compaction.Compactor} –
      the caller should summarise the earlier history, replace elided
      messages with the summary and then issue {!Redraw}. *)
  | Quit
  (** Explicit quit (:q or Ctrl-C without selection). The host terminates the Notty
      session, release resources and exit. *)
  | Chat_scrolled of bool
  (** A conversation-history scroll was consumed. The payload is [true] when
      the visible viewport moved and therefore requires a redraw. *)
  | Prepare_chat_destination of Controller_types.chat_destination
  (** A nonlocal history destination requires asynchronous exact corridor
      preparation before it can be shown. *)
  | Shell_approval_response of string * Shell_runtime.Approval_broker.ui_response
  | Shell_grant_revoke_requested of int * string
  | Shell_management_refresh_requested of int
  | Moderator_input_response of string
  | Unhandled
  (** Controller didn’t deal with the event – propagate it to higher-level
      handlers or ignore it. *)

(** [handle_key ~model ~term ev] is the {b single} public function of the
    controller hierarchy.  It examines [model.mode] and forwards [ev] to the
    appropriate key-map – Insert, Normal, Cmdline or Search – then returns the
    resulting {!reaction}.

    {1 Parameters}
    @param model The mutable snapshot of the UI state that will be modified
    {i in-place}.  Only the record fields are changed – no network or disk IO
    happens here.
    @param term  The Notty terminal abstraction used to query run-time
    geometry with {!Notty_eio.Term.size}.  The value is {i never} modified.
    @param ev    The raw event received from {!Notty.Unescape.event}.

    {1 Return value}
    A {!Controller_types.reaction}.  The caller {b must} pattern-match on the
    result and perform the side-effects described by the variant:
    {ul
    {- {!Redraw} – re-render the viewport}
    {- {!Submit_input} – wrap the draft into an OpenAI request and append a
       pending entry to the history}
    {- {!Cancel_or_quit} – either cancel an in-flight stream or quit when
       idle}
    {- {!Compact_context} – asynchronously trigger context compaction}
    {- {!Quit} – stop the application immediately}
    {- {!Unhandled} – fall back to global shortcuts or ignore the event}}

    {1 Example}
    Return the typed reaction to the application host:
    {[
      let dispatch ~model ~term event =
        Chat_tui.Controller.handle_key ~model ~term event
    ]}
    The caller must handle every reaction; use {!App} for a complete host.
    In particular, history deletion requires authority, not just a redraw. *)
val handle_key
  :  model:Model.t
  -> term:Notty_eio.Term.t
  -> Notty.Unescape.event
  -> reaction

(** [is_ctrl_g ev] recognizes modifier-aware Ctrl-G and terminal BEL. *)
val is_ctrl_g : Notty.Unescape.event -> bool
