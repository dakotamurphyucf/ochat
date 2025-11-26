(** Event controller for the Chat-TUI.

    [`Controller`] is the {b single entry-point} that the main event loop uses
    to translate a raw {!Notty.Unescape.event} into an in-memory mutation of
    the {!Chat_tui.Model.t}.  All logic in the file is {e pure with respect to
    IO} – it only edits the mutable record fields of the given [model] and
    returns a {!reaction} value that tells the caller what to do next.

    Key maps are split into three sub-states that mirror Vim semantics:

    • {b Insert}  – free-form text editing (default)  
    • {b Normal}  – modal navigation & message manipulation (see
      {!Chat_tui.Controller_normal})  
    • {b Cmdline} – ':' command prompt (see {!Chat_tui.Controller_cmdline})

    [`Controller.handle_key`] is therefore a {e dispatcher}: it selects a
    key-handler based on [model.mode] and forwards the event.  Insert-mode is
    implemented locally in this compilation unit, whereas Normal- and
    Command-line modes live in their respective sub-modules.

    {1 Insert-mode shortcuts}
    The implementation provides a pragmatic subset of desktop-editor
    shortcuts.  Selected examples:

    {ul
    {- Left / Right arrow keys – move the caret by one character}
    {- Ctrl+Up / Ctrl+Down – move the caret by one visual line inside the
       multi-line prompt}
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
    {- ESC – switch to Normal mode when pressed without modifiers; when a
       modifier is present it bubbles up as {!Cancel_or_quit}}
    }

    The set of bindings is intentionally conservative; unsupported keys are
    returned as {!Unhandled} so that outer layers may implement fallbacks.

    {1 Reactions}
    All controller variants share the same {!reaction} type (defined in
    {!Chat_tui.Controller_types}):

{[
| Redraw         – visible state changed; re-render the UI
| Submit_input   – draft is ready; send it to the assistant
| Cancel_or_quit – ESC; cancel streaming or quit if idle
| Compact_context – user requested context compaction
| Quit           – immediate termination (Ctrl-C / 'q')
| Unhandled      – event not recognised; try other handlers
]}

    {b Performance}: all operations run in O(length input) at worst (duplicate
    line); typical keystrokes are sub-millisecond. *)

type reaction = Controller_types.reaction =
  | Redraw (** The event modified the visible state – caller should refresh. *)
  | Submit_input (** User pressed Meta+Enter to submit the prompt. *)
  | Cancel_or_quit
  (** ESC request – cancel a running request via {!Eio.Switch.fail} when a
      stream is in flight, or fall back to {!Quit} when idle. *)
  | Compact_context
  (** Trigger conversation compaction via {!Context_compaction.Compactor} –
      the caller should summarise the earlier history, replace elided
      messages with the summary and then issue {!Redraw}. *)
  | Quit
  (** Immediate quit (Ctrl-C / q).  The main loop should terminate the Notty
      session, release resources and exit. *)
  | Unhandled
  (** Controller didn’t deal with the event – propagate it to higher-level
      handlers or ignore it. *)

(** [handle_key ~model ~term ev] is the {b single} public function of the
    controller hierarchy.  It examines [model.mode] and forwards [ev] to the
    appropriate key-map – Insert (local), Normal or Cmdline – then returns the
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
    Dispatch an event inside the main loop:
    {[
      let reaction = Chat_tui.Controller.handle_key ~model ~term ev in
      match reaction with
      | Redraw -> Renderer.draw ~model ~term
      | Submit_input -> send_prompt_to_assistant model
      | Cancel_or_quit -> handle_escape model
      | Quit -> exit 0
      | Unhandled -> ()
      | Compact_context -> Context_compaction.request ()
    ]} *)
val handle_key
  :  model:Model.t
  -> term:Notty_eio.Term.t
  -> Notty.Unescape.event
  -> reaction
