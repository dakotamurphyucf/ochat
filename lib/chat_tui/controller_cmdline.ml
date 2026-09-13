(** Command-line controller – handles Vim-style ':' prompt.

    This module becomes active when the TUI’s editor is in
    {!Model.Cmdline} mode.  While in this mode the bottom line of the
    UI turns into a ':' command prompt that accepts a tiny subset of
    ex-style commands.  The responsibilities of this module are to

    • mutate the {!Model.cmdline} / {!Model.cmdline_cursor} fields when
      the user types or moves the caret;
    • leave command-line mode once the command has been evaluated;
    • convert the command into a {!Controller_types.reaction} value so
      that the outer controller can decide whether to redraw, submit the
      prompt, or terminate the program.

    The set of recognised commands is deliberately minimal and kept in
    sync with the Vim semantics wherever it makes sense:

    ┌─────────────┬───────────────────────────────────────────────────┐
    │ Command     │ Effect                                           │
    ├─────────────┼───────────────────────────────────────────────────┤
    │ `q`, `quit` │ Quit the application immediately                │
    │ `w`         │ "Write" – submit the current input buffer        │
    │ `wq`        │ Submit the buffer and then quit                 │
    │ `c`, `cmp`, compact │ Summarise conversation context (compact) │
    │ `d`, delete │ Delete the currently selected message           │
    │ `e`, edit   │ Yank the selected message into the prompt       │
    └─────────────┴───────────────────────────────────────────────────┘

    Unknown commands are ignored and simply trigger a redraw so the
    prompt disappears.  All commands are matched case-insensitively.

    The implementation is purely in-memory; there is no IO here – the
    caller performs the heavy-weight operations (networking, persistence
    …) after receiving the reaction value.
*)

open Core
open Controller_types

let insert_text model text =
  let buf = Model.cmdline model in
  let pos = Model.cmdline_cursor model in
  let before = String.sub buf ~pos:0 ~len:pos in
  let after = String.sub buf ~pos ~len:(String.length buf - pos) in
  Model.set_cmdline model (before ^ text ^ after);
  Model.set_cmdline_cursor model (pos + String.length text)
;;

(** [backspace m] removes the character immediately left of the cursor in
    the command-line buffer of [m] and moves the cursor one position to
    the left.  Doing nothing if the cursor is at position 0. *)

let backspace model =
  let buf = Model.cmdline model in
  let pos = Model.cmdline_cursor model in
  if pos > 0
  then (
    let previous = Utf8_edit.previous buf pos in
    let before = String.sub buf ~pos:0 ~len:previous in
    let after = String.sub buf ~pos ~len:(String.length buf - pos) in
    Model.set_cmdline model (before ^ after);
    Model.set_cmdline_cursor model previous)
;;

let add_rejection_notice model text =
  ignore
    (Model.apply_patch model (Types.Add_placeholder_message { role = "system"; text })
     : Model.t)
;;

(** [execute_command m line] evaluates the normalized [line] (without the
    leading ':') and returns the resulting {!reaction}.  The function is
    case-insensitive and trims surrounding whitespace before matching.

    Regardless of the command’s success the function always leaves
    command-line mode, clears the prompt and resets the cursor, therefore
    callers do not need to worry about state hygiene.

    The recognised commands map to reactions as follows:

    • `q`, `quit`, `wq` → {!Quit}
    • `w`               → {!Submit_input}
    • `d`, `delete`     → Delete the currently selected message and return
      {!Redraw}
    • `e`, `edit`       → Copy the selected message into the insert buffer
      and return {!Redraw}
    • `c`, `cmp`, `compact` → Summarise conversation context via {!Compact_context}
    • `work`, `jobs` → Open the attached-session work overview.

    Any other input results in {!Redraw} to signal that a screen update is
    needed to hide the prompt again. *)

let execute_command model line : reaction =
  let open String in
  let cmd = lowercase (strip line) in
  (* Leave command-line mode regardless of command *)
  Model.set_mode model Model.Normal;
  Model.set_cmdline model "";
  Model.set_cmdline_cursor model 0;
  match cmd with
  | "q" | "quit" -> Quit
  | "w" -> Submit_input
  | "wq" -> Quit
  | "c" | "cmp" | "compact" -> Compact_context
  | "work" | "jobs" ->
    Model.set_active_page model Model.Page_id.Work;
    Redraw
  | "shell" | "security" ->
    Model.set_active_page model Model.Page_id.Shell_security;
    Shell_management_refresh_requested (Model.begin_shell_management_load model)
  | "delete" | "d" ->
    (match Model.selected_projected_row model with
     | Some { source = Canonical { entry_id }; _ } -> Delete_history entry_id
     | _ ->
       add_rejection_notice model "Select a canonical history entry to delete.";
       Redraw)
  | "edit" | "e" ->
    (match Model.selected_projected_row model with
     | None ->
       add_rejection_notice model "No projected row is selected.";
       Redraw
     | Some row ->
       (match row.Projected_message.source with
        | Canonical _ ->
          let _, txt = row.message in
          Model.set_input_line model txt;
          Model.set_cursor_pos model (String.length txt);
          Model.set_mode model Model.Insert;
          Model.set_draft_mode model Model.Plain;
          Redraw
        | Moderator_inserted _
        | Moderator_replacement _
        | Streaming _
        | Pending_approval _
        | Placeholder _ ->
          add_rejection_notice model "Cannot edit a noncanonical projected row.";
          Redraw))
  | "noh" | "nohlsearch" ->
    Model.clear_last_search model;
    Redraw
  | _ -> Redraw
;;

(** [handle_key_cmdline ~model ~term ev] is the top-level dispatch
    function used by {!Chat_tui.Controller} while the editor is in
    command-line mode.  It updates [model] according to the Notty
    [ev]ent and returns the matching {!reaction} variant.

    The [term] argument is ignored for now but kept in the signature for
    symmetry with other controller modules. *)

let handle_key_cmdline ~(model : Model.t) ~term:_ (ev : Notty.Unescape.event) : reaction =
  match ev with
  | `Key (`Enter, _) -> execute_command model (Model.cmdline model)
  | `Key (`Escape, _) ->
    Model.set_mode model Model.Normal;
    Redraw
  | `Key (`Backspace, _) ->
    backspace model;
    Redraw
  | `Key (`ASCII c, mods) when List.is_empty mods ->
    insert_text model (String.of_char c);
    Redraw
  | `Key (`Arrow `Left, _) ->
    let pos = Model.cmdline_cursor model in
    Model.set_cmdline_cursor model (Utf8_edit.previous (Model.cmdline model) pos);
    Redraw
  | `Key (`Arrow `Right, _) ->
    let pos = Model.cmdline_cursor model in
    Model.set_cmdline_cursor model (Utf8_edit.next (Model.cmdline model) pos);
    Redraw
  | `Key (`Uchar u, []) ->
    insert_text model (Utf8_edit.uchar u);
    Redraw
  | _ -> Unhandled
;;
