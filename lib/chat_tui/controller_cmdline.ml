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

(** [insert_char m c] inserts printable character [c] at the current
    cursor position inside the command-line buffer of [m].  The cursor is
    moved one position to the right afterwards.  The function does not
    perform any UTF-8 validation – the surrounding controller guarantees
    that only single-byte ASCII reaches this path. *)

let insert_char model c =
  let buf = Model.cmdline model in
  let pos = Model.cmdline_cursor model in
  let before = String.sub buf ~pos:0 ~len:pos in
  let after = String.sub buf ~pos ~len:(String.length buf - pos) in
  Model.set_cmdline model (before ^ String.of_char c ^ after);
  Model.set_cmdline_cursor model (pos + 1)
;;

(** [backspace m] removes the character immediately left of the cursor in
    the command-line buffer of [m] and moves the cursor one position to
    the left.  Doing nothing if the cursor is at position 0. *)

let backspace model =
  let buf = Model.cmdline model in
  let pos = Model.cmdline_cursor model in
  if pos > 0
  then (
    let before = String.sub buf ~pos:0 ~len:(pos - 1) in
    let after = String.sub buf ~pos ~len:(String.length buf - pos) in
    Model.set_cmdline model (before ^ after);
    Model.set_cmdline_cursor model (pos - 1))
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
  | "delete" | "d" ->
    (match Model.selected_msg model with
     | None -> Redraw
     | Some sel_idx ->
       let msgs = Model.messages model in
       if Int.(sel_idx < 0) || Int.(sel_idx >= List.length msgs)
       then Redraw
       else (
         let new_msgs = List.filteri msgs ~f:(fun i _ -> Int.(i <> sel_idx)) in
         Model.set_messages model new_msgs;
         (* Adjust selection to previous message or None *)
         let new_len = List.length new_msgs in
         if Int.(new_len = 0)
         then Model.select_message model None
         else Model.select_message model (Some (Int.min (new_len - 1) sel_idx));
         Redraw))
  | "edit" | "e" ->
    (match Model.selected_msg model with
     | None -> Redraw
     | Some sel_idx ->
       (match List.nth (Model.messages model) sel_idx with
        | None -> Redraw
        | Some (_role, txt) ->
          Model.set_input_line model txt;
          Model.set_cursor_pos model (String.length txt);
          Model.set_mode model Model.Insert;
          (* Enable Raw mode for safe editing as per design.*)
          Model.set_draft_mode model Model.Raw_xml;
          Redraw))
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
    insert_char model c;
    Redraw
  | `Key (`Arrow `Left, _) ->
    let pos = Model.cmdline_cursor model in
    if pos > 0 then Model.set_cmdline_cursor model (pos - 1);
    Redraw
  | `Key (`Arrow `Right, _) ->
    let pos = Model.cmdline_cursor model in
    if pos < String.length (Model.cmdline model)
    then Model.set_cmdline_cursor model (pos + 1);
    Redraw
  | _ -> Unhandled
;;
