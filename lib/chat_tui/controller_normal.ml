(** Normal-mode key handling for the Ochat terminal UI.

    The {!Controller_normal} module implements the subset of the Vim-inspired
    key bindings that operate while the input area is in {e Normal} editor
    mode.  It receives raw {!Notty.Unescape.event} values, mutates the
    in-memory {!Chat_tui.Model.t} accordingly, and returns a
    {!Controller_types.reaction} telling the caller whether a screen refresh
    is required or additional action (e.g.
    {!Controller_types.Submit_input}) should be triggered.

    The module has {b no side-effects} besides changing the fields inside the
    mutable model – network requests, file IO, or long-running function calls
    are all handled elsewhere.  Keeping the logic pure makes unit testing
    easier and ensures that the controller focuses exclusively on
    user-interaction concerns such as cursor movement, scrolling and text
    editing.

    {1 Implementation overview}

    • High-level public API consists of a single entry point
      {!handle_key_normal} – every other value is an implementation detail.

    • Helpers like {!move_cursor_vertically}, {!skip_space_backward} and
      {!skip_word_forward} encapsulate reusable cursor-navigation logic.

    • Two reference cells, {!val:pending_g} and {!val:pending_dd}, keep track
      of multi-keystroke commands ("gg" and "dd").

    All byte indices refer to the UTF-8 encoded {!Model.input_line}.  The
    controller therefore treats the string as a raw byte sequence – full
    Unicode support is postponed until a later milestone (see issue #142).
*)

open Core
module UC = Stdlib.Uchar
module Scroll_box = Notty_scroll_box
open Controller_types
(* -------------------------------------------------------------------- *)
(* Utility helpers – shared [line_bounds] lives in {!Controller_shared}.   *)
(* -------------------------------------------------------------------- *)

(** [move_cursor_vertically model ~term ~dir] moves the caret [dir] visual rows
      up (`dir = -1`) or down (`dir = 1`).  The function keeps the {i visual
      column} constant where possible, i.e.

      The operation is no-op when the cursor is already on the first or last
      display row of the input. *)

let move_cursor_vertically model ~term ~dir =
  let box_width, _ = Notty_eio.Term.size term in
  match Input_display.cursor_pos_after_vertical_move ~box_width ~model ~dir with
  | None -> ()
  | Some new_pos -> Model.set_cursor_pos model new_pos
;;

(* Word navigation helpers (adapted from Insert-mode implementation) *)

(** [skip_space_backward s j] returns the index of the first non-blank
    character to the {b left} of [j].  Consecutive runs of whitespace are
    skipped first, followed by the preceding word.  The helper mimics the
    behaviour of Vim's "b" command and is used by {!handle_key_normal}. *)
let skip_space_backward s j =
  let rec skip_space i =
    if i <= 0
    then 0
    else if Char.is_whitespace (String.get s (i - 1))
    then skip_space (i - 1)
    else i
  in
  let rec skip_word i =
    if i <= 0
    then 0
    else if not (Char.is_whitespace (String.get s (i - 1)))
    then skip_word (i - 1)
    else i
  in
  let i = skip_space j in
  skip_word i
;;

(** [skip_word_forward s len j] returns the index after the next word to
      the {b right} of [j] in [s].  Whitespace after the word is consumed as
      well so that the returned position corresponds to the first printable
      character of the following word (or [len] if at the end of the
      string).  Implements the semantics of Vim's "w" motion. *)
let skip_word_forward s len j =
  let rec skip_non_space i =
    if i >= len
    then len
    else if Char.is_whitespace (String.get s i)
    then i
    else skip_non_space (i + 1)
  in
  let rec skip_space i =
    if i >= len
    then len
    else if Char.is_whitespace (String.get s i)
    then skip_space (i + 1)
    else i
  in
  let after_word = skip_non_space j in
  skip_space after_word
;;

(* -------------------------------------------------------------------- *)
(* Main Normal-mode key-handler                                           *)
(* -------------------------------------------------------------------- *)

(** State flags for multi-key commands.

    • [pending_g] – set after receiving a solitary 'g'.  A subsequent 'g'
      within the same [handle_key_normal] invocation triggers the "gg" motion
      (scroll to top).  Any other key clears the flag.

    • [pending_dd] – set after a single 'd'.  A second 'd' performs a line
      deletion; anything else resets the flag. *)

let pending_g = ref false
let pending_dd = ref false

(** [handle_key_normal ~model ~term ev] processes the Normal-mode key event
      [ev] and mutates [model] accordingly.

      The function recognises a small subset of common Vim commands that are
      useful inside a chat prompt – cursor motions, word navigation, line
      editing commands, and scrolling.  Whenever the resulting state change
      affects what is displayed on screen, the function returns {!Redraw} so
      that the caller can re-render the UI.  Some events need to propagate
      further to the main application (e.g. ':' opens command-line mode),
      which is signalled via other variants of {!reaction}.

      The exact mapping is intentionally kept minimal and intuitive rather
      than striving for full Vim parity – more specialised or rarely used
      motions are left to future extensions. *)
let handle_key_normal ~(model : Model.t) ~term (ev : Notty.Unescape.event) : reaction =
  match ev with
  (* -------------------------------------------------------------- *)
  (* Enter command-line mode with ':'                               *)
  | `Key (`ASCII ':', mods) when List.is_empty mods ->
    Model.set_mode model Cmdline;
    Model.set_cmdline model "";
    Model.set_cmdline_cursor model 0;
    Redraw
  (* ------------------------------------------------------------------ *)
  (* Simple cursor left / right                                           *)
  | `Key (`ASCII 'h', mods) when List.is_empty mods ->
    let pos = Model.cursor_pos model in
    if pos > 0 then Model.set_cursor_pos model (pos - 1);
    Redraw
  | `Key (`ASCII 'l', mods) when List.is_empty mods ->
    let pos = Model.cursor_pos model in
    let len = String.length (Model.input_line model) in
    if pos < len then Model.set_cursor_pos model (pos + 1);
    Redraw
  (* ------------------------------------------------------------------ *)
  (* Up / Down by visual line                                             *)
  | `Key (`ASCII 'k', mods) when List.is_empty mods ->
    move_cursor_vertically model ~term ~dir:(-1);
    Redraw
  | `Key (`ASCII 'j', mods) when List.is_empty mods ->
    move_cursor_vertically model ~term ~dir:1;
    Redraw
  (* ------------------------------------------------------------------ *)
  (* Word-wise navigation                                                 *)
  | `Key (`ASCII 'w', mods) when List.is_empty mods ->
    let s = Model.input_line model in
    let len = String.length s in
    let pos = Model.cursor_pos model in
    let new_pos = skip_word_forward s len pos in
    Model.set_cursor_pos model new_pos;
    Redraw
  | `Key (`ASCII 'b', mods) when List.is_empty mods ->
    let s = Model.input_line model in
    let pos = Model.cursor_pos model in
    let new_pos = skip_space_backward s pos in
    Model.set_cursor_pos model new_pos;
    Redraw
  (* ------------------------------------------------------------------ *)
  (* Line boundaries                                                      *)
  | `Key (`ASCII '0', mods) when List.is_empty mods ->
    let s = Model.input_line model in
    let pos = Model.cursor_pos model in
    let start, _ = Controller_shared.line_bounds s pos in
    Model.set_cursor_pos model start;
    Redraw
  | `Key (`ASCII '$', mods) when List.is_empty mods ->
    let s = Model.input_line model in
    let pos = Model.cursor_pos model in
    let _, eol = Controller_shared.line_bounds s pos in
    Model.set_cursor_pos model eol;
    Redraw
  (* ------------------------------------------------------------------ *)
  (* gg / G – top / bottom scrolling                                     *)
  | `Key (`ASCII 'g', mods) when List.is_empty mods ->
    if !pending_g
    then (
      (* second 'g' – go to top *)
      pending_g := false;
      (* Scroll to top and select first message if available *)
      (match Model.messages model with
       | _ :: _ -> Model.select_message model (Some 0)
       | [] -> Model.select_message model None);
      Scroll_box.scroll_to_top (Model.scroll_box model);
      Redraw)
    else (
      pending_g := true;
      Unhandled)
  | `Key (`ASCII _, _) when !pending_g ->
    (* any other key resets the flag *)
    pending_g := false;
    Unhandled
  | `Key (`ASCII _, _) when !pending_dd ->
    pending_dd := false;
    Unhandled
  | `Key (`ASCII 'G', mods) when List.is_empty mods ->
    let screen_w, screen_h = Notty_eio.Term.size term in
    let layout = Chat_page_layout.compute ~screen_w ~screen_h ~model in
    Scroll_box.scroll_to_bottom (Model.scroll_box model) ~height:layout.scroll_height;
    (* Also move selection to bottom when in selection mode *)
    let msg_count = List.length (Model.messages model) in
    if msg_count > 0 then Model.select_message model (Some (msg_count - 1));
    Redraw
  (* ------------------------------------------------------------------ *)
  (* Phase 2 – Edit operations *)

  (* 'a' append => move cursor right if possible and enter Insert mode *)
  | `Key (`ASCII 'a', mods) when List.is_empty mods ->
    let len = String.length (Model.input_line model) in
    let pos = Model.cursor_pos model in
    let new_pos = if pos < len then pos + 1 else pos in
    Model.set_cursor_pos model new_pos;
    Model.set_mode model Insert;
    Redraw
  (* 'o' – open new line below *)
  | `Key (`ASCII 'o', mods) when List.is_empty mods ->
    Model.push_undo model;
    let s = Model.input_line model in
    let pos = Model.cursor_pos model in
    let _, line_end = Controller_shared.line_bounds s pos in
    let insertion_pos, insert_text =
      if line_end < String.length s && Char.equal (String.get s line_end) '\n'
      then line_end + 1, "\n"
      else line_end, "\n"
    in
    let before = String.sub s ~pos:0 ~len:insertion_pos in
    let after = String.sub s ~pos:insertion_pos ~len:(String.length s - insertion_pos) in
    Model.set_input_line model (before ^ insert_text ^ after);
    Model.set_cursor_pos model insertion_pos;
    Model.set_mode model Insert;
    Redraw
  (* 'O' – open new line above *)
  | `Key (`ASCII 'O', mods) when List.is_empty mods ->
    Model.push_undo model;
    let s = Model.input_line model in
    let pos = Model.cursor_pos model in
    let line_start, _ = Controller_shared.line_bounds s pos in
    let insertion_pos = line_start in
    let before = String.sub s ~pos:0 ~len:insertion_pos in
    let after = String.sub s ~pos:insertion_pos ~len:(String.length s - insertion_pos) in
    Model.set_input_line model (before ^ "\n" ^ after);
    Model.set_cursor_pos model insertion_pos;
    Model.set_mode model Insert;
    Redraw
  (* 'x' – delete character under cursor *)
  | `Key (`ASCII 'x', mods) when List.is_empty mods ->
    let s = Model.input_line model in
    let pos = Model.cursor_pos model in
    if pos < String.length s
    then (
      Model.push_undo model;
      let before = String.sub s ~pos:0 ~len:pos in
      let after = String.sub s ~pos:(pos + 1) ~len:(String.length s - pos - 1) in
      Model.set_input_line model (before ^ after));
    Redraw
  (* 'dd' – delete current line *)
  | `Key (`ASCII 'd', mods) when List.is_empty mods ->
    if !pending_dd
    then (
      pending_dd := false;
      let s = Model.input_line model in
      let pos = Model.cursor_pos model in
      let line_start, line_end = Controller_shared.line_bounds s pos in
      let deletion_end =
        if line_end < String.length s && Char.equal (String.get s line_end) '\n'
        then line_end + 1
        else line_end
      in
      Model.push_undo model;
      let before = String.sub s ~pos:0 ~len:line_start in
      let after = String.sub s ~pos:deletion_end ~len:(String.length s - deletion_end) in
      let new_s = before ^ after in
      Model.set_input_line model new_s;
      Model.set_cursor_pos model (Int.min line_start (String.length new_s));
      Redraw)
    else (
      pending_dd := true;
      Unhandled)
  (* Undo 'u' *)
  | `Key (`ASCII 'u', mods) when List.is_empty mods ->
    if Model.undo model then Redraw else Unhandled
  (* Redo Ctrl-r *)
  | `Key (`ASCII 'r', mods) when List.equal Poly.( = ) mods [ `Ctrl ] ->
    if Model.redo model then Redraw else Unhandled
  (* Toggle Raw-XML draft mode with bare 'r' in Normal mode *)
  | `Key (`ASCII 'r', mods) when List.is_empty mods ->
    let new_mode =
      match Model.draft_mode model with
      | Model.Plain -> Model.Raw_xml
      | Model.Raw_xml -> Model.Plain
    in
    Model.set_draft_mode model new_mode;
    Redraw
  (* ensure other keys reset dd_state etc. but dd_state is local; not necessary. *)
  (* ------------------------------------------------------------------ *)
  (* Phase 5 – Message selection keys                                   *)

  (* Move selection to previous message with '[' *)
  | `Key (`ASCII '[', mods) when List.is_empty mods ->
    let msgs = Model.messages model in
    let msg_count = List.length msgs in
    if msg_count > 0
    then (
      let new_idx =
        match Model.selected_msg model with
        | None -> msg_count - 1 (* start at bottom *)
        | Some i -> Int.max 0 (i - 1)
      in
      Model.select_message model (Some new_idx));
    Redraw
  (* Move selection to next message with ']' *)
  | `Key (`ASCII ']', mods) when List.is_empty mods ->
    let msgs = Model.messages model in
    let msg_count = List.length msgs in
    if msg_count > 0
    then (
      let new_idx =
        match Model.selected_msg model with
        | None -> 0 (* start at top *)
        | Some i -> Int.min (msg_count - 1) (i + 1)
      in
      Model.select_message model (Some new_idx));
    Redraw
  (* gg already handled above: after scrolling to top we set selection too *)
  | _ -> Unhandled
;;
