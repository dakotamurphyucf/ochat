open Core
module UC = Stdlib.Uchar
module Scroll_box = Notty_scroll_box
open Controller_types
(* -------------------------------------------------------------------- *)
(* Utility helpers – shared line_bounds moved to Controller_shared.          *)
(* -------------------------------------------------------------------- *)

let move_cursor_vertically model ~dir =
  let input = Model.input_line model in
  let pos = Model.cursor_pos model in
  let col =
    let start, _ = Controller_shared.line_bounds input pos in
    pos - start
  in
  let rec seek_line i step remaining =
    if remaining = 0 || i < 0 || i >= String.length input
    then i
    else (
      let i' =
        match dir with
        | -1 (* up *) ->
          (* move left until before newline *)
          let j = ref (i - 1) in
          while !j >= 0 && not (Char.equal (String.get input !j) '\n') do
            decr j
          done;
          !j
        | 1 ->
          let j = ref (i + 1) in
          while !j < String.length input && not (Char.equal (String.get input !j) '\n') do
            incr j
          done;
          !j
        | _ -> i
      in
      seek_line i' step (remaining - 1))
  in
  let target_line_pos =
    let start, _ = Controller_shared.line_bounds input pos in
    let i = if dir < 0 then start else snd (Controller_shared.line_bounds input pos) in
    seek_line i dir 1
  in
  (* At line start of new line; now move to desired column or EOL *)
  let target_line_start, target_line_end =
    Controller_shared.line_bounds input target_line_pos
  in
  let new_pos = Int.min (target_line_start + col) target_line_end in
  Model.set_cursor_pos model new_pos
;;

(* Word navigation helpers (adapted from Insert-mode implementation) *)

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

let pending_g = ref false
let pending_dd = ref false

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
    move_cursor_vertically model ~dir:(-1);
    Redraw
  | `Key (`ASCII 'j', mods) when List.is_empty mods ->
    move_cursor_vertically model ~dir:1;
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
    let _, screen_h = Notty_eio.Term.size term in
    let input_h =
      match String.split_lines (Model.input_line model) with
      | [] -> 1
      | ls -> List.length ls
    in
    Scroll_box.scroll_to_bottom (Model.scroll_box model) ~height:(screen_h - input_h);
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
