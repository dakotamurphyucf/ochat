(** Insert-mode implementation and mode dispatcher for {!Chat_tui.Controller}.

    The file consists of two conceptual parts:

     1. {b Insert-mode key map}.  A self-contained handler that translates raw
       {!Notty.Unescape.event}s into in-place mutations of {!Chat_tui.Model.t}
       while the editor is in [Insert] mode.  The supported shortcut set is a
       pragmatic union of readline, Vim and typical GUI-editor bindings.  All
       mutations are pure OCaml data changes – rendering and network IO are
       handled elsewhere.

     2. {b Dispatcher}.  [handle_key] examines [Model.mode] and forwards the
       event to the correct handler: the local Insert map, or the Normal /
       Cmdline controllers living in their own compilation units.  The
       function therefore represents the single public entry-point for the
       caller.

    Implementation details (kill-ring, scrolling maths, etc.) are kept
    private to avoid polluting the interface.  Refer to [controller.doc.md]
    for a high-level usage guide and key table. *)

open Core
module UC = Stdlib.Uchar
module Scroll_box = Notty_scroll_box

(* Re-expose the constructors locally so that the remainder of this file can
   stay unchanged.  The alias keeps them identical to the single source of
   truth in [Controller_types]. *)
(* Re-expose the constructors locally so that the remainder of this file can
   stay unchanged.  The alias keeps them identical to the single source of
   truth in [Controller_types]. *)
type reaction = Controller_types.reaction =
  | Redraw
  | Submit_input
  | Cancel_or_quit
  | Quit
  | Unhandled

(* -------------------------------------------------------------------- *)
(* Helper – update the input_line ref while keeping it UTF-8 safe.       *)
(* For the purpose of the demo we take the simple approach of slicing   *)
(* bytes which works as long as the terminal only inputs ASCII.         *)
(* -------------------------------------------------------------------- *)

let append_char (model : Model.t) c =
  let pos_ref = Model.cursor_pos model in
  (* Reset history browsing pointer when user edits *)
  let s = Model.input_line model in
  let pos = pos_ref in
  let before = String.sub s ~pos:0 ~len:pos in
  let after = String.sub s ~pos ~len:(String.length s - pos) in
  Model.set_input_line model (before ^ String.of_char c ^ after);
  Model.set_cursor_pos model (pos + 1)
;;

let backspace (model : Model.t) =
  let input_ref = Model.input_line model in
  let pos_ref = Model.cursor_pos model in
  let pos = pos_ref in
  let s = input_ref in
  if pos > 0
  then (
    let before = String.sub s ~pos:0 ~len:(pos - 1) in
    let after = String.sub s ~pos ~len:(String.length s - pos) in
    Model.set_input_line model (before ^ after);
    Model.set_cursor_pos model (pos - 1))
;;

(* -------------------------------------------------------------------- *)
(* Kill buffer – last piece of text removed with a kill command (Ctrl-K/U/W) *)
(* -------------------------------------------------------------------- *)

let kill_buffer : string ref = ref ""
let kill text = kill_buffer := text

let yank (model : Model.t) =
  if String.is_empty !kill_buffer
  then ()
  else (
    let s = Model.input_line model in
    let pos = Model.cursor_pos model in
    let before = String.sub s ~pos:0 ~len:pos in
    let after = String.sub s ~pos ~len:(String.length s - pos) in
    Model.set_input_line model (before ^ !kill_buffer ^ after);
    Model.set_cursor_pos model (pos + String.length !kill_buffer))
;;

(* -------------------------------------------------------------------- *)
(* Line helpers                                                          *)
(* -------------------------------------------------------------------- *)

let delete_range (model : Model.t) ~first ~last =
  (* Remove [first,last) from input line. Assumes indices are valid. *)
  if first >= last
  then ()
  else (
    let s = Model.input_line model in
    let before = String.sub s ~pos:0 ~len:first in
    let after = String.sub s ~pos:last ~len:(String.length s - last) in
    Model.set_input_line model (before ^ after);
    (* cursor moves to [first] *)
    Model.set_cursor_pos model first)
;;

(* -------------------------------------------------------------------- *)
(* Selection helpers (depends on [delete_range])                         *)
(* -------------------------------------------------------------------- *)

let selection_active (model : Model.t) = Model.selection_active model

let copy_selection (model : Model.t) =
  match Model.selection_anchor model with
  | None -> ()
  | Some anchor ->
    let pos = Model.cursor_pos model in
    let start_idx, end_idx = if anchor <= pos then anchor, pos else pos, anchor in
    if start_idx <> end_idx
    then (
      let input = Model.input_line model in
      let len = end_idx - start_idx in
      if start_idx >= 0 && start_idx + len <= String.length input
      then (
        let text = String.sub input ~pos:start_idx ~len in
        kill text));
    Model.clear_selection model
;;

let cut_selection (model : Model.t) =
  match Model.selection_anchor model with
  | None -> ()
  | Some anchor ->
    let pos = Model.cursor_pos model in
    let start_idx, end_idx = if anchor <= pos then anchor, pos else pos, anchor in
    if start_idx <> end_idx
    then (
      let input = Model.input_line model in
      let len = end_idx - start_idx in
      if start_idx >= 0 && start_idx + len <= String.length input
      then (
        let text = String.sub input ~pos:start_idx ~len in
        kill text;
        delete_range model ~first:start_idx ~last:end_idx));
    Model.clear_selection model
;;

let kill_to_eol (model : Model.t) =
  let s = Model.input_line model in
  let pos = Model.cursor_pos model in
  let _, line_end = Controller_shared.line_bounds s pos in
  (* Include newline char, if any *)
  let line_end =
    if line_end < String.length s && Char.equal (String.get s line_end) '\n'
    then line_end + 1
    else line_end
  in
  let killed = String.sub s ~pos ~len:(line_end - pos) in
  kill killed;
  delete_range model ~first:pos ~last:line_end
;;

let kill_to_bol (model : Model.t) =
  let s = Model.input_line model in
  let pos = Model.cursor_pos model in
  let line_start, _ = Controller_shared.line_bounds s pos in
  let killed = String.sub s ~pos:line_start ~len:(pos - line_start) in
  kill killed;
  delete_range model ~first:line_start ~last:pos
;;

let kill_prev_word (model : Model.t) =
  let s = Model.input_line model in
  let pos = Model.cursor_pos model in
  if pos = 0
  then ()
  else (
    let rec skip_space i =
      if i > 0 && Char.is_whitespace (String.get s (i - 1)) then skip_space (i - 1) else i
    in
    let rec skip_word i =
      if i > 0 && not (Char.is_whitespace (String.get s (i - 1)))
      then skip_word (i - 1)
      else i
    in
    let end_pos = pos in
    let start = skip_word (skip_space pos) in
    let killed = String.sub s ~pos:start ~len:(end_pos - start) in
    kill killed;
    delete_range model ~first:start ~last:end_pos)
;;

(* -------------------------------------------------------------------- *)
(* Vertical cursor movement helpers (Ctrl-Up / Ctrl-Down)                *)
(* -------------------------------------------------------------------- *)

let move_cursor_vertically (model : Model.t) ~dir =
  (* dir = -1 for up, +1 for down *)
  let s = Model.input_line model in
  let pos = Model.cursor_pos model in
  let len = String.length s in
  let line_start, line_end = Controller_shared.line_bounds s pos in
  (* Determine current column (in bytes) *)
  let col = pos - line_start in
  let target_line_start, target_line_end =
    if dir < 0
    then (* move up *)
      if line_start = 0
      then line_start, line_end
      else (
        (* find previous line bounds *)
        let rec find_prev_start i =
          if i <= 0
          then 0
          else if Char.equal (String.get s (i - 1)) '\n'
          then i
          else find_prev_start (i - 1)
        in
        let prev_end =
          line_start - 1
          (* the '\n' of prev line *)
        in
        let prev_start = find_prev_start prev_end in
        prev_start, prev_end)
    else if
      (* move down *)
      line_end >= len
    then line_start, line_end
    else (
      let next_start = line_end + 1 in
      let rec find_next_end i =
        if i >= len
        then len
        else if Char.equal (String.get s i) '\n'
        then i
        else find_next_end (i + 1)
      in
      let next_end = find_next_end next_start in
      next_start, next_end)
  in
  if target_line_start = line_start && target_line_end = line_end
  then () (* can't move *)
  else (
    let new_col = Int.min col (target_line_end - target_line_start) in
    Model.set_cursor_pos model (target_line_start + new_col))
;;

(* -------------------------------------------------------------------- *)
(* Duplicate line (Meta+Shift+Up / Down)                                 *)
(* -------------------------------------------------------------------- *)

let duplicate_line (model : Model.t) ~below =
  let s = Model.input_line model in
  let pos = Model.cursor_pos model in
  let line_start, line_end = Controller_shared.line_bounds s pos in
  let line_str = String.sub s ~pos:line_start ~len:(line_end - line_start) in
  let with_newline =
    if line_end >= String.length s || Char.equal (String.get s line_end) '\n'
    then line_str ^ "\n"
    else line_str
  in
  if below
  then (
    (* insert after the current line *)
    let insert_pos =
      if line_end >= String.length s then String.length s else line_end + 1
    in
    let before = String.sub s ~pos:0 ~len:insert_pos in
    let after = String.sub s ~pos:insert_pos ~len:(String.length s - insert_pos) in
    Model.set_input_line model (before ^ with_newline ^ after);
    (* keep cursor on original line *)
    Model.set_cursor_pos model pos)
  else (
    (* insert before current line *)
    let before = String.sub s ~pos:0 ~len:line_start in
    let after = String.sub s ~pos:line_start ~len:(String.length s - line_start) in
    Model.set_input_line model (before ^ with_newline ^ after);
    (* cursor shifts by line length + newline *)
    Model.set_cursor_pos model (pos + String.length with_newline))
;;

(* -------------------------------------------------------------------- *)
(* Indent / Unindent current line (Meta+Shift+Right / Left)              *)
(* -------------------------------------------------------------------- *)

let indent_line (model : Model.t) ~amount =
  let s = Model.input_line model in
  let pos = Model.cursor_pos model in
  let line_start, _ = Controller_shared.line_bounds s pos in
  if amount > 0
  then (
    let before = String.sub s ~pos:0 ~len:line_start in
    let after = String.sub s ~pos:line_start ~len:(String.length s - line_start) in
    let indent = String.make amount ' ' in
    Model.set_input_line model (before ^ indent ^ after);
    Model.set_cursor_pos model (pos + amount))
  else (
    let max_remove = Int.min (-amount) (String.length s - line_start) in
    (* Count up to [max_remove] consecutive spaces starting at [line_start]. *)
    let rec count k =
      if k >= max_remove
      then k
      else if line_start + k >= String.length s
      then k
      else if Char.equal (String.get s (line_start + k)) ' '
      then count (k + 1)
      else k
    in
    let remove = count 0 in
    if remove <= 0
    then ()
    else (
      delete_range model ~first:line_start ~last:(line_start + remove);
      (* adjust cursor: cannot move before line_start *)
      Model.set_cursor_pos model (Int.max line_start (pos - remove))))
;;

(* -------------------------------------------------------------------- *)
(* Scrolling helpers                                                    *)
(* -------------------------------------------------------------------- *)

let scroll_by_lines (model : Model.t) ~term delta =
  let _, screen_h = Notty_eio.Term.size term in
  (* Number of lines occupied by the multiline input editor. *)
  let input_height =
    match String.split_lines (Model.input_line model) with
    | [] -> 1
    | ls -> List.length ls
  in
  let history_h = Int.max 1 (screen_h - input_height - 2) in
  Scroll_box.scroll_by (Model.scroll_box model) ~height:history_h delta
;;

let page_size ~term (model : Model.t) =
  let _, screen_h = Notty_eio.Term.size term in
  let input_height =
    match String.split_lines (Model.input_line model) with
    | [] -> 1
    | ls -> List.length ls
  in
  screen_h - input_height
;;

let skip_word s len j =
  let rec skip_word i =
    if i >= len
    then len
    else if Char.is_whitespace (String.get s i)
    then skip_space i
    else skip_word (i + 1)
  and skip_space i =
    if i >= len
    then len
    else if Char.is_whitespace (String.get s i)
    then skip_space (i + 1)
    else i
  in
  skip_word j
;;

let skip_space s j =
  let rec skip_space i =
    if i <= 0
    then 0
    else if Char.is_whitespace (String.get s (i - 1))
    then skip_space (i - 1)
    else skip_word (i - 1)
  and skip_word i =
    if i <= 0
    then 0
    else if not (Char.is_whitespace (String.get s (i - 1)))
    then skip_word (i - 1)
    else i
  in
  skip_space j
;;

(* -------------------------------------------------------------------- *)
(* Main dispatcher – Insert-mode implementation                              *)
(* -------------------------------------------------------------------- *)

let handle_key_insert ~(model : Model.t) ~term (ev : Notty.Unescape.event) : reaction =
  match ev with
  (* ----------------------------------------------------------------- *)
  (*  Ctrl-A / Ctrl-E fallback (terminals that don't set [`Ctrl] flag)  *)
  | `Key (`ASCII '\001', _) ->
    (* Ctrl-A fallback: beginning of line *)
    let s = Model.input_line model in
    let pos = Model.cursor_pos model in
    let rec find_bol i =
      if i <= 0
      then 0
      else if Char.equal (String.get s (i - 1)) '\n'
      then i
      else find_bol (i - 1)
    in
    Model.set_cursor_pos model (find_bol pos);
    Redraw
  (* Uchar Ctrl-A fallback omitted to reduce dependency issues. *)
  | `Key (`ASCII '\005', _) ->
    (* Ctrl-E fallback: end of line *)
    let s = Model.input_line model in
    let pos = Model.cursor_pos model in
    let len = String.length s in
    let rec find_eol i =
      if i >= len
      then len
      else if Char.equal (String.get s i) '\n'
      then i
      else find_eol (i + 1)
    in
    Model.set_cursor_pos model (find_eol pos);
    Redraw
    (* Uchar Ctrl-E fallback omitted. *)
  | `Key (`ASCII c, mods) when List.is_empty mods && Char.to_int c >= 0x20 ->
    (* Printable ASCII insertion.  Ignore control chars so we can use them
       for shortcuts on terminals that don’t report modifier state. *)
    append_char model c;
    Redraw
  | `Key (`Backspace, mods) when List.is_empty mods ->
    backspace model;
    Redraw
  (* ----------------------------------------------------------------- *)
  (*  Quality-of-life kill/ yank / redraw shortcuts                     *)
  | `Key (`ASCII ('k' | 'K'), [ `Ctrl ]) ->
    kill_to_eol model;
    Redraw
  | `Key (`ASCII ('u' | 'U'), [ `Ctrl ]) ->
    kill_to_bol model;
    Redraw
  | `Key (`ASCII ('w' | 'W'), [ `Ctrl ]) ->
    kill_prev_word model;
    Redraw
  | `Key (`Backspace, mods) when List.mem mods `Meta ~equal:Poly.equal ->
    kill_prev_word model;
    Redraw
  | `Key (`ASCII ('y' | 'Y'), [ `Ctrl ]) ->
    yank model;
    Redraw
  (* ----------------------------------------------------------------- *)
  (* Selection toggle (Meta-v)                                          *)
  | `Key (`ASCII ('v' | 'V'), mods) when List.mem mods `Meta ~equal:Poly.equal ->
    (match Model.selection_anchor model with
     | None -> Model.set_selection_anchor model (Model.cursor_pos model)
     | Some _ -> Model.clear_selection model);
    Redraw
  (* Alternate toggle key: Meta-S or U+00DF (ß) often sent for Alt-s *)
  | `Key (`ASCII ('s' | 'S'), mods) when List.mem mods `Meta ~equal:Poly.equal ->
    (match Model.selection_anchor model with
     | None -> Model.set_selection_anchor model (Model.cursor_pos model)
     | Some _ -> Model.clear_selection model);
    Redraw
  | `Key (`Uchar u, _) when UC.to_int u = 0x00DF ->
    (match Model.selection_anchor model with
     | None -> Model.set_selection_anchor model (Model.cursor_pos model)
     | Some _ -> Model.clear_selection model);
    Redraw
  | `Key (`ASCII ('l' | 'L'), [ `Ctrl ]) ->
    (* Ctrl-L – force redraw / recenter *)
    Redraw
  | `Key (`Arrow `Up, mods) when List.is_empty mods ->
    (* let _dh = Model.draft_history model in
    let ptr = Model.draft_history_pos model in
    let cursor_at_bol = !(Model.cursor_pos model) = 0 in
    if !ptr > 0 || cursor_at_bol
    then (
      let dh = Model.draft_history model in
      let ptr = Model.draft_history_pos model in
      if !ptr > 0
      then (
        ptr := !ptr - 1;
        (match List.nth !dh !ptr with
         | None -> ()
         | Some prev ->
           Model.input_line model := prev;
           Model.cursor_pos model := String.length prev);
        (* Clear any selection when recalling history *)
        Model.clear_selection model);
      Redraw)
    else (
      model.auto_follow := false;
      scroll_by_lines model ~term (-1);
      Redraw) *)
    Model.set_auto_follow model false;
    scroll_by_lines model ~term (-1);
    Redraw
  (* ----------------------------------------------------------------- *)
  (*  Cursor vertical move within editor (Ctrl-Up / Ctrl-Down)          *)
  | `Key (`Arrow `Up, mods) when List.mem mods `Ctrl ~equal:Poly.equal ->
    move_cursor_vertically model ~dir:(-1);
    Redraw
  | `Key (`Arrow `Down, mods) when List.mem mods `Ctrl ~equal:Poly.equal ->
    move_cursor_vertically model ~dir:1;
    Redraw
  (* ----------------------------------------------------------------- *)
  (*  Character-wise cursor movement.                                   *)
  (*  NOTE:  These cases must come after the more specific word-move    *)
  (*  cases (Ctrl/Meta + Arrow) so that they don't shadow them.         *)
  | `Key (`Arrow `Left, mods) when List.is_empty mods ->
    let pos = Model.cursor_pos model in
    if pos > 0 then Model.set_cursor_pos model (pos - 1);
    Redraw
  | `Key (`Arrow `Right, mods) when List.is_empty mods ->
    let pos = Model.cursor_pos model in
    let input = Model.input_line model in
    if pos < String.length input then Model.set_cursor_pos model (pos + 1);
    Redraw
  (* ----------------------------------------------------------------- *)
  (* Copy / Cut when selection active                                   *)
  | `Key (`ASCII ('c' | 'C'), [ `Ctrl ]) when selection_active model ->
    copy_selection model;
    Redraw
  | `Key (`ASCII ('x' | 'X'), [ `Ctrl ]) when selection_active model ->
    cut_selection model;
    Redraw
  (* ────────────────────────────────────────────────────────────────── *)
  (*  Word-wise navigation (Ctrl/Meta + ← / →)                         *)
  (*  We deliberately interpret both Ctrl and Meta (Alt) modifiers so  *)
  (*  the feature works even if the terminal maps one modifier or the  *)
  (*  other.                                                           *)
  | `Key (`Arrow `Left, mods)
    when List.exists mods ~f:(fun m -> Poly.equal m `Ctrl || Poly.equal m `Meta)
         && not (List.mem mods `Shift ~equal:Poly.equal) ->
    let pos = Model.cursor_pos model in
    let s = Model.input_line model in
    let len = String.length s in
    let new_pos = if len = 0 then 0 else skip_space s pos in
    Model.set_cursor_pos model new_pos;
    Redraw
  (* Meta-b / Meta-f word-wise navigation (common on macOS terminals)    *)
  | `Key (`ASCII 'b', mods) when List.mem mods `Meta ~equal:Poly.equal ->
    let pos = Model.cursor_pos model in
    let s = Model.input_line model in
    Model.set_cursor_pos model (skip_space s pos);
    Redraw
  | `Key (`ASCII 'f', mods) when List.mem mods `Meta ~equal:Poly.equal ->
    let pos = Model.cursor_pos model in
    let s = Model.input_line model in
    let len = String.length s in
    Model.set_cursor_pos model (skip_word s len pos);
    Redraw
  | `Key (`Arrow `Right, mods)
    when List.exists mods ~f:(fun m -> Poly.equal m `Ctrl || Poly.equal m `Meta)
         && not (List.mem mods `Shift ~equal:Poly.equal) ->
    let pos = Model.cursor_pos model in
    let s = Model.input_line model in
    let len = String.length s in
    let new_pos = if len = 0 then 0 else skip_word s len pos in
    Model.set_cursor_pos model new_pos;
    Redraw
    (* ────────────────────────────────────────────────────────────────── *)
    (*  Beginning / end of line (Ctrl-A / Ctrl-E)                         *)
  | `Key (`ASCII ('a' | 'A'), [ `Ctrl ]) ->
    (* Start of line *)
    let s = Model.input_line model in
    let pos = Model.cursor_pos model in
    let rec find_bol i =
      if i <= 0
      then 0
      else if Char.equal (String.get s (i - 1)) '\n'
      then i
      else find_bol (i - 1)
    in
    Model.set_cursor_pos model (find_bol pos);
    Redraw
  | `Key (`ASCII ('e' | 'E'), [ `Ctrl ]) ->
    (* End of line *)
    let s = Model.input_line model in
    let pos = Model.cursor_pos model in
    let len = String.length s in
    let rec find_eol i =
      if i >= len
      then len
      else if Char.equal (String.get s i) '\n'
      then i
      else find_eol (i + 1)
    in
    Model.set_cursor_pos model (find_eol pos);
    Redraw
  (* ────────────────────────────────────────────────────────────────── *)
  (*  Beginning / end of entire message (Ctrl+Home / Ctrl+End)         *)
  | `Key (`Home, mods) when List.exists mods ~f:(Poly.equal `Ctrl) ->
    Model.set_cursor_pos model 0;
    Redraw
  | `Key (`End, mods) when List.exists mods ~f:(Poly.equal `Ctrl) ->
    let input = Model.input_line model in
    Model.set_cursor_pos model (String.length input);
    Redraw
  | `Key (`Arrow `Down, mods) when List.is_empty mods ->
    (* let dh = Model.draft_history model in
    let ptr = Model.draft_history_pos model in
    let cursor_at_eol =
      let pos = !(Model.cursor_pos model) in
      pos = String.length !(Model.input_line model)
    in
    if !ptr < List.length !dh || cursor_at_eol
    then (
      let dh = Model.draft_history model in
      let ptr = Model.draft_history_pos model in
      if !ptr < List.length !dh
      then (
        ptr := !ptr + 1;
        let new_text =
          if !ptr = List.length !dh then "" else Option.value_exn (List.nth !dh !ptr)
        in
        Model.input_line model := new_text;
        Model.cursor_pos model := String.length new_text;
        Model.clear_selection model);
      Redraw)
    else (
      model.auto_follow := false;
      scroll_by_lines model ~term 1;
      Redraw) *)
    Model.set_auto_follow model false;
    scroll_by_lines model ~term 1;
    Redraw
  (* ----------------------------------------------------------------- *)
  (* Duplicate current line (Meta+Shift+Up / Meta+Shift+Down)           *)
  | `Key (`Arrow `Up, mods)
    when List.mem mods `Meta ~equal:Poly.equal && List.mem mods `Shift ~equal:Poly.equal
    ->
    duplicate_line model ~below:false;
    Redraw
  | `Key (`Arrow `Down, mods)
    when List.mem mods `Meta ~equal:Poly.equal && List.mem mods `Shift ~equal:Poly.equal
    ->
    duplicate_line model ~below:true;
    Redraw
  (* Indent / Unindent (Meta+Shift+Right / Meta+Shift+Left) *)
  | `Key (`Arrow `Right, mods)
    when List.mem mods `Meta ~equal:Poly.equal && List.mem mods `Shift ~equal:Poly.equal
    ->
    indent_line model ~amount:2;
    Redraw
  | `Key (`Arrow `Left, mods)
    when List.mem mods `Meta ~equal:Poly.equal && List.mem mods `Shift ~equal:Poly.equal
    ->
    indent_line model ~amount:(-2);
    Redraw
  | `Key (`Page `Up, _) ->
    Model.set_auto_follow model false;
    let ps = page_size ~term model in
    scroll_by_lines model ~term (-ps);
    Redraw
  | `Key (`Page `Down, _) ->
    Model.set_auto_follow model false;
    let ps = page_size ~term model in
    scroll_by_lines model ~term ps;
    Redraw
  | `Key (`Home, _) ->
    Model.set_auto_follow model false;
    Scroll_box.scroll_to_top (Model.scroll_box model);
    Redraw
  | `Key (`End, _) ->
    Model.set_auto_follow model true;
    let _, screen_h = Notty_eio.Term.size term in
    let input_h =
      match String.split_lines (Model.input_line model) with
      | [] -> 1
      | ls -> List.length ls
    in
    Scroll_box.scroll_to_bottom (Model.scroll_box model) ~height:(screen_h - input_h);
    Redraw
  | `Key (`Enter, []) ->
    (* Literal newline inside the input buffer *)
    append_char model '\n';
    Redraw
  (* High-level actions -------------------------------------------------- *)
  | `Key (`Enter, mods) when List.mem mods `Meta ~equal:Poly.equal ->
    (* Submit user input for processing *)
    Submit_input
  | `Key (`Escape, _) -> Cancel_or_quit
  | `Key (`ASCII 'C', [ `Ctrl ]) | `Key (`ASCII 'q', _) -> Quit
  | _ -> Unhandled
;;

(* -------------------------------------------------------------------- *)
(*  Top-level dispatcher that selects the keymap by [Model.mode].         *)
(* -------------------------------------------------------------------- *)

let handle_key ~(model : Model.t) ~term (ev : Notty.Unescape.event) : reaction =
  match Model.mode model with
  | Insert ->
    (match ev with
     (* Switch to Normal mode on bare ESC. *)
     | `Key (`Escape, mods) when List.is_empty mods ->
       Model.set_mode model Normal;
       Redraw
     | `Key (`ASCII 'r', mods) when List.equal Poly.( = ) mods [ `Ctrl ] ->
       (* Toggle Raw-XML draft mode in Insert state. *)
       let new_mode =
         match Model.draft_mode model with
         | Model.Plain -> Model.Raw_xml
         | Model.Raw_xml -> Model.Plain
       in
       Model.set_draft_mode model new_mode;
       Redraw
     | _ -> handle_key_insert ~model ~term ev)
  | Normal ->
    (match ev with
     | `Key (`ASCII 'i', mods) when List.is_empty mods ->
       Model.set_mode model Insert;
       Redraw
     | `Key (`Escape, _) -> Cancel_or_quit
     | _ -> Controller_normal.handle_key_normal ~model ~term ev)
  | Cmdline -> Controller_cmdline.handle_key_cmdline ~model ~term ev
;;
