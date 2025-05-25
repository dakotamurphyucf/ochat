open Core
module UC = Stdlib.Uchar
module Scroll_box = Notty_scroll_box

(** Outcome of handling a keyboard event. *)
type reaction =
  | Redraw (** Model was modified – caller should redraw.      *)
  | Submit_input (** User pressed Meta+Enter – send current input.   *)
  | Cancel_or_quit (** ESC – cancel request if running, else quit.     *)
  | Quit (** Immediate quit (Ctrl-C / q).                    *)
  | Unhandled (** Event not recognised by this layer.            *)

(* -------------------------------------------------------------------- *)
(* Helper – update the input_line ref while keeping it UTF-8 safe.       *)
(* For the purpose of the demo we take the simple approach of slicing   *)
(* bytes which works as long as the terminal only inputs ASCII.         *)
(* -------------------------------------------------------------------- *)

let append_char (model : Model.t) c =
  let input_ref = Model.input_line model in
  let pos_ref = Model.cursor_pos model in
  (* Reset history browsing pointer when user edits *)
  let dh_len = List.length !(Model.draft_history model) in
  let ptr_ref = Model.draft_history_pos model in
  if !ptr_ref <> dh_len then ptr_ref := dh_len;
  let s = !input_ref in
  let pos = !pos_ref in
  let before = String.sub s ~pos:0 ~len:pos in
  let after = String.sub s ~pos ~len:(String.length s - pos) in
  input_ref := before ^ String.of_char c ^ after;
  pos_ref := pos + 1
;;

let backspace (model : Model.t) =
  let input_ref = Model.input_line model in
  let pos_ref = Model.cursor_pos model in
  let dh_len = List.length !(Model.draft_history model) in
  let ptr_ref = Model.draft_history_pos model in
  if !ptr_ref <> dh_len then ptr_ref := dh_len;
  let pos = !pos_ref in
  let s = !input_ref in
  if pos > 0
  then (
    let before = String.sub s ~pos:0 ~len:(pos - 1) in
    let after = String.sub s ~pos ~len:(String.length s - pos) in
    input_ref := before ^ after;
    pos_ref := pos - 1)
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
    let input_ref = Model.input_line model in
    let pos_ref = Model.cursor_pos model in
    let s = !input_ref in
    let pos = !pos_ref in
    let before = String.sub s ~pos:0 ~len:pos in
    let after = String.sub s ~pos ~len:(String.length s - pos) in
    input_ref := before ^ !kill_buffer ^ after;
    pos_ref := pos + String.length !kill_buffer)
;;

(* -------------------------------------------------------------------- *)
(* Line helpers                                                          *)
(* -------------------------------------------------------------------- *)

let line_bounds s pos =
  (* returns (start_index, end_index_exclusive) of the current line that
     contains [pos].  Works on byte indices. *)
  let len = String.length s in
  let rec find_start i =
    if i <= 0
    then 0
    else if Char.equal (String.get s (i - 1)) '\n'
    then i
    else find_start (i - 1)
  in
  let rec find_end i =
    if i >= len
    then len
    else if Char.equal (String.get s i) '\n'
    then i
    else find_end (i + 1)
  in
  let start_idx = find_start pos in
  let end_idx = find_end pos in
  start_idx, end_idx
;;

let delete_range (model : Model.t) ~first ~last =
  (* Remove [first,last) from input line. Assumes indices are valid. *)
  if first >= last
  then ()
  else (
    let input_ref = Model.input_line model in
    let pos_ref = Model.cursor_pos model in
    let s = !input_ref in
    let before = String.sub s ~pos:0 ~len:first in
    let after = String.sub s ~pos:last ~len:(String.length s - last) in
    input_ref := before ^ after;
    (* cursor moves to [first] *)
    pos_ref := first)
;;

(* -------------------------------------------------------------------- *)
(* Selection helpers (depends on [delete_range])                         *)
(* -------------------------------------------------------------------- *)

let selection_active (model : Model.t) = Model.selection_active model

let copy_selection (model : Model.t) =
  match !(Model.selection_anchor model) with
  | None -> ()
  | Some anchor ->
    let pos = !(Model.cursor_pos model) in
    let start_idx, end_idx = if anchor <= pos then anchor, pos else pos, anchor in
    if start_idx <> end_idx
    then (
      let input = !(Model.input_line model) in
      let len = end_idx - start_idx in
      if start_idx >= 0 && start_idx + len <= String.length input
      then (
        let text = String.sub input ~pos:start_idx ~len in
        kill text));
    Model.clear_selection model
;;

let cut_selection (model : Model.t) =
  match !(Model.selection_anchor model) with
  | None -> ()
  | Some anchor ->
    let pos = !(Model.cursor_pos model) in
    let start_idx, end_idx = if anchor <= pos then anchor, pos else pos, anchor in
    if start_idx <> end_idx
    then (
      let input = !(Model.input_line model) in
      let len = end_idx - start_idx in
      if start_idx >= 0 && start_idx + len <= String.length input
      then (
        let text = String.sub input ~pos:start_idx ~len in
        kill text;
        delete_range model ~first:start_idx ~last:end_idx));
    Model.clear_selection model
;;

let kill_to_eol (model : Model.t) =
  let input_ref = Model.input_line model in
  let pos_ref = Model.cursor_pos model in
  let s = !input_ref in
  let pos = !pos_ref in
  let _, line_end = line_bounds s pos in
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
  let input_ref = Model.input_line model in
  let pos_ref = Model.cursor_pos model in
  let s = !input_ref in
  let pos = !pos_ref in
  let line_start, _ = line_bounds s pos in
  let killed = String.sub s ~pos:line_start ~len:(pos - line_start) in
  kill killed;
  delete_range model ~first:line_start ~last:pos
;;

let kill_prev_word (model : Model.t) =
  let input_ref = Model.input_line model in
  let pos_ref = Model.cursor_pos model in
  let s = !input_ref in
  let pos = !pos_ref in
  if pos = 0
  then ()
  else (
    let dh_len = List.length !(Model.draft_history model) in
    let ptr_ref = Model.draft_history_pos model in
    if !ptr_ref <> dh_len then ptr_ref := dh_len;
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
  let input_ref = Model.input_line model in
  let pos_ref = Model.cursor_pos model in
  let s = !input_ref in
  let len = String.length s in
  let pos = !pos_ref in
  let line_start, line_end = line_bounds s pos in
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
    pos_ref := target_line_start + new_col)
;;

(* -------------------------------------------------------------------- *)
(* Duplicate line (Meta+Shift+Up / Down)                                 *)
(* -------------------------------------------------------------------- *)

let duplicate_line (model : Model.t) ~below =
  let input_ref = Model.input_line model in
  let pos_ref = Model.cursor_pos model in
  let s = !input_ref in
  let pos = !pos_ref in
  let line_start, line_end = line_bounds s pos in
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
    input_ref := before ^ with_newline ^ after;
    (* keep cursor on original line *)
    pos_ref := pos)
  else (
    (* insert before current line *)
    let before = String.sub s ~pos:0 ~len:line_start in
    let after = String.sub s ~pos:line_start ~len:(String.length s - line_start) in
    input_ref := before ^ with_newline ^ after;
    (* cursor shifts by line length + newline *)
    pos_ref := pos + String.length with_newline)
;;

(* -------------------------------------------------------------------- *)
(* Indent / Unindent current line (Meta+Shift+Right / Left)              *)
(* -------------------------------------------------------------------- *)

let indent_line (model : Model.t) ~amount =
  let input_ref = Model.input_line model in
  let pos_ref = Model.cursor_pos model in
  let s = !input_ref in
  let pos = !pos_ref in
  let line_start, _ = line_bounds s pos in
  if amount > 0
  then (
    let before = String.sub s ~pos:0 ~len:line_start in
    let after = String.sub s ~pos:line_start ~len:(String.length s - line_start) in
    let indent = String.make amount ' ' in
    input_ref := before ^ indent ^ after;
    pos_ref := pos + amount)
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
      pos_ref := Int.max line_start (pos - remove)))
;;

(* -------------------------------------------------------------------- *)
(* Scrolling helpers                                                    *)
(* -------------------------------------------------------------------- *)

let scroll_by_lines (model : Model.t) ~term delta =
  let _, screen_h = Notty_eio.Term.size term in
  (* Number of lines occupied by the multiline input editor. *)
  let input_height =
    match String.split_lines !(Model.input_line model) with
    | [] -> 1
    | ls -> List.length ls
  in
  let history_h = Int.max 1 (screen_h - input_height) in
  Scroll_box.scroll_by model.scroll_box ~height:history_h delta
;;

let page_size ~term (model : Model.t) =
  let _, screen_h = Notty_eio.Term.size term in
  let input_height =
    match String.split_lines !(Model.input_line model) with
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
(* Main dispatcher                                                      *)
(* -------------------------------------------------------------------- *)

let handle_key ~(model : Model.t) ~term (ev : Notty.Unescape.event) : reaction =
  match ev with
  (* ----------------------------------------------------------------- *)
  (*  Ctrl-A / Ctrl-E fallback (terminals that don't set [`Ctrl] flag)  *)
  | `Key (`ASCII '\001', _) ->
    (* Ctrl-A fallback: beginning of line *)
    let input_ref = Model.input_line model in
    let pos_ref = Model.cursor_pos model in
    let s = !input_ref in
    let rec find_bol i =
      if i <= 0
      then 0
      else if Char.equal (String.get s (i - 1)) '\n'
      then i
      else find_bol (i - 1)
    in
    pos_ref := find_bol !pos_ref;
    Redraw
  (* Uchar Ctrl-A fallback omitted to reduce dependency issues. *)
  | `Key (`ASCII '\005', _) ->
    (* Ctrl-E fallback: end of line *)
    let input_ref = Model.input_line model in
    let pos_ref = Model.cursor_pos model in
    let s = !input_ref in
    let len = String.length s in
    let rec find_eol i =
      if i >= len
      then len
      else if Char.equal (String.get s i) '\n'
      then i
      else find_eol (i + 1)
    in
    pos_ref := find_eol !pos_ref;
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
    (match !(Model.selection_anchor model) with
     | None -> Model.set_selection_anchor model !(Model.cursor_pos model)
     | Some _ -> Model.clear_selection model);
    Redraw
  (* Alternate toggle key: Meta-S or U+00DF (ß) often sent for Alt-s *)
  | `Key (`ASCII ('s' | 'S'), mods) when List.mem mods `Meta ~equal:Poly.equal ->
    (match !(Model.selection_anchor model) with
     | None -> Model.set_selection_anchor model !(Model.cursor_pos model)
     | Some _ -> Model.clear_selection model);
    Redraw
  | `Key (`Uchar u, _) when UC.to_int u = 0x00DF ->
    (match !(Model.selection_anchor model) with
     | None -> Model.set_selection_anchor model !(Model.cursor_pos model)
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
    model.auto_follow := false;
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
    let pos_ref = Model.cursor_pos model in
    if !pos_ref > 0 then pos_ref := !pos_ref - 1;
    Redraw
  | `Key (`Arrow `Right, mods) when List.is_empty mods ->
    let pos_ref = Model.cursor_pos model in
    let input_ref = Model.input_line model in
    if !pos_ref < String.length !input_ref then pos_ref := !pos_ref + 1;
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
    let pos_ref = Model.cursor_pos model in
    let input_ref = Model.input_line model in
    let s = !input_ref in
    let len = String.length s in
    let new_pos = if len = 0 then 0 else skip_space s !pos_ref in
    pos_ref := new_pos;
    Redraw
  (* Meta-b / Meta-f word-wise navigation (common on macOS terminals)    *)
  | `Key (`ASCII 'b', mods) when List.mem mods `Meta ~equal:Poly.equal ->
    let pos_ref = Model.cursor_pos model in
    let input_ref = Model.input_line model in
    let s = !input_ref in
    pos_ref := skip_space s !pos_ref;
    Redraw
  | `Key (`ASCII 'f', mods) when List.mem mods `Meta ~equal:Poly.equal ->
    let pos_ref = Model.cursor_pos model in
    let input_ref = Model.input_line model in
    let s = !input_ref in
    let len = String.length s in
    pos_ref := skip_word s len !pos_ref;
    Redraw
  | `Key (`Arrow `Right, mods)
    when List.exists mods ~f:(fun m -> Poly.equal m `Ctrl || Poly.equal m `Meta)
         && not (List.mem mods `Shift ~equal:Poly.equal) ->
    let pos_ref = Model.cursor_pos model in
    let input_ref = Model.input_line model in
    let s = !input_ref in
    let len = String.length s in
    let new_pos = if len = 0 then 0 else skip_word s len !pos_ref in
    pos_ref := new_pos;
    Redraw
    (* ────────────────────────────────────────────────────────────────── *)
    (*  Beginning / end of line (Ctrl-A / Ctrl-E)                         *)
  | `Key (`ASCII ('a' | 'A'), [ `Ctrl ]) ->
    (* Start of line *)
    let input_ref = Model.input_line model in
    let pos_ref = Model.cursor_pos model in
    let s = !input_ref in
    let rec find_bol i =
      if i <= 0
      then 0
      else if Char.equal (String.get s (i - 1)) '\n'
      then i
      else find_bol (i - 1)
    in
    pos_ref := find_bol !pos_ref;
    Redraw
  | `Key (`ASCII ('e' | 'E'), [ `Ctrl ]) ->
    (* End of line *)
    let input_ref = Model.input_line model in
    let pos_ref = Model.cursor_pos model in
    let s = !input_ref in
    let len = String.length s in
    let rec find_eol i =
      if i >= len
      then len
      else if Char.equal (String.get s i) '\n'
      then i
      else find_eol (i + 1)
    in
    pos_ref := find_eol !pos_ref;
    Redraw
  (* ────────────────────────────────────────────────────────────────── *)
  (*  Beginning / end of entire message (Ctrl+Home / Ctrl+End)         *)
  | `Key (`Home, mods) when List.exists mods ~f:(Poly.equal `Ctrl) ->
    let pos_ref = Model.cursor_pos model in
    pos_ref := 0;
    Redraw
  | `Key (`End, mods) when List.exists mods ~f:(Poly.equal `Ctrl) ->
    let input_ref = Model.input_line model in
    let pos_ref = Model.cursor_pos model in
    pos_ref := String.length !input_ref;
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
    model.auto_follow := false;
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
    model.auto_follow := false;
    let ps = page_size ~term model in
    scroll_by_lines model ~term (-ps);
    Redraw
  | `Key (`Page `Down, _) ->
    model.auto_follow := false;
    let ps = page_size ~term model in
    scroll_by_lines model ~term ps;
    Redraw
  | `Key (`Home, _) ->
    model.auto_follow := false;
    Scroll_box.scroll_to_top model.scroll_box;
    Redraw
  | `Key (`End, _) ->
    model.auto_follow := true;
    let _, screen_h = Notty_eio.Term.size term in
    let input_h =
      match String.split_lines !(Model.input_line model) with
      | [] -> 1
      | ls -> List.length ls
    in
    Scroll_box.scroll_to_bottom model.scroll_box ~height:(screen_h - input_h);
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
