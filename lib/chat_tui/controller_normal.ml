(** Normal-mode key handling for the Ochat terminal UI.

    This module implements a Vim-inspired Normal mode for the prompt editor,
    plus app-specific history navigation.

    Notable semantics:
    - ArrowUp/ArrowDown scroll the conversation history (trackpad-friendly).
    - gg/G are canonical Vim motions on the *input buffer* (not history).
    - / and ? search message history; n/N repeat history search.
    - Supports counts (e.g. 10j, 3w, 5gg, 2G).
    - Supports operator-pending y/d/c with motions {w, b, e, 0, $, ^} and
      linewise yy/dd/cc.
    - Adds character-wise Visual mode (Vim-style) via `v` using
      Model.selection_anchor:
        * `v` toggles selection
        * with selection active: `y` yanks, `d` deletes, `c` changes
    - Adds Vim find-on-line:
        * f{char}, F{char}, t{char}, T{char}
        * ; repeat last find, , repeat in opposite direction
        * works with operators: df{c}, ct{c}, yF{c}, etc.

    All byte indices refer to the UTF-8 encoded {!Model.input_line}. *)

open Core
module UC = Stdlib.Uchar
module Scroll_box = Notty_scroll_box
open Controller_types

(* -------------------------------------------------------------------- *)
(* Cursor movement helpers (visual vertical movement via Input_display)  *)
(* -------------------------------------------------------------------- *)

let move_cursor_vertically model ~term ~dir =
  let box_width, _ = Notty_eio.Term.size term in
  match Input_display.cursor_pos_after_vertical_move ~box_width ~model ~dir with
  | None -> ()
  | Some new_pos -> Model.set_cursor_pos model new_pos
;;

(* -------------------------------------------------------------------- *)
(* Word / line navigation helpers                                        *)
(* -------------------------------------------------------------------- *)

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

let first_non_blank_in_line s pos =
  let line_start, line_end = Controller_shared.line_bounds s pos in
  let rec loop i =
    if i >= line_end
    then line_end
    else if Char.is_whitespace (String.get s i)
    then loop (i + 1)
    else i
  in
  loop line_start
;;

let end_of_word s pos =
  let len = String.length s in
  if len = 0
  then 0
  else (
    let pos = Int.max 0 (Int.min pos (len - 1)) in
    let is_space i = i < len && Char.is_whitespace (String.get s i) in
    let is_word i = i < len && not (Char.is_whitespace (String.get s i)) in
    (* Vim-ish 'e':
       - if on whitespace, go to start of next word
       - then go to last char of that word *)
    let rec skip_spaces i =
      if i >= len then len else if is_space i then skip_spaces (i + 1) else i
    in
    let rec skip_word i =
      if i >= len then len else if is_word i then skip_word (i + 1) else i
    in
    let start = if is_space pos then skip_spaces pos else pos in
    if start >= len
    then len
    else (
      let after = skip_word start in
      Int.max start (after - 1)))
;;

(* -------------------------------------------------------------------- *)
(* gg/G helpers (canonical Vim on input buffer)                          *)
(* -------------------------------------------------------------------- *)

let goto_line_start (model : Model.t) ~(line_1_based : int) =
  let s = Model.input_line model in
  let target = Int.max 1 line_1_based in
  let rec scan i line =
    if line >= target
    then i
    else (
      match String.index_from s i '\n' with
      | None -> String.length s
      | Some j ->
        let next = Int.min (String.length s) (j + 1) in
        scan next (line + 1))
  in
  Model.set_cursor_pos model (scan 0 1)
;;

let goto_last_line_start (model : Model.t) =
  let s = Model.input_line model in
  match String.rindex s '\n' with
  | None -> Model.set_cursor_pos model 0
  | Some i -> Model.set_cursor_pos model (Int.min (String.length s) (i + 1))
;;

(* -------------------------------------------------------------------- *)
(* History scrolling helpers                                             *)
(* -------------------------------------------------------------------- *)

let scroll_by_lines (model : Model.t) ~term delta =
  let screen_w, screen_h = Notty_eio.Term.size term in
  let layout = Chat_page_layout.compute ~screen_w ~screen_h ~model in
  let scroll_height = layout.scroll_height in
  Scroll_box.scroll_by (Model.scroll_box model) ~height:scroll_height delta;
  if
    Scroll_box.max_scroll (Model.scroll_box model) ~height:scroll_height
    = Scroll_box.scroll (Model.scroll_box model)
  then Model.set_auto_follow model true
;;

let page_size ~term (model : Model.t) =
  let screen_w, screen_h = Notty_eio.Term.size term in
  let layout = Chat_page_layout.compute ~screen_w ~screen_h ~model in
  layout.scroll_height
;;

(* -------------------------------------------------------------------- *)
(* Simple text editing helpers (Normal mode)                             *)
(* -------------------------------------------------------------------- *)

let insert_text_at (model : Model.t) ~pos text =
  if String.is_empty text
  then ()
  else (
    let s = Model.input_line model in
    let pos = Int.min (String.length s) (Int.max 0 pos) in
    let before = String.sub s ~pos:0 ~len:pos in
    let after = String.sub s ~pos ~len:(String.length s - pos) in
    Model.set_input_line model (before ^ text ^ after);
    Model.set_cursor_pos model (pos + String.length text))
;;

let delete_range (model : Model.t) ~first ~last =
  let s = Model.input_line model in
  let len = String.length s in
  let first = Int.max 0 (Int.min len first) in
  let last = Int.max 0 (Int.min len last) in
  if first >= last
  then ()
  else (
    let before = String.sub s ~pos:0 ~len:first in
    let after = String.sub s ~pos:last ~len:(len - last) in
    let new_s = before ^ after in
    Model.set_input_line model new_s;
    Model.set_cursor_pos model (Int.min first (String.length new_s)))
;;

let yank_current_line model =
  let s = Model.input_line model in
  let pos = Model.cursor_pos model in
  let line_start, line_end = Controller_shared.line_bounds s pos in
  let yank_end =
    if line_end < String.length s && Char.equal (String.get s line_end) '\n'
    then line_end + 1
    else line_end
  in
  Controller_register.set (String.sub s ~pos:line_start ~len:(yank_end - line_start))
;;

let delete_current_line model =
  let s = Model.input_line model in
  let pos = Model.cursor_pos model in
  let line_start, line_end = Controller_shared.line_bounds s pos in
  let del_end =
    if line_end < String.length s && Char.equal (String.get s line_end) '\n'
    then line_end + 1
    else line_end
  in
  Controller_register.set (String.sub s ~pos:line_start ~len:(del_end - line_start));
  delete_range model ~first:line_start ~last:del_end
;;

let change_current_line model =
  (* Vim-ish cc: delete line, enter insert at line start *)
  delete_current_line model;
  Model.set_mode model Model.Insert
;;

(* -------------------------------------------------------------------- *)
(* Visual selection helpers (character-wise Visual mode via `v`)          *)
(* -------------------------------------------------------------------- *)

let selection_range (model : Model.t) : (int * int) option =
  match Model.selection_anchor model with
  | None -> None
  | Some anchor ->
    let cur = Model.cursor_pos model in
    let a, b = if anchor <= cur then anchor, cur else cur, anchor in
    if a = b then None else Some (a, b)
;;

let yank_selection (model : Model.t) : unit =
  match selection_range model with
  | None -> Model.clear_selection model
  | Some (a, b) ->
    let s = Model.input_line model in
    let len = String.length s in
    let a = Int.max 0 (Int.min len a) in
    let b = Int.max 0 (Int.min len b) in
    if a < b then Controller_register.set (String.sub s ~pos:a ~len:(b - a));
    Model.clear_selection model
;;

let delete_selection (model : Model.t) : unit =
  match selection_range model with
  | None -> Model.clear_selection model
  | Some (a, b) ->
    let s = Model.input_line model in
    let len = String.length s in
    let a = Int.max 0 (Int.min len a) in
    let b = Int.max 0 (Int.min len b) in
    if a < b
    then (
      Controller_register.set (String.sub s ~pos:a ~len:(b - a));
      delete_range model ~first:a ~last:b);
    Model.clear_selection model
;;

(* -------------------------------------------------------------------- *)
(* Operator / motion state machine                                       *)
(* -------------------------------------------------------------------- *)

type op =
  | Delete
  | Change
  | Yank

type find_dir =
  | Forward
  | Backward

type find_spec =
  { ch : char
  ; dir : find_dir
  ; till : bool
  }

type find_prefix =
  { dir : find_dir
  ; till : bool
  ; op : op option
  }

type pending =
  | None_pending
  | Pending_g
  | Pending_op of op
  | Pending_find_prefix of find_prefix

type state =
  { pending : pending
  ; count : int option
  }

let st : state ref = ref { pending = None_pending; count = None }
let clear_state () = st := { pending = None_pending; count = None }
let last_find : find_spec option ref = ref None

let push_digit (d : int) =
  let c =
    match !st.count with
    | None -> d
    | Some n -> (n * 10) + d
  in
  st := { !st with count = Some c }
;;

let take_count_default (default : int) : int =
  let n = Option.value !st.count ~default in
  st := { !st with count = None };
  n
;;

let has_pending_state () =
  match !st.pending with
  | None_pending -> Option.is_some !st.count
  | _ -> true
;;

let is_pending_g () =
  match !st.pending with
  | Pending_g -> true
  | _ -> false
;;

let pending_op () =
  match !st.pending with
  | Pending_op op -> Some op
  | _ -> None
;;

let reverse_dir = function
  | Forward -> Backward
  | Backward -> Forward
;;

let find_on_line_n ~(s : string) ~(pos : int) ~(dir : find_dir) ~(ch : char) ~(n : int)
  : int option
  =
  let n = Int.max 1 n in
  let len = String.length s in
  if len = 0
  then None
  else (
    let pos = Int.max 0 (Int.min (len - 1) pos) in
    let line_start, line_end = Controller_shared.line_bounds s pos in
    let rec forward_from i k =
      if i >= line_end
      then None
      else (
        match String.index_from s i ch with
        | None -> None
        | Some j ->
          if j >= line_end
          then None
          else if k = 1
          then Some j
          else forward_from (j + 1) (k - 1))
    in
    let rec backward_from i k =
      if i < line_start
      then None
      else (
        match String.rindex_from s i ch with
        | None -> None
        | Some j ->
          if j < line_start
          then None
          else if k = 1
          then Some j
          else backward_from (j - 1) (k - 1))
    in
    match dir with
    | Forward ->
      let start = Int.min line_end (pos + 1) in
      forward_from start n
    | Backward ->
      let start = Int.max line_start (pos - 1) in
      backward_from start n)
;;

type motion =
  | W
  | B
  | E
  | FirstNonBlank
  | LineStart
  | LineEnd
  | Find of find_spec

let motion_target_for_nav ~(s : string) ~(pos : int) ~(motion : motion) ~(count : int)
  : int
  =
  let len = String.length s in
  let pos = Int.max 0 (Int.min len pos) in
  let count = Int.max 1 count in
  let rec step p k =
    if k <= 0
    then p
    else (
      let p' =
        match motion with
        | W -> skip_word_forward s len p
        | B -> skip_space_backward s p
        | E -> if p >= len then len else end_of_word s p
        | FirstNonBlank -> first_non_blank_in_line s p
        | LineStart -> fst (Controller_shared.line_bounds s p)
        | LineEnd -> snd (Controller_shared.line_bounds s p)
        | Find { ch; dir; till } ->
          (match find_on_line_n ~s ~pos:p ~dir ~ch ~n:1 with
           | None -> p
           | Some j ->
             if not till
             then j
             else (
               match dir with
               | Forward -> Int.max p (j - 1)
               | Backward -> Int.min len (j + 1)))
      in
      step p' (k - 1))
  in
  step pos count
;;

let motion_target_for_op_exclusive
      ~(s : string)
      ~(pos : int)
      ~(motion : motion)
      ~(count : int)
  : int
  =
  (* Returns an exclusive end for forward motions when appropriate. *)
  let len = String.length s in
  let t = motion_target_for_nav ~s ~pos ~motion ~count in
  match motion with
  | E ->
    (* 'e' is inclusive in Vim for operators: de deletes through end-of-word *)
    Int.min len (t + 1)
  | Find { ch; dir; till } ->
    (* For find motions, compute exclusive boundary based on match location.
       This is a pragmatic half-open range rule consistent with the existing controller:
       - forward f: include match -> j+1
       - forward t: stop before match -> j
       - backward F: endpoint at match j
       - backward T: endpoint after match -> j+1 *)
    (match find_on_line_n ~s ~pos ~dir ~ch ~n:count with
     | None -> pos
     | Some j ->
       (match dir, till with
        | Forward, false -> Int.min len (j + 1)
        | Forward, true -> Int.max 0 j
        | Backward, false -> Int.max 0 j
        | Backward, true -> Int.min len (j + 1)))
  | _ -> t
;;

let apply_op_to_motion ~(model : Model.t) ~(op : op) ~(motion : motion) ~(count : int)
  : unit
  =
  let count = Int.max 1 count in
  let s0 = Model.input_line model in
  let len0 = String.length s0 in
  let start_pos = Int.max 0 (Int.min len0 (Model.cursor_pos model)) in
  let target_pos = motion_target_for_op_exclusive ~s:s0 ~pos:start_pos ~motion ~count in
  let a, b =
    if start_pos <= target_pos then start_pos, target_pos else target_pos, start_pos
  in
  let a = Int.max 0 (Int.min len0 a) in
  let b = Int.max 0 (Int.min len0 b) in
  if a = b
  then ()
  else (
    let yanked = String.sub s0 ~pos:a ~len:(b - a) in
    Controller_register.set yanked;
    match op with
    | Yank -> Model.set_cursor_pos model (Int.max 0 (Int.min len0 target_pos))
    | Delete ->
      Model.push_undo model;
      delete_range model ~first:a ~last:b
    | Change ->
      Model.push_undo model;
      delete_range model ~first:a ~last:b;
      Model.set_mode model Model.Insert)
;;

type mods = (Notty.Unescape.mods[@deriving sexp])

let sexp_of_mods = function
  | [] -> Sexp.Atom "[]"
  | mods ->
    Sexp.List
      (List.map mods ~f:(fun m ->
         match m with
         | `Shift -> Sexp.Atom "Shift"
         | `Ctrl -> Sexp.Atom "Ctrl"
         | `Meta -> Sexp.Atom "Meta"))
;;

(* -------------------------------------------------------------------- *)
(* Main Normal-mode key-handler                                          *)
(* -------------------------------------------------------------------- *)

let handle_key_normal ~(model : Model.t) ~term (ev : Notty.Unescape.event) : reaction =
  let repeat n f =
    for _ = 1 to Int.max 1 n do
      f ()
    done
  in
  match ev with
  (* -------------------------------------------------------------- *)
  (* Visual-mode (character-wise)                                    *)
  | `Key (`Escape, mods) when List.is_empty mods && Model.selection_active model ->
    (* Esc clears Visual selection without triggering cancel/quit. *)
    clear_state ();
    Model.clear_selection model;
    Redraw
  | `Key (`ASCII 'v', mods) when List.is_empty mods ->
    clear_state ();
    (match Model.selection_anchor model with
     | None -> Model.set_selection_anchor model (Model.cursor_pos model)
     | Some _ -> Model.clear_selection model);
    Redraw
  | `Key (`ASCII 'y', mods) when List.is_empty mods && Model.selection_active model ->
    clear_state ();
    yank_selection model;
    Redraw
  | `Key (`ASCII 'd', mods) when List.is_empty mods && Model.selection_active model ->
    clear_state ();
    Model.push_undo model;
    delete_selection model;
    Redraw
  | `Key (`ASCII 'c', mods) when List.is_empty mods && Model.selection_active model ->
    clear_state ();
    Model.push_undo model;
    delete_selection model;
    Model.set_mode model Model.Insert;
    Redraw
  (* -------------------------------------------------------------- *)
  (* Cancel pending count/op/find with Esc (Vim-like), unless selection *)
  | `Key (`Escape, mods) when List.is_empty mods && has_pending_state () ->
    clear_state ();
    Redraw
  (* -------------------------------------------------------------- *)
  (* ':' Command-line mode                                           *)
  | `Key (`ASCII ':', mods) when List.is_empty mods ->
    clear_state ();
    Model.set_mode model Cmdline;
    Model.set_cmdline model "";
    Model.set_cmdline_cursor model 0;
    Redraw
  (* -------------------------------------------------------------- *)
  (* History search prompt                                           *)
  | `Key (`ASCII '/', mods) when List.is_empty mods ->
    clear_state ();
    Model.set_mode model (Model.Search Model.Forward);
    Model.set_search_query model "";
    Model.set_search_cursor model 0;
    Redraw
  | `Key (`ASCII '?', mods) when List.is_empty mods ->
    clear_state ();
    Model.set_mode model (Model.Search Model.Backward);
    Model.set_search_query model "";
    Model.set_search_cursor model 0;
    Redraw
  | `Key (`ASCII 'n', mods) when List.is_empty mods ->
    clear_state ();
    if Controller_history_search.repeat_last ~model ~term ~reverse:false
    then Redraw
    else Unhandled
  | `Key (`ASCII 'N', mods) when List.is_empty mods ->
    clear_state ();
    if Controller_history_search.repeat_last ~model ~term ~reverse:true
    then Redraw
    else Unhandled
  (* -------------------------------------------------------------- *)
  (* ArrowUp/Down scroll history (preference)                        *)
  | `Key (`Arrow `Up, mods) when List.is_empty mods ->
    clear_state ();
    Model.set_auto_follow model false;
    scroll_by_lines model ~term (-1);
    Redraw
  | `Key (`Arrow `Down, mods) when List.is_empty mods ->
    clear_state ();
    Model.set_auto_follow model false;
    scroll_by_lines model ~term 1;
    Redraw
  | `Key (`Arrow `Down, mods) when List.mem mods `Ctrl ~equal:Poly.equal ->
    clear_state ();
    Model.set_auto_follow model false;
    scroll_by_lines model ~term 1;
    Redraw
  | `Key (`Arrow `Up, mods) when List.mem mods `Ctrl ~equal:Poly.equal ->
    clear_state ();
    Model.set_auto_follow model false;
    scroll_by_lines model ~term (-1);
    Redraw
  (* Ctrl-f/b/d/u page/half-page history scrolling *)
  | `Key (`ASCII ('b' | 'B'), [ `Ctrl ]) ->
    clear_state ();
    Model.set_auto_follow model false;
    let ps = page_size ~term model in
    scroll_by_lines model ~term (-ps);
    Redraw
  | `Key (`ASCII ('f' | 'F'), [ `Ctrl ]) ->
    clear_state ();
    Model.set_auto_follow model false;
    let ps = page_size ~term model in
    scroll_by_lines model ~term ps;
    Redraw
  | `Key (`ASCII ('u' | 'U'), [ `Ctrl ]) ->
    clear_state ();
    Model.set_auto_follow model false;
    let ps = page_size ~term model / 2 in
    scroll_by_lines model ~term (-Int.max 1 ps);
    Redraw
  | `Key (`ASCII ('d' | 'D'), [ `Ctrl ]) ->
    clear_state ();
    Model.set_auto_follow model false;
    let ps = page_size ~term model / 2 in
    scroll_by_lines model ~term (Int.max 1 ps);
    Redraw
  (* -------------------------------------------------------------- *)
  (* Cursor vertical move within editor (Meta/Shift + arrows)         *)
  | `Key (`Arrow `Up, mods) when List.mem mods `Meta ~equal:Poly.equal ->
    clear_state ();
    move_cursor_vertically model ~term ~dir:(-1);
    Redraw
  | `Key (`Arrow `Up, mods) when List.mem mods `Shift ~equal:Poly.equal ->
    clear_state ();
    move_cursor_vertically model ~term ~dir:(-1);
    Redraw
  | `Key (`Arrow `Down, mods) when List.mem mods `Meta ~equal:Poly.equal ->
    clear_state ();
    move_cursor_vertically model ~term ~dir:1;
    Redraw
  | `Key (`Arrow `Down, mods) when List.mem mods `Shift ~equal:Poly.equal ->
    clear_state ();
    move_cursor_vertically model ~term ~dir:1;
    Redraw
  (* -------------------------------------------------------------- *)
  (* Find-on-line: start prefix (f/F/t/T)                             *)
  | `Key (`ASCII 'f', mods) when List.is_empty mods ->
    let op = pending_op () in
    st := { !st with pending = Pending_find_prefix { dir = Forward; till = false; op } };
    Unhandled
  | `Key (`ASCII 'F', mods) when List.is_empty mods ->
    let op = pending_op () in
    st := { !st with pending = Pending_find_prefix { dir = Backward; till = false; op } };
    Unhandled
  | `Key (`ASCII 't', mods) when List.is_empty mods ->
    let op = pending_op () in
    st := { !st with pending = Pending_find_prefix { dir = Forward; till = true; op } };
    Unhandled
  | `Key (`ASCII 'T', mods) when List.is_empty mods ->
    let op = pending_op () in
    st := { !st with pending = Pending_find_prefix { dir = Backward; till = true; op } };
    Unhandled
  (* Repeat last find: ; forward, , reverse direction *)
  | `Key (`ASCII (';' as _k), mods) when List.is_empty mods ->
    (match !last_find with
     | None -> Unhandled
     | Some spec0 ->
       let n = take_count_default 1 in
       let op = pending_op () in
       clear_state ();
       let spec = spec0 in
       (match op with
        | Some op ->
          apply_op_to_motion ~model ~op ~motion:(Find spec) ~count:n;
          Redraw
        | None ->
          let s = Model.input_line model in
          let pos0 = Model.cursor_pos model in
          let pos = motion_target_for_nav ~s ~pos:pos0 ~motion:(Find spec) ~count:n in
          Model.set_cursor_pos model pos;
          Redraw))
  | `Key (`ASCII (',' as _k), mods) when List.is_empty mods ->
    (match !last_find with
     | None -> Unhandled
     | Some spec0 ->
       let n = take_count_default 1 in
       let op = pending_op () in
       clear_state ();
       let spec = { spec0 with dir = reverse_dir spec0.dir } in
       (match op with
        | Some op ->
          apply_op_to_motion ~model ~op ~motion:(Find spec) ~count:n;
          Redraw
        | None ->
          let s = Model.input_line model in
          let pos0 = Model.cursor_pos model in
          let pos = motion_target_for_nav ~s ~pos:pos0 ~motion:(Find spec) ~count:n in
          Model.set_cursor_pos model pos;
          Redraw))
  (* -------------------------------------------------------------- *)
  (* Operator-pending resolution for motions: w/b/e/^/0/$             *)
  | `Key (`ASCII 'w', mods) when List.is_empty mods && Option.is_some (pending_op ()) ->
    let op = Option.value_exn (pending_op ()) in
    let n = take_count_default 1 in
    clear_state ();
    apply_op_to_motion ~model ~op ~motion:W ~count:n;
    Redraw
  | `Key (`ASCII 'b', mods) when List.is_empty mods && Option.is_some (pending_op ()) ->
    let op = Option.value_exn (pending_op ()) in
    let n = take_count_default 1 in
    clear_state ();
    apply_op_to_motion ~model ~op ~motion:B ~count:n;
    Redraw
  | `Key (`ASCII 'e', mods) when List.is_empty mods && Option.is_some (pending_op ()) ->
    let op = Option.value_exn (pending_op ()) in
    let n = take_count_default 1 in
    clear_state ();
    apply_op_to_motion ~model ~op ~motion:E ~count:n;
    Redraw
  | `Key (`ASCII '^', mods) when List.is_empty mods && Option.is_some (pending_op ()) ->
    let op = Option.value_exn (pending_op ()) in
    ignore (take_count_default 1 : int);
    clear_state ();
    apply_op_to_motion ~model ~op ~motion:FirstNonBlank ~count:1;
    Redraw
  | `Key (`ASCII '0', mods) when List.is_empty mods && Option.is_some (pending_op ()) ->
    let op = Option.value_exn (pending_op ()) in
    clear_state ();
    apply_op_to_motion ~model ~op ~motion:LineStart ~count:1;
    Redraw
  | `Key (`ASCII '$', mods) when List.is_empty mods && Option.is_some (pending_op ()) ->
    let op = Option.value_exn (pending_op ()) in
    let n = take_count_default 1 in
    clear_state ();
    apply_op_to_motion ~model ~op ~motion:LineEnd ~count:n;
    Redraw
  (* Linewise operator repetition: yy/dd/cc *)
  | `Key (`ASCII 'y', mods) when List.is_empty mods ->
    (match !st.pending with
     | Pending_op Yank ->
       let _n = take_count_default 1 in
       clear_state ();
       yank_current_line model;
       Redraw
     | _ ->
       st := { !st with pending = Pending_op Yank };
       Unhandled)
  | `Key (`ASCII 'd', mods) when List.is_empty mods ->
    (match !st.pending with
     | Pending_op Delete ->
       let _n = take_count_default 1 in
       clear_state ();
       Model.push_undo model;
       delete_current_line model;
       Redraw
     | _ ->
       st := { !st with pending = Pending_op Delete };
       Unhandled)
  | `Key (`ASCII 'c', mods) when List.is_empty mods ->
    (match !st.pending with
     | Pending_op Change ->
       let _n = take_count_default 1 in
       clear_state ();
       Model.push_undo model;
       change_current_line model;
       Redraw
     | _ ->
       st := { !st with pending = Pending_op Change };
       Unhandled)
  (* If an operator is pending and something else arrives, cancel it *)
  | `Key (`ASCII _, _) when Option.is_some (pending_op ()) ->
    clear_state ();
    Unhandled
  (* -------------------------------------------------------------- *)
  (* Count handling (digits)                                         *)
  | `Key (`ASCII c, mods) when List.is_empty mods && Char.is_digit c ->
    (match c, !st.count, !st.pending with
     | '0', None, None_pending ->
       (* bare 0 is "start of line" motion *)
       clear_state ();
       let s = Model.input_line model in
       let pos = Model.cursor_pos model in
       let start, _ = Controller_shared.line_bounds s pos in
       Model.set_cursor_pos model start;
       Redraw
     | _ ->
       let d = Char.to_int c - Char.to_int '0' in
       push_digit d;
       Unhandled)
  (* -------------------------------------------------------------- *)
  (* Handle pending 'g' prefix                                       *)
  | `Key (`ASCII 'g', mods) when List.is_empty mods ->
    (match !st.pending with
     | Pending_g ->
       (* gg: go to line {count} or 1 *)
       st := { pending = None_pending; count = !st.count };
       let n = take_count_default 1 in
       goto_line_start model ~line_1_based:n;
       Redraw
     | None_pending ->
       st := { !st with pending = Pending_g };
       Unhandled
     | Pending_op _ | Pending_find_prefix _ ->
       (* dgg/cgg/ygg not supported yet; cancel pending *)
       clear_state ();
       Unhandled)
  (* Any non-'g' key clears pending g prefix *)
  | `Key (`ASCII _, _) when is_pending_g () ->
    clear_state ();
    Unhandled
  (* Canonical G: goto last line start, or {count}G to line N *)
  | `Key (`ASCII 'G', mods) when List.is_empty mods ->
    let n_opt = !st.count in
    clear_state ();
    (match n_opt with
     | None ->
       goto_last_line_start model;
       Redraw
     | Some n ->
       goto_line_start model ~line_1_based:n;
       Redraw)
  (* -------------------------------------------------------------- *)
  (* Basic motions with counts (no pending operator)                  *)
  | `Key (`ASCII 'h', mods) when List.is_empty mods ->
    let n = take_count_default 1 in
    let pos = Model.cursor_pos model in
    Model.set_cursor_pos model (Int.max 0 (pos - n));
    Redraw
  | `Key (`ASCII 'l', mods) when List.is_empty mods ->
    let n = take_count_default 1 in
    let pos = Model.cursor_pos model in
    let len = String.length (Model.input_line model) in
    Model.set_cursor_pos model (Int.min len (pos + n));
    Redraw
  | `Key (`ASCII 'k', mods) when List.is_empty mods ->
    let n = take_count_default 1 in
    repeat n (fun () -> move_cursor_vertically model ~term ~dir:(-1));
    Redraw
  | `Key (`ASCII 'j', mods) when List.is_empty mods ->
    let n = take_count_default 1 in
    repeat n (fun () -> move_cursor_vertically model ~term ~dir:1);
    Redraw
  | `Key (`ASCII 'w', mods) when List.is_empty mods ->
    let n = take_count_default 1 in
    let s = Model.input_line model in
    let len = String.length s in
    let pos0 = Model.cursor_pos model in
    let pos =
      List.fold (List.init n ~f:Fn.id) ~init:pos0 ~f:(fun p _ ->
        skip_word_forward s len p)
    in
    Model.set_cursor_pos model pos;
    Redraw
  | `Key (`ASCII 'b', mods) when List.is_empty mods ->
    let n = take_count_default 1 in
    let s = Model.input_line model in
    let pos0 = Model.cursor_pos model in
    let pos =
      List.fold (List.init n ~f:Fn.id) ~init:pos0 ~f:(fun p _ -> skip_space_backward s p)
    in
    Model.set_cursor_pos model pos;
    Redraw
  | `Key (`ASCII 'e', mods) when List.is_empty mods ->
    (* Simple repeated 'e': move to end of current/next word, repeating by stepping forward. *)
    let n = take_count_default 1 in
    let s = Model.input_line model in
    let len = String.length s in
    let rec step pos =
      if pos >= len
      then len
      else (
        let e = end_of_word s pos in
        Int.max pos e)
    in
    let rec loop pos k =
      if k <= 0
      then pos
      else (
        let e = step pos in
        let next = if e < len then e + 1 else e in
        loop next (k - 1))
    in
    let pos0 = Model.cursor_pos model in
    let p = loop pos0 n in
    let final = if p > 0 then p - 1 else 0 in
    Model.set_cursor_pos model (Int.min len final);
    Redraw
  | `Key (`ASCII '^', mods) when List.is_empty mods ->
    ignore (take_count_default 1 : int);
    let s = Model.input_line model in
    let pos = Model.cursor_pos model in
    Model.set_cursor_pos model (first_non_blank_in_line s pos);
    Redraw
  | `Key (`ASCII '$', mods) when List.is_empty mods ->
    clear_state ();
    let s = Model.input_line model in
    let pos = Model.cursor_pos model in
    let _, eol = Controller_shared.line_bounds s pos in
    Model.set_cursor_pos model eol;
    Redraw
  (* -------------------------------------------------------------- *)
  (* Insert-mode transitions / edits                                  *)
  | `Key (`ASCII 'a', mods) when List.is_empty mods ->
    clear_state ();
    let len = String.length (Model.input_line model) in
    let pos = Model.cursor_pos model in
    let new_pos = if pos < len then pos + 1 else pos in
    Model.set_cursor_pos model new_pos;
    Model.set_mode model Insert;
    Redraw
  | `Key (`ASCII 'o', mods) when List.is_empty mods ->
    clear_state ();
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
  | `Key (`ASCII 'O', mods) when List.is_empty mods ->
    clear_state ();
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
  (* Paste from shared register *)
  | `Key (`ASCII 'p', mods) when List.is_empty mods ->
    clear_state ();
    let text = Controller_register.get () in
    if String.is_empty text
    then Unhandled
    else (
      Model.push_undo model;
      let pos = Model.cursor_pos model in
      let len = String.length (Model.input_line model) in
      let insert_pos = if pos < len then pos + 1 else pos in
      insert_text_at model ~pos:insert_pos text;
      Redraw)
  | `Key (`ASCII 'P', mods) when List.is_empty mods ->
    clear_state ();
    let text = Controller_register.get () in
    if String.is_empty text
    then Unhandled
    else (
      Model.push_undo model;
      let pos = Model.cursor_pos model in
      insert_text_at model ~pos text;
      Redraw)
  (* x: delete char under cursor, yank it *)
  | `Key (`ASCII 'x', mods) when List.is_empty mods ->
    clear_state ();
    let s = Model.input_line model in
    let pos = Model.cursor_pos model in
    if pos < String.length s
    then (
      Model.push_undo model;
      Controller_register.set (String.sub s ~pos ~len:1);
      delete_range model ~first:pos ~last:(pos + 1));
    Redraw
  (* Resolve f/F/t/T with next ASCII char *)
  | `Key (`ASCII ch, mods) when List.is_empty mods ->
    (match !st.pending with
     | Pending_find_prefix { dir; till; op } ->
       let n = take_count_default 1 in
       let spec = { ch; dir; till } in
       last_find := Some spec;
       clear_state ();
       (match op with
        | Some op ->
          apply_op_to_motion ~model ~op ~motion:(Find spec) ~count:n;
          Redraw
        | None ->
          let s = Model.input_line model in
          let pos0 = Model.cursor_pos model in
          let pos = motion_target_for_nav ~s ~pos:pos0 ~motion:(Find spec) ~count:n in
          Model.set_cursor_pos model pos;
          Redraw)
     | _ -> Unhandled)
  | _ -> Unhandled
;;
