open Core

(* Helper to create a minimal [Model.t] value suitable for unit-testing the
   Normal-mode cursor motions.  We intentionally avoid constructing a real
   [Notty_eio.Term.t] instance because the handler under test never touches
   the terminal object for the specific key-strokes exercised here. *)

let make_model () : Chat_tui.Model.t =
  let open Chat_tui in
  let scroll_box = Notty_scroll_box.create Notty.I.empty in
  Model.create
    ~history_items:[]
    ~messages:[]
    ~input_line:"hello world"
    ~auto_follow:true
    ~msg_buffers:(Hashtbl.create (module String))
    ~function_name_by_id:(Hashtbl.create (module String))
    ~reasoning_idx_by_id:(Hashtbl.create (module String))
    ~fetch_sw:None
    ~scroll_box
    ~cursor_pos:0
    ~selection_anchor:None
    ~mode:Chat_tui.Model.Normal
    ~draft_mode:Chat_tui.Model.Plain
    ~selected_msg:None
    ~undo_stack:[]
    ~redo_stack:[]
    ~cmdline:""
    ~cmdline_cursor:0
;;

(* Dummy value – safe because the normal-mode handler does not dereference
   the terminal for the `w` / `b` movements tested below. *)

let dummy_term : Notty_eio.Term.t = Obj.magic 0

let%expect_test "normal_mode_w_and_b_move_cursor" =
  let m = make_model () in
  let open Chat_tui in
  let event_w : Notty.Unescape.event = `Key (`ASCII 'w', []) in
  ignore (Controller_normal.handle_key_normal ~model:m ~term:dummy_term event_w);
  (* Cursor should now be at index 6 – the start of "world" *)
  Printf.printf "%d\n" (Model.cursor_pos m);
  let event_b : Notty.Unescape.event = `Key (`ASCII 'b', []) in
  ignore (Controller_normal.handle_key_normal ~model:m ~term:dummy_term event_b);
  (* Back to 0 *)
  Printf.printf "%d\n" (Model.cursor_pos m);
  [%expect
    {|6
0|}]
;;
