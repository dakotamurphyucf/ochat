open Core

let make_model ?(text = "") () : Chat_tui.Model.t =
  let open Chat_tui in
  let scroll_box = Notty_scroll_box.create Notty.I.empty in
  Model.create
    ~history_items:[]
    ~messages:[]
    ~input_line:text
    ~auto_follow:true
    ~msg_buffers:(Hashtbl.create (module String))
    ~function_name_by_id:(Hashtbl.create (module String))
    ~reasoning_idx_by_id:(Hashtbl.create (module String))
    ~tool_output_by_index:(Hashtbl.create (module Int))
    ~tasks:[]
    ~kv_store:(Hashtbl.create (module String))
    ~fetch_sw:None
    ~scroll_box
    ~cursor_pos:(String.length text)
    ~selection_anchor:None
    ~mode:Chat_tui.Model.Insert
    ~draft_mode:Chat_tui.Model.Plain
    ~selected_msg:None
    ~undo_stack:[]
    ~redo_stack:[]
    ~cmdline:""
    ~cmdline_cursor:0
;;

let%expect_test "undo / redo basic" =
  let m = make_model ~text:"abc" () in
  (* push current state, then delete last char *)
  Chat_tui.Model.push_undo m;
  Chat_tui.Model.set_input_line m "ab";
  Chat_tui.Model.set_cursor_pos m 2;
  (* Undo should restore "abc" and cursor 3 *)
  assert (Chat_tui.Model.undo m);
  print_endline (Chat_tui.Model.input_line m);
  print_endline (Int.to_string (Chat_tui.Model.cursor_pos m));
  (* Redo should go forward to "ab" *)
  assert (Chat_tui.Model.redo m);
  print_endline (Chat_tui.Model.input_line m);
  print_endline (Int.to_string (Chat_tui.Model.cursor_pos m));
  [%expect
    {| 
    abc
    3
    ab
    2
  |}]
;;
