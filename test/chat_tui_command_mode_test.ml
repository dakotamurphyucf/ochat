open Core

(* Helper: construct a minimal model with default values. *)

let make_model () : Chat_tui.Model.t =
  let open Chat_tui in
  let scroll_box = Notty_scroll_box.create Notty.I.empty in
  Model.create
    ~history_items:[]
    ~messages:[]
    ~input_line:""
    ~auto_follow:true
    ~msg_buffers:(Hashtbl.create (module String))
    ~function_name_by_id:(Hashtbl.create (module String))
    ~reasoning_idx_by_id:(Hashtbl.create (module String))
    ~tasks:[]
    ~kv_store:(Hashtbl.create (module String))
    ~fetch_sw:None
    ~scroll_box
    ~cursor_pos:0
    ~selection_anchor:None
    ~mode:Chat_tui.Model.Insert
    ~draft_mode:Chat_tui.Model.Plain
    ~selected_msg:None
    ~undo_stack:[]
    ~redo_stack:[]
    ~cmdline:""
    ~cmdline_cursor:0
;;

let%expect_test "toggle_mode cycles" =
  let m = make_model () in
  let show () =
    print_endline
      (match Chat_tui.Model.mode m with
       | Chat_tui.Model.Insert -> "Insert"
       | Chat_tui.Model.Normal -> "Normal"
       | Chat_tui.Model.Cmdline -> "Cmd")
  in
  show ();
  (* Initial *)
  Chat_tui.Model.toggle_mode m;
  show ();
  (* After first toggle *)
  Chat_tui.Model.toggle_mode m;
  show ();
  (* After second toggle, back to Insert *)
  [%expect
    {| 
    Insert
    Normal
    Insert
  |}]
;;
