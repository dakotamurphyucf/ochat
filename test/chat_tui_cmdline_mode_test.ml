open Core

(* Simple unit test covering Phase-3 command-line entry *)

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

let%expect_test "colon q enters command mode and quits" =
  let m = make_model () in
  let dummy_term : Notty_eio.Term.t = Obj.magic 0 in
  (* 1. Press ':' in Normal mode *)
  let reaction1 =
    Chat_tui.Controller.handle_key ~model:m ~term:dummy_term (`Key (`ASCII ':', []))
  in
  (match reaction1 with
   | Chat_tui.Controller.Redraw -> ()
   | _ -> print_endline "unexpected reaction");
  (* Model should now be in Cmdline mode *)
  (match Chat_tui.Model.mode m with
   | Chat_tui.Model.Cmdline -> print_endline "cmdline entered"
   | _ -> print_endline "mode error");
  (* 2. Type 'q' *)
  ignore
    (Chat_tui.Controller.handle_key ~model:m ~term:dummy_term (`Key (`ASCII 'q', [])));
  (* 3. Press Enter *)
  let reaction3 =
    Chat_tui.Controller.handle_key ~model:m ~term:dummy_term (`Key (`Enter, []))
  in
  (match reaction3 with
   | Chat_tui.Controller.Quit -> print_endline "quit reaction"
   | _ -> print_endline "unexpected final");
  [%expect
    {|
    cmdline entered
    quit reaction
  |}]
;;
