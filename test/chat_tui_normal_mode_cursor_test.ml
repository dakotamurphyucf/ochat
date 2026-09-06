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
    ~tool_output_by_index:(Hashtbl.create (module Int))
    ~tasks:[]
    ~kv_store:(Hashtbl.create (module String))
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

let check_selection_escape activity ~cursor =
  let open Chat_tui in
  let model = make_model () in
  Model.set_activity model activity;
  Model.set_selection_anchor model 0;
  Model.set_cursor_pos model cursor;
  let dispatch event = Controller.handle_key ~model ~term:dummy_term event in
  ignore (dispatch (`Key (`ASCII '2', [])) : Controller.reaction);
  let reaction = dispatch (`Key (`Escape, [])) in
  assert (Poly.equal reaction Controller.Redraw);
  assert (Poly.equal (Model.mode model) Model.Normal);
  assert (Poly.equal (Model.active_page model) Model.Page_id.Chat);
  assert (not (Model.selection_active model));
  assert (String.equal (Model.input_line model) "hello world");
  assert (Model.cursor_pos model = cursor);
  assert (Poly.equal (Model.activity model) activity);
  assert (not (Model.undo model));
  ignore (dispatch (`Key (`ASCII 'l', [])) : Controller.reaction);
  assert (Model.cursor_pos model = cursor + 1);
  assert (Poly.equal (dispatch (`Key (`Escape, []))) Controller.Cancel_or_quit)
;;

let%expect_test "public controller clears Visual selection before cancel or quit" =
  List.iter
    [ None; Some (Chat_tui.Model.Assistant Thinking); Some Chat_tui.Model.Compacting ]
    ~f:(fun activity ->
      List.iter [ 0; 3 ] ~f:(fun cursor -> check_selection_escape activity ~cursor));
  [%expect {| |}]
;;

let%expect_test "Escape preserves history selection with and without Visual draft selection" =
  let open Chat_tui in
  let model = make_model () in
  let entry_id =
    History_entry.Id.create ~namespace:"escape" ~sequence:0 |> Result.ok_or_failwith
  in
  let row = Projected_message.canonical_row ~entry_id ("assistant", "history") in
  Model.reconcile_projected_rows model [ row ];
  Model.reconcile_messages model [ row.message ];
  Model.select_projected model (Some row.id);
  let dispatch event = Controller.handle_key ~model ~term:dummy_term event in
  List.iter [ Some 0; None ] ~f:(fun anchor ->
    Option.iter anchor ~f:(Model.set_selection_anchor model);
    let expected =
      if Option.is_some anchor then Controller.Redraw else Controller.Cancel_or_quit
    in
    assert (Poly.equal (dispatch (`Key (`Escape, []))) expected);
    assert (not (Model.selection_active model));
    assert (Poly.equal (Model.selected_projected_id model) (Some row.id)));
  ignore (dispatch (`Key (`ASCII ':', [])) : Controller.reaction);
  String.iter "delete" ~f:(fun character ->
    ignore (dispatch (`Key (`ASCII character, [])) : Controller.reaction));
  assert (Poly.equal (dispatch (`Key (`Enter, []))) (Controller.Delete_history entry_id));
  [%expect {| |}]
;;
