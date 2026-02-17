open Core

let render_to_string ~(w : int) ~(h : int) (model : Chat_tui.Model.t) : string =
  let img, _cursor = Chat_tui.Renderer.render_full ~size:(w, h) ~model in
  let buf = Buffer.create 256 in
  Notty.Render.to_buffer buf Notty.Cap.dumb (0, 0) (w, h) img;
  Buffer.contents buf
;;

let make_model ?(input_line = "") ?(cursor_pos = 0) ?(mode = Chat_tui.Model.Insert) ()
  : Chat_tui.Model.t
  =
  let open Chat_tui in
  let scroll_box = Notty_scroll_box.create Notty.I.empty in
  Model.create
    ~history_items:[]
    ~messages:[]
    ~input_line
    ~auto_follow:true
    ~msg_buffers:(Hashtbl.create (module String))
    ~function_name_by_id:(Hashtbl.create (module String))
    ~reasoning_idx_by_id:(Hashtbl.create (module String))
    ~tool_output_by_index:(Hashtbl.create (module Int))
    ~tasks:[]
    ~kv_store:(Hashtbl.create (module String))
    ~fetch_sw:None
    ~scroll_box
    ~cursor_pos
    ~selection_anchor:None
    ~mode
    ~draft_mode:Model.Plain
    ~selected_msg:None
    ~undo_stack:[]
    ~redo_stack:[]
    ~cmdline:""
    ~cmdline_cursor:0
;;

let set_relevant_completion (model : Chat_tui.Model.t) ~(text : string) : unit =
  let open Chat_tui in
  let completion : Model.typeahead_completion =
    { text
    ; base_input = Model.input_line model
    ; base_cursor = Model.cursor_pos model
    ; generation = 0
    }
  in
  Model.set_typeahead_completion model (Some completion)
;;

let reaction_to_string (r : Chat_tui.Controller.reaction) : string =
  match r with
  | Chat_tui.Controller.Redraw -> "Redraw"
  | Chat_tui.Controller.Submit_input -> "Submit_input"
  | Chat_tui.Controller.Cancel_or_quit -> "Cancel_or_quit"
  | Chat_tui.Controller.Compact_context -> "Compact_context"
  | Chat_tui.Controller.Quit -> "Quit"
  | Chat_tui.Controller.Unhandled -> "Unhandled"
;;

let mode_to_string (m : Chat_tui.Model.editor_mode) : string =
  match m with
  | Chat_tui.Model.Insert -> "Insert"
  | Chat_tui.Model.Normal -> "Normal"
  | Chat_tui.Model.Cmdline -> "Cmdline"
;;

let%expect_test "hints are absent when there is no completion" =
  let model = make_model ~input_line:"hi" ~cursor_pos:2 () in
  let s = render_to_string ~w:200 ~h:10 model in
  printf "hints_present=%b\n" (String.is_substring s ~substring:"[Tab accept all]");
  [%expect
    {|
    failed to load HTML grammar: json > object: not enough input
    failed to load Markdown grammar: (Sys_error
     "lib/chat_tui/grammars/markdown.tmLanguage.json: No such file or directory")
    hints_present=false
    |}]
;;

let%expect_test "hints are present when the completion is relevant" =
  let model = make_model ~input_line:"hi" ~cursor_pos:2 () in
  set_relevant_completion model ~text:" there";
  let s = render_to_string ~w:200 ~h:10 model in
  printf "hints_present=%b\n" (String.is_substring s ~substring:"[Tab accept all]");
  [%expect {| hints_present=true |}]
;;

let%expect_test "inline ghost text renders on the cursor line" =
  let model = make_model ~input_line:"hi" ~cursor_pos:2 () in
  set_relevant_completion model ~text:" there";
  let s = render_to_string ~w:12 ~h:10 model in
  let line =
    String.split_lines s
    |> List.find ~f:(fun l -> String.is_substring l ~substring:"│> hi")
    |> Option.value_exn
  in
  print_endline (String.strip line);
  [%expect {| │> hi there│ |}]
;;

let%expect_test "multiline completion renders indicator, not extra rows" =
  let model = make_model ~input_line:"hi" ~cursor_pos:2 () in
  set_relevant_completion model ~text:" there\nSECOND\nTHIRD";
  let s = render_to_string ~w:60 ~h:10 model in
  printf
    "indicator=%b second_line_visible=%b\n"
    (String.is_substring s ~substring:"… (+2 more lines)")
    (String.is_substring s ~substring:"SECOND");
  [%expect {| indicator=true second_line_visible=false |}]
;;

let%expect_test "preview popup overlays and does not move cursor" =
  let model = make_model ~input_line:"hi" ~cursor_pos:2 () in
  set_relevant_completion model ~text:" there\nSECOND\nTHIRD";
  let img_closed, cursor_closed = Chat_tui.Renderer.render_full ~size:(80, 20) ~model in
  ignore (img_closed : Notty.I.t);
  Chat_tui.Model.set_typeahead_preview_open model true;
  let s_open = render_to_string ~w:80 ~h:20 model in
  let _img_open, cursor_open = Chat_tui.Renderer.render_full ~size:(80, 20) ~model in
  printf
    "overlay=%b cursor_unchanged=%b\n"
    (String.is_substring s_open ~substring:"completion preview")
    Poly.(cursor_closed = cursor_open);
  [%expect {| overlay=true cursor_unchanged=true |}]
;;

let dummy_term : Notty_eio.Term.t = Obj.magic 0

let%expect_test "controller: Tab accepts all and clears completion" =
  let model = make_model ~input_line:"hi" ~cursor_pos:2 () in
  set_relevant_completion model ~text:" there";
  let reaction =
    Chat_tui.Controller.handle_key ~model ~term:dummy_term (`Key (`Tab, []))
  in
  printf
    "reaction=%s input=%S completion_present=%b\n"
    (reaction_to_string reaction)
    (Chat_tui.Model.input_line model)
    (Option.is_some (Chat_tui.Model.typeahead_completion model));
  [%expect {| reaction=Redraw input="hi there" completion_present=false |}]
;;

let%expect_test "controller: Shift+Tab accepts one line and keeps remainder relevant" =
  let model = make_model ~input_line:"" ~cursor_pos:0 () in
  set_relevant_completion model ~text:"hello\nworld";
  let reaction =
    Chat_tui.Controller.handle_key ~model ~term:dummy_term (`Key (`Tab, [ `Shift ]))
  in
  let completion = Chat_tui.Model.typeahead_completion model in
  let remainder =
    Option.map completion ~f:(fun c -> c.Chat_tui.Model.text)
    |> Option.value ~default:"<none>"
  in
  printf
    "reaction=%s input=%S cursor=%d remainder=%S relevant=%b\n"
    (reaction_to_string reaction)
    (Chat_tui.Model.input_line model)
    (Chat_tui.Model.cursor_pos model)
    remainder
    (Chat_tui.Model.typeahead_is_relevant model);
  [%expect {| reaction=Redraw input="hello\n" cursor=6 remainder="world" relevant=true |}]
;;

let%expect_test "controller: typing dismisses completion and closes preview" =
  let model = make_model ~input_line:"hi" ~cursor_pos:2 () in
  set_relevant_completion model ~text:" there";
  Chat_tui.Model.set_typeahead_preview_open model true;
  let reaction =
    Chat_tui.Controller.handle_key ~model ~term:dummy_term (`Key (`ASCII 'x', []))
  in
  printf
    "reaction=%s input=%S preview_open=%b completion_present=%b\n"
    (reaction_to_string reaction)
    (Chat_tui.Model.input_line model)
    (Chat_tui.Model.typeahead_preview_open model)
    (Option.is_some (Chat_tui.Model.typeahead_completion model));
  [%expect {| reaction=Redraw input="hix" preview_open=false completion_present=false |}]
;;

let%expect_test
    "controller: bare ESC closes preview, then dismisses, then switches to Normal"
  =
  let model = make_model ~input_line:"hi" ~cursor_pos:2 () in
  set_relevant_completion model ~text:" there";
  Chat_tui.Model.set_typeahead_preview_open model true;
  let r1 = Chat_tui.Controller.handle_key ~model ~term:dummy_term (`Key (`Escape, [])) in
  let after1 =
    Chat_tui.Model.typeahead_preview_open model, Chat_tui.Model.typeahead_completion model
  in
  let r2 = Chat_tui.Controller.handle_key ~model ~term:dummy_term (`Key (`Escape, [])) in
  let after2 =
    Chat_tui.Model.typeahead_preview_open model, Chat_tui.Model.typeahead_completion model
  in
  let r3 = Chat_tui.Controller.handle_key ~model ~term:dummy_term (`Key (`Escape, [])) in
  printf
    "r1=%s after1=(preview=%b completion=%b) r2=%s after2=(preview=%b completion=%b) \
     r3=%s mode=%s\n"
    (reaction_to_string r1)
    (fst after1)
    (Option.is_some (snd after1))
    (reaction_to_string r2)
    (fst after2)
    (Option.is_some (snd after2))
    (reaction_to_string r3)
    (mode_to_string (Chat_tui.Model.mode model));
  [%expect
    {| r1=Redraw after1=(preview=false completion=true) r2=Redraw after2=(preview=false completion=false) r3=Redraw mode=Normal |}]
;;

let%expect_test "controller: Ctrl+Space encoding closes preview when already open" =
  let model = make_model ~input_line:"hi" ~cursor_pos:2 () in
  set_relevant_completion model ~text:" there";
  Chat_tui.Model.set_typeahead_preview_open model true;
  let _ =
    Chat_tui.Controller.handle_key ~model ~term:dummy_term (`Key (`ASCII ' ', [ `Ctrl ]))
  in
  printf "after_space_ctrl=%b\n" (Chat_tui.Model.typeahead_preview_open model);
  Chat_tui.Model.set_typeahead_preview_open model true;
  let _ =
    Chat_tui.Controller.handle_key ~model ~term:dummy_term (`Key (`ASCII '@', [ `Ctrl ]))
  in
  printf "after_at_ctrl=%b\n" (Chat_tui.Model.typeahead_preview_open model);
  Chat_tui.Model.set_typeahead_preview_open model true;
  let _ =
    Chat_tui.Controller.handle_key ~model ~term:dummy_term (`Key (`ASCII '\000', []))
  in
  printf "after_nul=%b\n" (Chat_tui.Model.typeahead_preview_open model);
  [%expect
    {|
    after_space_ctrl=false
    after_at_ctrl=false
    after_nul=false
    |}]
;;
