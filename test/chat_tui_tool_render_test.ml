open Core

let render_to_string ~(w : int) ~(h : int) (model : Chat_tui.Model.t) : string =
  let img, _ = Chat_tui.Renderer.render_full ~size:(w, h) ~model in
  let buf = Buffer.create 256 in
  Notty.Render.to_buffer buf Notty.Cap.dumb (0, 0) (w, h) img;
  Buffer.contents buf
;;

let make_base_model ~(messages : Chat_tui.Types.message list)
      ~(tool_outputs : (int, Chat_tui.Types.tool_output_kind) Hashtbl.t)
  : Chat_tui.Model.t
  =
  let open Chat_tui in
  let scroll_box = Notty_scroll_box.create Notty.I.empty in
  Model.create
    ~history_items:[]
    ~messages
    ~input_line:""
    ~auto_follow:true
    ~msg_buffers:(Hashtbl.create (module String))
    ~function_name_by_id:(Hashtbl.create (module String))
    ~reasoning_idx_by_id:(Hashtbl.create (module String))
    ~tool_output_by_index:tool_outputs
    ~tasks:[]
    ~kv_store:(Hashtbl.create (module String))
    ~fetch_sw:None
    ~scroll_box
    ~cursor_pos:0
    ~selection_anchor:None
    ~mode:Model.Insert
    ~draft_mode:Model.Plain
    ~selected_msg:None
    ~undo_stack:[]
    ~redo_stack:[]
    ~cmdline:""
    ~cmdline_cursor:0
;;

let%expect_test "tool call rendering preserves call text" =
  let messages =
    [ "tool", "read_file({\"file\": \"lib/foo.ml\"})" ]
  in
  let tool_outputs = Hashtbl.create (module Int) in
  let model = make_base_model ~messages ~tool_outputs in
  let s = render_to_string ~w:80 ~h:10 model in
  String.split_lines s
  |> List.iter ~f:(fun line ->
       if String.is_substring line ~substring:"read_file("
       then print_endline (String.strip line));
  [%expect {|
    failed to load JSON grammar: json > object > array > object > string: unterminated string
    failed to load HTML grammar: json > object: not enough input
    failed to load Markdown grammar: (Sys_error
     "lib/chat_tui/grammars/markdown.tmLanguage.json: No such file or directory")
    read_file({"file": "lib/foo.ml"})
    |}]
;;

let%expect_test "apply_patch output rendering keeps status and patch text" =
  let open Chat_tui in
  let body =
    String.concat
      ~sep:"\n"
      [ "Patch applied successfully!"
      ; ""
      ; "*** Begin Patch"
      ; "*** Add File: foo.txt"
      ; "+ line"
      ; "*** End Patch"
      ]
  in
  let messages = [ "tool_output", body ] in
  let tool_outputs = Hashtbl.create (module Int) in
  Hashtbl.set tool_outputs ~key:0 ~data:Types.Apply_patch;
  let model = make_base_model ~messages ~tool_outputs in
  let s = render_to_string ~w:80 ~h:12 model in
  let lines = String.split_lines s in
  List.iter lines ~f:(fun line ->
    if String.is_substring line ~substring:"Patch applied successfully!"
    then print_endline (String.strip line));
  List.iter lines ~f:(fun line ->
    if String.is_substring line ~substring:"*** Add File: foo.txt"
    then print_endline (String.strip line));
  [%expect
    {|
      Patch applied successfully!
      *** Add File: foo.txt
    |}]
;;

let%expect_test "read_file output rendering preserves contents" =
  let open Chat_tui in
  let body =
    String.concat
      ~sep:"\n"
      [ "let x = 1"
      ; "let y = 2"
      ; "---"
      ; "[File truncated]"
      ]
  in
  let messages = [ "tool_output", body ] in
  let tool_outputs = Hashtbl.create (module Int) in
  Hashtbl.set tool_outputs ~key:0 ~data:(Types.Read_file { path = Some "foo.ml" });
  let model = make_base_model ~messages ~tool_outputs in
  let s = render_to_string ~w:40 ~h:12 model in
  let lines = String.split_lines s in
  List.iter lines ~f:(fun line ->
    if String.is_substring line ~substring:"let x = 1"
    then print_endline (String.strip line));
  List.iter lines ~f:(fun line ->
    if String.is_substring line ~substring:"[File truncated]"
    then print_endline (String.strip line));
  [%expect
    {|
      let x = 1
      [File truncated]
    |}]
;;

let%expect_test "read_directory output rendering preserves entries" =
  let open Chat_tui in
  let body = String.concat ~sep:"\n" [ "."; ".."; "foo.ml"; "bar/" ] in
  let messages = [ "tool_output", body ] in
  let tool_outputs = Hashtbl.create (module Int) in
  Hashtbl.set tool_outputs ~key:0 ~data:(Types.Read_directory { path = Some "/tmp" });
  let model = make_base_model ~messages ~tool_outputs in
  let s = render_to_string ~w:20 ~h:12 model in
  String.split_lines s
  |> List.iter ~f:(fun line ->
       if
         String.is_substring line ~substring:"."
         || String.is_substring line ~substring:"foo.ml"
         || String.is_substring line ~substring:"bar/"
       then print_endline (String.strip line));
  [%expect
    {|
      .
      ..
      foo.ml
      bar/
    |}]
;;

