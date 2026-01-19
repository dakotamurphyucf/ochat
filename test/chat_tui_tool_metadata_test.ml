open Core

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
    ~tool_output_by_index:(Hashtbl.create (module Int))
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

let%expect_test "tool_output_kind: streaming read_file populates path" =
  let module Stream = Chat_tui.Stream in
  let module Model = Chat_tui.Model in
  let module Types = Chat_tui.Types in
  let module Res = Stream.Res in
  let module Res_stream = Stream.Res_stream in
  let module Item = Res_stream.Item in
  let m = make_model () in
  let fc : Res.Function_call.t =
    { name = "read_file"
    ; arguments = "{\"file\": \"lib/foo.ml\"}"
    ; call_id = "call-read-file"
    ; _type = "function_call"
    ; id = None
    ; status = Some "in_progress"
    }
  in
  let apply ev =
    let patches = Stream.handle_event ~model:m ev in
    ignore (Model.apply_patches m patches : Model.t)
  in
  apply
    (Res_stream.Output_item_added
       { item = Item.Function_call fc; output_index = 0; type_ = "output_item_added" });
  apply
    (Res_stream.Function_call_arguments_delta
       { delta = fc.arguments
       ; item_id = fc.call_id
       ; output_index = 0
       ; type_ = "function_call_arguments_delta"
       });
  apply
    (Res_stream.Function_call_arguments_done
       { arguments = fc.arguments
       ; item_id = fc.call_id
       ; output_index = 0
       ; type_ = "function_call_arguments_done"
       });
  let out : Res.Function_call_output.t =
    { output = Res.Tool_output.Output.Text "file-contents"
    ; call_id = fc.call_id
    ; _type = "function_call_output"
    ; id = None
    ; status = Some "completed"
    }
  in
  let patches_out = Stream.handle_fn_out ~model:m out in
  ignore (Model.apply_patches m patches_out : Model.t);
  let tbl = Model.tool_output_by_index m in
  (match Hashtbl.find tbl 0 with
   | None -> print_endline "none"
   | Some kind ->
     (match kind with
      | Types.Read_file { path } ->
        Printf.printf "Read_file path=%s\n" (Option.value path ~default:"<none>")
      | Types.Apply_patch -> print_endline "Apply_patch"
      | Types.Read_directory { path } ->
        Printf.printf "Read_directory path=%s\n" (Option.value path ~default:"<none>")
      | Types.Other { name } ->
        Printf.printf "Other name=%s\n" (Option.value name ~default:"<none>")));
  [%expect {| Read_file path=lib/foo.ml |}]
;;

let%expect_test "tool_output_kind: streaming read_directory populates path" =
  let module Stream = Chat_tui.Stream in
  let module Model = Chat_tui.Model in
  let module Types = Chat_tui.Types in
  let module Res = Stream.Res in
  let module Res_stream = Stream.Res_stream in
  let module Item = Res_stream.Item in
  let m = make_model () in
  let fc : Res.Function_call.t =
    { name = "read_directory"
    ; arguments = "{\"path\": \"/tmp\"}"
    ; call_id = "call-read-dir"
    ; _type = "function_call"
    ; id = None
    ; status = Some "in_progress"
    }
  in
  let apply ev =
    let patches = Stream.handle_event ~model:m ev in
    ignore (Model.apply_patches m patches : Model.t)
  in
  apply
    (Res_stream.Output_item_added
       { item = Item.Function_call fc; output_index = 0; type_ = "output_item_added" });
  apply
    (Res_stream.Function_call_arguments_delta
       { delta = fc.arguments
       ; item_id = fc.call_id
       ; output_index = 0
       ; type_ = "function_call_arguments_delta"
       });
  apply
    (Res_stream.Function_call_arguments_done
       { arguments = fc.arguments
       ; item_id = fc.call_id
       ; output_index = 0
       ; type_ = "function_call_arguments_done"
       });
  let out : Res.Function_call_output.t =
    { output = Res.Tool_output.Output.Text "listing"
    ; call_id = fc.call_id
    ; _type = "function_call_output"
    ; id = None
    ; status = Some "completed"
    }
  in
  let patches_out = Stream.handle_fn_out ~model:m out in
  ignore (Model.apply_patches m patches_out : Model.t);
  let tbl = Model.tool_output_by_index m in
  (match Hashtbl.find tbl 0 with
   | None -> print_endline "none"
   | Some kind ->
     (match kind with
      | Types.Read_directory { path } ->
        Printf.printf "Read_directory path=%s\n" (Option.value path ~default:"<none>")
      | Types.Read_file { path } ->
        Printf.printf "Read_file path=%s\n" (Option.value path ~default:"<none>")
      | Types.Apply_patch -> print_endline "Apply_patch"
      | Types.Other { name } ->
        Printf.printf "Other name=%s\n" (Option.value name ~default:"<none>")));
  [%expect {| Read_directory path=/tmp |}]
;;

let%expect_test "tool_output_kind: streaming apply_patch is classified" =
  let module Stream = Chat_tui.Stream in
  let module Model = Chat_tui.Model in
  let module Types = Chat_tui.Types in
  let module Res = Stream.Res in
  let module Res_stream = Stream.Res_stream in
  let module Item = Res_stream.Item in
  let m = make_model () in
  let fc : Res.Function_call.t =
    { name = "apply_patch"
    ; arguments = "{\"patch\": \"*** Begin Patch*** End Patch\"}"
    ; call_id = "call-apply-patch"
    ; _type = "function_call"
    ; id = None
    ; status = Some "in_progress"
    }
  in
  let apply ev =
    let patches = Stream.handle_event ~model:m ev in
    ignore (Model.apply_patches m patches : Model.t)
  in
  apply
    (Res_stream.Output_item_added
       { item = Item.Function_call fc; output_index = 0; type_ = "output_item_added" });
  apply
    (Res_stream.Function_call_arguments_delta
       { delta = fc.arguments
       ; item_id = fc.call_id
       ; output_index = 0
       ; type_ = "function_call_arguments_delta"
       });
  apply
    (Res_stream.Function_call_arguments_done
       { arguments = fc.arguments
       ; item_id = fc.call_id
       ; output_index = 0
       ; type_ = "function_call_arguments_done"
       });
  let out : Res.Function_call_output.t =
    { output = Res.Tool_output.Output.Text "ok"
    ; call_id = fc.call_id
    ; _type = "function_call_output"
    ; id = None
    ; status = Some "completed"
    }
  in
  let patches_out = Stream.handle_fn_out ~model:m out in
  ignore (Model.apply_patches m patches_out : Model.t);
  let tbl = Model.tool_output_by_index m in
  (match Hashtbl.find tbl 0 with
   | None -> print_endline "none"
   | Some kind ->
     (match kind with
      | Types.Apply_patch -> print_endline "Apply_patch"
      | Types.Read_file { path } ->
        Printf.printf "Read_file path=%s\n" (Option.value path ~default:"<none>")
      | Types.Read_directory { path } ->
        Printf.printf "Read_directory path=%s\n" (Option.value path ~default:"<none>")
      | Types.Other { name } ->
        Printf.printf "Other name=%s\n" (Option.value name ~default:"<none>")));
  [%expect {| Apply_patch |}]
;;

let%expect_test "rebuild_tool_output_index classifies history items" =
  let module Model = Chat_tui.Model in
  let module Types = Chat_tui.Types in
  let module Res = Openai.Responses in
  let module Item = Res.Item in
  let fc_read : Res.Function_call.t =
    { name = "read_file"
    ; arguments = "{\"file\": \"foo.txt\"}"
    ; call_id = "hist-read"
    ; _type = "function_call"
    ; id = None
    ; status = Some "completed"
    }
  in
  let fco_read : Res.Function_call_output.t =
    { output = Res.Tool_output.Output.Text "contents"
    ; call_id = fc_read.call_id
    ; _type = "function_call_output"
    ; id = None
    ; status = Some "completed"
    }
  in
  let fc_dir : Res.Function_call.t =
    { name = "read_directory"
    ; arguments = "{\"path\": \"/var/log\"}"
    ; call_id = "hist-dir"
    ; _type = "function_call"
    ; id = None
    ; status = Some "completed"
    }
  in
  let fco_dir : Res.Function_call_output.t =
    { output = Res.Tool_output.Output.Text "listing"
    ; call_id = fc_dir.call_id
    ; _type = "function_call_output"
    ; id = None
    ; status = Some "completed"
    }
  in
  let history =
    [ Item.Function_call fc_read
    ; Item.Function_call_output fco_read
    ; Item.Function_call fc_dir
    ; Item.Function_call_output fco_dir
    ]
  in
  let messages = Chat_tui.Conversation.of_history history in
  let scroll_box = Notty_scroll_box.create Notty.I.empty in
  let model =
    Model.create
      ~history_items:history
      ~messages
      ~input_line:""
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
      ~mode:Model.Insert
      ~draft_mode:Model.Plain
      ~selected_msg:None
      ~undo_stack:[]
      ~redo_stack:[]
      ~cmdline:""
      ~cmdline_cursor:0
  in
  Model.rebuild_tool_output_index model;
  let tbl = Model.tool_output_by_index model in
  let show idx =
    match Hashtbl.find tbl idx with
    | None -> Printf.printf "%d: none\n" idx
    | Some kind ->
      (match kind with
       | Types.Read_file { path } ->
         Printf.printf "%d: Read_file path=%s\n" idx (Option.value path ~default:"<none>")
       | Types.Read_directory { path } ->
         Printf.printf
           "%d: Read_directory path=%s\n"
           idx
           (Option.value path ~default:"<none>")
       | Types.Apply_patch -> Printf.printf "%d: Apply_patch\n" idx
       | Types.Other { name } ->
         Printf.printf "%d: Other name=%s\n" idx (Option.value name ~default:"<none>"))
  in
  List.iter [ 0; 1; 2; 3 ] ~f:show;
  [%expect
    {|
      0: none
      1: Read_file path=foo.txt
      2: none
      3: Read_directory path=/var/log
    |}]
;;

let%expect_test "lang_of_path maps common extensions" =
  let open Chat_tui.Renderer in
  let cases =
    [ "foo.ml"
    ; "foo.mli"
    ; "README.md"
    ; "data.json"
    ; "script.sh"
    ; "notes.txt"
    ; "noext"
    ; "UPPER.ML"
    ]
  in
  List.iter cases ~f:(fun path ->
    let lang = lang_of_path path |> Option.value ~default:"<none>" in
    Printf.printf "%s -> %s\n" path lang);
  [%expect
    {|
      foo.ml -> ocaml
      foo.mli -> ocaml
      README.md -> markdown
      data.json -> json
      script.sh -> bash
      notes.txt -> <none>
      noext -> <none>
      UPPER.ML -> ocaml
    |}]
;;

let%expect_test
    "tool_output_kind: streaming read_file with item id populates path immediately"
  =
  let module Stream = Chat_tui.Stream in
  let module Model = Chat_tui.Model in
  let module Types = Chat_tui.Types in
  let module Res = Stream.Res in
  let module Res_stream = Stream.Res_stream in
  let module Item = Res_stream.Item in
  let m = make_model () in
  let fc : Res.Function_call.t =
    { name = "read_file"
    ; arguments = "{\"file\": \"lib/foo.ml\"}"
    ; call_id = "call-read-file"
    ; _type = "function_call"
    ; id = Some "item-123"
    ; status = Some "in_progress"
    }
  in
  let apply ev =
    let patches = Stream.handle_event ~model:m ev in
    ignore (Model.apply_patches m patches : Model.t)
  in
  apply
    (Res_stream.Output_item_added
       { item = Item.Function_call fc; output_index = 0; type_ = "output_item_added" });
  apply
    (Res_stream.Function_call_arguments_delta
       { delta = fc.arguments
       ; item_id = Option.value_exn fc.id
       ; output_index = 0
       ; type_ = "function_call_arguments_delta"
       });
  apply
    (Res_stream.Function_call_arguments_done
       { arguments = fc.arguments
       ; item_id = Option.value_exn fc.id
       ; output_index = 0
       ; type_ = "function_call_arguments_done"
       });
  let out : Res.Function_call_output.t =
    { output = Res.Tool_output.Output.Text "file-contents"
    ; call_id = fc.call_id
    ; _type = "function_call_output"
    ; id = None
    ; status = Some "completed"
    }
  in
  let patches_out = Stream.handle_fn_out ~model:m out in
  ignore (Model.apply_patches m patches_out : Model.t);
  let tbl = Model.tool_output_by_index m in
  (match Hashtbl.find tbl 1 with
   | None -> print_endline "none"
   | Some kind ->
     (match kind with
      | Types.Read_file { path } ->
        Printf.printf "Read_file path=%s\n" (Option.value path ~default:"<none>")
      | Types.Apply_patch -> print_endline "Apply_patch"
      | Types.Read_directory { path } ->
        Printf.printf "Read_directory path=%s\n" (Option.value path ~default:"<none>")
      | Types.Other { name } ->
        Printf.printf "Other name=%s\n" (Option.value name ~default:"<none>")));
  [%expect {| Read_file path=lib/foo.ml |}]
;;

let%expect_test "tool_output_kind: read_file output can arrive before arguments_done" =
  let module Stream = Chat_tui.Stream in
  let module Model = Chat_tui.Model in
  let module Types = Chat_tui.Types in
  let module Res = Stream.Res in
  let module Res_stream = Stream.Res_stream in
  let module Item = Res_stream.Item in
  let m = make_model () in
  let fc : Res.Function_call.t =
    { name = "read_file"
    ; arguments = ""
    ; call_id = "call-read-file"
    ; _type = "function_call"
    ; id = Some "item-123"
    ; status = Some "in_progress"
    }
  in
  let apply ev =
    let patches = Stream.handle_event ~model:m ev in
    ignore (Model.apply_patches m patches : Model.t)
  in
  apply
    (Res_stream.Output_item_added
       { item = Item.Function_call fc; output_index = 0; type_ = "output_item_added" });
  let out : Res.Function_call_output.t =
    { output = Res.Tool_output.Output.Text "contents"
    ; call_id = fc.call_id
    ; _type = "function_call_output"
    ; id = None
    ; status = Some "completed"
    }
  in
  let patches_out = Stream.handle_fn_out ~model:m out in
  ignore (Model.apply_patches m patches_out : Model.t);
  apply
    (Res_stream.Function_call_arguments_done
       { arguments = "{\"file\": \"README.md\"}"
       ; item_id = Option.value_exn fc.id
       ; output_index = 0
       ; type_ = "function_call_arguments_done"
       });
  let tbl = Model.tool_output_by_index m in
  (match Hashtbl.find tbl 1 with
   | None -> print_endline "none"
   | Some kind ->
     (match kind with
      | Types.Read_file { path } ->
        Printf.printf "Read_file path=%s\n" (Option.value path ~default:"<none>")
      | Types.Apply_patch -> print_endline "Apply_patch"
      | Types.Read_directory { path } ->
        Printf.printf "Read_directory path=%s\n" (Option.value path ~default:"<none>")
      | Types.Other { name } ->
        Printf.printf "Other name=%s\n" (Option.value name ~default:"<none>")));
  [%expect {| Read_file path=README.md |}]
;;
