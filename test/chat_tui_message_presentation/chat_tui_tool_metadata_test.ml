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
  let allocator =
    History_entry.Allocator.create ~namespace:"tool-metadata-test" ~next_sequence:0
    |> Result.ok_or_failwith
  in
  let entries =
    List.map history ~f:(History_entry.create ~allocator)
    |> Result.all
    |> Result.ok_or_failwith
  in
  let projection = Chat_tui.Conversation.project_entries entries in
  let messages = Chat_tui.Conversation.messages projection in
  let scroll_box = Notty_scroll_box.create Notty.I.empty in
  let model =
    Model.create
      ~history_items:entries
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
  Model.reconcile_projected_rows model (Chat_tui.Conversation.rows projection);
  Model.reconcile_messages model messages;
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

let%expect_test
    "rebuild_tool_output_index_for_items aligns tool metadata with moderated visible \
     history"
  =
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
  let visible_history =
    [ Item.Input_message
        { role = Res.Input_message.System
        ; content = [ Res.Input_message.Text { text = "policy"; _type = "input_text" } ]
        ; _type = "message"
        }
    ; Item.Function_call fc_read
    ; Item.Function_call_output fco_read
    ]
  in
  let allocator =
    History_entry.Allocator.create ~namespace:"visible-tool-metadata" ~next_sequence:0
    |> Result.ok_or_failwith
  in
  let visible_history =
    List.map visible_history ~f:(History_entry.create ~allocator)
    |> Result.all
    |> Result.ok_or_failwith
  in
  let projection = Chat_tui.Conversation.project_entries visible_history in
  let scroll_box = Notty_scroll_box.create Notty.I.empty in
  let model =
    Model.create
      ~history_items:[]
      ~messages:(Chat_tui.Conversation.messages projection)
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
  Model.reconcile_projected_rows model (Chat_tui.Conversation.rows projection);
  Model.reconcile_messages model (Chat_tui.Conversation.messages projection);
  Model.rebuild_tool_output_index_for_items model visible_history;
  let tbl = Model.tool_output_by_index model in
  List.iter [ 0; 1; 2 ] ~f:(fun idx ->
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
         Printf.printf "%d: Other name=%s\n" idx (Option.value name ~default:"<none>")));
  [%expect
    {|
      0: none
      1: none
      2: Read_file path=foo.txt
    |}]
;;

let%expect_test "lang_of_path maps common extensions" =
  let open Chat_tui.Renderer in
  let cases =
    [ "foo.ml"
    ; "foo.mli"
    ; "main.py"
    ; "lib.rs"
    ; "app.js"
    ; "component.jsx"
    ; "app.ts"
    ; "component.tsx"
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
      main.py -> python
      lib.rs -> rust
      app.js -> javascript
      component.jsx -> jsx
      app.ts -> typescript
      component.tsx -> tsx
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

let%expect_test
    "tool_output_kind: read_file args_done in same stream batch as output_item_added \
     populates path"
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
    ; arguments = ""
    ; call_id = "call-read-file"
    ; _type = "function_call"
    ; id = Some "item-123"
    ; status = Some "in_progress"
    }
  in
  let events : Res_stream.t list =
    [ Res_stream.Output_item_added
        { item = Item.Function_call fc; output_index = 0; type_ = "output_item_added" }
    ; Res_stream.Function_call_arguments_done
        { arguments = "{\"file\": \"README.md\"}"
        ; item_id = Option.value_exn fc.id
        ; output_index = 0
        ; type_ = "function_call_arguments_done"
        }
    ]
  in
  let patches = Stream.handle_events ~model:m events in
  ignore (Model.apply_patches m patches : Model.t);
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

let%expect_test "tool_output_kind: output can arrive before output_item_added (read_file)"
  =
  let module Stream = Chat_tui.Stream in
  let module Model = Chat_tui.Model in
  let module Types = Chat_tui.Types in
  let module Res = Stream.Res in
  let module Res_stream = Stream.Res_stream in
  let module Item = Res_stream.Item in
  let m = make_model () in
  let call_id = "call-read-file" in
  let out : Res.Function_call_output.t =
    { output = Res.Tool_output.Output.Text "contents"
    ; call_id
    ; _type = "function_call_output"
    ; id = None
    ; status = Some "completed"
    }
  in
  let patches_out = Stream.handle_fn_out ~model:m out in
  ignore (Model.apply_patches m patches_out : Model.t);
  let fc : Res.Function_call.t =
    { name = "read_file"
    ; arguments = "{\"file\": \"README.md\"}"
    ; call_id
    ; _type = "function_call"
    ; id = Some "item-123"
    ; status = Some "in_progress"
    }
  in
  let patches_call =
    Stream.handle_event
      ~model:m
      (Res_stream.Output_item_added
         { item = Item.Function_call fc; output_index = 0; type_ = "output_item_added" })
  in
  ignore (Model.apply_patches m patches_call : Model.t);
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
  [%expect {| Read_file path=README.md |}]
;;

let%expect_test
    "tool_output_kind: output can arrive before output_item_added (apply_patch)"
  =
  let module Stream = Chat_tui.Stream in
  let module Model = Chat_tui.Model in
  let module Types = Chat_tui.Types in
  let module Res = Stream.Res in
  let module Res_stream = Stream.Res_stream in
  let module Item = Res_stream.Item in
  let m = make_model () in
  let call_id = "call-apply-patch" in
  let out : Res.Function_call_output.t =
    { output = Res.Tool_output.Output.Text "ok"
    ; call_id
    ; _type = "function_call_output"
    ; id = None
    ; status = Some "completed"
    }
  in
  let patches_out = Stream.handle_fn_out ~model:m out in
  ignore (Model.apply_patches m patches_out : Model.t);
  let fc : Res.Function_call.t =
    { name = "apply_patch"
    ; arguments = "{\"patch\": \"*** Begin Patch*** End Patch\"}"
    ; call_id
    ; _type = "function_call"
    ; id = Some "item-123"
    ; status = Some "in_progress"
    }
  in
  let patches_call =
    Stream.handle_event
      ~model:m
      (Res_stream.Output_item_added
         { item = Item.Function_call fc; output_index = 0; type_ = "output_item_added" })
  in
  ignore (Model.apply_patches m patches_call : Model.t);
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

let%expect_test
    "tool_output_kind: parallel tool outputs stay separated (read_file + apply_patch)"
  =
  let module Stream = Chat_tui.Stream in
  let module Model = Chat_tui.Model in
  let module Types = Chat_tui.Types in
  let module Res = Stream.Res in
  let module Res_stream = Stream.Res_stream in
  let module Item = Res_stream.Item in
  let m = make_model () in
  (* Simulate two tool calls running in parallel where outputs arrive before the
     corresponding Output_item_added announcements. *)
  let call_patch = "call-apply-patch" in
  let call_read = "call-read-file" in
  let out_patch : Res.Function_call_output.t =
    { output = Res.Tool_output.Output.Text "ok"
    ; call_id = call_patch
    ; _type = "function_call_output"
    ; id = None
    ; status = Some "completed"
    }
  in
  let out_read : Res.Function_call_output.t =
    { output = Res.Tool_output.Output.Text "contents"
    ; call_id = call_read
    ; _type = "function_call_output"
    ; id = None
    ; status = Some "completed"
    }
  in
  (* Output arrives in one order... *)
  ignore (Model.apply_patches m (Stream.handle_fn_out ~model:m out_patch) : Model.t);
  ignore (Model.apply_patches m (Stream.handle_fn_out ~model:m out_read) : Model.t);
  (* ...then the tool calls are announced in the opposite order. *)
  let fc_read : Res.Function_call.t =
    { name = "read_file"
    ; arguments = "{\"file\": \"README.md\"}"
    ; call_id = call_read
    ; _type = "function_call"
    ; id = Some "item-read"
    ; status = Some "in_progress"
    }
  in
  let fc_patch : Res.Function_call.t =
    { name = "apply_patch"
    ; arguments = "{\"patch\": \"*** Begin Patch*** End Patch\"}"
    ; call_id = call_patch
    ; _type = "function_call"
    ; id = Some "item-patch"
    ; status = Some "in_progress"
    }
  in
  let apply ev =
    ignore (Model.apply_patches m (Stream.handle_event ~model:m ev) : Model.t)
  in
  apply
    (Res_stream.Output_item_added
       { item = Item.Function_call fc_read
       ; output_index = 0
       ; type_ = "output_item_added"
       });
  apply
    (Res_stream.Output_item_added
       { item = Item.Function_call fc_patch
       ; output_index = 0
       ; type_ = "output_item_added"
       });
  let tbl = Model.tool_output_by_index m in
  let show idx =
    match Hashtbl.find tbl idx with
    | None -> Printf.printf "%d: none\n" idx
    | Some kind ->
      (match kind with
       | Types.Apply_patch -> Printf.printf "%d: Apply_patch\n" idx
       | Types.Read_file { path } ->
         Printf.printf "%d: Read_file path=%s\n" idx (Option.value path ~default:"<none>")
       | Types.Read_directory { path } ->
         Printf.printf
           "%d: Read_directory path=%s\n"
           idx
           (Option.value path ~default:"<none>")
       | Types.Other { name } ->
         Printf.printf "%d: Other name=%s\n" idx (Option.value name ~default:"<none>"))
  in
  List.iter [ 0; 1 ] ~f:show;
  [%expect
    {|
      0: Apply_patch
      1: Read_file path=README.md
    |}]
;;

let%expect_test "entry-id tool output keeps read_file metadata immediately" =
  let module Stream = Chat_tui.Stream in
  let module Model = Chat_tui.Model in
  let module Types = Chat_tui.Types in
  let module Res = Stream.Res in
  let module Res_stream = Stream.Res_stream in
  let m = make_model () in
  let call_id = "call-entry-read-file" in
  let fc : Res.Function_call.t =
    { name = "read_file"
    ; arguments = {|{"file":"lib/immediate.ml"}|}
    ; call_id
    ; _type = "function_call"
    ; id = Some "provider-item"
    ; status = Some "completed"
    }
  in
  let patches =
    Stream.handle_event
      ~model:m
      (Res_stream.Output_item_added
         { item = Res_stream.Item.Function_call fc
         ; output_index = 0
         ; type_ = "output_item_added"
         })
  in
  ignore (Model.apply_patches m patches : Model.t);
  let entry_id =
    History_entry.Id.create ~namespace:"tool-metadata" ~sequence:0
    |> Result.ok_or_failwith
  in
  let output =
    Res.Item.Function_call_output
      { output = Res.Tool_output.Output.Text "let immediate = true"
      ; call_id
      ; _type = "function_call_output"
      ; id = None
      ; status = Some "completed"
      }
  in
  let patches = Stream.handle_tool_out ~model:m ~entry_id output in
  ignore (Model.apply_patches m patches : Model.t);
  let id = History_entry.Id.to_string entry_id in
  let row_id = (Hashtbl.find_exn (Model.msg_buffers m) id).row_id in
  (match Model.tool_output_for_row m ~id:row_id with
   | Some (Types.Read_file { path }) ->
     print_endline (Option.value path ~default:"<none>")
   | None | Some _ -> print_endline "wrong classification");
  [%expect {| lib/immediate.ml |}]
;;
