open Core

(* Helper: construct minimal model suitable for stream tests *)

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

let%expect_test "parallel_tool_calls_basic_flow" =
  let module Stream = Chat_tui.Stream in
  let module Model = Chat_tui.Model in
  let module Res = Stream.Res in
  let module Res_stream = Stream.Res_stream in
  let module Item = Res_stream.Item in
  let m = make_model () in
  (* Construct two function-call items *)
  let fc1 : Res.Function_call.t =
    { name = "echo1"
    ; arguments = ""
    ; call_id = "call-1"
    ; _type = "function_call"
    ; id = None
    ; status = Some "in_progress"
    }
  in
  let fc2 : Res.Function_call.t =
    { name = "echo2"
    ; arguments = ""
    ; call_id = "call-2"
    ; _type = "function_call"
    ; id = None
    ; status = Some "in_progress"
    }
  in
  (* Helper to apply a single event *)
  let apply ev =
    let patches = Stream.handle_event ~model:m ev in
    ignore (Model.apply_patches m patches)
  in
  (* Announce first call and its arguments *)
  apply
    (Res_stream.Output_item_added
       { item = Item.Function_call fc1; output_index = 0; type_ = "output_item_added" });
  apply
    (Res_stream.Function_call_arguments_delta
       { delta = "\"foo\""
       ; item_id = "call-1"
       ; output_index = 0
       ; type_ = "function_call_arguments_delta"
       });
  (* Interleave second call announcement *)
  apply
    (Res_stream.Output_item_added
       { item = Item.Function_call fc2; output_index = 1; type_ = "output_item_added" });
  (* Finish arguments for call-1 *)
  apply
    (Res_stream.Function_call_arguments_done
       { arguments = "\"foo\""
       ; item_id = "call-1"
       ; output_index = 0
       ; type_ = "function_call_arguments_done"
       });
  (* Stream arguments for second call *)
  apply
    (Res_stream.Function_call_arguments_delta
       { delta = "\"bar\""
       ; item_id = "call-2"
       ; output_index = 1
       ; type_ = "function_call_arguments_delta"
       });
  apply
    (Res_stream.Function_call_arguments_done
       { arguments = "\"bar\""
       ; item_id = "call-2"
       ; output_index = 1
       ; type_ = "function_call_arguments_done"
       });
  (* Inject function outputs – intentionally out-of-order *)
  let patches_out2 =
    Stream.handle_fn_out
      ~model:m
      { output = "result2"
      ; call_id = "call-2"
      ; _type = "function_call_output"
      ; id = None
      ; status = Some "completed"
      }
  in
  ignore (Model.apply_patches m patches_out2);
  let patches_out1 =
    Stream.handle_fn_out
      ~model:m
      { output = "result1"
      ; call_id = "call-1"
      ; _type = "function_call_output"
      ; id = None
      ; status = Some "completed"
      }
  in
  ignore (Model.apply_patches m patches_out1);
  (* Print resulting messages for verification *)
  List.iter m.messages ~f:(fun (role, text) -> Printf.printf "%s: %s\n" role text);
  [%expect
    {|tool: result1
tool: result2|}]
;;
