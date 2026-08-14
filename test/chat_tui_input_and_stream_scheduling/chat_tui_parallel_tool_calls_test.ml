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

let show_calls model =
  Chat_tui.Model.active_agent_calls model
  |> List.iter ~f:(fun call ->
    let progress =
      Chat_tui.Model.agent_call_progress_entries call
      |> List.map ~f:Chat_tui.Model.progress_entry_text
      |> String.concat
    in
    print_s
      [%sexp
        (Chat_tui.Model.agent_call_id call : string)
      , (Chat_tui.Model.agent_call_start_order call : int)
      , (progress : string)
      , (Chat_tui.Model.agent_call_is_truncated call : bool)])
;;

let page_name model =
  match Chat_tui.Model.active_page model with
  | Chat -> "Chat"
  | Agent -> "Agent"
  | Shell_security -> "Shell_security"
;;

let%expect_test "Agent calls preserve ordering, selection, and isolated progress" =
  let module Model = Chat_tui.Model in
  let model = make_model () in
  ignore
    (Model.agent_call_started
       model
       ~call_id:"call-1"
       ~name:"one"
       ~kind:`Function
       ~payload:"{}"
       ~agent_page_kind:Chat_response.Tool_execution_event.Subagent
     : bool);
  ignore
    (Model.agent_call_started
       model
       ~call_id:"call-2"
       ~name:"two"
       ~kind:`Custom
       ~payload:"input"
       ~agent_page_kind:Chat_response.Tool_execution_event.Shell_script
     : bool);
  ignore
    (Model.agent_call_progress
       model
       ~call_id:"call-1"
       { channel = `Assistant; update = Append "a" }
     : bool);
  ignore
    (Model.agent_call_progress
       model
       ~call_id:"call-2"
       { channel = `Stdout; update = Append "b" }
     : bool);
  show_calls model;
  print_endline
    (Model.selected_agent_call model |> Option.value_exn |> Model.agent_call_id);
  Model.set_active_page model Agent;
  Model.select_next_agent_call model;
  ignore
    (Model.agent_call_finished model ~outcome:Returned ~output:None ~call_id:"call-1"
     : bool);
  print_endline (page_name model);
  ignore
    (Model.agent_call_finished model ~outcome:Returned ~output:None ~call_id:"call-2"
     : bool);
  print_s
    [%sexp
      (page_name model : string), (List.length (Model.active_agent_calls model) : int)];
  [%expect
    {|
    (call-1 0 a false)
    (call-2 1 b false)
    call-1
    Agent
    (Agent 2)
    |}]
;;

let%expect_test "finishing the visible selected call returns to Chat" =
  let module Model = Chat_tui.Model in
  let model = make_model () in
  List.iter [ "one"; "two" ] ~f:(fun call_id ->
    ignore
      (Model.agent_call_started
         model
         ~call_id
         ~name:call_id
         ~kind:`Function
         ~payload:"{}"
         ~agent_page_kind:Chat_response.Tool_execution_event.Subagent
       : bool));
  Model.set_active_page model Agent;
  ignore
    (Model.agent_call_finished model ~outcome:Returned ~output:None ~call_id:"one" : bool);
  print_s
    [%sexp
      (page_name model : string)
    , (Model.selected_agent_call model |> Option.value_exn |> Model.agent_call_id
       : string)];
  [%expect {| (Agent one) |}]
;;

let%expect_test "execution events remove completed calls independently" =
  let module Execution = Chat_response.Tool_execution_event in
  let module Model = Chat_tui.Model in
  let model = make_model () in
  let apply = function
    | Execution.Started { call_id; name; kind; payload } ->
      Model.agent_call_started
        model
        ~call_id
        ~name
        ~kind
        ~payload
        ~agent_page_kind:Execution.Subagent
    | Progress { call_id; progress } -> Model.agent_call_progress model ~call_id progress
    | Trace { call_id; trace } -> Model.agent_call_trace model ~call_id trace
    | Finished { call_id; outcome = _; output = _ } ->
      Model.agent_call_finished model ~call_id ~outcome:Returned ~output:None
  in
  let events =
    [ Execution.Started
        { call_id = "one"; name = "tool"; kind = `Function; payload = "{}" }
    ; Started { call_id = "two"; name = "tool"; kind = `Function; payload = "{}" }
    ; Progress { call_id = "one"; progress = { channel = `Stdout; update = Append "a" } }
    ; Progress { call_id = "two"; progress = { channel = `Stdout; update = Append "b" } }
    ; Finished { call_id = "two"; outcome = Returned; output = None }
    ]
  in
  List.iter events ~f:(fun event -> ignore (apply event : bool));
  show_calls model;
  [%expect
    {|
    (one 0 a false)
    (two 1 b false)
    |}]
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
      { output = Res.Tool_output.Output.Text "result2"
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
      { output = Res.Tool_output.Output.Text "result1"
      ; call_id = "call-1"
      ; _type = "function_call_output"
      ; id = None
      ; status = Some "completed"
      }
  in
  ignore (Model.apply_patches m patches_out1);
  (* Print resulting messages for verification *)
  List.iter (Model.messages m) ~f:(fun (role, text) -> Printf.printf "%s: %s\n" role text);
  [%expect
    {|tool: result1
tool: result2|}]
;;
