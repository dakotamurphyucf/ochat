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
  | Work -> "Work"
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

let%expect_test "parallel neutral drafts and actual commits keep host results separate" =
  let module P = History_entry.Payload in
  let ok = Result.ok_or_failwith in
  let allocator =
    History_entry.Allocator.create ~namespace:"parallel" ~next_sequence:0 |> ok
  in
  let model = make_model () in
  let runtime = Chat_tui.App_runtime.create ~history_allocator:allocator ~model () in
  let throttle = Chat_tui.Redraw_throttle.create ~fps:60. ~enqueue_redraw:ignore in
  let scope =
    Transcript.Scope.create
      ~source:(Transcript.Source_id.of_string "source" |> ok)
      ~attempt:(Transcript.Attempt_id.of_string "attempt" |> ok)
      ~relation:Root
    |> ok
  in
  let first = History_entry.Allocator.allocate allocator |> ok in
  let second = History_entry.Allocator.allocate allocator |> ok in
  let descriptor entry_id name =
    Transcript.Item.create
      ~scope
      ~id:(Transcript.Item_id.of_string name |> ok)
      ~entry_id:(Some entry_id)
      ~header:(Some (Call Function))
      ~call_name:(Some name)
    |> ok
  in
  let first_item = descriptor first "echo1" in
  let second_item = descriptor second "echo2" in
  List.iter
    [ first_item, "\"foo\""; second_item, "\"bar\"" ]
    ~f:(fun (item, input) ->
      List.iter
        [ Transcript.Stream.Item_announced item
        ; Changed { target = Call_input item; change = Replace input }
        ]
        ~f:(fun view ->
          let event =
            Transcript.Stream.create view ~limits:Document_schema.Limits.default |> ok
          in
          Chat_tui.App_stream_apply.apply_transcript_event
            runtime
            throttle
            ~viewport_height:20
            event
          |> ok));
  printf
    "canonical before commit: %d\n"
    (List.length (Chat_tui.Model.history_items model));
  let payload semantic =
    P.Semantic.create semantic ~metadata:P.Metadata.empty |> ok |> P.authored
  in
  let call id name input =
    History_entry.create_with_id
      ~id
      (payload
         (Call
            { kind = Function
            ; name
            ; namespace = Absent
            ; input_bytes = input
            ; async = Absent
            }))
  in
  let result target text =
    History_entry.create
      ~allocator
      (payload (Result { relation = Bound target; kind = Function; output = Text text }))
    |> ok
  in
  List.iter
    [ call first "echo1" "\"foo\""
    ; call second "echo2" "\"bar\""
    ; result second "result2"
    ; result first "result1"
    ]
    ~f:(Chat_tui.App_stream_apply.apply_history_committed runtime throttle);
  List.iter (Chat_tui.Model.messages model) ~f:(fun (role, text) ->
    printf "%s: %s\n" role text);
  [%expect
    {|
    canonical before commit: 0
    tool: echo1("foo")
    tool: echo2("bar")
    tool_output: result2
    tool_output: result1
  |}]
;;
