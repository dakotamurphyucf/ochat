open Core
module Execution = Chat_response.Tool_execution_event
module For_testing = Chat_tui.App_streaming.For_testing
module Res = Openai.Responses

let make_model () =
  Chat_tui.Model.create
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
    ~scroll_box:(Notty_scroll_box.create Notty.I.empty)
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

let text_delta text =
  Res.Response_stream.Output_text_delta
    { content_index = 0
    ; delta = text
    ; item_id = "message"
    ; output_index = 0
    ; type_ = "response.output_text.delta"
    }
;;

let tool_output call_id text =
  Res.Item.Function_call_output
    { output = Res.Tool_output.Output.Text text
    ; call_id
    ; _type = "function_call_output"
    ; id = None
    ; status = None
    }
;;

let custom_tool_output call_id text =
  Res.Item.Custom_tool_call_output
    { output = Res.Tool_output.Output.Text text
    ; call_id
    ; _type = "custom_tool_call_output"
    ; id = None
    }
;;

let history_entry item =
  let allocator =
    History_entry.Allocator.create ~namespace:"streaming-fixture" ~next_sequence:0
    |> Result.ok_or_failwith
  in
  History_entry.create ~allocator item |> Result.ok_or_failwith
;;

let describe_event = function
  | `Sourced_stream (op_id, sourced) ->
    (match sourced.Chat_response.Sourced_response_event.event with
     | Res.Response_stream.Output_text_delta { delta; _ } ->
       sprintf "%d:text:%s" op_id delta
     | _ -> sprintf "%d:sourced" op_id)
  | `Sourced_stream_batch (op_id, sourced) ->
    sprintf "%d:text-batch:%d" op_id (List.length sourced)
  | `Tool_execution
      ( op_id
      , Execution.Progress { call_id; progress = { channel = _; update = Append text } }
      ) -> sprintf "%d:progress:%s:%s" op_id call_id text
  | `Tool_execution (op_id, Execution.Started { call_id; _ }) ->
    sprintf "%d:started:%s" op_id call_id
  | `Tool_execution (op_id, Execution.Finished { call_id; _ }) ->
    sprintf "%d:finished:%s" op_id call_id
  | `Tool_execution (op_id, Execution.Progress { call_id; progress = _ }) ->
    sprintf "%d:replace:%s" op_id call_id
  | `Tool_output (op_id, entry) ->
    (match History_entry.item entry with
     | Res.Item.Function_call_output output -> sprintf "%d:output:%s" op_id output.call_id
     | Res.Item.Custom_tool_call_output output ->
       sprintf "%d:custom-output:%s" op_id output.call_id
     | _ -> sprintf "%d:tool-output" op_id)
  | `Streaming_done (op_id, _) -> sprintf "%d:done" op_id
  | _ -> "other"
;;

let drain stream =
  let rec loop acc =
    match Eio.Stream.take_nonblocking stream with
    | None -> List.rev acc
    | Some event -> loop (event :: acc)
  in
  loop []
;;

let%expect_test "finish flushes deltas, progress, and output before done" =
  Eio_main.run
  @@ fun _env ->
  let stream : Chat_tui.App_events.internal_event Eio.Stream.t = Eio.Stream.create 16 in
  For_testing.finish
    ~internal_stream:stream
    ~op_id:7
    ~events:
      [ Sourced_stream (Chat_response.Sourced_response_event.outer (text_delta "final"))
      ; Tool_execution
          (Execution.Progress
             { call_id = "call"; progress = { channel = `Stdout; update = Append "a" } })
      ; Tool_execution
          (Execution.Progress
             { call_id = "call"; progress = { channel = `Stdout; update = Append "b" } })
      ; Tool_output (history_entry (custom_tool_output "call" "result"))
      ]
    ~items:[];
  drain stream |> List.iter ~f:(fun event -> print_endline (describe_event event));
  [%expect
    {|
    7:text:final
    7:progress:call:ab
    7:custom-output:call
    7:done
    |}]
;;

let%expect_test "coalescing preserves per-call lifecycle order" =
  Eio_main.run
  @@ fun _env ->
  let stream : Chat_tui.App_events.internal_event Eio.Stream.t = Eio.Stream.create 16 in
  let started call_id =
    For_testing.Tool_execution
      (Execution.Started { call_id; name = call_id; kind = `Function; payload = "{}" })
  in
  let progress call_id text =
    For_testing.Tool_execution
      (Execution.Progress
         { call_id; progress = { channel = `Activity; update = Append text } })
  in
  let finished call_id =
    For_testing.Tool_execution
      (Execution.Finished { call_id; outcome = Returned; output = None })
  in
  For_testing.finish
    ~internal_stream:stream
    ~op_id:9
    ~events:
      [ started "one"
      ; progress "one" "a"
      ; progress "one" "b"
      ; started "two"
      ; progress "two" "x"
      ; progress "one" "c"
      ; Tool_output (history_entry (tool_output "two" "result-two"))
      ; finished "two"
      ; Tool_output (history_entry (tool_output "one" "result-one"))
      ; finished "one"
      ]
    ~items:[];
  drain stream |> List.iter ~f:(fun event -> print_endline (describe_event event));
  [%expect
    {|
    9:started:one
    9:progress:one:ab
    9:started:two
    9:progress:two:x
    9:progress:one:c
    9:output:two
    9:finished:two
    9:output:one
    9:finished:one
    9:done
    |}]
;;

let with_reducer
      ?(initial_size = 80, 24)
      ?(before_run = fun ~env:_ ~model:_ ~runtime:_ ~internal:_ -> ())
      f
  =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun ui_sw ->
  let model = make_model () in
  let input : Chat_tui.App_events.input_event Eio.Stream.t = Eio.Stream.create 128 in
  let internal : Chat_tui.App_events.internal_event Eio.Stream.t =
    Eio.Stream.create 256
  in
  let redraw = Eio.Stream.create 1 in
  let enqueued = ref 0 in
  let drawn = ref 0 in
  let size = ref initial_size in
  let resize_layout = ref None in
  let throttler =
    Chat_tui.Redraw_throttle.create ~fps:60. ~enqueue_redraw:(fun () -> incr enqueued)
  in
  let ui : Chat_tui.App_context.Ui.t =
    { term = (Obj.magic 0 : Notty_eio.Term.t)
    ; size = (fun () -> !size)
    ; throttler
    ; redraw = (fun () -> incr drawn)
    ; redraw_immediate = (fun () -> incr drawn)
    ; latest_frame_generation = (fun () -> 0)
    ; resize_and_redraw =
        (fun ~size:new_size ~layout ->
          incr drawn;
          resize_layout := Some (new_size, layout))
    ; render_current_with_layout =
        (fun ~size:new_size ~layout ->
          incr drawn;
          resize_layout := Some (new_size, layout))
    }
  in
  let cwd = Eio.Stdenv.cwd env in
  let services : Chat_tui.App_context.Services.t =
    { env
    ; ui_sw
    ; cwd
    ; cache = Chat_response.Cache.create ~max_size:1 ()
    ; datadir = cwd
    ; session = None
    }
  in
  let streams : Chat_tui.App_context.Streams.t = { input; internal; redraw } in
  let shared : Chat_tui.App_context.Resources.t = { services; streams; ui } in
  let runtime =
    Chat_tui.App_runtime.create
      ~agent_page_classifications:
        [ "worker", Chat_response.Tool_execution_event.Subagent
        ; "research", Chat_response.Tool_execution_event.Subagent
        ; "process", Chat_response.Tool_execution_event.Shell_script
        ]
      ~model
      ()
  in
  let streaming : Chat_tui.App_streaming.Context.t =
    { shared
    ; allocator = runtime.history_allocator
    ; cfg = Chat_response.Config.default
    ; tools = []
    ; tool_tbl = Hashtbl.create (module String)
    ; moderator = None
    ; safe_point_input = Some (Chat_tui.App_runtime.safe_point_input_source runtime)
    ; parallel_tool_calls = true
    ; history_compaction = false
    }
  in
  let submit : Chat_tui.App_submit.Context.t =
    { runtime; streaming; start_streaming = (fun ~history:_ ~op_id:_ -> ()) }
  in
  let compaction : Chat_tui.App_compaction.Context.t = { shared; runtime } in
  let context : Chat_tui.App_reducer.Context.t =
    { runtime; shared; submit; compaction; cancelled = Chat_tui.App_streaming.Cancelled }
  in
  before_run ~env ~model ~runtime ~internal;
  let finished, resolve_finished = Eio.Promise.create () in
  Eio.Fiber.fork ~sw:ui_sw (fun () ->
    Eio.Promise.resolve resolve_finished (Chat_tui.App_reducer.run context));
  let pump_until ?(max_iters = 2_000) predicate =
    let rec loop remaining =
      if predicate ()
      then ()
      else if remaining = 0
      then failwith "timeout waiting for reducer"
      else (
        Eio.Fiber.yield ();
        loop (remaining - 1))
    in
    loop max_iters
  in
  let send event = Eio.Stream.add internal event in
  let send_input event = Eio.Stream.add input event in
  f
    ~model
    ~runtime
    ~ui_sw
    ~throttler
    ~enqueued
    ~drawn
    ~size
    ~resize_layout
    ~send
    ~send_input
    ~pump_until;
  (match runtime.Chat_tui.App_runtime.op with
   | None -> ()
   | Some (Chat_tui.App_runtime.Streaming { id; _ } | Starting_streaming { id }) ->
     send (`Streaming_done (id, Chat_tui.Model.history_items model));
     pump_until (fun () -> Option.is_none runtime.Chat_tui.App_runtime.op)
   | Some (Chat_tui.App_runtime.Compacting { id; _ } | Starting_compaction { id }) ->
     send (`Compaction_done (id, Chat_tui.Model.history_items model));
     pump_until (fun () -> Option.is_none runtime.Chat_tui.App_runtime.op));
  Chat_tui.Model.set_mode model Chat_tui.Model.Normal;
  Eio.Stream.add input (`Key (`Escape, []));
  ignore (Eio.Promise.await finished : bool)
;;

let%expect_test "resize snapshots multiline input and history layout once" =
  with_reducer
  @@ fun ~model
       ~runtime:_
       ~ui_sw:_
       ~throttler:_
       ~enqueued:_
       ~drawn
       ~size
       ~resize_layout
       ~send
       ~send_input:_
       ~pump_until ->
  let input = "This draft wraps onto several rows when the terminal becomes narrow." in
  Chat_tui.Model.set_input_line model input;
  Chat_tui.Model.set_cursor_pos model (String.length input);
  let before = !drawn in
  size := 24, 18;
  send `Resize;
  send (`Resize_settled 1);
  pump_until (fun () -> !drawn > before);
  let rendered_size, actual = Option.value_exn !resize_layout in
  let expected = Chat_tui.Chat_page_layout.compute ~screen_w:24 ~screen_h:18 ~model in
  print_s
    [%sexp
      (( rendered_size
       , (actual.input_box_height, actual.history_height, actual.scroll_height)
       , (expected.input_box_height, expected.history_height, expected.scroll_height)
       , !drawn - before )
       : (int * int) * (int * int * int) * (int * int * int) * int)];
  [%expect {| ((24 18) (6 11 10) (6 11 10) 1) |}]
;;

let%expect_test "unchanged resize notification does not redraw" =
  with_reducer
  @@ fun ~model:_
       ~runtime:_
       ~ui_sw:_
       ~throttler:_
       ~enqueued:_
       ~drawn
       ~size
       ~resize_layout:_
       ~send
       ~send_input:_
       ~pump_until ->
  let before = !drawn in
  send `Resize;
  send (`Resize_settled 1);
  size := 24, 18;
  send `Resize;
  send (`Resize_settled 2);
  pump_until (fun () -> !drawn > before);
  print_s [%sexp (!drawn - before : int)];
  [%expect {| 1 |}]
;;

let%expect_test "transient startup resize does not relayout warm history" =
  with_reducer
  @@ fun ~model:_
       ~runtime:_
       ~ui_sw:_
       ~throttler:_
       ~enqueued:_
       ~drawn
       ~size
       ~resize_layout:_
       ~send
       ~send_input:_
       ~pump_until ->
  let before = !drawn in
  size := 96, 24;
  send `Resize;
  size := 80, 24;
  send (`Resize_settled 1);
  size := 81, 24;
  send `Resize;
  send (`Resize_settled 2);
  pump_until (fun () -> !drawn > before);
  print_s [%sexp (!drawn - before : int)];
  [%expect {| 1 |}]
;;

let%expect_test "loading resize reconciles a provisional startup width" =
  let startup_config =
    Chat_tui.Chat_render_worker_runtime.Config.create
      ~custom_grammars:[]
      ~theme_generation:0
      ~grammar_generation:0
  in
  with_reducer ~initial_size:(72, 24) ~before_run:(fun ~env ~model ~runtime ~internal ->
    Chat_tui.App_runtime.arm_startup_render
      runtime
      ~domain_mgr:(Eio.Stdenv.domain_mgr env)
      ~config:startup_config
      ~code_cache_capacity:8;
    Chat_tui.Renderer_page_chat.prepare_startup_history ~size:(40, 24) ~model;
    Eio.Stream.add internal `Resize;
    Eio.Stream.add internal (`Resize_settled 1))
  @@ fun ~model
       ~runtime:_
       ~ui_sw:_
       ~throttler:_
       ~enqueued:_
       ~drawn
       ~size:_
       ~resize_layout:_
       ~send:_
       ~send_input:_
       ~pump_until ->
  pump_until (fun () ->
    Option.equal Int.equal (Chat_tui.Model.active_history_width model) (Some 72));
  print_s
    [%sexp ((Chat_tui.Model.active_history_width model, !drawn > 0) : int option * bool)];
  [%expect {| ((72) true) |}]
;;

let%expect_test
    "reducer keeps Chat streaming while Agent is visible and ignores stale events"
  =
  with_reducer
  @@ fun ~model
       ~runtime
       ~ui_sw
       ~throttler
       ~enqueued
       ~drawn
       ~size:_
       ~resize_layout:_
       ~send
       ~send_input:_
       ~pump_until ->
  let op_id = Chat_tui.App_runtime.alloc_op_id runtime in
  runtime.Chat_tui.App_runtime.op
  <- Some (Chat_tui.App_runtime.Streaming { id = op_id; sw = ui_sw });
  send
    (`Tool_execution
        ( op_id
        , Execution.Started
            { call_id = "call"; name = "worker"; kind = `Function; payload = "{}" } ));
  pump_until (fun () -> not (List.is_empty (Chat_tui.Model.active_agent_calls model)));
  Chat_tui.Model.set_active_page model Agent;
  send
    (`Sourced_stream
        (op_id, Chat_response.Sourced_response_event.outer (text_delta "streamed")));
  for index = 1 to 20 do
    send
      (`Tool_execution
          ( op_id
          , Execution.Progress
              { call_id = "call"
              ; progress = { channel = `Activity; update = Append (Int.to_string index) }
              } ))
  done;
  let sentinel_draws = !drawn in
  send `Redraw;
  pump_until (fun () -> !drawn > sentinel_draws);
  Chat_tui.Redraw_throttle.tick throttler;
  let active_op_unchanged =
    match runtime.Chat_tui.App_runtime.op with
    | Some (Streaming { id; sw }) -> Int.equal id op_id && phys_equal sw ui_sw
    | _ -> false
  in
  print_s
    [%sexp
      (Chat_tui.Model.messages model : (string * string) list)
    , (List.length (Chat_tui.Model.active_agent_calls model) : int)
    , (active_op_unchanged : bool)
    , (!enqueued : int)];
  send
    (`Tool_execution
        ( op_id + 1
        , Execution.Finished { call_id = "call"; outcome = Returned; output = None } ));
  send
    (`Tool_execution
        ( op_id + 1
        , Execution.Started
            { call_id = "stale"; name = "stale"; kind = `Function; payload = "{}" } ));
  send
    (`Sourced_stream
        (op_id + 1, Chat_response.Sourced_response_event.outer (text_delta "STALE")));
  send (`Streaming_done (op_id + 1, []));
  let stale_sentinel = !drawn in
  send `Redraw;
  pump_until (fun () -> !drawn > stale_sentinel);
  printf
    "after stale calls=%s messages=%s page=%s current=%b\n"
    (Chat_tui.Model.active_agent_calls model
     |> List.map ~f:Chat_tui.Model.agent_call_id
     |> String.concat ~sep:",")
    (Chat_tui.Model.messages model |> List.map ~f:snd |> String.concat ~sep:",")
    (match Chat_tui.Model.active_page model with
     | Chat_tui.Model.Page_id.Chat -> "Chat"
     | Agent -> "Agent"
     | Shell_security -> "Shell_security")
    (match runtime.Chat_tui.App_runtime.op with
     | Some (Streaming { id; _ }) -> Int.equal id op_id
     | _ -> false);
  let unfinished =
    Res.Item.Function_call
      { name = "worker"
      ; arguments = {|{"x":1}|}
      ; call_id = "repair-call"
      ; _type = "function_call"
      ; id = None
      ; status = Some "in_progress"
      }
  in
  let unfinished_entry =
    History_entry.create ~allocator:runtime.history_allocator unfinished
    |> Result.ok_or_failwith
  in
  Chat_tui.Model.set_history_items model [ unfinished_entry ];
  send (`Streaming_error (op_id, Chat_tui.App_streaming.Cancelled));
  pump_until (fun () -> Option.is_none runtime.Chat_tui.App_runtime.op);
  let repair =
    match Chat_tui.Model.history_items model |> History_entry.items with
    | [ Res.Item.Function_call call; Res.Item.Function_call_output output ] ->
      let text =
        match output.output with
        | Text text -> text
        | Content _ -> ""
      in
      call.call_id, output.call_id, String.is_substring text ~substring:"did not complete"
    | _ -> "unexpected", "unexpected", false
  in
  let messages_after_error = Chat_tui.Model.messages model in
  send (`Streaming_error (op_id, Chat_tui.App_streaming.Cancelled));
  let duplicate_sentinel = !drawn in
  send `Redraw;
  pump_until (fun () -> !drawn > duplicate_sentinel);
  print_s
    [%sexp
      (List.length (Chat_tui.Model.active_agent_calls model) : int)
    , (Chat_tui.Model.active_page model
       |> function
       | Chat_tui.Model.Page_id.Chat -> "Chat"
       | Agent -> "Agent"
       | Shell_security -> "Shell_security"
       : string)
    , (List.length messages_after_error : int)
    , (List.length (Chat_tui.Model.messages model) : int)
    , (repair : string * string * bool)];
  [%expect
    {|
    (((assistant streamed)) 1 true 1)
    after stale calls=call messages=streamed page=Agent current=true
    (0 Chat 3 3 (repair-call repair-call true))
    |}]
;;

let%expect_test "compaction error reports failure without changing history" =
  with_reducer
  @@ fun ~model
       ~runtime
       ~ui_sw
       ~throttler:_
       ~enqueued:_
       ~drawn:_
       ~size:_
       ~resize_layout:_
       ~send
       ~send_input:_
       ~pump_until ->
  let history_before = Chat_tui.Model.history_items model in
  let op_id = Chat_tui.App_runtime.alloc_op_id runtime in
  runtime.Chat_tui.App_runtime.op
  <- Some (Chat_tui.App_runtime.Compacting { id = op_id; sw = ui_sw });
  send (`Compaction_error (op_id, Failure "failed"));
  pump_until (fun () -> Option.is_none runtime.Chat_tui.App_runtime.op);
  print_s
    [%sexp
      (Chat_tui.Model.messages model : (string * string) list)
    , (phys_equal history_before (Chat_tui.Model.history_items model) : bool)];
  [%expect {| (((error "Compaction failed.")) true) |}]
;;

let%expect_test "wrapped compaction cancellation is reported as cancellation" =
  with_reducer
  @@ fun ~model
       ~runtime
       ~ui_sw
       ~throttler:_
       ~enqueued:_
       ~drawn:_
       ~size:_
       ~resize_layout:_
       ~send
       ~send_input:_
       ~pump_until ->
  let op_id = Chat_tui.App_runtime.alloc_op_id runtime in
  runtime.Chat_tui.App_runtime.op
  <- Some (Chat_tui.App_runtime.Compacting { id = op_id; sw = ui_sw });
  send
    (`Compaction_error
        (op_id, Eio.Cancel.Cancelled Chat_tui.App_reducer.Compaction_cancelled));
  pump_until (fun () -> Option.is_none runtime.Chat_tui.App_runtime.op);
  print_s [%sexp (Chat_tui.Model.messages model : (string * string) list)];
  [%expect {| ((error "Compaction cancelled.")) |}]
;;

let%expect_test "successful completion clears transient Agent state immediately" =
  with_reducer
  @@ fun ~model
       ~runtime
       ~ui_sw
       ~throttler:_
       ~enqueued:_
       ~drawn
       ~size:_
       ~resize_layout:_
       ~send
       ~send_input:_
       ~pump_until ->
  let op_id = Chat_tui.App_runtime.alloc_op_id runtime in
  runtime.Chat_tui.App_runtime.op
  <- Some (Chat_tui.App_runtime.Streaming { id = op_id; sw = ui_sw });
  send
    (`Tool_execution
        ( op_id
        , Execution.Started
            { call_id = "call"; name = "worker"; kind = `Function; payload = "{}" } ));
  pump_until (fun () -> not (List.is_empty (Chat_tui.Model.active_agent_calls model)));
  Chat_tui.Model.set_active_page model Agent;
  let draws_before = !drawn in
  send (`Streaming_done (op_id, []));
  pump_until (fun () -> Option.is_none runtime.Chat_tui.App_runtime.op);
  print_s
    [%sexp
      (List.length (Chat_tui.Model.active_agent_calls model) : int)
    , (Chat_tui.Model.active_page model
       |> function
       | Chat_tui.Model.Page_id.Chat -> "Chat"
       | Agent -> "Agent"
       | Shell_security -> "Shell_security"
       : string)
    , (!drawn > draws_before : bool)];
  [%expect {| (0 Chat true) |}]
;;

let%expect_test
    "process-backed custom command output reaches Chat after execution finishes"
  =
  with_reducer
  @@ fun ~model
       ~runtime
       ~ui_sw
       ~throttler:_
       ~enqueued:_
       ~drawn
       ~size:_
       ~resize_layout:_
       ~send
       ~send_input:_
       ~pump_until ->
  let op_id = Chat_tui.App_runtime.alloc_op_id runtime in
  runtime.Chat_tui.App_runtime.op
  <- Some (Chat_tui.App_runtime.Streaming { id = op_id; sw = ui_sw });
  send
    (`Tool_execution
        ( op_id
        , Execution.Started
            { call_id = "custom"; name = "process"; kind = `Function; payload = "{}" } ));
  pump_until (fun () -> not (List.is_empty (Chat_tui.Model.active_agent_calls model)));
  Chat_tui.Model.set_active_page model Agent;
  send
    (`Tool_execution
        ( op_id
        , Execution.Progress
            { call_id = "custom"
            ; progress = { channel = `Stdout; update = Append "live" }
            } ));
  send
    (`Tool_execution
        ( op_id
        , Execution.Finished { call_id = "custom"; outcome = Returned; output = None } ));
  pump_until (fun () ->
    Chat_tui.Model.active_agent_calls model
    |> List.hd
    |> Option.bind ~f:Chat_tui.Model.agent_call_outcome
    |> Option.is_some);
  let draws_before = !drawn in
  send (`Tool_output (op_id, history_entry (tool_output "custom" "FINAL")));
  send `Redraw;
  pump_until (fun () -> !drawn > draws_before);
  print_s
    [%sexp
      (Chat_tui.Model.messages model : (string * string) list)
    , (List.exists
         (Chat_tui.Model.history_items model |> History_entry.items)
         ~f:(function
           | Res.Item.Function_call_output output -> String.equal output.call_id "custom"
           | _ -> false)
       : bool)
    , (List.is_empty (Chat_tui.Model.active_agent_calls model) : bool)];
  [%expect {| (((tool_output FINAL)) true false) |}]
;;

let%expect_test "Chat tool calls receive completion before canonical outputs" =
  with_reducer
  @@ fun ~model
       ~runtime
       ~ui_sw
       ~throttler:_
       ~enqueued:_
       ~drawn
       ~size:_
       ~resize_layout:_
       ~send
       ~send_input:_
       ~pump_until ->
  let op_id = Chat_tui.App_runtime.alloc_op_id runtime in
  runtime.Chat_tui.App_runtime.op
  <- Some (Chat_tui.App_runtime.Streaming { id = op_id; sw = ui_sw });
  let add_call call_id =
    send
      (`Sourced_stream
          ( op_id
          , Chat_response.Sourced_response_event.outer
              (Res.Response_stream.Output_item_added
                 { item =
                     Res.Response_stream.Item.Function_call
                       { name = "research"
                       ; arguments = "{}"
                       ; call_id
                       ; _type = "function_call"
                       ; id = Some ("item-" ^ call_id)
                       ; status = None
                       }
                 ; output_index = 0
                 ; type_ = "response.output_item.added"
                 }) ));
    send
      (`Tool_execution
          ( op_id
          , Execution.Started
              { call_id; name = "research"; kind = `Function; payload = "{}" } ))
  in
  List.iter [ "one"; "two"; "three" ] ~f:add_call;
  pump_until (fun () -> List.length (Chat_tui.Model.messages model) = 3);
  List.iter [ "three"; "one"; "two" ] ~f:(fun call_id ->
    send
      (`Tool_execution
          (op_id, Execution.Finished { call_id; outcome = Returned; output = None })));
  let draws_before = !drawn in
  send `Redraw;
  pump_until (fun () -> !drawn > draws_before);
  let outcomes =
    List.init 3 ~f:(fun idx ->
      Chat_tui.Model.tool_call_outcome_for_message model ~idx |> Option.is_some)
  in
  print_s
    [%sexp
      (outcomes : bool list)
    , (Chat_tui.Model.messages model : (string * string) list)
    , (List.exists
         (Chat_tui.Model.history_items model |> History_entry.items)
         ~f:(function
           | Res.Item.Function_call_output _ -> true
           | _ -> false)
       : bool)];
  [%expect
    {|
    ((true true true) ((tool "research(") (tool "research(") (tool "research("))
     false)
    |}]
;;

let%expect_test "early Ctrl-G opens Agent when the first tool starts" =
  with_reducer
  @@ fun ~model
       ~runtime
       ~ui_sw
       ~throttler:_
       ~enqueued:_
       ~drawn
       ~size:_
       ~resize_layout:_
       ~send
       ~send_input
       ~pump_until ->
  let op_id = Chat_tui.App_runtime.alloc_op_id runtime in
  runtime.Chat_tui.App_runtime.op
  <- Some (Chat_tui.App_runtime.Streaming { id = op_id; sw = ui_sw });
  send_input (`Key (`ASCII '\007', []));
  pump_until (fun () ->
    Option.equal Int.equal runtime.Chat_tui.App_runtime.pending_agent_toggle (Some op_id));
  send
    (`Tool_execution
        ( op_id
        , Execution.Started
            { call_id = "call"; name = "worker"; kind = `Function; payload = "{}" } ));
  let draws_before = !drawn in
  send `Redraw;
  pump_until (fun () -> !drawn > draws_before);
  print_s
    [%sexp
      (Chat_tui.Model.active_page model
       |> function
       | Chat_tui.Model.Page_id.Chat -> "Chat"
       | Agent -> "Agent"
       | Shell_security -> "Shell_security"
       : string)
    , (runtime.Chat_tui.App_runtime.pending_agent_toggle : int option)];
  [%expect {| (Agent ()) |}]
;;

let%expect_test "early Ctrl-G toggles off and terminal events clear pending intent" =
  with_reducer
  @@ fun ~model
       ~runtime
       ~ui_sw
       ~throttler:_
       ~enqueued:_
       ~drawn
       ~size:_
       ~resize_layout:_
       ~send
       ~send_input
       ~pump_until ->
  let op_id = Chat_tui.App_runtime.alloc_op_id runtime in
  runtime.Chat_tui.App_runtime.op
  <- Some (Chat_tui.App_runtime.Streaming { id = op_id; sw = ui_sw });
  send_input (`Key (`ASCII 'G', [ `Ctrl ]));
  pump_until (fun () -> Option.is_some runtime.Chat_tui.App_runtime.pending_agent_toggle);
  send_input (`Key (`ASCII 'G', [ `Ctrl ]));
  pump_until (fun () -> Option.is_none runtime.Chat_tui.App_runtime.pending_agent_toggle);
  send
    (`Tool_execution
        ( op_id
        , Execution.Started
            { call_id = "call"; name = "worker"; kind = `Function; payload = "{}" } ));
  pump_until (fun () -> not (List.is_empty (Chat_tui.Model.active_agent_calls model)));
  print_s
    [%sexp
      (Chat_tui.Model.active_page model
       |> function
       | Chat_tui.Model.Page_id.Chat -> "Chat"
       | Agent -> "Agent"
       | Shell_security -> "Shell_security"
       : string)];
  Chat_tui.Model.clear_agent_calls model;
  send_input (`Key (`ASCII 'G', [ `Ctrl ]));
  pump_until (fun () -> Option.is_some runtime.Chat_tui.App_runtime.pending_agent_toggle);
  send (`Streaming_error (op_id, Chat_tui.App_streaming.Cancelled));
  pump_until (fun () -> Option.is_none runtime.Chat_tui.App_runtime.op);
  let next_id = Chat_tui.App_runtime.alloc_op_id runtime in
  runtime.Chat_tui.App_runtime.op
  <- Some (Chat_tui.App_runtime.Streaming { id = next_id; sw = ui_sw });
  send
    (`Tool_execution
        ( next_id
        , Execution.Started
            { call_id = "next"; name = "worker"; kind = `Function; payload = "{}" } ));
  let draws_before = !drawn in
  send `Redraw;
  pump_until (fun () -> !drawn > draws_before);
  print_s
    [%sexp
      (runtime.Chat_tui.App_runtime.pending_agent_toggle : int option)
    , (Chat_tui.Model.active_page model
       |> function
       | Chat_tui.Model.Page_id.Chat -> "Chat"
       | Agent -> "Agent"
       | Shell_security -> "Shell_security"
       : string)];
  [%expect
    {|
    Chat
    (() Chat)
    |}]
;;
