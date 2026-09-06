open Core
module Res = Openai.Responses
module Loop = Chat_response.Agent_response_loop

let output_text id text : Res.Output_message.t =
  { role = Assistant
  ; id
  ; content = [ { annotations = []; text; _type = "output_text" } ]
  ; status = "completed"
  ; phase = None
  ; _type = "message"
  }
;;

let done_message id text =
  Res.Response_stream.Output_item_done
    { item = Output_message (output_text id text)
    ; output_index = 0
    ; type_ = "response.output_item.done"
    }
;;

let text_delta id text =
  Res.Response_stream.Output_text_delta
    { content_index = 0
    ; delta = text
    ; item_id = id
    ; output_index = 0
    ; type_ = "response.output_text.delta"
    }
;;

let assistant_texts items =
  List.filter_map items ~f:(function
    | Res.Item.Output_message message ->
      Some
        (List.map message.content ~f:(fun content -> content.text)
         |> String.concat ~sep:" ")
    | _ -> None)
;;

let input_message text : Res.Item.t =
  Input_message
    { role = User
    ; content = [ Res.Input_message.Text { text; _type = "input_text" } ]
    ; _type = "message"
    }
;;

let function_call : Res.Function_call.t =
  { name = "echo"
  ; arguments = {|{"text":"hello"}|}
  ; call_id = "parity-call"
  ; _type = "function_call"
  ; id = Some "provider-call"
  ; status = None
  }
;;

let response output : Res.Response.t =
  { id = "response-test"
  ; object_ = "response"
  ; created_at = 0
  ; status = Res.Status.Completed
  ; error = None
  ; incomplete_details = None
  ; instructions = None
  ; max_output_tokens = None
  ; model = "test-model"
  ; output
  ; parallel_tool_calls = Some true
  ; previous_response_id = None
  ; reasoning = None
  ; store = None
  ; temperature = None
  ; text = None
  ; tool_choice = None
  ; tools = None
  ; top_p = None
  ; truncation = None
  ; usage = None
  ; user = None
  ; metadata = None
  }
;;

let stream_done item =
  Res.Response_stream.Output_item_done
    { item; output_index = 0; type_ = "response.output_item.done" }
;;

let create_tool_table () =
  let table : (string, Ochat_function.runner) Hashtbl.t =
    Hashtbl.create (module String)
  in
  Hashtbl.set table ~key:"echo" ~data:(fun ~invocation:_ payload ->
    Res.Tool_output.Output.Text payload);
  table
;;

let item_equal left right =
  String.equal
    (Jsonaf.to_string (Res.Item.jsonaf_of_t left))
    (Jsonaf.to_string (Res.Item.jsonaf_of_t right))
;;

let%expect_test "observed and unobserved entry execution preserve payloads and IDs" =
  Eio_main.run
  @@ fun env ->
  let dir = Eio.Stdenv.cwd env in
  let ctx =
    Chat_response.Ctx.create
      ~env
      ~dir
      ~tool_dir:dir
      ~cache:(Chat_response.Cache.create ~max_size:1 ())
  in
  let create_allocator () =
    History_entry.Allocator.create ~namespace:"driver-parity" ~next_sequence:0
    |> Result.ok_or_failwith
  in
  let create_initial allocator =
    History_entry.create ~allocator (input_message "hello") |> Result.ok_or_failwith
  in
  let blocking_allocator = create_allocator () in
  let blocking_initial = create_initial blocking_allocator in
  let blocking_responses =
    Queue.of_list
      [ response [ Function_call function_call ]
      ; response [ Output_message (output_text "provider-message" "done") ]
      ]
  in
  let blocking_inputs = Queue.create () in
  let post : Chat_response.Response_loop.post =
    fun ~sw:_ ~dir:_ ~inputs ->
    Queue.enqueue blocking_inputs inputs;
    Queue.dequeue_exn blocking_responses
  in
  let blocking_history =
    Chat_response.Driver.run_entries
      ~ctx
      ~allocator:blocking_allocator
      ~post
      ~model:Res.Request.Gpt4
      ~tool_tbl:(create_tool_table ())
      [ blocking_initial ]
  in
  let observed_allocator = create_allocator () in
  let observed_initial = create_initial observed_allocator in
  let observed_responses =
    Queue.of_list
      [ [ stream_done (Res.Response_stream.Item.Function_call function_call) ]
      ; [ stream_done
            (Res.Response_stream.Item.Output_message
               (output_text "provider-message" "done"))
        ]
      ]
  in
  let observed_inputs = Queue.create () in
  let post_stream : Chat_response.Agent_response_loop.post_stream =
    fun ~sw:_ ~dir:_ ~inputs ->
    Queue.enqueue observed_inputs inputs;
    Queue.dequeue_exn observed_responses |> Stdlib.List.to_seq
  in
  let observer : Chat_response.Driver.agent_observer =
    { on_event = ignore; on_tool_execution = ignore }
  in
  let observed_history =
    Chat_response.Driver.run_entries
      ~ctx
      ~allocator:observed_allocator
      ~observer
      ~post_stream
      ~model:Res.Request.Gpt4
      ~tool_tbl:(create_tool_table ())
      [ observed_initial ]
  in
  let ids history =
    List.map history ~f:(fun entry -> History_entry.Id.sequence (History_entry.id entry))
  in
  let requests_equal =
    List.equal
      (List.equal item_equal)
      (Queue.to_list blocking_inputs)
      (Queue.to_list observed_inputs)
  in
  print_s
    [%sexp
      (List.equal
         item_equal
         (History_entry.items blocking_history)
         (History_entry.items observed_history)
       : bool)
    , (ids blocking_history : int list)
    , (ids observed_history : int list)
    , (requests_equal : bool)
    , (History_entry.Allocator.next_sequence blocking_allocator : int)
    , (History_entry.Allocator.next_sequence observed_allocator : int)];
  [%expect {| (true (0 1 2 3) (0 1 2 3) true 4 4) |}]
;;

let%expect_test "observed fork keeps child identity and history isolated" =
  Eio_main.run
  @@ fun env ->
  let dir = Eio.Stdenv.cwd env in
  let ctx =
    Chat_response.Ctx.create
      ~env
      ~dir
      ~tool_dir:dir
      ~cache:(Chat_response.Cache.create ~max_size:1 ())
  in
  let allocator =
    History_entry.Allocator.create ~namespace:"task6-parent" ~next_sequence:0
    |> Result.ok_or_failwith
  in
  let initial =
    History_entry.create ~allocator (input_message "start") |> Result.ok_or_failwith
  in
  let fork_call : Res.Function_call.t =
    { name = "fork"
    ; arguments = {|{"command":"inspect","arguments":["one"]}|}
    ; call_id = "fork-call"
    ; _type = "function_call"
    ; id = Some "reused-provider-id"
    ; status = None
    }
  in
  let responses =
    Queue.of_list
      [ [ stream_done (Res.Response_stream.Item.Function_call fork_call) ]
      ; [ stream_done
            (Res.Response_stream.Item.Output_message
               (output_text "reused-provider-id" "child result"))
        ]
      ; [ stream_done
            (Res.Response_stream.Item.Output_message
               (output_text "reused-provider-id" "parent done"))
        ]
      ]
  in
  let requests = Queue.create () in
  let post_stream : Chat_response.Agent_response_loop.post_stream =
    fun ~sw:_ ~dir:_ ~inputs ->
    Queue.enqueue requests inputs;
    Queue.dequeue_exn responses |> Stdlib.List.to_seq
  in
  let sourced = Queue.create () in
  let observer : Chat_response.Agent_response_loop.observer =
    { on_event = ignore; on_tool_execution = ignore }
  in
  let history =
    Chat_response.Agent_response_loop.run_entries
      ~ctx
      ~allocator
      ~observer
      ~on_sourced_event:(Queue.enqueue sourced)
      ~post_stream
      ~model:Res.Request.Gpt4
      ~tool_tbl:(String.Table.create ())
      [ initial ]
  in
  let kinds =
    List.map history ~f:(fun entry ->
      match History_entry.item entry with
      | Res.Item.Input_message _ -> "input"
      | Function_call _ -> "call"
      | Function_call_output _ -> "output"
      | Output_message _ -> "message"
      | _ -> "other")
  in
  let ids =
    List.map history ~f:(fun entry ->
      let id = History_entry.id entry in
      History_entry.Id.namespace id, History_entry.Id.sequence id)
  in
  let sources =
    Queue.to_list sourced
    |> List.map ~f:(fun event -> Option.is_some event.invocation_id, event.parent_call_id)
  in
  let child_source =
    Queue.to_list sourced |> List.find_map_exn ~f:(fun event -> event.invocation_id)
  in
  let child_request = List.nth_exn (Queue.to_list requests) 1 in
  let child_has_isolated_entries =
    List.exists child_request ~f:(function
      | Res.Item.Function_call_output output ->
        String.equal output.call_id "fork-call"
        &&
          (match output.output with
          | Res.Tool_output.Output.Text text ->
            String.is_substring text ~substring:"Forked Agent"
          | Content _ -> false)
      | _ -> false)
  in
  let parent_has_child_payload =
    List.exists history ~f:(fun entry ->
      match History_entry.item entry with
      | Res.Item.Output_message message ->
        List.exists message.content ~f:(fun part -> String.equal part.text "child result")
      | _ -> false)
  in
  print_s
    [%sexp
      (kinds : string list)
    , (ids : (string * int) list)
    , (sources : (bool * string option) list)
    , (String.is_prefix child_source ~prefix:"fork-invocation-" : bool)
    , (String.equal child_source "fork-call" : bool)
    , (child_has_isolated_entries : bool)
    , (parent_has_child_payload : bool)
    , (Queue.length requests : int)
    , (History_entry.Allocator.next_sequence allocator : int)];
  [%expect
    {|
    ((input call output message)
     ((task6-parent 0) (task6-parent 1) (task6-parent 2) (task6-parent 3))
     ((false ()) (true (fork-call)) (false ())) true false true false 3 4)
    |}]
;;

let%expect_test "observed streaming loop forwards deltas and preserves final text" =
  Eio_main.run
  @@ fun env ->
  let cache = Chat_response.Cache.create ~max_size:1 () in
  let ctx =
    Chat_response.Ctx.create
      ~env
      ~dir:(Eio.Stdenv.cwd env)
      ~tool_dir:(Eio.Stdenv.cwd env)
      ~cache
  in
  let events = Queue.create () in
  let observer : Loop.observer =
    { on_event = Queue.enqueue events; on_tool_execution = (fun _ -> ()) }
  in
  let post_stream ~sw:_ ~dir:_ ~inputs:_ =
    Stdlib.List.to_seq
      [ text_delta "message-1" "hello"; done_message "message-1" "hello" ]
  in
  let tool_tbl = String.Table.create () in
  let history =
    Loop.run ~ctx ~model:Res.Request.Gpt4 ~tool_tbl ~observer ~post_stream []
  in
  Queue.iter events ~f:(function
    | Res.Response_stream.Output_text_delta { delta; _ } -> print_endline delta
    | _ -> ());
  print_s [%sexp (assistant_texts history : string list)];
  [%expect
    {|
    hello
    (hello)
    |}]
;;

let%expect_test "observed streaming loop retries one parsing failure" =
  Eio_main.run
  @@ fun env ->
  let cache = Chat_response.Cache.create ~max_size:1 () in
  let ctx =
    Chat_response.Ctx.create
      ~env
      ~dir:(Eio.Stdenv.cwd env)
      ~tool_dir:(Eio.Stdenv.cwd env)
      ~cache
  in
  let attempts = ref 0 in
  let post_stream ~sw:_ ~dir:_ ~inputs:_ =
    Int.incr attempts;
    if Int.equal !attempts 1
    then raise (Res.Response_stream_parsing_error (`Null, Failure "malformed stream"))
    else Stdlib.List.to_seq [ done_message "message-2" "recovered" ]
  in
  let observer : Loop.observer =
    { on_event = (fun _ -> ()); on_tool_execution = (fun _ -> ()) }
  in
  let history =
    Loop.run
      ~ctx
      ~model:Res.Request.Gpt4
      ~tool_tbl:(String.Table.create ())
      ~observer
      ~post_stream
      []
  in
  print_s [%sexp (!attempts : int), (assistant_texts history : string list)];
  [%expect {| (2 (recovered)) |}]
;;

let%expect_test "observed loop uses explicit response artifact directory" =
  Eio_main.run
  @@ fun env ->
  let cache = Chat_response.Cache.create ~max_size:1 () in
  let prompt_dir = Eio.Path.(Eio.Stdenv.cwd env / "prompt-dir") in
  let response_dir = Eio.Path.(Eio.Stdenv.cwd env / "response-dir") in
  let ctx =
    Chat_response.Ctx.create ~env ~dir:prompt_dir ~tool_dir:(Eio.Stdenv.cwd env) ~cache
  in
  let selected_dir = ref None in
  let post_stream ~sw:_ ~dir ~inputs:_ =
    selected_dir := Some (Eio.Path.native_exn dir);
    Stdlib.List.to_seq [ done_message "message-dir" "done" ]
  in
  let observer : Loop.observer = { on_event = ignore; on_tool_execution = ignore } in
  ignore
    (Loop.run
       ~ctx
       ~response_dir
       ~model:Res.Request.Gpt4
       ~tool_tbl:(String.Table.create ())
       ~observer
       ~post_stream
       []
     : Res.Item.t list);
  print_s
    [%sexp
      (Option.value_exn !selected_dir : string)
    , (Eio.Path.native_exn prompt_dir : string)
    , (Eio.Path.native_exn response_dir : string)];
  [%expect {| (./response-dir ./prompt-dir ./response-dir) |}]
;;

let show_progress { Ochat_function.Progress.channel; update } =
  let channel =
    match channel with
    | `Assistant -> "assistant"
    | `Reasoning -> "reasoning"
    | `Stdout -> "stdout"
    | `Stderr -> "stderr"
    | `Activity -> "activity"
  in
  let update =
    match update with
    | Append text -> "append:" ^ text
    | Replace text -> "replace:" ^ text
  in
  Printf.sprintf "%s %s" channel update
;;

let%expect_test "Agent trace labels deltas and nested tool activity" =
  let progress = Queue.create () in
  let traces = Queue.create () in
  let trace =
    Chat_response.Agent_trace.create
      ~emit:(Queue.enqueue progress)
      ~emit_trace:(Queue.enqueue traces)
  in
  let observer : Chat_response.Agent_response_loop.observer =
    { on_event = Chat_response.Agent_trace.on_event trace
    ; on_tool_execution = Chat_response.Agent_trace.on_tool_execution trace
    }
  in
  observer.on_event (text_delta "message-3" "answer");
  observer.on_event
    (Res.Response_stream.Reasoning_summary_text_delta
       { summary_index = 0
       ; delta = "thought"
       ; item_id = "reasoning-1"
       ; output_index = 0
       ; type_ = "response.reasoning_summary_text.delta"
       });
  observer.on_tool_execution
    (Started
       { call_id = "nested-1"
       ; name = "lookup"
       ; kind = `Function
       ; payload = {|{"query":"x"}|}
       });
  observer.on_tool_execution
    (Progress
       { call_id = "nested-1"; progress = { channel = `Stdout; update = Append "chunk" } });
  observer.on_tool_execution
    (Finished
       { call_id = "nested-1"
       ; outcome = Returned
       ; output = Some (Openai.Responses.Tool_output.Output.Text "result")
       });
  Queue.iter progress ~f:(fun item -> print_endline (show_progress item));
  printf "traces=%d\n" (Queue.length traces);
  [%expect
    {|
    assistant append:answer
    reasoning append:thought
    traces=3
    |}]
;;
