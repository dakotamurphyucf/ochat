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

let allocator namespace =
  History_entry.Allocator.create ~namespace ~next_sequence:0 |> Result.ok_or_failwith
;;

let fixture_ctx env ?dir ~namespace ~post_stream () =
  let dir = Option.value dir ~default:(Eio.Stdenv.cwd env) in
  Inference_fixture.create ~namespace ~default_model:"test-model" ~post_stream
  |> fun fixture ->
  Inference_fixture.ctx
    fixture
    ~env
    ~dir
    ~tool_dir:(Eio.Stdenv.cwd env)
    ~cache:(Chat_response.Cache.create ~max_size:1 ())
    ()
;;

let stream_response (response : Res.Response.t) =
  List.mapi response.output ~f:(fun output_index item ->
    let item = Res.Response_stream.Item.t_of_jsonaf (Res.Item.jsonaf_of_t item) in
    Res.Response_stream.Output_item_done
      { item; output_index; type_ = "response.output_item.done" })
  |> Stdlib.List.to_seq
;;

let added_message id =
  Res.Response_stream.Output_item_added
    { item = Output_message (output_text id "")
    ; output_index = 0
    ; type_ = "response.output_item.added"
    }
;;

let canonical_texts entries =
  List.concat_map entries ~f:(fun entry ->
    match
      History_entry.Payload.Semantic.view
        (History_entry.Payload.semantic (History_entry.payload entry))
    with
    | Message { role = Assistant; content } ->
      List.filter_map content ~f:(function
        | Text { text; _ } -> Some text
        | _ -> None)
    | _ -> [])
;;

let%expect_test "observed and unobserved entry execution preserve payloads and IDs" =
  Eio_main.run
  @@ fun env ->
  let create_initial allocator =
    Openai.Responses_history.create ~allocator (input_message "hello")
    |> Result.ok_or_failwith
  in
  let run ~observed =
    let entries = allocator "driver-parity" in
    let initial = create_initial entries in
    let responses =
      Queue.of_list
        [ response [ Function_call function_call ]
        ; response [ Output_message (output_text "provider-message" "done") ]
        ]
    in
    let requests = Queue.create () in
    let ctx =
      fixture_ctx
        env
        ~namespace:(if observed then "observed" else "plain")
        ~post_stream:(fun ~sw:_ ~inputs ->
          Queue.enqueue requests inputs;
          stream_response (Queue.dequeue_exn responses))
        ()
    in
    let observer : Chat_response.Driver.agent_observer =
      { on_event = ignore; on_tool_execution = ignore }
    in
    let history =
      Chat_response.Driver.run_entries
        ~ctx
        ~allocator:entries
        ?observer:(if observed then Some observer else None)
        ~tool_tbl:(create_tool_table ())
        [ initial ]
    in
    history, Queue.to_list requests, History_entry.Allocator.next_sequence entries
  in
  let plain, plain_requests, plain_next = run ~observed:false in
  let observed, observed_requests, observed_next = run ~observed:true in
  let ids history =
    List.map history ~f:(fun entry -> History_entry.Id.sequence (History_entry.id entry))
  in
  print_s
    [%sexp
      (List.equal
         (fun left right ->
            History_entry.Id.equal (History_entry.id left) (History_entry.id right)
            && Document_schema.Json.equal
                 (History_entry.Payload.to_json (History_entry.payload left))
                 (History_entry.Payload.to_json (History_entry.payload right)))
         plain
         observed
       : bool)
    , (ids plain : int list)
    , (ids observed : int list)
    , (List.equal (List.equal item_equal) plain_requests observed_requests : bool)
    , (plain_next : int)
    , (observed_next : int)];
  [%expect {| (true (0 1 2 3) (0 1 2 3) true 4 4) |}]
;;

let%expect_test "observed fork keeps child identity and history isolated" =
  Eio_main.run
  @@ fun env ->
  let entries = allocator "task6-parent" in
  let initial =
    Openai.Responses_history.create ~allocator:entries (input_message "start")
    |> Result.ok_or_failwith
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
      ; [ done_message "reused-provider-id" "child result" ]
      ; [ done_message "reused-provider-id" "parent done" ]
      ]
  in
  let requests = Queue.create () in
  let ctx =
    fixture_ctx
      env
      ~namespace:"fork-parity"
      ~post_stream:(fun ~sw:_ ~inputs ->
        Queue.enqueue requests inputs;
        Queue.dequeue_exn responses |> Stdlib.List.to_seq)
      ()
  in
  let scopes = Queue.create () in
  let observer : Loop.observer =
    { on_event =
        (fun event ->
          match Transcript.Stream.view event with
          | Source_started { scope; _ } -> Queue.enqueue scopes scope
          | _ -> ())
    ; on_tool_execution = ignore
    }
  in
  let history =
    Loop.run_entries
      ~ctx
      ~allocator:entries
      ~observer
      ~tool_tbl:(String.Table.create ())
      [ initial ]
  in
  let kinds =
    List.map history ~f:(fun entry ->
      match
        History_entry.Payload.Semantic.view
          (History_entry.Payload.semantic (History_entry.payload entry))
      with
      | Message { role = User; _ } -> "input"
      | Call _ -> "call"
      | Result _ -> "output"
      | Message { role = Assistant; _ } -> "message"
      | _ -> "other")
  in
  let ids =
    List.map history ~f:(fun entry ->
      let id = History_entry.id entry in
      History_entry.Id.namespace id, History_entry.Id.sequence id)
  in
  let source_list = Queue.to_list scopes in
  let sources =
    List.map source_list ~f:(fun (scope : Transcript.Scope.t) ->
      match scope.relation with
      | Root -> false, None
      | Nested parent -> true, parent.call_alias)
  in
  let child_scope = List.nth_exn source_list 1 in
  let parent_scope = List.hd_exn source_list in
  let child_request = List.nth_exn (Queue.to_list requests) 1 in
  let child_has_isolated_entries =
    List.exists child_request ~f:(function
      | Function_call_output { call_id; output = Text text; _ } ->
        String.equal call_id "fork-call"
        && String.is_substring text ~substring:"Forked Agent"
      | _ -> false)
  in
  let parent_has_child_payload =
    List.mem (canonical_texts history) "child result" ~equal:String.equal
  in
  print_s
    [%sexp
      (kinds : string list)
    , (ids : (string * int) list)
    , (sources : (bool * string option) list)
    , (not (Transcript.Scope.Key.equal child_scope.key parent_scope.key) : bool)
    , (child_has_isolated_entries : bool)
    , (parent_has_child_payload : bool)
    , (Queue.length requests : int)
    , (History_entry.Allocator.next_sequence entries : int)];
  [%expect
    {|
    ((input call output message)
     ((task6-parent 0) (task6-parent 1) (task6-parent 2) (task6-parent 3))
     ((false ()) (true (fork-call)) (false ())) true true false 3 4)
    |}]
;;

let%expect_test "observed streaming loop forwards deltas and preserves final text" =
  Eio_main.run
  @@ fun env ->
  let ctx =
    fixture_ctx
      env
      ~namespace:"deltas"
      ~post_stream:(fun ~sw:_ ~inputs:_ ->
        Stdlib.List.to_seq
          [ added_message "message-1"
          ; text_delta "message-1" "hello"
          ; done_message "message-1" "hello"
          ])
      ()
  in
  let events = Queue.create () in
  let observer : Loop.observer =
    { on_event = Queue.enqueue events; on_tool_execution = ignore }
  in
  let history =
    Loop.run_entries
      ~ctx
      ~allocator:(allocator "delta-history")
      ~observer
      ~tool_tbl:(String.Table.create ())
      []
  in
  Queue.iter events ~f:(fun event ->
    match Transcript.Stream.view event with
    | Changed { change = Append text; _ } -> print_endline text
    | _ -> ());
  print_s [%sexp (canonical_texts history : string list)];
  [%expect
    {|
    hello
    (hello)
    |}]
;;

let%expect_test "selected streaming loop preserves parsing failure without hidden retry" =
  Eio_main.run
  @@ fun env ->
  let attempts = ref 0 in
  let ctx =
    fixture_ctx
      env
      ~namespace:"parse-failure"
      ~post_stream:(fun ~sw:_ ~inputs:_ ->
        Int.incr attempts;
        raise (Res.Response_stream_parsing_error (`Null, Failure "malformed stream")))
      ()
  in
  let observer : Loop.observer = { on_event = ignore; on_tool_execution = ignore } in
  let propagated =
    try
      ignore
        (Loop.run_entries
           ~ctx
           ~allocator:(allocator "failed-history")
           ~observer
           ~tool_tbl:(String.Table.create ())
           []
         : History_entry.t list);
      false
    with
    | Res.Response_stream_parsing_error _ -> true
  in
  print_s [%sexp (!attempts : int), (propagated : bool)];
  [%expect {| (1 true) |}]
;;

let%expect_test "retired provider transport rejects before selected or transport effects" =
  Eio_main.run
  @@ fun env ->
  let selected_calls = ref 0 in
  let transport_calls = ref 0 in
  let ctx =
    fixture_ctx
      env
      ~namespace:"retired"
      ~post_stream:(fun ~sw:_ ~inputs:_ ->
        Int.incr selected_calls;
        Stdlib.List.to_seq [])
      ()
  in
  let response_dir = Eio.Path.(Eio.Stdenv.cwd env / "response-dir") in
  let post_stream ~sw:_ ~dir:_ ~inputs:_ =
    Int.incr transport_calls;
    Stdlib.List.to_seq []
  in
  let observer : Loop.observer = { on_event = ignore; on_tool_execution = ignore } in
  let rejected =
    try
      ignore
        (Loop.run_entries
           ~ctx
           ~allocator:(allocator "retired-history")
           ~response_dir
           ~observer
           ~post_stream
           ~tool_tbl:(String.Table.create ())
           []
         : History_entry.t list);
      false
    with
    | Invalid_argument _ -> true
  in
  print_s [%sexp (rejected : bool), (!selected_calls : int), (!transport_calls : int)];
  [%expect {| (true 0 0) |}]
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
  (* This test exercises the isolated legacy DTO-to-trace ingress helper;
     selected executors publish typed Transcript events instead. *)
  Chat_response.Agent_trace.on_event trace (text_delta "message-3" "answer");
  Chat_response.Agent_trace.on_event
    trace
    (Res.Response_stream.Reasoning_summary_text_delta
       { summary_index = 0
       ; delta = "thought"
       ; item_id = "reasoning-1"
       ; output_index = 0
       ; type_ = "response.reasoning_summary_text.delta"
       });
  Chat_response.Agent_trace.on_tool_execution
    trace
    (Started
       { call_id = "nested-1"
       ; name = "lookup"
       ; kind = `Function
       ; payload = {|{"query":"x"}|}
       });
  Chat_response.Agent_trace.on_tool_execution
    trace
    (Progress
       { call_id = "nested-1"; progress = { channel = `Stdout; update = Append "chunk" } });
  Chat_response.Agent_trace.on_tool_execution
    trace
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
