open Core
module Res = Openai.Responses
module Loop = Chat_response.Response_loop
module Output = Res.Tool_output.Output

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

let input_message text : Res.Item.t =
  Input_message
    { role = User
    ; content = [ Res.Input_message.Text { text; _type = "input_text" } ]
    ; _type = "message"
    }
;;

let function_call : Res.Item.t =
  Function_call
    { name = "echo"
    ; arguments = {|{"text":"hello"}|}
    ; call_id = "shared-correlation"
    ; _type = "function_call"
    ; id = Some "provider-call"
    ; status = None
    }
;;

let output_message text : Res.Item.t =
  Output_message
    { role = Assistant
    ; id = "provider-message"
    ; content = [ { annotations = []; text; _type = "output_text" } ]
    ; status = "completed"
    ; phase = None
    ; _type = "message"
    }
;;

let create_entry ~allocator item =
  History_entry.create ~allocator item |> Result.ok_or_failwith
;;

let%expect_test "blocking loop preserves IDs and allocates each new occurrence once" =
  Eio_main.run
  @@ fun env ->
  let dir = Eio.Stdenv.cwd env in
  let cache = Chat_response.Cache.create ~max_size:1 () in
  let ctx = Chat_response.Ctx.create ~env ~dir ~tool_dir:dir ~cache in
  let allocator =
    History_entry.Allocator.create ~namespace:"blocking-test" ~next_sequence:0
    |> Result.ok_or_failwith
  in
  let initial = create_entry ~allocator (input_message "hello") in
  let tool_tbl : (string, Ochat_function.runner) Hashtbl.t =
    Hashtbl.create (module String)
  in
  Hashtbl.set tool_tbl ~key:"echo" ~data:(fun ~invocation:_ payload ->
    Output.Text payload);
  let responses =
    Queue.of_list [ response [ function_call ]; response [ output_message "done" ] ]
  in
  let request_inputs = Queue.create () in
  let post : Loop.post =
    fun ~sw:_ ~dir:_ ~inputs ->
    Queue.enqueue request_inputs inputs;
    Queue.dequeue_exn responses
  in
  let history =
    Loop.run_entries ~ctx ~allocator ~post ~model:Res.Request.Gpt4 ~tool_tbl [ initial ]
  in
  let ids =
    List.map history ~f:(fun entry -> History_entry.Id.sequence (History_entry.id entry))
  in
  let kinds =
    List.map history ~f:(fun entry ->
      match History_entry.item entry with
      | Input_message _ -> "input"
      | Function_call _ -> "call"
      | Function_call_output _ -> "output"
      | Output_message _ -> "message"
      | _ -> "other")
  in
  print_s
    [%sexp
      (ids : int list)
    , (kinds : string list)
    , (Queue.length request_inputs : int)
    , (History_entry.Allocator.next_sequence allocator : int)];
  [%expect {| ((0 1 2 3) (input call output message) 2 4) |}]
;;

let%expect_test "blocking request payload matches entry projection" =
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
    History_entry.Allocator.create ~namespace:"payload-parity" ~next_sequence:0
    |> Result.ok_or_failwith
  in
  let initial_items = [ input_message "one"; input_message "two" ] in
  let initial_entries = List.map initial_items ~f:(create_entry ~allocator) in
  let posted = ref [] in
  let post : Loop.post =
    fun ~sw:_ ~dir:_ ~inputs ->
    posted := inputs;
    response []
  in
  ignore
    (Chat_response.Driver.run_entries
       ~ctx
       ~allocator
       ~post
       ~model:Res.Request.Gpt4
       ~tool_tbl:(String.Table.create ())
       initial_entries
     : History_entry.t list);
  print_s
    [%sexp
      (List.equal
         (fun left right ->
            String.equal
              (Jsonaf.to_string (Res.Item.jsonaf_of_t left))
              (Jsonaf.to_string (Res.Item.jsonaf_of_t right)))
         initial_items
         !posted
       : bool)];
  [%expect {| true |}]
;;

let%expect_test "request compaction preserves application IDs" =
  let allocator =
    History_entry.Allocator.create ~namespace:"compaction-test" ~next_sequence:0
    |> Result.ok_or_failwith
  in
  let call call_id =
    Res.Item.Function_call
      { name = "read_file"
      ; arguments = {|{"path":"same.txt"}|}
      ; call_id
      ; _type = "function_call"
      ; id = None
      ; status = None
      }
  in
  let output call_id text =
    Chat_response.Tool_call.output_item
      ~kind:Chat_response.Tool_call.Kind.Function
      ~call_id
      ~output:(Output.Text text)
  in
  let entries =
    [ call "one"; output "one" "old"; call "two"; output "two" "new" ]
    |> List.map ~f:(create_entry ~allocator)
  in
  let compacted = Chat_response.Compact_history.collapse_read_file_entries entries in
  let ids_preserved =
    List.map2_exn entries compacted ~f:(fun before after ->
      History_entry.Id.equal (History_entry.id before) (History_entry.id after))
  in
  print_s [%sexp (ids_preserved : bool list)];
  [%expect {| (true true true true) |}]
;;

let%expect_test "stream aliases reuse one canonical ID and separate tool output" =
  let allocator =
    History_entry.Allocator.create ~namespace:"stream-test" ~next_sequence:0
    |> Result.ok_or_failwith
  in
  let registry = Chat_response.History_stream_event.Registry.create ~allocator in
  let scope = Chat_response.History_stream_event.Registry.create_scope registry in
  let item = output_message "done" in
  let stream_item =
    match item with
    | Res.Item.Output_message message -> Res.Response_stream.Item.Output_message message
    | _ -> assert false
  in
  let added =
    Res.Response_stream.Output_item_added
      { item = stream_item; output_index = 0; type_ = "response.output_item.added" }
  in
  let delta =
    Res.Response_stream.Output_text_delta
      { item_id = "provider-message"
      ; output_index = 0
      ; content_index = 0
      ; delta = "done"
      ; type_ = "response.output_text.delta"
      }
  in
  let done_ =
    Res.Response_stream.Output_item_done
      { item = stream_item; output_index = 0; type_ = "response.output_item.done" }
  in
  let observe event =
    Chat_response.History_stream_event.Registry.observe registry ~scope ~source:None event
    |> Option.value_exn
    |> History_entry.Id.sequence
  in
  let added_id = observe added in
  let delta_id = observe delta in
  let done_id = observe done_ in
  let output_id =
    Chat_response.History_stream_event.Registry.tool_output
      registry
      ~scope
      ~source:None
      ~call_id:"shared-correlation"
    |> History_entry.Id.sequence
  in
  print_s [%sexp ((added_id, delta_id, done_id, output_id) : int * int * int * int)];
  [%expect {| (0 0 0 1) |}]
;;

let%expect_test "stream aliases are scoped by source" =
  let allocator =
    History_entry.Allocator.create ~namespace:"stream-source-test" ~next_sequence:0
    |> Result.ok_or_failwith
  in
  let registry = Chat_response.History_stream_event.Registry.create ~allocator in
  let scope = Chat_response.History_stream_event.Registry.create_scope registry in
  let event =
    Res.Response_stream.Output_text_delta
      { item_id = "same-provider-id"
      ; output_index = 0
      ; content_index = 0
      ; delta = "x"
      ; type_ = "response.output_text.delta"
      }
  in
  let sequence source =
    Chat_response.History_stream_event.Registry.observe registry ~scope ~source event
    |> Option.value_exn
    |> History_entry.Id.sequence
  in
  let outer = sequence None in
  let fork = sequence (Some "fork-call") in
  print_s [%sexp ((outer, fork) : int * int)];
  [%expect {| (0 1) |}]
;;

let%expect_test "stream aliases are scoped by provider response" =
  let allocator =
    History_entry.Allocator.create ~namespace:"stream-response-test" ~next_sequence:0
    |> Result.ok_or_failwith
  in
  let registry = Chat_response.History_stream_event.Registry.create ~allocator in
  let event =
    Res.Response_stream.Output_text_delta
      { item_id = "reused-provider-id"
      ; output_index = 0
      ; content_index = 0
      ; delta = "x"
      ; type_ = "response.output_text.delta"
      }
  in
  let observe scope =
    Chat_response.History_stream_event.Registry.observe registry ~scope ~source:None event
    |> Option.value_exn
    |> History_entry.Id.sequence
  in
  let first =
    observe (Chat_response.History_stream_event.Registry.create_scope registry)
  in
  let second =
    observe (Chat_response.History_stream_event.Registry.create_scope registry)
  in
  print_s [%sexp ((first, second) : int * int)];
  [%expect {| (0 1) |}]
;;

let%expect_test "fork history retains parent IDs and allocates one child instruction" =
  let parent_allocator =
    History_entry.Allocator.create ~namespace:"parent" ~next_sequence:0
    |> Result.ok_or_failwith
  in
  let parent = [ create_entry ~allocator:parent_allocator (input_message "parent") ] in
  let invocation_id = Chat_response.Fork.Invocation_id.create () in
  let child_allocator =
    Chat_response.Fork.allocator ~parent_namespace:"parent" invocation_id
  in
  let child =
    Chat_response.Fork.history_entries
      ~allocator:child_allocator
      ~history:parent
      ~arguments:{|{"command":"inspect","arguments":["one"]}|}
      ~call_id:"reused-call"
  in
  let parent_id = History_entry.id (List.hd_exn parent) in
  let retained_id = History_entry.id (List.hd_exn child) in
  let instruction = List.last_exn child in
  let instruction_call_id =
    match History_entry.item instruction with
    | Res.Item.Function_call_output output -> output.call_id
    | _ -> failwith "Expected fork instruction output"
  in
  let instruction_namespace = History_entry.Id.namespace (History_entry.id instruction) in
  print_s
    [%sexp
      (History_entry.Id.equal parent_id retained_id : bool)
    , (String.is_prefix instruction_namespace ~prefix:"parent/fork-invocation-" : bool)
    , (instruction_call_id : string)
    , (History_entry.Allocator.next_sequence parent_allocator : int)
    , (History_entry.Allocator.next_sequence child_allocator : int)];
  [%expect {| (true true reused-call 1 1) |}]
;;

let%expect_test "repeated call IDs get distinct fork invocation identities" =
  let first = Chat_response.Fork.Invocation_id.create () in
  let second = Chat_response.Fork.Invocation_id.create () in
  print_s
    [%sexp
      (String.equal
         (Chat_response.Fork.Invocation_id.to_string first)
         (Chat_response.Fork.Invocation_id.to_string second)
       : bool)];
  [%expect {| false |}]
;;
