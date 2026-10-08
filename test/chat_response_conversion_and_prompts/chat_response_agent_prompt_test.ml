open Core

let fixture_ctx ~env ~dir ~tool_dir ~cache =
  let fixture =
    Inference_fixture.create
      ~namespace:"chat_response_agent_prompt_test"
      ~default_model:"fixture-model"
      ~post_stream:(fun ~sw:_ ~inputs:_ ->
        failwith "fixture unexpectedly dispatched inference")
  in
  Inference_fixture.ctx fixture ~env ~dir ~tool_dir ~cache ()
;;

module CM = Prompt.Chat_markdown
module Converter = Chat_response.Converter
module Ctx = Chat_response.Ctx
module Res = Openai.Responses
module Tool = Chat_response.Tool

let write_nested_prompt env name =
  let cwd = Eio.Stdenv.cwd env in
  let root = Eio.Path.(cwd / ("task10_nested_prompt_" ^ name)) in
  let agents_dir = Eio.Path.(root / "agents") in
  Eio.Path.mkdirs ~perm:0o755 agents_dir;
  Eio.Path.save
    ~create:(`Or_truncate 0o644)
    Eio.Path.(agents_dir / "moderator.chatml")
    "let x = 1\n";
  Eio.Path.save
    ~create:(`Or_truncate 0o644)
    Eio.Path.(agents_dir / "child.chatmd")
    "<script language=\"chatml\" kind=\"moderator\" src=\"moderator.chatml\" \
     /><system>nested</system>";
  root, Eio.Path.(agents_dir / "child.chatmd")
;;

let inspecting_run_agent ?prompt_dir ?session_id ~ctx:_ prompt_xml items =
  let dir =
    Option.value_exn prompt_dir ~message:"expected prompt_dir for local nested agent"
  in
  let elements = CM.parse_chat_inputs ~dir prompt_xml in
  print_s
    [%sexp
      (session_id : string option)
    , (List.length items : int)
    , (elements : CM.top_level_elements list)];
  "NESTED"
;;

let text_output = function
  | Res.Tool_output.Output.Text text -> print_endline text
  | Res.Tool_output.Output.Content parts ->
    List.iter parts ~f:(function
      | Res.Tool_output.Output_part.Input_text { text } -> print_endline text
      | Input_image { image_url; _ } -> print_endline image_url)
;;

let%expect_test "converter rebases local nested agent prompts to their prompt directory" =
  Eio_main.run
  @@ fun env ->
  let root, _ = write_nested_prompt env "converter" in
  let cache = Chat_response.Cache.create ~max_size:5 () in
  let ctx = fixture_ctx ~env ~dir:root ~cache ~tool_dir:(Eio.Stdenv.cwd env) in
  let text =
    Converter.string_of_items
      ~ctx
      ~run_agent:inspecting_run_agent
      [ CM.Agent { url = "agents/child.chatmd"; is_local = true; items = [] } ]
  in
  print_endline text;
  [%expect
    {|
    ((agents/child.chatmd) 0
     ((Script
       ((id main) (language chatml) (kind moderator)
        (source (Src ((path moderator.chatml) (source_text "let x = 1\n"))))))
      (System
       ((role system) (type_ ()) (content ((Text nested))) (name ()) (id ())
        (status ()) (phase ()) (ochat_history_id ()) (source_context (<prompt>))
        (function_call ()) (tool_call ()) (tool_call_id ())))))
    NESTED
    |}]
;;

let%expect_test "agent tool declarations reuse local nested prompt moderation" =
  Eio_main.run
  @@ fun env ->
  let root, _ = write_nested_prompt env "tool" in
  let cache = Chat_response.Cache.create ~max_size:5 () in
  let ctx = fixture_ctx ~env ~dir:root ~cache ~tool_dir:(Eio.Stdenv.cwd env) in
  Eio.Switch.run
  @@ fun sw ->
  let tools =
    Tool.of_declaration
      ~sw
      ~ctx
      ~run_agent:(fun ?prompt_dir ?session_id ?observer:_ ~source:_ ~ctx prompt items ->
        inspecting_run_agent ?prompt_dir ?session_id ~ctx prompt items)
      (CM.Agent
         { name = "nested"
         ; description = None
         ; agent = "agents/child.chatmd"
         ; is_local = true
         })
  in
  let tool = List.hd_exn tools in
  text_output (tool.run {|{"input":"hello"}|});
  [%expect
    {|
    ((agents/child.chatmd) 1
     ((Script
       ((id main) (language chatml) (kind moderator)
        (source (Src ((path moderator.chatml) (source_text "let x = 1\n"))))))
      (System
       ((role system) (type_ ()) (content ((Text nested))) (name ()) (id ())
        (status ()) (phase ()) (ochat_history_id ()) (source_context (<prompt>))
        (function_call ()) (tool_call ()) (tool_call_id ())))))
    NESTED
    |}]
;;

let%expect_test "agent tool forwards observed nested activity but direct run stays silent"
  =
  Eio_main.run
  @@ fun env ->
  let root, _ = write_nested_prompt env "observed-tool" in
  let cache = Chat_response.Cache.create ~max_size:5 () in
  let ctx = fixture_ctx ~env ~dir:root ~cache ~tool_dir:(Eio.Stdenv.cwd env) in
  Eio.Switch.run
  @@ fun sw ->
  let observer_count = ref 0 in
  let sources = Queue.create () in
  let run_agent ?prompt_dir:_ ?session_id:_ ?observer ~source ~ctx:_ _prompt _items =
    Queue.enqueue sources source;
    Option.iter
      observer
      ~f:(fun (observer : Chat_response.Agent_response_loop.observer) ->
        Int.incr observer_count;
        let ok = Result.ok_or_failwith in
        let scope =
          Transcript.Scope.create
            ~source:(Transcript.Source_id.of_string "injected-child" |> ok)
            ~attempt:(Transcript.Attempt_id.of_string "actual-fixture-invocation" |> ok)
            ~relation:Root
          |> ok
        in
        let item =
          Transcript.Item.create
            ~scope
            ~id:(Transcript.Item_id.of_string "message" |> ok)
            ~entry_id:None
            ~header:None
            ~call_name:None
          |> ok
        in
        let part =
          Transcript.Part.create
            ~item
            ~id:(Transcript.Part_id.of_string "text" |> ok)
            ~index:(Some 0)
            ~kind:Text
          |> ok
        in
        Transcript.Stream.create
          (Changed { target = Content part; change = Append "live" })
          ~limits:Transcript.Admission.default
        |> ok
        |> observer.on_event);
    "FINAL"
  in
  let tool =
    Tool.of_declaration
      ~sw
      ~ctx
      ~run_agent
      (CM.Agent
         { name = "nested"
         ; description = None
         ; agent = "agents/child.chatmd"
         ; is_local = true
         })
    |> List.hd_exn
  in
  text_output (tool.run {|{"input":"direct"}|});
  let progress = Queue.create () in
  let invocation =
    Ochat_function.Invocation.create (fun item -> Queue.enqueue progress item)
  in
  text_output (tool.run_with_progress ~invocation {|{"input":"observed"}|});
  text_output (tool.run {|{"input":"direct-again"}|});
  Queue.iter progress ~f:(fun { channel; update } ->
    let channel =
      match channel with
      | `Assistant -> "assistant"
      | _ -> "other"
    in
    let text =
      match update with
      | Append text | Replace text -> text
    in
    print_endline (channel ^ ":" ^ text));
  print_s [%sexp (!observer_count : int)];
  print_s
    [%sexp
      (Queue.length sources : int)
    , (List.length (List.dedup_and_sort (Queue.to_list sources) ~compare:String.compare)
       : int)
    , (List.for_all (Queue.to_list sources) ~f:(fun source ->
         not (String.equal source "agents/child.chatmd"))
       : bool)];
  [%expect
    {|
    FINAL
    FINAL
    FINAL
    assistant:live
    1
    (3 3 true)
    |}]
;;

let%expect_test "mcp prompt agents pass prompt-relative context into run_agent" =
  Eio_main.run
  @@ fun env ->
  let _, prompt_path = write_nested_prompt env "mcp" in
  let core = Mcp_server_core.create () in
  let dir = Eio.Stdenv.cwd env in
  let ctx =
    fixture_ctx ~env ~dir ~tool_dir:dir ~cache:(Chat_response.Cache.create ~max_size:1 ())
  in
  let _tool, handler, _prompt =
    Mcp_prompt_agent.of_chatmd_file_with_run_agent
      ~inference_context:ctx.inference_context
      ~inference_identity:ctx.inference_identity
      ~on_inference_attempt:ctx.on_inference_attempt
      ~on_inference_completion:ctx.on_inference_completion
      ~run_agent:(fun ?history_compaction:_ ?prompt_dir ?session_id ~ctx prompt items ->
        inspecting_run_agent ?prompt_dir ?session_id ~ctx prompt items)
      ~env
      ~core
      ~path:prompt_path
      ()
  in
  (match handler (`Object [ "input", `String "hello" ]) with
   | Ok (`String text) -> print_endline text
   | Ok json -> print_endline (Jsonaf.to_string json)
   | Error msg -> print_endline (Printf.sprintf "ERR: %s" msg));
  [%expect
    {|
    ((child) 1
     ((Script
       ((id main) (language chatml) (kind moderator)
        (source (Src ((path moderator.chatml) (source_text "let x = 1\n"))))))
      (System
       ((role system) (type_ ()) (content ((Text nested))) (name ()) (id ())
        (status ()) (phase ()) (ochat_history_id ()) (source_context (<prompt>))
        (function_call ()) (tool_call ()) (tool_call_id ())))))
    NESTED
    |}]
;;
