open Core
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
  let ctx = Ctx.create ~env ~dir:root ~cache ~tool_dir:(Eio.Stdenv.cwd env) in
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
        (status ()) (function_call ()) (tool_call ()) (tool_call_id ())))))
    NESTED
    |}]
;;

let%expect_test "agent tool declarations reuse local nested prompt moderation" =
  Eio_main.run
  @@ fun env ->
  let root, _ = write_nested_prompt env "tool" in
  let cache = Chat_response.Cache.create ~max_size:5 () in
  let ctx = Ctx.create ~env ~dir:root ~cache ~tool_dir:(Eio.Stdenv.cwd env) in
  Eio.Switch.run
  @@ fun sw ->
  let tools =
    Tool.of_declaration
      ~sw
      ~ctx
      ~run_agent:inspecting_run_agent
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
        (status ()) (function_call ()) (tool_call ()) (tool_call_id ())))))
    NESTED
    |}]
;;

let%expect_test "mcp prompt agents pass prompt-relative context into run_agent" =
  Eio_main.run
  @@ fun env ->
  let _, prompt_path = write_nested_prompt env "mcp" in
  let core = Mcp_server_core.create () in
  let _tool, handler, _prompt =
    Mcp_prompt_agent.of_chatmd_file_with_run_agent
      ~run_agent:(fun ?history_compaction:_ ?prompt_dir ?session_id ~ctx prompt items ->
        inspecting_run_agent ?prompt_dir ?session_id ~ctx prompt items)
      ~env
      ~core
      ~path:prompt_path
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
        (status ()) (function_call ()) (tool_call ()) (tool_call_id ())))))
    NESTED
    |}]
;;
