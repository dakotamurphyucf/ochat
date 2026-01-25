open Core
module Converter = Chat_response.Converter
module Ctx = Chat_response.Ctx
module CM = Prompt.Chat_markdown

(* A dummy [run_agent] implementation for unit-testing. *)
let stub_run_agent ~ctx:_ _prompt _items = "DUMMY"

let%expect_test "string_of_items handles text and image" =
  (* Use [Eio_main.run] to obtain an environment; the code under test only
     relies on the filesystem handle, so the test remains deterministic. *)
  Eio_main.run
  @@ fun env ->
  let dir = Eio.Stdenv.fs env in
  let cache = Chat_response.Cache.create ~max_size:5 () in
  let ctx = Ctx.create ~env ~dir ~cache ~tool_dir:dir in
  let basic txt : CM.content_item =
    CM.Basic
      { type_ = "text"
      ; text = Some txt
      ; image_url = None
      ; document_url = None
      ; is_local = false
      ; cleanup_html = false
      ; markdown = false
      }
  in
  let image_item : CM.content_item =
    CM.Basic
      { type_ = "image_url"
      ; text = None
      ; image_url = Some { url = "http://example.com/pic.png" }
      ; document_url = None
      ; is_local = false
      ; cleanup_html = false
      ; markdown = false
      }
  in
  let items = [ basic "hello"; image_item ] in
  let res = Converter.string_of_items ~ctx ~run_agent:stub_run_agent items in
  print_endline res;
  [%expect
    {|
hello
<img src="http://example.com/pic.png"/>
|}]
;;

let%expect_test "to_items converts user message with basic text" =
  Eio_main.run
  @@ fun env ->
  let dir = Eio.Stdenv.fs env in
  let cache = Chat_response.Cache.create ~max_size:5 () in
  let ctx = Ctx.create ~env ~dir ~cache ~tool_dir:dir in
  (* Build a CM.msg representing: <msg role="user">hello</msg> *)
  let basic_item : CM.content_item =
    CM.Basic
      { type_ = "text"
      ; text = Some "hello"
      ; image_url = None
      ; document_url = None
      ; is_local = false
      ; cleanup_html = false
      ; markdown = false
      }
  in
  let msg : CM.msg =
    { role = "user"
    ; content = Some (CM.Items [ basic_item ])
    ; name = None
    ; id = None
    ; status = None
    ; function_call = None
    ; tool_call = None
    ; tool_call_id = None
    ; type_ = None
    }
  in
  let items = Converter.to_items ~ctx ~run_agent:stub_run_agent [ CM.Msg msg ] in
  print_s [%sexp (items : Openai.Responses.Item.t list)];
  [%expect
    {|
    ((Input_message
      ((role User) (content ((Text ((text hello) (_type input_text)))))
       (_type message))))
    |}]
;;

let%expect_test "to_items converts <tool_call type=custom_tool_call>" =
  Eio_main.run
  @@ fun env ->
  let dir = Eio.Stdenv.fs env in
  let cache = Chat_response.Cache.create ~max_size:5 () in
  let ctx = Ctx.create ~env ~dir ~cache ~tool_dir:dir in
  let tool_call : CM.tool_call =
    { id = "call_1"; function_ = { name = "my_tool"; arguments = "" } }
  in
  let msg : CM.msg =
    { role = "assistant"
    ; type_ = Some "custom_tool_call"
    ; content = Some (CM.Text "{\"x\": 1}")
    ; name = None
    ; id = None
    ; status = None
    ; function_call = None
    ; tool_call = Some tool_call
    ; tool_call_id = Some "call_1"
    }
  in
  let items = Converter.to_items ~ctx ~run_agent:stub_run_agent [ CM.Tool_call msg ] in
  print_s [%sexp (items : Openai.Responses.Item.t list)];
  [%expect
    {|
    ((Custom_tool_call
      ((name my_tool) (input "{\"x\": 1}") (call_id call_1)
       (_type custom_tool_call) (id ()))))
    |}]
;;

let%expect_test "to_items converts <tool_response type=custom_tool_call>" =
  Eio_main.run
  @@ fun env ->
  let dir = Eio.Stdenv.fs env in
  let cache = Chat_response.Cache.create ~max_size:5 () in
  let ctx = Ctx.create ~env ~dir ~cache ~tool_dir:dir in
  let msg : CM.msg =
    { role = "tool"
    ; type_ = Some "custom_tool_call"
    ; content = Some (CM.Text "OK")
    ; name = None
    ; id = None
    ; status = None
    ; function_call = None
    ; tool_call = None
    ; tool_call_id = Some "call_1"
    }
  in
  let items =
    Converter.to_items ~ctx ~run_agent:stub_run_agent [ CM.Tool_response msg ]
  in
  print_s [%sexp (items : Openai.Responses.Item.t list)];
  [%expect
    {|
    ((Custom_tool_call_output
      ((output (Text OK)) (call_id call_1) (_type custom_tool_call_output)
       (id ()))))
    |}]
;;

let%expect_test "to_items converts <msg role=tool type=custom_tool_call>" =
  Eio_main.run
  @@ fun env ->
  let dir = Eio.Stdenv.fs env in
  let cache = Chat_response.Cache.create ~max_size:5 () in
  let ctx = Ctx.create ~env ~dir ~cache ~tool_dir:dir in
  let msg : CM.msg =
    { role = "tool"
    ; type_ = Some "custom_tool_call"
    ; content = Some (CM.Text "OK")
    ; name = None
    ; id = None
    ; status = None
    ; function_call = None
    ; tool_call = None
    ; tool_call_id = Some "call_1"
    }
  in
  let items = Converter.to_items ~ctx ~run_agent:stub_run_agent [ CM.Msg msg ] in
  print_s [%sexp (items : Openai.Responses.Item.t list)];
  [%expect
    {|
    ((Custom_tool_call_output
      ((output (Text OK)) (call_id call_1) (_type custom_tool_call_output)
       (id ()))))
    |}]
;;
