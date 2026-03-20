open Core
module Converter = Chat_response.Converter
module Ctx = Chat_response.Ctx
module CM = Prompt.Chat_markdown

(* A dummy [run_agent] implementation for unit-testing. *)
let stub_run_agent ~ctx:_ _prompt _items = "DUMMY"

let print_parse_chat_inputs ~dir xml =
  try
    let elements = CM.parse_chat_inputs ~dir xml in
    print_s [%sexp (elements : CM.top_level_elements list)]
  with
  | exn -> printf "ERR: %s\n" (Exn.to_string exn)
;;

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

let%expect_test "parse_chat_inputs retains inline script and converter ignores it" =
  Eio_main.run
  @@ fun env ->
  let dir = Eio.Stdenv.fs env in
  let cache = Chat_response.Cache.create ~max_size:5 () in
  let ctx = Ctx.create ~env ~dir ~cache ~tool_dir:dir in
  let elements =
    CM.parse_chat_inputs
      ~dir
      "<script language=\"chatml\" kind=\"moderator\">let x = 1</script><user>Hello</user>"
  in
  print_s [%sexp (elements : CM.top_level_elements list)];
  let items = Converter.to_items ~ctx ~run_agent:stub_run_agent elements in
  print_s [%sexp (items : Openai.Responses.Item.t list)];
  [%expect
    {|
    ((Script
      ((id main) (language chatml) (kind moderator)
       (source (Inline "let x = 1"))))
     (User
      ((role user) (type_ ()) (content ((Text Hello))) (name ()) (id ())
       (status ()) (function_call ()) (tool_call ()) (tool_call_id ()))))
    ((Input_message
      ((role User) (content ((Text ((text Hello) (_type input_text)))))
       (_type message))))
    |}]
;;

let%expect_test "parse_chat_inputs resolves script src relative to the prompt directory" =
  Eio_main.run
  @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  let prompt_dir = Eio.Path.(cwd / "script_prompt_dir") in
  Eio.Path.mkdir ~perm:0o755 prompt_dir;
  Eio.Path.save
    ~create:(`Or_truncate 0o644)
    Eio.Path.(prompt_dir / "moderator.chatml")
    "let x = 1\n";
  let elements =
    CM.parse_chat_inputs
      ~dir:prompt_dir
      "<script language=\"chatml\" kind=\"moderator\" src=\"moderator.chatml\" /><user>Hello</user>"
  in
  print_s [%sexp (elements : CM.top_level_elements list)];
  [%expect
    {|
    ((Script
      ((id main) (language chatml) (kind moderator)
       (source (Src ((path moderator.chatml) (source_text "let x = 1\n"))))))
     (User
      ((role user) (type_ ()) (content ((Text Hello))) (name ()) (id ())
       (status ()) (function_call ()) (tool_call ()) (tool_call_id ()))))
    |}]
;;

let%expect_test "parse_chat_inputs rejects invalid script declarations" =
  Eio_main.run
  @@ fun env ->
  let dir = Eio.Stdenv.cwd env in
  List.iter
    [ "<script language=\"python\" kind=\"moderator\">let x = 1</script>"
    ; "<script language=\"chatml\" kind=\"worker\">let x = 1</script>"
    ; "<script language=\"chatml\" kind=\"moderator\" src=\"moderator.chatml\">let x = 1</script>"
    ; "<script language=\"chatml\" kind=\"moderator\" extra=\"nope\">let x = 1</script>"
    ; "<script language=\"chatml\" kind=\"moderator\" src=\"missing.chatml\" />"
    ; "<script language=\"chatml\" kind=\"moderator\">one</script><script language=\"chatml\" kind=\"moderator\">two</script>"
    ]
    ~f:(print_parse_chat_inputs ~dir);
  [%expect
    {|
    ERR: (Failure "<script> only supports language=\"chatml\"; got \"python\".")
    ERR: (Failure "<script> only supports kind=\"moderator\"; got \"worker\".")
    ERR: (Failure "<script> cannot combine a src attribute with inline body text.")
    ERR: (Failure "<script> does not support attribute \"extra\".")
    ERR: (Failure
      "Failed to load <script src=\"missing.chatml\"> relative to the prompt directory.")
    ERR: (Failure
      "Expected at most one <script language=\"chatml\" kind=\"moderator\"> per prompt, found 2.")
    |}]
;;
