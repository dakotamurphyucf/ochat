open Core

let fixture_ctx ~env ~dir ~tool_dir ~cache =
  let fixture =
    Inference_fixture.create
      ~namespace:"chat_response_converter_test"
      ~default_model:"fixture-model"
      ~post_stream:(fun ~sw:_ ~inputs:_ ->
        failwith "fixture unexpectedly dispatched inference")
  in
  Inference_fixture.ctx fixture ~env ~dir ~tool_dir ~cache ()
;;

module Converter = Chat_response.Converter
module Ctx = Chat_response.Ctx
module CM = Prompt.Chat_markdown

(* A dummy [run_agent] implementation for unit-testing. *)
let stub_run_agent ?prompt_dir:_ ?session_id:_ ~ctx:_ _prompt _items = "DUMMY"

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
  let ctx = fixture_ctx ~env ~dir ~cache ~tool_dir:dir in
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
  let ctx = fixture_ctx ~env ~dir ~cache ~tool_dir:dir in
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
    ; phase = None
    ; ochat_history_id = None
    ; source_context = None
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

let%expect_test "id-less assistant input survives JSON round trip" =
  Eio_main.run
  @@ fun env ->
  let dir = Eio.Stdenv.fs env in
  let cache = Chat_response.Cache.create ~max_size:5 () in
  let ctx = fixture_ctx ~env ~dir ~cache ~tool_dir:dir in
  let elements = CM.parse_chat_inputs ~dir "<assistant>prior response</assistant>" in
  let item = Converter.to_items ~ctx ~run_agent:stub_run_agent elements |> List.hd_exn in
  let decoded =
    Openai.Responses.Item.jsonaf_of_t item |> Openai.Responses.Item.t_of_jsonaf
  in
  print_s [%sexp (decoded : Openai.Responses.Item.t)];
  [%expect
    {|
    (Input_message
     ((role Assistant)
      (content ((Text ((text "prior response") (_type input_text)))))
      (_type message)))
    |}]
;;

let%expect_test "to_items converts <tool_call type=custom_tool_call>" =
  Eio_main.run
  @@ fun env ->
  let dir = Eio.Stdenv.fs env in
  let cache = Chat_response.Cache.create ~max_size:5 () in
  let ctx = fixture_ctx ~env ~dir ~cache ~tool_dir:dir in
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
    ; phase = None
    ; ochat_history_id = None
    ; source_context = None
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
  let ctx = fixture_ctx ~env ~dir ~cache ~tool_dir:dir in
  let msg : CM.msg =
    { role = "tool"
    ; type_ = Some "custom_tool_call"
    ; content = Some (CM.Text "OK")
    ; name = None
    ; id = None
    ; status = None
    ; phase = None
    ; ochat_history_id = None
    ; source_context = None
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
  let ctx = fixture_ctx ~env ~dir ~cache ~tool_dir:dir in
  let msg : CM.msg =
    { role = "tool"
    ; type_ = Some "custom_tool_call"
    ; content = Some (CM.Text "OK")
    ; name = None
    ; id = None
    ; status = None
    ; phase = None
    ; ochat_history_id = None
    ; source_context = None
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
  let ctx = fixture_ctx ~env ~dir ~cache ~tool_dir:dir in
  let elements =
    CM.parse_chat_inputs
      ~dir
      "<script language=\"chatml\" kind=\"moderator\">let x = \
       1</script><user>Hello</user>"
  in
  print_s [%sexp (elements : CM.top_level_elements list)];
  let items = Converter.to_items ~ctx ~run_agent:stub_run_agent elements in
  print_s [%sexp (items : Openai.Responses.Item.t list)];
  [%expect
    {|
    ((Script
      ((id main) (language chatml) (kind moderator) (source (Inline "let x = 1"))
       (source_ref
        (((file <prompt>) (source_dir .) (prompt_dir .) (namespace ())
          (start_pos ((offset 0) (line 1) (column 1)))
          (end_pos ((offset 79) (line 1) (column 80)))
          (source_sha256
           b2eb074d9e142dc2d5fde9f4e5dd8f126766f570658bfa7b10ac0bc04f4cc495))))))
     (User
      ((role user) (type_ ()) (content ((Text Hello))) (name ()) (id ())
       (status ()) (phase ()) (ochat_history_id ()) (source_context (<prompt>))
       (function_call ()) (tool_call ()) (tool_call_id ()))))
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
      "<script language=\"chatml\" kind=\"moderator\" src=\"moderator.chatml\" \
       /><user>Hello</user>"
  in
  print_s [%sexp (elements : CM.top_level_elements list)];
  [%expect
    {|
    ((Script
      ((id main) (language chatml) (kind moderator)
       (source (Src ((path moderator.chatml) (source_text "let x = 1\n"))))
       (source_ref
        (((file <prompt>) (source_dir ./script_prompt_dir)
          (prompt_dir ./script_prompt_dir) (namespace ())
          (start_pos ((offset 0) (line 1) (column 1)))
          (end_pos ((offset 86) (line 1) (column 87)))
          (source_sha256
           55e1d2f55cdaa19bbf2f12422fd6a0e0f4914654fef6d74f2a2f3af9bfd88603))))))
     (User
      ((role user) (type_ ()) (content ((Text Hello))) (name ()) (id ())
       (status ()) (phase ()) (ochat_history_id ()) (source_context (<prompt>))
       (function_call ()) (tool_call ()) (tool_call_id ()))))
    |}]
;;

let%expect_test "parse_chat_inputs rejects invalid script declarations" =
  Eio_main.run
  @@ fun env ->
  let dir = Eio.Stdenv.cwd env in
  List.iter
    [ "<script language=\"python\" kind=\"moderator\">let x = 1</script>"
    ; "<script language=\"chatml\" kind=\"worker\">let x = 1</script>"
    ; "<script language=\"chatml\" kind=\"moderator\" src=\"moderator.chatml\">let x = \
       1</script>"
    ; "<script language=\"chatml\" kind=\"moderator\" extra=\"nope\">let x = 1</script>"
    ; "<script language=\"chatml\" kind=\"moderator\" src=\"missing.chatml\" />"
    ; "<script language=\"chatml\" kind=\"moderator\">one</script><script \
       language=\"chatml\" kind=\"moderator\">two</script>"
    ]
    ~f:(print_parse_chat_inputs ~dir);
  [%expect
    {|
    ERR: (Failure
      "error[chatmd.script_invalid_language] at script.language: only language=chatml is supported")
    ERR: (Failure
      "error[chatmd.script_invalid_kind] at script.kind: unsupported ChatML script kind: worker")
    ERR: (Failure
      "error[chatmd.script_conflicting_source] at script: script cannot combine src with inline source")
    ERR: (Failure
      "error[shell.unknown_attribute] at script: unknown attribute: extra")
    ERR: (Failure
      "error[chatmd.script_source_unavailable] at script.src: failed to load ChatML script \"missing.chatml\" relative to the prompt directory")
    ERR: (Failure
       "error[chatmd.multiple_moderators] at script: at most one moderator script may be selected\
      \nerror[chatmd.duplicate_script] at script.main: duplicate script id: main")
    |}]
;;

let%test_unit "owned content dispatch bypasses stale legacy cache in every output form" =
  Eio_main.run (fun env ->
    let cache = Chat_response.Cache.create ~max_size:5 () in
    let ctx = fixture_ctx ~env ~dir:env#cwd ~tool_dir:env#cwd ~cache in
    let elements =
      CM.parse_chat_inputs
        ~dir:env#cwd
        {|<user><agent src="__owned_content_missing__.chatmd" local>input</agent></user>|}
    in
    let message, declaration =
      match elements with
      | [ CM.User ({ content = Some (Items [ Agent declaration ]); _ } as message) ] ->
        message, declaration
      | _ -> failwith "expected actual content declaration"
    in
    Chat_response.Cache.find_or_add
      cache
      declaration
      ~ttl:Time_ns.Span.day
      ~default:(fun () -> "STALE")
    |> ignore;
    let calls = ref 0 in
    let failure = ref None in
    let runner =
      Chat_response.Tool.Agent_source_runner.
        { revision = Chatml_source.Digest.of_contents "owned-content" ~checkpoint:ignore
        ; run = (fun ~declaration:_ ~ctx:_ ~source:_ ~observer:_ _ -> assert false)
        ; run_content =
            (fun ~declaration:actual ~ctx:actual_ctx ->
              assert (phys_equal actual declaration);
              assert (phys_equal actual_ctx ctx);
              Int.incr calls;
              match !failure with
              | Some exn -> raise exn
              | None -> "OWNED")
        }
    in
    let legacy ?prompt_dir:_ ?session_id:_ ~ctx:_ _ _ =
      failwith "owned dispatch fell back"
    in
    let check () =
      [%test_eq: string]
        (Converter.string_of_items
           ~agent_source_runner:runner
           ~ctx
           ~run_agent:legacy
           [ CM.Agent declaration ])
        "OWNED";
      let result =
        Converter.to_items
          ~agent_source_runner:runner
          ~ctx
          ~run_agent:legacy
          [ CM.User message
          ; CM.Tool_response { message with role = "tool"; tool_call_id = Some "call" }
          ]
      in
      match result with
      | [ Openai.Responses.Item.Input_message
            { content = [ Text { text = first; _ } ]; _ }
        ; Function_call_output { output = Content [ Input_text { text = second } ]; _ }
        ] -> [%test_eq: string * string] (first, second) ("OWNED", "OWNED")
      | _ -> failwith "unexpected content conversion"
    in
    check ();
    [%test_eq: int] !calls 3;
    let sentinel = Failure "original content callback" in
    failure := Some sentinel;
    List.iter
      [ (fun () ->
          Converter.string_of_items
            ~agent_source_runner:runner
            ~ctx
            ~run_agent:legacy
            [ CM.Agent declaration ]
          |> ignore)
      ; (fun () ->
          Converter.to_items
            ~agent_source_runner:runner
            ~ctx
            ~run_agent:legacy
            [ CM.User message ]
          |> ignore)
      ; (fun () ->
          Converter.to_items
            ~agent_source_runner:runner
            ~ctx
            ~run_agent:legacy
            [ CM.Tool_response { message with role = "tool"; tool_call_id = Some "call" }
            ]
          |> ignore)
      ]
      ~f:(fun convert ->
        match convert () with
        | () -> assert false
        | exception actual -> assert (phys_equal actual sentinel));
    failure := None;
    check ();
    [%test_eq: string]
      (Chat_response.Cache.find_or_add
         cache
         declaration
         ~ttl:Time_ns.Span.day
         ~default:(fun () -> assert false))
      "STALE")
;;
