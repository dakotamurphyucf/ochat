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
  let ctx = Ctx.create ~env ~dir ~cache in
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
  let ctx = Ctx.create ~env ~dir ~cache in
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
