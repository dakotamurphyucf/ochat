open Core
module CM = Prompt.Chat_markdown
module Config = Chat_response.Config
module Converter = Chat_response.Converter
module Ctx = Chat_response.Ctx
module Res = Openai.Responses

let stub_run_agent ?prompt_dir:_ ?session_id:_ ~ctx:_ _prompt _items = "DUMMY"

let temp_dir prefix =
  let dir =
    Filename.concat Filename.temp_dir_name (prefix ^ Int.to_string (Random.int 1_000_000))
  in
  Core_unix.mkdir_p dir;
  dir
;;

let summarize_items items =
  List.map items ~f:(function
    | Res.Item.Function_call fc ->
      [%sexp ("function_call" : string), (fc.call_id : string), (fc.name : string)]
    | Res.Item.Function_call_output fco ->
      let text =
        match fco.output with
        | Res.Tool_output.Output.Text text -> text
        | Content parts ->
          List.map parts ~f:(function
            | Res.Tool_output.Output_part.Input_text { text } -> text
            | Input_image { image_url; _ } -> image_url)
          |> String.concat ~sep:""
      in
      [%sexp ("tool_response" : string), (fco.call_id : string), (text : string)]
    | Res.Item.Input_message im ->
      let role = Res.Input_message.role_to_string im.role in
      [%sexp ("input_message" : string), (role : string)]
    | item -> [%sexp ("other" : string), (Res.Item.sexp_of_t item : Sexp.t)])
;;

let%expect_test "persist_session materializes synthetic moderator messages" =
  let tmp_dir = temp_dir "persist_materialized_" in
  Eio_main.run
  @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let out_dir = Eio.Path.(fs / tmp_dir) in
  let datadir = Io.ensure_chatmd_dir ~cwd:out_dir in
  let prompt_file = "export.chatmd" in
  Io.save_doc ~dir:out_dir prompt_file "<system>hello</system>\n";
  let overlay =
    Session.Moderator_snapshot.Overlay.
      { empty with
        appended_messages =
          [ { Session.Moderator_snapshot.Message.id = "moderation-overlay-1"
            ; role = "assistant"
            ; content = "synthetic moderation output"
            ; meta = Session.Snapshot.Unit
            }
          ]
      ; deleted_message_ids = [ "msg-1" ]
      ; halted_reason = Some "done"
      }
  in
  let moderator_snapshot =
    Session.Moderator_snapshot.create
      ~script_id:"main"
      ~script_source_hash:"hash"
      ~overlay
      ()
  in
  Chat_tui.Persistence.persist_session
    ~dir:out_dir
    ~prompt_file
    ~datadir
    ~cfg:Config.default
    ~initial_msg_count:0
    ~moderator_snapshot:(Some moderator_snapshot)
    ~history_items:[];
  print_string (Io.load_doc ~dir:out_dir prompt_file);
  [%expect
    {|
    <system>hello</system>
    <msg role="assistant" id="moderation-overlay-1">
    synthetic moderation output
    </msg>
    <msg role="developer" id="moderation-deletion-msg-1">
    Moderator deleted message "msg-1" from the effective transcript.
    </msg>
    <msg role="system" id="moderation-halt">
    Session ended by moderator: done
    </msg>
    |}]
;;

let%expect_test "denied tool transcript round-trips through export and parse" =
  let tmp_dir = temp_dir "persist_denied_tool_" in
  Eio_main.run
  @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let out_dir = Eio.Path.(fs / tmp_dir) in
  let datadir = Io.ensure_chatmd_dir ~cwd:out_dir in
  let prompt_file = "export.chatmd" in
  Io.save_doc ~dir:out_dir prompt_file "<system>hello</system>\n";
  let history_items =
    [ Res.Item.Function_call
        { arguments = {|{"path":"foo.txt"}|}
        ; call_id = "call-1"
        ; name = "read_file"
        ; id = Some "tool-item-1"
        ; status = Some "completed"
        ; _type = "function_call"
        }
    ; Res.Item.Function_call_output
        { output = Res.Tool_output.Output.Text "Denied by moderator"
        ; call_id = "call-1"
        ; _type = "function_call_output"
        ; id = None
        ; status = None
        }
    ]
  in
  Chat_tui.Persistence.persist_session
    ~dir:out_dir
    ~prompt_file
    ~datadir
    ~cfg:Config.default
    ~initial_msg_count:0
    ~moderator_snapshot:None
    ~history_items;
  let prompt_xml = Io.load_doc ~dir:out_dir prompt_file in
  let cache = Chat_response.Cache.create ~max_size:16 () in
  let ctx = Ctx.create ~env ~dir:out_dir ~tool_dir:(Eio.Stdenv.cwd env) ~cache in
  let elements = CM.parse_chat_inputs ~dir:out_dir prompt_xml in
  let items = Converter.to_items ~ctx ~run_agent:stub_run_agent elements in
  print_s [%sexp (summarize_items items : Sexp.t list)];
  [%expect
    {|
    ((input_message system) (function_call call-1 read_file)
     (tool_response call-1 "Denied by moderator"))
    |}]
;;
