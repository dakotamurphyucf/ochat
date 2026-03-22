open Core
module App_runtime = Chat_tui.App_runtime
module CM = Prompt.Chat_markdown
module Manager = Chat_response.Moderator_manager
module Moderation = Chat_response.Moderation
module Res = Openai.Responses

let ok_or_fail = function
  | Ok value -> value
  | Error msg -> failwith msg
;;

let input_text text = Res.Input_message.Text { text; _type = "input_text" }

let model_of_history history =
  Chat_tui.Model.create
    ~history_items:history
    ~messages:(Chat_tui.Conversation.of_history history)
    ~input_line:""
    ~auto_follow:true
    ~msg_buffers:(Hashtbl.create (module String))
    ~function_name_by_id:(Hashtbl.create (module String))
    ~reasoning_idx_by_id:(Hashtbl.create (module String))
    ~tool_output_by_index:(Hashtbl.create (module Int))
    ~tasks:[]
    ~kv_store:(Hashtbl.create (module String))
    ~fetch_sw:None
    ~scroll_box:(Notty_scroll_box.create Notty.I.empty)
    ~cursor_pos:0
    ~selection_anchor:None
    ~mode:Chat_tui.Model.Insert
    ~draft_mode:Chat_tui.Model.Plain
    ~selected_msg:None
    ~undo_stack:[]
    ~redo_stack:[]
    ~cmdline:""
    ~cmdline_cursor:0
;;

let moderator_script =
  {|
    type state = { count : int }
    type event = [ `Session_start | `Session_resume | `Queued(string) ]

    let initial_state = { count = 0 }

    let on_event : context -> state -> event -> state task =
      fun ctx st ev ->
        match ev with
        | `Session_start ->
          Task.bind(Turn.prepend_system("policy"), fun ignored_turn ->
          Task.bind(Runtime.emit(`Queued("queued")), fun ignored_emit ->
          Task.pure({ count = st.count + 1 })))
        | `Session_resume ->
          Task.bind(Turn.prepend_system("resumed"), fun ignored_turn ->
          Task.pure({ count = st.count + 1 }))
        | `Queued(_) ->
          Task.bind
            (Turn.append_message
               ({ id = "synthetic-1"
                ; value =
                    Json.parse("{\"type\":\"message\",\"role\":\"assistant\",\"id\":\"synthetic-1\",\"content\":[{\"annotations\":[],\"text\":\"queued\",\"type\":\"output_text\"}],\"status\":\"completed\"}")
                }),
             fun ignored_turn ->
             Task.pure(st))
  |}
;;

let artifact () =
  let script =
    CM.
      { id = "main"
      ; language = "chatml"
      ; kind = "moderator"
      ; source = Inline moderator_script
      }
  in
  ok_or_fail (Manager.Registry.compile_script Manager.Registry.empty script) |> snd
;;

let create_manager ?snapshot () =
  let artifact = artifact () in
  let capabilities = Moderation.Capabilities.default in
  ok_or_fail (Manager.create ~artifact ~capabilities ?snapshot ())
;;

let print_messages messages =
  List.iter messages ~f:(fun (role, text) ->
    print_endline (Printf.sprintf "%s %S" role text))
;;

let%expect_test "runtime visible history reflects restored moderator snapshot" =
  let history =
    [ Res.Item.Input_message
        { role = Res.Input_message.User
        ; content = [ input_text "Hello" ]
        ; _type = "message"
        }
    ]
  in
  let seeded_manager = create_manager () in
  ignore
    (ok_or_fail
       (Manager.handle_event
          seeded_manager
          ~session_id:"session-1"
          ~now_ms:1
          ~history
          ~available_tools:[]
          ~session_meta:`Null
          ~event:Moderation.Event.Session_start)
     : Moderation.Outcome.t);
  ignore
    (ok_or_fail
       (Manager.drain_internal_events
          seeded_manager
          ~session_id:"session-1"
          ~now_ms:2
          ~history
          ~available_tools:[]
          ~session_meta:`Null)
     : Moderation.Outcome.t list);
  let snapshot = ok_or_fail (Manager.snapshot seeded_manager) in
  let resumed_manager = create_manager ~snapshot () in
  let moderator =
    Chat_response.In_memory_stream.
      { manager = resumed_manager
      ; session_id = "session-1"
      ; session_meta = `Null
      ; runtime_policy = Chat_response.Runtime_semantics.default_policy
      }
  in
  let runtime = App_runtime.create ~model:(model_of_history history) ~moderator () in
  print_messages (App_runtime.visible_messages_of_history runtime history);
  print_s
    [%sexp (Option.is_some (ok_or_fail (App_runtime.moderator_snapshot runtime)) : bool)];
  [%expect
    {|
    system "policy"
    user "Hello"
    assistant "queued"
    true
    |}]
;;

let%expect_test "runtime refresh_messages uses moderated visible history" =
  let history =
    [ Res.Item.Input_message
        { role = Res.Input_message.User
        ; content = [ input_text "Hello" ]
        ; _type = "message"
        }
    ]
  in
  let manager = create_manager () in
  ignore
    (ok_or_fail
       (Manager.handle_event
          manager
          ~session_id:"session-1"
          ~now_ms:1
          ~history
          ~available_tools:[]
          ~session_meta:`Null
          ~event:Moderation.Event.Session_start)
     : Moderation.Outcome.t);
  let moderator =
    Chat_response.In_memory_stream.
      { manager
      ; session_id = "session-1"
      ; session_meta = `Null
      ; runtime_policy = Chat_response.Runtime_semantics.default_policy
      }
  in
  let model = model_of_history history in
  let runtime = App_runtime.create ~model ~moderator () in
  App_runtime.refresh_messages runtime;
  print_messages (Chat_tui.Model.messages model);
  [%expect
    {|
    system "policy"
    user "Hello"
    |}]
;;

let%expect_test "runtime visible history reflects explicit session_resume moderation" =
  let history =
    [ Res.Item.Input_message
        { role = Res.Input_message.User
        ; content = [ input_text "Hello" ]
        ; _type = "message"
        }
    ]
  in
  let seeded_manager = create_manager () in
  ignore
    (ok_or_fail
       (Manager.handle_event
          seeded_manager
          ~session_id:"session-1"
          ~now_ms:1
          ~history
          ~available_tools:[]
          ~session_meta:`Null
          ~event:Moderation.Event.Session_start)
     : Moderation.Outcome.t);
  ignore
    (ok_or_fail
       (Manager.drain_internal_events
          seeded_manager
          ~session_id:"session-1"
          ~now_ms:2
          ~history
          ~available_tools:[]
          ~session_meta:`Null)
     : Moderation.Outcome.t list);
  let snapshot = ok_or_fail (Manager.snapshot seeded_manager) in
  let resumed_manager = create_manager ~snapshot () in
  ignore
    (ok_or_fail
       (Manager.handle_event
          resumed_manager
          ~session_id:"session-1"
          ~now_ms:3
          ~history
          ~available_tools:[]
          ~session_meta:`Null
          ~event:Moderation.Event.Session_resume)
     : Moderation.Outcome.t);
  let moderator =
    Chat_response.In_memory_stream.
      { manager = resumed_manager
      ; session_id = "session-1"
      ; session_meta = `Null
      ; runtime_policy = Chat_response.Runtime_semantics.default_policy
      }
  in
  let runtime = App_runtime.create ~model:(model_of_history history) ~moderator () in
  print_messages (App_runtime.visible_messages_of_history runtime history);
  [%expect
    {|
    system "policy"
    system "resumed"
    user "Hello"
    assistant "queued"
    |}]
;;
