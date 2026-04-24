open Core
module App_runtime = Chat_tui.App_runtime
module Builtin_surface = Chatml.Chatml_builtin_surface
module CM = Prompt.Chat_markdown
module Controller = Chat_tui.Moderator_session_controller
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

let ui_notify_artifact () =
  let script =
    CM.
      { id = "ui-main"
      ; language = "chatml"
      ; kind = "moderator"
      ; source =
          Inline
            {|
              type state = int
              type event = [ `Session_start ]

              let initial_state = 0

              let on_event : context -> int -> event -> int task =
                fun ctx st ev ->
                  match ev with
                  | `Session_start ->
                    Task.bind(Ui.notify("watch this"), fun ignored_notify ->
                    Task.pure(st + 1))
            |}
      }
  in
  ok_or_fail
    (Manager.Registry.compile_script
       ~surface:Builtin_surface.ui_moderator_surface
       Manager.Registry.empty
       script)
  |> snd
;;

let approval_prompt_artifact () =
  let script =
    CM.
      { id = "approval-main"
      ; language = "chatml"
      ; kind = "moderator"
      ; source =
          Inline
            {|
              type state = int
              type event = [ `Session_start ]

              let initial_state = 0

              let on_event : context -> int -> event -> int task =
                fun ctx st ev ->
                  match ev with
                  | `Session_start ->
                    Task.bind(Approval.ask_text("continue?"), fun answer ->
                    Task.pure(st + 1))
            |}
      }
  in
  ok_or_fail
    (Manager.Registry.compile_script
       ~surface:Builtin_surface.ui_moderator_surface
       Manager.Registry.empty
       script)
  |> snd
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

let%expect_test "Ui.notify becomes a visible notice without mutating canonical history" =
  let manager =
    ok_or_fail
      (Manager.create
         ~artifact:(ui_notify_artifact ())
         ~capabilities:Moderation.Capabilities.default
         ())
  in
  let outcome =
    ok_or_fail
      (Manager.handle_event
         manager
         ~session_id:"session-1"
         ~now_ms:1
         ~history:[]
         ~available_tools:[]
         ~session_meta:`Null
         ~event:Moderation.Event.Session_start)
  in
  let controller_outcome =
    Controller.of_outcomes
      ~policy:Chat_response.Runtime_semantics.default_policy
      ~turn_request:Controller.Ignore
      [ outcome ]
  in
  let model = model_of_history [] in
  let runtime = App_runtime.create ~model () in
  List.iter controller_outcome.system_notices ~f:(fun text ->
    ignore (App_runtime.add_system_notice_once runtime ~key:("system:" ^ text) text : bool));
  print_s [%sexp (outcome.ui_notifications : string list)];
  print_messages (Chat_tui.Model.messages model);
  print_s [%sexp (List.length (Chat_tui.Model.history_items model) : int)];
  [%expect
    {|
    ("watch this")
    system "watch this"
    0
    |}]
;;

let%expect_test "pending approval prompt is visible without mutating canonical history" =
  let manager =
    ok_or_fail
      (Manager.create
         ~artifact:(approval_prompt_artifact ())
         ~capabilities:Moderation.Capabilities.default
         ())
  in
  ignore
    (ok_or_fail
       (Manager.handle_event
          manager
          ~session_id:"session-1"
          ~now_ms:1
          ~history:[]
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
  let model = model_of_history [] in
  let runtime = App_runtime.create ~model ~moderator () in
  App_runtime.refresh_messages runtime;
  print_s
    [%sexp
      (Option.map (App_runtime.pending_approval runtime) ~f:App_runtime.render_pending_approval
       : string option)];
  print_messages (Chat_tui.Model.messages model);
  print_s [%sexp (List.length (Chat_tui.Model.history_items model) : int)];
  [%expect
    {|
    ("Approval requested: continue?")
    system "Approval requested: continue?"
    0
    |}]
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

let%expect_test "runtime refresh_messages reindexes tool metadata for moderated history" =
  let fc : Res.Function_call.t =
    { name = "read_file"
    ; arguments = "{\"file\": \"foo.txt\"}"
    ; call_id = "call-1"
    ; _type = "function_call"
    ; id = None
    ; status = Some "completed"
    }
  in
  let fco : Res.Function_call_output.t =
    { output = Res.Tool_output.Output.Text "contents"
    ; call_id = fc.call_id
    ; _type = "function_call_output"
    ; id = None
    ; status = Some "completed"
    }
  in
  let history = [ Res.Item.Function_call fc; Res.Item.Function_call_output fco ] in
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
  let tool_outputs = Chat_tui.Model.tool_output_by_index model in
  List.iter [ 0; 1; 2 ] ~f:(fun idx ->
    match Hashtbl.find tool_outputs idx with
    | None -> Printf.printf "%d: none\n" idx
    | Some (Chat_tui.Types.Read_file { path }) ->
      Printf.printf "%d: Read_file path=%s\n" idx (Option.value path ~default:"<none>")
    | Some Chat_tui.Types.Apply_patch -> Printf.printf "%d: Apply_patch\n" idx
    | Some (Chat_tui.Types.Read_directory { path }) ->
      Printf.printf "%d: Read_directory path=%s\n" idx (Option.value path ~default:"<none>")
    | Some (Chat_tui.Types.Other { name }) ->
      Printf.printf "%d: Other name=%s\n" idx (Option.value name ~default:"<none>"));
  [%expect
    {|
    system "policy"
    tool "read_file({\"file\": \"foo.txt\"})"
    tool_output "contents"
    0: none
    1: none
    2: Read_file path=foo.txt
    |}]
;;

let%expect_test "runtime refresh_messages clamps selected message after moderated refresh" =
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
  Chat_tui.Model.select_message model (Some 10);
  let runtime = App_runtime.create ~model ~moderator () in
  App_runtime.refresh_messages runtime;
  print_s [%sexp (Chat_tui.Model.selected_msg model : int option)];
  [%expect {| (1) |}]
;;

let%expect_test "runtime add_system_notice_once suppresses duplicate notices" =
  let model = model_of_history [] in
  let runtime = App_runtime.create ~model () in
  ignore
    (App_runtime.add_system_notice_once
       runtime
       ~key:"system:Session ended by moderator: done"
       "Session ended by moderator: done"
     : bool);
  ignore
    (App_runtime.add_system_notice_once
       runtime
       ~key:"system:Session ended by moderator: done"
       "Session ended by moderator: done"
     : bool);
  print_messages (Chat_tui.Model.messages model);
  [%expect {| system "Session ended by moderator: done" |}]
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
