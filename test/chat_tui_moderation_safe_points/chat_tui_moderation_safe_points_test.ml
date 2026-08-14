open Core
open Eio.Std
module App_runtime = Chat_tui.App_runtime
module Builtin_surface = Chatml.Chatml_builtin_surface
module CM = Prompt.Chat_markdown
module Lang = Chatml.Chatml_lang
module Manager = Chat_response.Moderator_manager
module Moderation = Chat_response.Moderation
module Res = Openai.Responses
module Stream = Chat_response.In_memory_stream
module Shell = Shell_access
module Shell_broker = Shell_runtime.Approval_broker

let ok_or_fail = function
  | Ok value -> value
  | Error msg -> failwith msg
;;

let input_text text = Res.Input_message.Text { text; _type = "input_text" }

let user_message text =
  Res.Item.Input_message
    { role = Res.Input_message.User; content = [ input_text text ]; _type = "message" }
;;

let model_of_history history =
  let allocator =
    History_entry.Allocator.create ~namespace:"moderator-wakeup-test" ~next_sequence:0
    |> ok_or_fail
  in
  let history =
    List.map history ~f:(History_entry.create ~allocator) |> Result.all |> ok_or_fail
  in
  Chat_tui.Model.create
    ~history_items:history
    ~messages:(Chat_tui.Conversation.of_history (History_entry.items history))
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

let shell_ui_request () =
  let fingerprint : Shell.Executable.fingerprint =
    { device = 1
    ; inode = 2
    ; mode = 0o755
    ; uid = 1
    ; gid = 1
    ; size = 10L
    ; mtime = 0.
    ; sha256 = String.make 64 'a'
    }
  in
  let executable : Shell.Executable.t =
    { requested = "echo"
    ; path = "/bin/echo"
    ; canonical_path = "/bin/echo"
    ; trusted = true
    ; fingerprint
    }
  in
  let context : Shell.Context.t =
    { request_id = "request-1"
    ; runtime_id = "commands"
    ; manifest_sha256 = String.make 64 'b'
    ; command = Shell.Command.create "echo" [ "hello" ]
    ; executable
    ; cwd = "/workspace"
    ; environment = [||]
    ; request_kind = Structured
    ; stdin_kind = Empty
    ; stdin_sha256 = None
    ; stdin_bytes = 0
    ; script_sha256 = None
    ; script_preview = None
    ; origin = Tool
    ; effects = [ Shell.Effect.Read_path "/workspace" ]
    ; capabilities = Shell.Capabilities.development ~workspace:"/workspace"
    ; policy_action = None
    ; policy_matches = []
    ; session_id = Some "session"
    }
  in
  let policy : Shell.Policy.decision =
    { action = Ask; matches = []; reason = "default ask policy" }
  in
  let identity : Shell.Approval.identity =
    { manifest_sha256 = context.manifest_sha256
    ; runtime_id = context.runtime_id
    ; request_kind = context.request_kind
    ; command_hash = "command"
    ; executable_sha256 = fingerprint.sha256
    ; argv = [ "echo"; "hello" ]
    ; cwd_sha256 = "cwd"
    ; environment_sha256 = "env"
    ; stdin_sha256 = None
    ; stdin_bytes = 0
    ; script_sha256 = None
    }
  in
  let approval_request : Shell.Approval.request =
    { context; policy; identity; display_command = "echo hello"; rationale = Some "test" }
  in
  Shell_broker.
    { id = "shell-approval-test"
    ; request = approval_request
    ; runtime_id = "commands"
    ; manifest_sha256 = context.manifest_sha256
    ; scopes = [ Chatmd_shell_spec.Shell_spec.Once; Exact_session ]
    }
;;

let moderator_script =
  {|
    type state = { count : int }
    type event = [ `Queued(string) ]

    let initial_state = { count = 0 }

    let on_event : context -> state -> event -> state task =
      fun ctx st ev ->
        match ev with
        | `Queued(text) ->
          Task.bind(Turn.append_item(Item.output_text_message("synthetic-1", text)), fun ignored_turn ->
          Task.bind(Runtime.request_turn(), fun ignored_request ->
          Task.pure({ count = st.count + 1 })))
  |}
;;

let model_job_moderator_script =
  {|
    type state = { seen : int }
    type event =
      [ `Model_job_succeeded(string, string, json)
      | `Model_job_failed(string, string, string)
      ]

    let initial_state = { seen = 0 }

    let on_event : context -> state -> event -> state task =
      fun ctx st ev ->
        match ev with
        | `Model_job_succeeded(job_id, recipe, result_json) ->
          let text = recipe ++ ":completed" in
          Task.bind
            (Turn.append_item(Item.output_text_message("job-" ++ job_id, text)),
             fun ignored_turn ->
             Task.pure({ seen = st.seen + 1 }))
        | `Model_job_failed(job_id, recipe, message) ->
          let text = recipe ++ ":ERROR:" ++ message in
          Task.bind
            (Turn.append_item(Item.output_text_message("job-" ++ job_id, text)),
             fun ignored_turn ->
             Task.pure({ seen = st.seen + 1 }))
  |}
;;

let artifact ?(source = moderator_script) () =
  let surface = Chatml.Chatml_builtin_surface.moderator_surface in
  let script =
    CM.{ id = "main"; language = "chatml"; kind = "moderator"; source = Inline source }
  in
  ok_or_fail (Manager.Registry.compile_script ~surface Manager.Registry.empty script)
  |> snd
;;

let create_moderator
      ?source
      ?(surface = Chatml.Chatml_builtin_surface.moderator_surface)
      ?(runtime_policy = Chat_response.Runtime_semantics.default_policy)
      ()
  =
  let allocator =
    History_entry.Allocator.create ~namespace:"moderator-wakeup-test" ~next_sequence:1
    |> ok_or_fail
  in
  let manager =
    ok_or_fail
      (Manager.create_entries
         ~artifact:
           (let script =
              CM.
                { id = "main"
                ; language = "chatml"
                ; kind = "moderator"
                ; source = Inline (Option.value source ~default:moderator_script)
                }
            in
            ok_or_fail
              (Manager.Registry.compile_script ~surface Manager.Registry.empty script)
            |> snd)
         ~capabilities:Moderation.Capabilities.default
         ~allocator
         ())
  in
  Chat_response.In_memory_stream.
    { manager; session_id = "session-1"; session_meta = `Null; runtime_policy }
;;

let runtime_policy_with_budget budget =
  { Chat_response.Runtime_semantics.default_policy with budget }
;;

let approval_ask_text_script =
  {|
    type state = { approved : string }
    type event = [ `Queued(string) ]

    let initial_state = { approved = "" }

    let on_event : context -> state -> event -> state task =
      fun ctx st ev ->
        match ev with
        | `Queued(prompt) ->
          Task.bind(Approval.ask_text(prompt), fun answer ->
          Task.bind(Turn.append_item(Item.output_text_message("approved-1", answer)), fun ignored_turn ->
          Task.pure({ approved = answer })))
  |}
;;

let approval_ask_choice_script =
  {|
    type state = { approved : string }
    type event = [ `Queued ]

    let initial_state = { approved = "" }

    let on_event : context -> state -> event -> state task =
      fun ctx st ev ->
        match ev with
        | `Queued ->
          Task.bind(Approval.ask_choice("Pick one", ["yes", "no"]), fun answer ->
          Task.bind(Turn.append_item(Item.output_text_message("choice-1", answer)), fun ignored_turn ->
          Task.pure({ approved = answer })))
  |}
;;

let startup_overlay_script =
  {|
    type state = { started : bool }
    type event = [ `Session_start ]

    let initial_state = { started = false }

    let on_event : context -> state -> event -> state task =
      fun ctx st ev ->
        match ev with
        | `Session_start ->
          Task.bind(Turn.prepend_system("startup policy"), fun ignored ->
          Task.pure({ started = true }))
  |}
;;

let startup_end_session_script =
  {|
    type state = { started : bool }
    type event = [ `Session_start ]

    let initial_state = { started = false }

    let on_event : context -> state -> event -> state task =
      fun ctx st ev ->
        match ev with
        | `Session_start ->
          Task.bind(Runtime.end_session("startup denied the session"), fun ignored ->
          Task.pure({ started = true }))
  |}
;;

let enqueue_queued_event moderator text =
  ok_or_fail
    (Manager.enqueue_internal_event
       moderator.Chat_response.In_memory_stream.manager
       (Lang.VVariant ("Queued", [ Lang.VString text ])))
;;

let print_messages messages =
  List.iter messages ~f:(fun (role, text) ->
    print_endline (Printf.sprintf "%s %S" role text))
;;

let input_message_text_exn (item : Res.Item.t) =
  match item with
  | Res.Item.Input_message message ->
    (match message.content with
     | [ Res.Input_message.Text { text; _ } ] -> text
     | _ -> failwith "expected a single text input message")
  | _ -> failwith "expected an input message"
;;

let string_of_turn_reason = function
  | App_runtime.User_submit -> "user_submit"
  | Moderator_request -> "moderator_request"
  | Idle_followup -> "idle_followup"
;;

let with_reducer_context
      ?(start_streaming = fun ~history:_ ~op_id:_ -> ())
      ?shell_approval_broker
      ?(moderator_startup_state = App_runtime.Ready)
      ~model
      ~moderator
      f
  =
  Eio_main.run
  @@ fun env ->
  Switch.run
  @@ fun ui_sw ->
  let input_stream : Chat_tui.App_events.input_event Eio.Stream.t =
    Eio.Stream.create 128
  in
  let internal_stream : Chat_tui.App_events.internal_event Eio.Stream.t =
    Eio.Stream.create 128
  in
  let redraw_stream = Eio.Stream.create 1 in
  let streams : Chat_tui.App_context.Streams.t =
    { input = input_stream; internal = internal_stream; redraw = redraw_stream }
  in
  let redraw () = () in
  let throttler =
    Chat_tui.Redraw_throttle.create ~fps:60. ~enqueue_redraw:(fun () -> ())
  in
  let dummy_term : Notty_eio.Term.t = Obj.magic 0 in
  let ui : Chat_tui.App_context.Ui.t =
    { term = dummy_term
    ; size = (fun () -> 80, 24)
    ; throttler
    ; redraw
    ; redraw_immediate = redraw
    ; latest_frame_generation = (fun () -> 0)
    ; resize_and_redraw = (fun ~size:_ ~layout:_ -> redraw ())
    ; render_current_with_layout = (fun ~size:_ ~layout:_ -> redraw ())
    }
  in
  let cwd = Eio.Stdenv.cwd env in
  let cache = Chat_response.Cache.create ~max_size:1 () in
  let services : Chat_tui.App_context.Services.t =
    { env; ui_sw; cwd; cache; datadir = cwd; session = None }
  in
  let shared : Chat_tui.App_context.Resources.t = { services; streams; ui } in
  let runtime =
    match moderator with
    | None ->
      Chat_tui.App_runtime.create
        ~model
        ~moderator_startup_state
        ?shell_approval_broker
        ()
    | Some moderator ->
      Chat_tui.App_runtime.create
        ~model
        ~moderator
        ~moderator_startup_state
        ?shell_approval_broker
        ()
  in
  let streaming : Chat_tui.App_streaming.Context.t =
    { shared
    ; allocator = runtime.history_allocator
    ; cfg = Chat_response.Config.default
    ; tools = []
    ; tool_tbl = Hashtbl.create (module String)
    ; moderator
    ; safe_point_input = Some (Chat_tui.App_runtime.safe_point_input_source runtime)
    ; parallel_tool_calls = true
    ; history_compaction = false
    }
  in
  let submit : Chat_tui.App_submit.Context.t = { runtime; streaming; start_streaming } in
  let compaction : Chat_tui.App_compaction.Context.t = { shared; runtime } in
  let ctx : Chat_tui.App_reducer.Context.t =
    { runtime; shared; submit; compaction; cancelled = Chat_tui.App_streaming.Cancelled }
  in
  let finished, finished_u = Promise.create () in
  Fiber.fork ~sw:ui_sw (fun () ->
    let quit_via_esc = Chat_tui.App_reducer.run ctx in
    Promise.resolve finished_u quit_via_esc);
  let pump () = Fiber.yield () in
  let pump_until ?(max_iters = 2_000) pred =
    let rec loop n =
      if pred ()
      then ()
      else if n = 0
      then failwith "timeout waiting for reducer to process events"
      else (
        pump ();
        loop (n - 1))
    in
    loop max_iters
  in
  let send_internal (ev : Chat_tui.App_events.internal_event) =
    Eio.Stream.add internal_stream ev
  in
  let send_input (ev : Chat_tui.App_events.input_event) =
    Eio.Stream.add input_stream ev
  in
  let finish_active_op () =
    match runtime.Chat_tui.App_runtime.op with
    | Some (App_runtime.Streaming { id; sw = _ } | App_runtime.Starting_streaming { id })
      ->
      Eio.Stream.add
        internal_stream
        (`Streaming_done (id, Chat_tui.Model.history_items model));
      pump_until (fun () -> Option.is_none runtime.Chat_tui.App_runtime.op)
    | Some (App_runtime.Compacting { id; sw = _ } | App_runtime.Starting_compaction { id })
      ->
      Eio.Stream.add
        internal_stream
        (`Compaction_done (id, Chat_tui.Model.history_items model));
      pump_until (fun () -> Option.is_none runtime.Chat_tui.App_runtime.op)
    | None -> ()
  in
  let stop () =
    finish_active_op ();
    Chat_tui.Model.set_mode model Chat_tui.Model.Normal;
    Eio.Stream.add input_stream (`Key (`Escape, []));
    Promise.await finished
  in
  f ~runtime ~services ~send_input ~send_internal ~pump ~pump_until ~stop
;;

let with_reducer ?(start_streaming = fun ~history:_ ~op_id:_ -> ()) ~model ~moderator f =
  with_reducer_context
    ~start_streaming
    ~model
    ~moderator:(Some moderator)
    (fun ~runtime ~services:_ ~send_input:_ ~send_internal ~pump:_ ~pump_until ~stop ->
       f ~runtime ~send_internal ~pump_until ~stop)
;;

let%expect_test "idle wakeup drains moderator queue and starts followup" =
  let history = [ user_message "Hello" ] in
  let model = model_of_history history in
  let moderator = create_moderator () in
  let started_turns = ref [] in
  enqueue_queued_event moderator "background";
  with_reducer
    ~model
    ~moderator
    ~start_streaming:(fun ~history ~op_id ->
      started_turns := (op_id, history) :: !started_turns)
    (fun ~runtime ~send_internal ~pump_until ~stop ->
       send_internal `Moderator_wakeup;
       pump_until (fun () ->
         (match runtime.App_runtime.op with
          | Some (App_runtime.Starting_streaming _) -> true
          | Some
              ( App_runtime.Streaming _
              | App_runtime.Compacting _
              | App_runtime.Starting_compaction _ )
          | None -> false)
         && Option.is_none runtime.App_runtime.session_controller.pending_turn_request
         && (not (App_runtime.is_moderator_dirty runtime))
         && List.length (Chat_tui.Model.messages model) = 2);
       print_messages (Chat_tui.Model.messages model);
       print_endline
         (Printf.sprintf
            "reason=%s started_history=%d pending=%b"
            (Option.value_map
               (App_runtime.active_turn_start_reason runtime)
               ~default:"<none>"
               ~f:string_of_turn_reason)
            (List.length (snd (List.hd_exn !started_turns)))
            (Option.is_some runtime.App_runtime.session_controller.pending_turn_request));
       ignore (stop () : bool));
  [%expect
    {|
    user "Hello"
    assistant "background"
    reason=idle_followup started_history=1 pending=false
    |}]
;;

let%expect_test "wakeup during active turn is deferred until safe point" =
  let history = [ user_message "Hello" ] in
  let model = model_of_history history in
  let moderator = create_moderator () in
  let started_turns = ref [] in
  enqueue_queued_event moderator "later";
  with_reducer
    ~model
    ~moderator
    ~start_streaming:(fun ~history ~op_id ->
      started_turns := (op_id, history) :: !started_turns)
    (fun ~runtime ~send_internal ~pump_until ~stop ->
       runtime.App_runtime.op <- Some (App_runtime.Starting_streaming { id = 7 });
       send_internal `Moderator_wakeup;
       pump_until (fun () -> App_runtime.is_moderator_dirty runtime);
       print_messages (Chat_tui.Model.messages model);
       print_endline
         (Printf.sprintf
            "dirty=%b pending=%b started_before_done=%d"
            (App_runtime.is_moderator_dirty runtime)
            (Option.is_some runtime.App_runtime.session_controller.pending_turn_request)
            (List.length !started_turns));
       send_internal (`Streaming_done (7, Chat_tui.Model.history_items model));
       pump_until (fun () ->
         (match runtime.App_runtime.op with
          | Some (App_runtime.Starting_streaming _) -> true
          | Some
              ( App_runtime.Streaming _
              | App_runtime.Compacting _
              | App_runtime.Starting_compaction _ )
          | None -> false)
         && Option.is_none runtime.App_runtime.session_controller.pending_turn_request
         && (not (App_runtime.is_moderator_dirty runtime))
         && List.length (Chat_tui.Model.messages model) = 2);
       print_messages (Chat_tui.Model.messages model);
       print_endline
         (Printf.sprintf
            "dirty=%b reason=%s started_history=%d pending=%b"
            (App_runtime.is_moderator_dirty runtime)
            (Option.value_map
               (App_runtime.active_turn_start_reason runtime)
               ~default:"<none>"
               ~f:string_of_turn_reason)
            (List.length (snd (List.hd_exn !started_turns)))
            (Option.is_some runtime.App_runtime.session_controller.pending_turn_request));
       ignore (stop () : bool));
  [%expect
    {|
    user "Hello"
    dirty=true pending=false started_before_done=0
    user "Hello"
    assistant "later"
    dirty=false reason=idle_followup started_history=1 pending=false
    |}]
;;

let%expect_test "automatic follow-up turns stop at the configured host limit" =
  let history = [ user_message "Hello" ] in
  let model = model_of_history history in
  let runtime_policy =
    runtime_policy_with_budget
      { Chat_response.Runtime_semantics.default_budget_policy with
        max_followup_turns = 1
      }
  in
  let moderator = create_moderator ~runtime_policy () in
  let started_turns = ref [] in
  with_reducer
    ~model
    ~moderator
    ~start_streaming:(fun ~history ~op_id ->
      started_turns := (op_id, history) :: !started_turns)
    (fun ~runtime ~send_internal ~pump_until ~stop ->
       send_internal (`Start_turn App_runtime.Idle_followup);
       pump_until (fun () -> List.length !started_turns = 1);
       let first_op_id, _ = List.hd_exn !started_turns in
       send_internal (`Streaming_done (first_op_id, Chat_tui.Model.history_items model));
       pump_until (fun () -> Option.is_none runtime.App_runtime.op);
       send_internal (`Start_turn App_runtime.Idle_followup);
       pump_until (fun () ->
         Hash_set.mem runtime.App_runtime.shown_notice_keys "budget:max-followup-turns");
       print_endline
         (Printf.sprintf
            "started=%d history=%d notice=%b pending=%b"
            (List.length !started_turns)
            (List.length (Chat_tui.Model.history_items model))
            (Hash_set.mem
               runtime.App_runtime.shown_notice_keys
               "budget:max-followup-turns")
            (Option.is_some runtime.App_runtime.session_controller.pending_turn_request));
       ignore (stop () : bool));
  [%expect {| started=1 history=1 notice=true pending=false |}]
;;

let%expect_test
    "approval ask_text resumes without appending a fake user item and then restores \
     normal submit"
  =
  let model = model_of_history [] in
  let moderator =
    create_moderator
      ~surface:Builtin_surface.ui_moderator_surface
      ~source:approval_ask_text_script
      ()
  in
  let started_turns = ref [] in
  enqueue_queued_event moderator "continue?";
  with_reducer
    ~model
    ~moderator
    ~start_streaming:(fun ~history ~op_id ->
      started_turns := (op_id, history) :: !started_turns)
    (fun ~runtime ~send_internal ~pump_until ~stop ->
       send_internal `Moderator_wakeup;
       pump_until (fun () -> App_runtime.has_pending_input runtime);
       print_messages (Chat_tui.Model.messages model);
       print_endline
         (Printf.sprintf
            "pending=%s history=%d"
            (Option.value_map
               (App_runtime.pending_input runtime)
               ~default:"none"
               ~f:(fun input -> App_runtime.render_pending_input input))
            (List.length (Chat_tui.Model.history_items model)));
       send_internal
         (`Submit_requested
             { App_runtime.text = " approved "; draft_mode = Chat_tui.Model.Plain });
       pump_until (fun () -> not (App_runtime.has_pending_input runtime));
       print_messages (Chat_tui.Model.messages model);
       print_endline
         (Printf.sprintf
            "after_approval history=%d started=%d"
            (List.length (Chat_tui.Model.history_items model))
            (List.length !started_turns));
       send_internal
         (`Submit_requested
             { App_runtime.text = "hello"; draft_mode = Chat_tui.Model.Plain });
       pump_until (fun () ->
         List.length (Chat_tui.Model.history_items model) = 1
         && List.length !started_turns = 1);
       print_messages (Chat_tui.Model.messages model);
       print_endline
         (Printf.sprintf
            "after_submit history=%d started=%d reason=%s"
            (List.length (Chat_tui.Model.history_items model))
            (List.length !started_turns)
            (Option.value_map
               (App_runtime.active_turn_start_reason runtime)
               ~default:"<none>"
               ~f:string_of_turn_reason));
       ignore (stop () : bool));
  [%expect
    {|
    pending=Approval requested: continue? history=0
    assistant "approved"
    after_approval history=0 started=0
    assistant "approved"
    user "hello"
    after_submit history=1 started=1 reason=user_submit
    |}]
;;

let%expect_test
    "approval ask_choice keeps the prompt active until a valid choice is submitted"
  =
  let model = model_of_history [] in
  let moderator =
    create_moderator
      ~surface:Builtin_surface.ui_moderator_surface
      ~source:approval_ask_choice_script
      ()
  in
  let started_turns = ref [] in
  ok_or_fail
    (Manager.enqueue_internal_event
       moderator.Chat_response.In_memory_stream.manager
       (Lang.VVariant ("Queued", [])));
  with_reducer
    ~model
    ~moderator
    ~start_streaming:(fun ~history ~op_id ->
      started_turns := (op_id, history) :: !started_turns)
    (fun ~runtime ~send_internal ~pump_until ~stop ->
       send_internal `Moderator_wakeup;
       pump_until (fun () -> App_runtime.has_pending_input runtime);
       print_messages (Chat_tui.Model.messages model);
       send_internal
         (`Submit_requested
             { App_runtime.text = "maybe"; draft_mode = Chat_tui.Model.Plain });
       pump_until (fun () ->
         Option.exists (Chat_tui.Model.moderator_modal model) ~f:(fun modal ->
           Option.is_some modal.validation_error));
       print_messages (Chat_tui.Model.messages model);
       print_endline
         (Printf.sprintf
            "pending=%b history=%d started=%d"
            (App_runtime.has_pending_input runtime)
            (List.length (Chat_tui.Model.history_items model))
            (List.length !started_turns));
       send_internal
         (`Submit_requested
             { App_runtime.text = " yes "; draft_mode = Chat_tui.Model.Plain });
       pump_until (fun () -> not (App_runtime.has_pending_input runtime));
       print_messages (Chat_tui.Model.messages model);
       print_endline
         (Printf.sprintf
            "pending=%b history=%d started=%d"
            (App_runtime.has_pending_input runtime)
            (List.length (Chat_tui.Model.history_items model))
            (List.length !started_turns));
       ignore (stop () : bool));
  [%expect
    {|
    pending=true history=0 started=0
    assistant "yes"
    pending=false history=0 started=0
    |}]
;;

let%expect_test "automatic follow-up turns respect the sliding-window rate limit" =
  let history = [ user_message "Hello" ] in
  let model = model_of_history history in
  let runtime_policy =
    runtime_policy_with_budget
      { Chat_response.Runtime_semantics.default_budget_policy with
        max_followup_turns = 10
      ; turn_rate_limit = Some { max_turns = 1; window_ms = 60_000 }
      }
  in
  let moderator = create_moderator ~runtime_policy () in
  let started_turns = ref [] in
  with_reducer
    ~model
    ~moderator
    ~start_streaming:(fun ~history ~op_id ->
      started_turns := (op_id, history) :: !started_turns)
    (fun ~runtime ~send_internal ~pump_until ~stop ->
       send_internal (`Start_turn App_runtime.Moderator_request);
       pump_until (fun () -> List.length !started_turns = 1);
       let first_op_id, _ = List.hd_exn !started_turns in
       send_internal (`Streaming_done (first_op_id, Chat_tui.Model.history_items model));
       pump_until (fun () -> Option.is_none runtime.App_runtime.op);
       send_internal (`Start_turn App_runtime.Idle_followup);
       pump_until (fun () ->
         Hash_set.mem runtime.App_runtime.shown_notice_keys "budget:turn-rate-limit");
       print_endline
         (Printf.sprintf
            "started=%d rate_notice=%b"
            (List.length !started_turns)
            (Hash_set.mem runtime.App_runtime.shown_notice_keys "budget:turn-rate-limit"));
       ignore (stop () : bool));
  [%expect {| started=1 rate_notice=true |}]
;;

let%expect_test "Pause_followup_turns suppresses automatic follow-up scheduling" =
  let history = [ user_message "Hello" ] in
  let model = model_of_history history in
  let runtime_policy =
    runtime_policy_with_budget
      { Chat_response.Runtime_semantics.default_budget_policy with
        pause_conditions = [ Chat_response.Runtime_semantics.Pause_followup_turns ]
      }
  in
  let moderator = create_moderator ~runtime_policy () in
  let started_turns = ref [] in
  with_reducer
    ~model
    ~moderator
    ~start_streaming:(fun ~history ~op_id ->
      started_turns := (op_id, history) :: !started_turns)
    (fun ~runtime ~send_internal ~pump_until ~stop ->
       send_internal (`Start_turn App_runtime.Idle_followup);
       pump_until (fun () ->
         Hash_set.mem runtime.App_runtime.shown_notice_keys "budget:pause-followup-turns");
       print_endline
         (Printf.sprintf
            "started=%d notice=%b history=%d"
            (List.length !started_turns)
            (Hash_set.mem
               runtime.App_runtime.shown_notice_keys
               "budget:pause-followup-turns")
            (List.length (Chat_tui.Model.history_items model)));
       ignore (stop () : bool));
  [%expect {| started=0 notice=true history=1 |}]
;;

let%expect_test "Pause_internal_event_drains leaves queued events pending while idle" =
  let history = [ user_message "Hello" ] in
  let model = model_of_history history in
  let runtime_policy =
    runtime_policy_with_budget
      { Chat_response.Runtime_semantics.default_budget_policy with
        pause_conditions = [ Chat_response.Runtime_semantics.Pause_internal_event_drains ]
      }
  in
  let moderator = create_moderator ~runtime_policy () in
  enqueue_queued_event moderator "background";
  with_reducer ~model ~moderator (fun ~runtime ~send_internal ~pump_until ~stop ->
    send_internal `Moderator_wakeup;
    pump_until (fun () ->
      Hash_set.mem
        runtime.App_runtime.shown_notice_keys
        "budget:pause-internal-event-drains");
    let queued =
      ok_or_fail (Manager.snapshot moderator.manager)
      |> fun snapshot ->
      List.length snapshot.Session.Moderator_snapshot.queued_internal_events
    in
    print_endline
      (Printf.sprintf
         "dirty=%b queued=%d notice=%b history=%d"
         (App_runtime.is_moderator_dirty runtime)
         queued
         (Hash_set.mem
            runtime.App_runtime.shown_notice_keys
            "budget:pause-internal-event-drains")
         (List.length (Chat_tui.Model.history_items model)));
    ignore (stop () : bool));
  [%expect {| dirty=true queued=1 notice=true history=1 |}]
;;

let%expect_test "background model completion surfaces while idle without user action" =
  let history = [ user_message "Hello" ] in
  let model = model_of_history history in
  let moderator = create_moderator ~source:model_job_moderator_script () in
  with_reducer_context
    ~model
    ~moderator:(Some moderator)
    (fun ~runtime:_ ~services ~send_input:_ ~send_internal ~pump:_ ~pump_until ~stop ->
       let ctx =
         Chat_response.Ctx.create
           ~env:services.env
           ~dir:services.cwd
           ~tool_dir:services.cwd
           ~cache:services.cache
       in
       let exec_context : Chat_response.Model_executor.exec_context =
         { ctx
         ; run_agent =
             (fun ?history_compaction:_
               ?prompt_dir:_
               ?session_id:_
               ~ctx:_
               _prompt_xml
               items ->
               let input =
                 match items with
                 | [ CM.Basic basic ] -> Option.value basic.text ~default:""
                 | _ -> ""
               in
               "echo:" ^ input)
         ; fetch_prompt = (fun ~ctx:_ ~prompt ~is_local:_ -> Ok (prompt, None))
         }
       in
       let executor =
         Chat_response.Model_executor.create ~sw:services.ui_sw ~exec_context ()
       in
       Chat_response.Model_executor.register_session
         executor
         ~session_id:moderator.session_id
         ~manager:moderator.manager
         ~on_wakeup:(fun () -> send_internal `Moderator_wakeup);
       let recipe =
         Chat_response.Model_executor.recipe_agent_prompt_v1
           executor
           ~session_id:moderator.session_id
       in
       let payload =
         `Object
           [ "prompt", `String "<prompt/>"
           ; "input", `String "hi"
           ; "session_id", `String "nested-session"
           ]
       in
       let job_id = recipe.spawn ~payload |> Result.ok_or_failwith in
       Chat_response.Model_executor.await_job executor ~job_id |> Result.ok_or_failwith;
       pump_until (fun () -> List.length (Chat_tui.Model.messages model) = 2);
       print_messages (Chat_tui.Model.messages model);
       let queued =
         ok_or_fail (Manager.snapshot moderator.manager)
         |> fun snapshot ->
         List.length snapshot.Session.Moderator_snapshot.queued_internal_events
       in
       print_endline (Printf.sprintf "queued=%d" queued);
       ignore (stop () : bool));
  [%expect
    {|
    user "Hello"
    assistant "agent_prompt_v1:completed"
    queued=0
    |}]
;;

let%expect_test "submit while streaming queues a deferred safe-point note" =
  let history = [ user_message "Hello" ] in
  let model = model_of_history history in
  let moderator = create_moderator () in
  with_reducer ~model ~moderator (fun ~runtime ~send_internal ~pump_until ~stop ->
    runtime.App_runtime.op <- Some (App_runtime.Starting_streaming { id = 7 });
    send_internal
      (`Submit_requested
          { text = "Please use ripgrep"; draft_mode = Chat_tui.Model.Plain });
    pump_until (fun () -> App_runtime.has_deferred_user_notes runtime);
    print_messages (Chat_tui.Model.messages model);
    let safe_point_input = App_runtime.safe_point_input_source runtime in
    let entries = safe_point_input.consume_entries () in
    List.iter entries ~f:(fun note ->
      print_endline (App_runtime.render_deferred_user_note { entry = note }));
    print_endline
      (Printf.sprintf
         "remaining=%b history=%d"
         (App_runtime.has_deferred_user_notes runtime)
         (List.length (Chat_tui.Model.history_items model)));
    ignore (stop () : bool));
  [%expect
    {|
    user "Hello"
    Please use ripgrep
    remaining=false history=1
    |}]
;;

let%expect_test "deferred note survives when follow-up turns are paused" =
  let history = [ user_message "Hello" ] in
  let model = model_of_history history in
  let runtime_policy =
    runtime_policy_with_budget
      { Chat_response.Runtime_semantics.default_budget_policy with
        pause_conditions = [ Chat_response.Runtime_semantics.Pause_followup_turns ]
      }
  in
  let moderator = create_moderator ~runtime_policy () in
  let started_turns = ref [] in
  with_reducer
    ~model
    ~moderator
    ~start_streaming:(fun ~history ~op_id ->
      started_turns := (op_id, history) :: !started_turns)
    (fun ~runtime ~send_internal ~pump_until ~stop ->
       runtime.App_runtime.op <- Some (App_runtime.Starting_streaming { id = 7 });
       send_internal
         (`Submit_requested
             { text = "Use ripgrep after the current turn"
             ; draft_mode = Chat_tui.Model.Plain
             });
       pump_until (fun () -> App_runtime.has_deferred_user_notes runtime);
       send_internal (`Start_turn App_runtime.Idle_followup);
       pump_until (fun () ->
         Hash_set.mem runtime.App_runtime.shown_notice_keys "budget:pause-followup-turns");
       send_internal (`Streaming_done (7, Chat_tui.Model.history_items model));
       pump_until (fun () -> List.length !started_turns = 1);
       let _, started_history = List.hd_exn !started_turns in
       let deferred = List.last_exn started_history in
       print_endline (App_runtime.render_deferred_user_note { entry = deferred });
       print_endline
         (Printf.sprintf
            "remaining=%b notice=%b history=%d reason=%s"
            (App_runtime.has_deferred_user_notes runtime)
            (Hash_set.mem
               runtime.App_runtime.shown_notice_keys
               "budget:pause-followup-turns")
            (List.length started_history)
            (Option.value_map
               (App_runtime.active_turn_start_reason runtime)
               ~default:"<none>"
               ~f:string_of_turn_reason));
       ignore (stop () : bool));
  [%expect
    {|
    Use ripgrep after the current turn
    remaining=false notice=true history=2 reason=user_submit
    |}]
;;

let%expect_test "late deferred note is adopted and starts a follow-up without moderator" =
  let history = [ user_message "Hello" ] in
  let model = model_of_history history in
  let started_turns = ref [] in
  with_reducer_context
    ~model
    ~moderator:None
    ~start_streaming:(fun ~history ~op_id ->
      started_turns := (op_id, history) :: !started_turns)
    (fun ~runtime ~services:_ ~send_input:_ ~send_internal ~pump:_ ~pump_until ~stop ->
       runtime.App_runtime.op <- Some (App_runtime.Starting_streaming { id = 7 });
       send_internal
         (`Submit_requested
             { text = "Use ripgrep on the next turn"; draft_mode = Chat_tui.Model.Plain });
       pump_until (fun () -> App_runtime.has_deferred_user_notes runtime);
       send_internal (`Streaming_done (7, Chat_tui.Model.history_items model));
       pump_until (fun () -> List.length !started_turns = 1);
       let _, started_history = List.hd_exn !started_turns in
       let entry = List.last_exn started_history in
       print_endline (App_runtime.render_deferred_user_note { entry });
       print_endline
         (Printf.sprintf
            "remaining=%b started_history=%d canonical_history=%d id_preserved=%b"
            (App_runtime.has_deferred_user_notes runtime)
            (List.length started_history)
            (List.length (Chat_tui.Model.history_items model))
            (History_entry.Id.equal
               (History_entry.id entry)
               (History_entry.id (List.last_exn (Chat_tui.Model.history_items model)))));
       ignore (stop () : bool));
  [%expect
    {|
    Use ripgrep on the next turn
    remaining=false started_history=2 canonical_history=2 id_preserved=true
    |}]
;;

let%expect_test
    "late deferred entries preserve FIFO identity and reject unsupported drafts"
  =
  let history = [ user_message "Hello" ] in
  let model = model_of_history history in
  let started_turns = ref [] in
  with_reducer_context
    ~model
    ~moderator:None
    ~start_streaming:(fun ~history ~op_id ->
      started_turns := (op_id, history) :: !started_turns)
    (fun ~runtime ~services:_ ~send_input:_ ~send_internal ~pump:_ ~pump_until ~stop ->
       runtime.App_runtime.op <- Some (App_runtime.Starting_streaming { id = 7 });
       let submit text draft_mode =
         send_internal (`Submit_requested { text; draft_mode })
       in
       submit "first" Chat_tui.Model.Plain;
       submit "second" Chat_tui.Model.Plain;
       submit "<user>raw</user>" Chat_tui.Model.Raw_xml;
       submit "   " Chat_tui.Model.Plain;
       pump_until (fun () ->
         Queue.length runtime.App_runtime.session_controller.deferred_user_notes = 2);
       let raw_xml_rejected =
         List.exists (Chat_tui.Model.messages model) ~f:(fun (_, text) ->
           String.is_substring
             text
             ~substring:"Raw XML cannot be submitted while an assistant turn is active.")
       in
       let queued_ids =
         Queue.to_list runtime.App_runtime.session_controller.deferred_user_notes
         |> List.map ~f:(fun note -> History_entry.id note.entry)
       in
       send_internal (`Streaming_done (7, Chat_tui.Model.history_items model));
       pump_until (fun () -> List.length !started_turns = 1);
       let _, started_history = List.hd_exn !started_turns in
       let deferred = List.drop started_history 1 in
       let texts =
         List.map deferred ~f:(fun entry ->
           App_runtime.render_deferred_user_note { entry })
       in
       let started_ids = List.map deferred ~f:History_entry.id in
       print_s
         [%sexp
           (texts : string list)
         , (List.equal History_entry.Id.equal queued_ids started_ids : bool)
         , (App_runtime.has_deferred_user_notes runtime : bool)
         , (raw_xml_rejected : bool)];
       ignore (stop () : bool));
  [%expect {| ((first second) true false true) |}]
;;

let%expect_test "shell approval is local input and accepts once" =
  let model = model_of_history [] in
  let broker = Shell_broker.create () in
  with_reducer_context
    ~model
    ~moderator:None
    ~shell_approval_broker:broker
    (fun ~runtime ~services ~send_input:_ ~send_internal ~pump:_ ~pump_until ~stop ->
       let response, response_u = Promise.create () in
       Fiber.fork ~sw:services.Chat_tui.App_context.Services.ui_sw (fun () ->
         Shell_broker.request broker (shell_ui_request ()) |> Promise.resolve response_u);
       pump_until (fun () -> Option.is_some (Shell_broker.pending broker));
       send_internal `Shell_approval_changed;
       pump_until (fun () -> App_runtime.has_pending_input runtime);
       let rendered =
         App_runtime.pending_input runtime
         |> Option.value_exn
         |> App_runtime.render_pending_input
       in
       send_internal
         (`Submit_requested { text = "invalid"; draft_mode = Chat_tui.Model.Plain });
       pump_until (fun () ->
         List.exists (Chat_tui.Model.messages model) ~f:(fun (_, text) ->
           String.is_substring text ~substring:"Reply with once"));
       send_internal
         (`Submit_requested { text = "once"; draft_mode = Chat_tui.Model.Plain });
       pump_until (fun () -> Promise.is_resolved response);
       let approved =
         match Promise.await response with
         | Shell.Approval.Approve -> true
         | Approve_for _ | Deny _ | Rewrite _ -> false
       in
       printf
         "visible=%b pending=%b approved=%b history=%d\n"
         (String.is_substring rendered ~substring:"echo hello")
         (App_runtime.has_pending_input runtime)
         approved
         (List.length (Chat_tui.Model.history_items model));
       ignore (stop () : bool));
  [%expect {| visible=true pending=false approved=true history=0 |}]
;;

let%expect_test "shell approval accepts exact-session scope" =
  let model = model_of_history [] in
  let broker = Shell_broker.create () in
  with_reducer_context
    ~model
    ~moderator:None
    ~shell_approval_broker:broker
    (fun ~runtime ~services ~send_input:_ ~send_internal ~pump:_ ~pump_until ~stop ->
       let response, response_u = Promise.create () in
       Fiber.fork ~sw:services.Chat_tui.App_context.Services.ui_sw (fun () ->
         Shell_broker.request broker (shell_ui_request ()) |> Promise.resolve response_u);
       pump_until (fun () -> Option.is_some (Shell_broker.pending broker));
       send_internal `Shell_approval_changed;
       pump_until (fun () -> App_runtime.has_pending_input runtime);
       send_internal
         (`Submit_requested { text = "session"; draft_mode = Chat_tui.Model.Plain });
       pump_until (fun () -> Promise.is_resolved response);
       let exact_session =
         match Promise.await response with
         | Shell.Approval.Approve_for (Exact_session { expires_at = None }) -> true
         | Approve | Approve_for _ | Deny _ | Rewrite _ -> false
       in
       printf
         "pending=%b exact-session=%b history=%d\n"
         (App_runtime.has_pending_input runtime)
         exact_session
         (List.length (Chat_tui.Model.history_items model));
       ignore (stop () : bool));
  [%expect {| pending=false exact-session=true history=0 |}]
;;

let%expect_test "shell approval preserves an explicit denial reason" =
  let model = model_of_history [] in
  let broker = Shell_broker.create () in
  with_reducer_context
    ~model
    ~moderator:None
    ~shell_approval_broker:broker
    (fun ~runtime ~services ~send_input:_ ~send_internal ~pump:_ ~pump_until ~stop ->
       let response, response_u = Promise.create () in
       Fiber.fork ~sw:services.Chat_tui.App_context.Services.ui_sw (fun () ->
         Shell_broker.request broker (shell_ui_request ()) |> Promise.resolve response_u);
       pump_until (fun () -> Option.is_some (Shell_broker.pending broker));
       send_internal `Shell_approval_changed;
       pump_until (fun () -> App_runtime.has_pending_input runtime);
       send_internal
         (`Submit_requested
             { text = "deny private material"; draft_mode = Chat_tui.Model.Plain });
       pump_until (fun () -> Promise.is_resolved response);
       let reason =
         match Promise.await response with
         | Shell.Approval.Deny reason -> reason
         | Approve | Approve_for _ | Rewrite _ -> "unexpected"
       in
       printf
         "pending=%b reason=%S history=%d\n"
         (App_runtime.has_pending_input runtime)
         reason
         (List.length (Chat_tui.Model.history_items model));
       ignore (stop () : bool));
  [%expect {| pending=false reason="private material" history=0 |}]
;;

let%expect_test "escape cancels a shell approval without quitting the session" =
  let model = model_of_history [] in
  let broker = Shell_broker.create () in
  with_reducer_context
    ~model
    ~moderator:None
    ~shell_approval_broker:broker
    (fun ~runtime ~services ~send_input ~send_internal ~pump:_ ~pump_until ~stop ->
       let response, response_u = Promise.create () in
       Fiber.fork ~sw:services.Chat_tui.App_context.Services.ui_sw (fun () ->
         Shell_broker.request broker (shell_ui_request ()) |> Promise.resolve response_u);
       pump_until (fun () -> Option.is_some (Shell_broker.pending broker));
       send_internal `Shell_approval_changed;
       pump_until (fun () -> App_runtime.has_pending_input runtime);
       Chat_tui.Model.set_mode model Chat_tui.Model.Normal;
       send_input (`Key (`Escape, []));
       pump_until (fun () -> Promise.is_resolved response);
       let cancelled =
         match Promise.await response with
         | Shell.Approval.Deny reason -> String.is_substring reason ~substring:"cancelled"
         | Approve | Approve_for _ | Rewrite _ -> false
       in
       printf
         "pending=%b cancelled=%b quit=%b\n"
         (App_runtime.has_pending_input runtime)
         cancelled
         !(runtime.App_runtime.quit_via_esc);
       ignore (stop () : bool));
  [%expect {| pending=false cancelled=true quit=false |}]
;;

let%expect_test "moderator input follows a higher-priority shell approval" =
  let model = model_of_history [] in
  let broker = Shell_broker.create () in
  let moderator =
    create_moderator
      ~surface:Builtin_surface.ui_moderator_surface
      ~source:approval_ask_text_script
      ()
  in
  enqueue_queued_event moderator "continue moderation?";
  with_reducer_context
    ~model
    ~moderator:(Some moderator)
    ~shell_approval_broker:broker
    (fun ~runtime ~services ~send_input:_ ~send_internal ~pump:_ ~pump_until ~stop ->
       send_internal `Moderator_wakeup;
       pump_until (fun () -> App_runtime.has_pending_input runtime);
       let response, response_u = Promise.create () in
       Fiber.fork ~sw:services.Chat_tui.App_context.Services.ui_sw (fun () ->
         Shell_broker.request broker (shell_ui_request ()) |> Promise.resolve response_u);
       pump_until (fun () -> Option.is_some (Shell_broker.pending broker));
       send_internal `Shell_approval_changed;
       pump_until (fun () ->
         match App_runtime.pending_input runtime with
         | Some (App_runtime.Shell _) -> true
         | Some (Moderator _) | None -> false);
       let shell_visible =
         App_runtime.pending_input runtime
         |> Option.value_exn
         |> App_runtime.render_pending_input
         |> String.is_substring ~substring:"echo hello"
       in
       send_internal
         (`Submit_requested { text = "once"; draft_mode = Chat_tui.Model.Plain });
       pump_until (fun () -> Promise.is_resolved response);
       pump_until (fun () ->
         match App_runtime.pending_input runtime with
         | Some (App_runtime.Moderator _) -> true
         | Some (Shell _) | None -> false);
       let moderator_visible =
         App_runtime.pending_input runtime
         |> Option.value_exn
         |> App_runtime.render_pending_input
         |> String.is_substring ~substring:"continue moderation?"
       in
       ignore (App_runtime.refresh_messages runtime : Chat_tui.Model.projection_damage);
       ignore (App_runtime.refresh_messages runtime : Chat_tui.Model.projection_damage);
       printf
         "shell=%b moderator=%b history=%d\n"
         shell_visible
         moderator_visible
         (List.length (Chat_tui.Model.history_items model));
       ignore (stop () : bool));
  [%expect {| shell=true moderator=true history=0 |}]
;;

let startup_outcome moderator history =
  Manager.handle_event_entries
    moderator.Chat_response.In_memory_stream.manager
    ~session_id:moderator.session_id
    ~now_ms:1
    ~history
    ~available_tools:[]
    ~session_meta:`Null
    ~event:Moderation.Event.Session_start
  |> ok_or_fail
;;

let%expect_test "user submission waits for startup overlay before beginning a turn" =
  let model = model_of_history [] in
  let moderator = create_moderator ~source:startup_overlay_script () in
  let started = ref None in
  with_reducer_context
    ~model
    ~moderator:(Some moderator)
    ~moderator_startup_state:App_runtime.Starting
    ~start_streaming:(fun ~history ~op_id -> started := Some (op_id, history))
    (fun ~runtime ~services:_ ~send_input:_ ~send_internal ~pump:_ ~pump_until ~stop ->
       send_internal
         (`Submit_requested { text = "hello"; draft_mode = Chat_tui.Model.Plain });
       pump_until (fun () -> Queue.length runtime.App_runtime.pending = 1);
       printf
         "before history=%d started=%b queued=%d\n"
         (List.length (Chat_tui.Model.history_items model))
         (Option.is_some !started)
         (Queue.length runtime.App_runtime.pending);
       let outcome = startup_outcome moderator (Chat_tui.Model.history_items model) in
       send_internal (`Moderator_startup_completed (Ok [ outcome ]));
       pump_until (fun () -> Option.is_some !started);
       let _, started_history = Option.value_exn !started in
       let effective =
         Manager.effective_history_entries moderator.manager started_history
         |> History_entry.items
         |> Chat_tui.Conversation.of_history
       in
       print_messages effective;
       printf
         "after history=%d effective=%d state=%s\n"
         (List.length (Chat_tui.Model.history_items model))
         (List.length effective)
         (match App_runtime.moderator_startup_state runtime with
          | Starting -> "starting"
          | Ready -> "ready"
          | Failed _ -> "failed");
       ignore (stop () : bool));
  [%expect
    {|
    before history=0 started=false queued=1
    system "startup policy"
    user "hello"
    after history=1 effective=2 state=ready
    |}]
;;

let%expect_test "startup end-session discards queued user submission" =
  let model = model_of_history [] in
  let moderator = create_moderator ~source:startup_end_session_script () in
  let started = ref false in
  with_reducer_context
    ~model
    ~moderator:(Some moderator)
    ~moderator_startup_state:App_runtime.Starting
    ~start_streaming:(fun ~history:_ ~op_id:_ -> started := true)
    (fun ~runtime ~services:_ ~send_input:_ ~send_internal ~pump:_ ~pump_until ~stop ->
       send_internal
         (`Submit_requested { text = "must not run"; draft_mode = Chat_tui.Model.Plain });
       pump_until (fun () -> Queue.length runtime.App_runtime.pending = 1);
       let outcome = startup_outcome moderator (Chat_tui.Model.history_items model) in
       send_internal (`Moderator_startup_completed (Ok [ outcome ]));
       pump_until (fun () -> Option.is_some runtime.App_runtime.halted_reason);
       printf
         "history=%d started=%b queued=%d halted=%b\n"
         (List.length (Chat_tui.Model.history_items model))
         !started
         (Queue.length runtime.App_runtime.pending)
         (Option.is_some runtime.App_runtime.halted_reason);
       ignore (stop () : bool));
  [%expect {| history=0 started=false queued=0 halted=true |}]
;;

let%expect_test "startup failure fails closed and discards queued actions" =
  let model = model_of_history [] in
  let started = ref false in
  with_reducer_context
    ~model
    ~moderator:None
    ~moderator_startup_state:App_runtime.Starting
    ~start_streaming:(fun ~history:_ ~op_id:_ -> started := true)
    (fun ~runtime ~services:_ ~send_input:_ ~send_internal ~pump:_ ~pump_until ~stop ->
       send_internal
         (`Submit_requested { text = "must not run"; draft_mode = Chat_tui.Model.Plain });
       pump_until (fun () -> Queue.length runtime.App_runtime.pending = 1);
       send_internal (`Moderator_startup_completed (Error "broken startup"));
       pump_until (fun () ->
         match App_runtime.moderator_startup_state runtime with
         | Failed _ -> true
         | Starting | Ready -> false);
       printf
         "history=%d started=%b queued=%d halted=%b\n"
         (List.length (Chat_tui.Model.history_items model))
         !started
         (Queue.length runtime.App_runtime.pending)
         (Option.is_some runtime.App_runtime.halted_reason);
       ignore (stop () : bool));
  [%expect {| history=0 started=false queued=0 halted=true |}]
;;
