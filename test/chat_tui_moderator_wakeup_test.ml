open Core
open Eio.Std

module App_runtime = Chat_tui.App_runtime
module CM = Prompt.Chat_markdown
module Lang = Chatml.Chatml_lang
module Manager = Chat_response.Moderator_manager
module Moderation = Chat_response.Moderation
module Res = Openai.Responses
module Stream = Chat_response.In_memory_stream

let ok_or_fail = function
  | Ok value -> value
  | Error msg -> failwith msg
;;

let input_text text = Res.Input_message.Text { text; _type = "input_text" }

let user_message text =
  Res.Item.Input_message
    { role = Res.Input_message.User
    ; content = [ input_text text ]
    ; _type = "message"
    }
;;

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
  let script =
    CM.
      { id = "main"
      ; language = "chatml"
      ; kind = "moderator"
      ; source = Inline source
      }
  in
  ok_or_fail (Manager.Registry.compile_script Manager.Registry.empty script) |> snd
;;

let create_moderator ?source () =
  let manager =
    ok_or_fail
      (Manager.create
         ~artifact:(artifact ?source ())
         ~capabilities:Moderation.Capabilities.default
         ())
  in
  Chat_response.In_memory_stream.
    { manager
    ; session_id = "session-1"
    ; session_meta = `Null
    ; runtime_policy = Chat_response.Runtime_semantics.default_policy
    }
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
  let streams : Chat_tui.App_context.Streams.t = { input = input_stream; internal = internal_stream } in
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
    | None -> Chat_tui.App_runtime.create ~model ()
    | Some moderator -> Chat_tui.App_runtime.create ~model ~moderator ()
  in
  let streaming : Chat_tui.App_streaming.Context.t =
    { shared
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
  let finish_active_op () =
    match runtime.Chat_tui.App_runtime.op with
    | Some
        (App_runtime.Streaming { id; sw = _ }
        | App_runtime.Starting_streaming { id }) ->
      Eio.Stream.add internal_stream (`Streaming_done (id, Chat_tui.Model.history_items model));
      pump_until (fun () -> Option.is_none runtime.Chat_tui.App_runtime.op)
    | Some
        (App_runtime.Compacting { id; sw = _ }
        | App_runtime.Starting_compaction { id }) ->
      Eio.Stream.add internal_stream (`Compaction_done (id, Chat_tui.Model.history_items model));
      pump_until (fun () -> Option.is_none runtime.Chat_tui.App_runtime.op)
    | None -> ()
  in
  let stop () =
    finish_active_op ();
    Chat_tui.Model.set_mode model Chat_tui.Model.Normal;
    Eio.Stream.add input_stream (`Key (`Escape, []));
    Promise.await finished
  in
  f ~runtime ~services ~send_internal ~pump ~pump_until ~stop
;;

let with_reducer ?(start_streaming = fun ~history:_ ~op_id:_ -> ()) ~model ~moderator f =
  with_reducer_context
    ~start_streaming
    ~model
    ~moderator:(Some moderator)
    (fun ~runtime ~services:_ ~send_internal ~pump:_ ~pump_until ~stop ->
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
    ~start_streaming:(fun ~history ~op_id -> started_turns := (op_id, history) :: !started_turns)
    (fun ~runtime ~send_internal ~pump_until ~stop ->
    send_internal `Moderator_wakeup;
    pump_until (fun () ->
      (match runtime.App_runtime.op with
       | Some (App_runtime.Starting_streaming _) -> true
       | Some
           (App_runtime.Streaming _
           | App_runtime.Compacting _
           | App_runtime.Starting_compaction _)
         | None -> false)
      && Option.is_none runtime.App_runtime.session_controller.pending_turn_request
      && not (App_runtime.is_moderator_dirty runtime)
      && List.length (Chat_tui.Model.messages model) = 3);
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
    assistant "(thinking\226\128\166)"
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
    ~start_streaming:(fun ~history ~op_id -> started_turns := (op_id, history) :: !started_turns)
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
    send_internal (`Streaming_done (7, history));
    pump_until (fun () ->
      (match runtime.App_runtime.op with
       | Some (App_runtime.Starting_streaming _) -> true
       | Some
           (App_runtime.Streaming _
           | App_runtime.Compacting _
           | App_runtime.Starting_compaction _)
         | None -> false)
      && Option.is_none runtime.App_runtime.session_controller.pending_turn_request
      && not (App_runtime.is_moderator_dirty runtime)
      && List.length (Chat_tui.Model.messages model) = 3);
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
    assistant "(thinking\226\128\166)"
    dirty=false reason=idle_followup started_history=1 pending=false
    |}]
;;

let%expect_test "background model completion surfaces while idle without user action" =
  let history = [ user_message "Hello" ] in
  let model = model_of_history history in
  let moderator = create_moderator ~source:model_job_moderator_script () in
  with_reducer_context
    ~model
    ~moderator:(Some moderator)
    (fun ~runtime:_ ~services ~send_internal ~pump:_ ~pump_until ~stop ->
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
             (fun ?history_compaction:_ ?prompt_dir:_ ?session_id:_ ~ctx:_ _prompt_xml items ->
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
         ok_or_fail (Manager.snapshot moderator.manager) |> fun snapshot ->
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
    send_internal (`Submit_requested { text = "Please use ripgrep"; draft_mode = Chat_tui.Model.Plain });
    pump_until (fun () -> App_runtime.has_deferred_user_notes runtime);
    print_messages (Chat_tui.Model.messages model);
    let safe_point_input = App_runtime.safe_point_input_source runtime in
    let rendered = safe_point_input.consume () |> Option.value ~default:"<none>" in
    print_endline rendered;
    print_endline
      (Printf.sprintf
         "remaining=%b history=%d"
         (App_runtime.has_deferred_user_notes runtime)
         (List.length (Chat_tui.Model.history_items model)));
    ignore (stop () : bool));
  [%expect
    {|
    user "Hello"

    <system-reminder>
    This is a Note From the User:
    Please use ripgrep
    </system-reminder>

    remaining=false history=1
    |}]
;;

let%expect_test "deferred note survives reducer turn boundary without moderator" =
  let history = [ user_message "Hello" ] in
  let model = model_of_history history in
  with_reducer_context
    ~model
    ~moderator:None
    (fun ~runtime ~services:_ ~send_internal ~pump:_ ~pump_until ~stop ->
       runtime.App_runtime.op <- Some (App_runtime.Starting_streaming { id = 7 });
       send_internal
         (`Submit_requested
             { text = "Use ripgrep on the next turn"
             ; draft_mode = Chat_tui.Model.Plain
             });
       pump_until (fun () -> App_runtime.has_deferred_user_notes runtime);
       send_internal (`Streaming_done (7, history));
       pump_until (fun () -> Option.is_none runtime.App_runtime.op);
       let prepared =
         ok_or_fail
           (Stream.prepare_turn_inputs
              ~moderator:None
              ~safe_point_input:(App_runtime.safe_point_input_source runtime)
              ~available_tools:[]
              ~now_ms:1
              ~history:(Chat_tui.Model.history_items model)
              ())
       in
       print_endline (input_message_text_exn (List.last_exn prepared));
       print_endline
         (Printf.sprintf
            "remaining=%b prepared=%d canonical_history=%d"
            (App_runtime.has_deferred_user_notes runtime)
            (List.length prepared)
            (List.length (Chat_tui.Model.history_items model)));
       ignore (stop () : bool));
  [%expect
    {|

    <system-reminder>
    This is a Note From the User:
    Use ripgrep on the next turn
    </system-reminder>

    remaining=false prepared=2 canonical_history=1
    |}]
;;
