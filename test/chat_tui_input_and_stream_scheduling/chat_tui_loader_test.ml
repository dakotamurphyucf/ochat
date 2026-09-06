open Core

let make_model () =
  Chat_tui.Model.create
    ~history_items:[]
    ~messages:[]
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

let%expect_test "loader labels assistant thinking, tool work, and compaction" =
  let model = make_model () in
  Chat_tui.Model.set_activity model (Some (Assistant Thinking));
  printf "%s\n" (Chat_tui.Renderer_component_loader.status_text model |> Option.value_exn);
  Chat_tui.Model.set_activity model (Some (Assistant Writing));
  printf "%s\n" (Chat_tui.Renderer_component_loader.status_text model |> Option.value_exn);
  Chat_tui.Model.set_activity model (Some (Assistant Working));
  printf "%s\n" (Chat_tui.Renderer_component_loader.status_text model |> Option.value_exn);
  ignore
    (Chat_tui.Model.agent_call_started
       model
       ~call_id:"call"
       ~name:"research"
       ~kind:`Function
       ~payload:"{}"
       ~agent_page_kind:Chat_response.Tool_execution_event.Subagent
     : bool);
  printf "%s\n" (Chat_tui.Renderer_component_loader.status_text model |> Option.value_exn);
  Chat_tui.Model.set_activity model (Some Compacting);
  printf "%s\n" (Chat_tui.Renderer_component_loader.status_text model |> Option.value_exn);
  [%expect
    {|
    Thinking
    Writing
    Working
    Working
    Compacting
    |}]
;;

let%expect_test "shimmer highlight travels across the activity text" =
  let render frame =
    Chat_tui.Renderer_component_loader.render ~base_attr:Notty.A.empty ~frame "Work"
  in
  let render_ansi image =
    let buffer = Buffer.create 32 in
    Notty.Render.to_buffer buffer Notty.Cap.ansi (0, 0) (4, 1) image;
    Buffer.contents buffer
  in
  printf
    "same-width=%b animated=%b styled=%b\n"
    (Int.equal (Notty.I.width (render 0)) (Notty.I.width (render 9)))
    (not (String.equal (render_ansi (render 0)) (render_ansi (render 9))))
    (render_ansi (render 0) |> String.to_list |> List.exists ~f:(Char.equal '\027'));
  [%expect {| same-width=true animated=true styled=true |}]
;;

let%expect_test "shimmer snake grows, fills the text, and falls off toward the end" =
  let render frame =
    Chat_tui.Renderer_component_loader.render ~base_attr:Notty.A.empty ~frame "Thinking"
  in
  let render_ansi image =
    let buffer = Buffer.create 64 in
    Notty.Render.to_buffer buffer Notty.Cap.ansi (0, 0) (8, 1) image;
    Buffer.contents buffer
  in
  let same left right =
    String.equal (render_ansi (render left)) (render_ansi (render right))
  in
  printf
    "edge-lingers=%b middle-moves=%b midpoint-changes=%b tail-moves=%b\n"
    (same 0 3)
    (not (same 9 12))
    (not (same 0 24))
    (not (same 27 36));
  [%expect
    {| edge-lingers=true middle-moves=true midpoint-changes=true tail-moves=true |}]
;;

let%expect_test "repeated streaming activity does not restart the shimmer" =
  let model = make_model () in
  Chat_tui.Model.set_activity model (Some (Assistant Writing));
  for _ = 1 to 12 do
    Chat_tui.Model.advance_animation_frame model
  done;
  Chat_tui.Model.set_activity model (Some (Assistant Writing));
  printf "same-state-frame=%d\n" (Chat_tui.Model.animation_frame model);
  Chat_tui.Model.set_activity model (Some (Assistant Working));
  printf "changed-state-frame=%d\n" (Chat_tui.Model.animation_frame model);
  [%expect
    {|
    same-state-frame=12
    changed-state-frame=0
    |}]
;;

let%expect_test "repeated working activity does not restart the shimmer" =
  let model = make_model () in
  Chat_tui.Model.set_activity model (Some (Assistant Working));
  for _ = 1 to 12 do
    Chat_tui.Model.advance_animation_frame model;
    Chat_tui.Model.set_activity model (Some (Assistant Working))
  done;
  printf "working-frame=%d\n" (Chat_tui.Model.animation_frame model);
  [%expect {| working-frame=12 |}]
;;

let%expect_test "loader advances and stops with the assistant turn" =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun ui_sw ->
  let model = make_model () in
  let input = Eio.Stream.create 128 in
  let internal = Eio.Stream.create 16 in
  let redraw = Eio.Stream.create 1 in
  let enqueued = ref 0 in
  let drawn = ref 0 in
  let throttler =
    Chat_tui.Redraw_throttle.create ~fps:60. ~enqueue_redraw:(fun () -> incr enqueued)
  in
  let ui : Chat_tui.App_context.Ui.t =
    { term = (Obj.magic 0 : Notty_eio.Term.t)
    ; size = (fun () -> 80, 24)
    ; throttler
    ; redraw = (fun () -> incr drawn)
    ; redraw_immediate = (fun () -> incr drawn)
    ; latest_frame_generation = (fun () -> 0)
    ; resize_and_redraw = (fun ~size:_ ~layout:_ -> incr drawn)
    ; render_current_with_layout = (fun ~size:_ ~layout:_ -> incr drawn)
    }
  in
  let cwd = Eio.Stdenv.cwd env in
  let services : Chat_tui.App_context.Services.t =
    { env
    ; ui_sw
    ; cwd
    ; cache = Chat_response.Cache.create ~max_size:1 ()
    ; datadir = cwd
    ; session = None
    }
  in
  let streams : Chat_tui.App_context.Streams.t = { input; internal; redraw } in
  let shared : Chat_tui.App_context.Resources.t = { services; streams; ui } in
  let runtime = Chat_tui.App_runtime.create ~model () in
  let streaming : Chat_tui.App_streaming.Context.t =
    { shared
    ; allocator = runtime.history_allocator
    ; cfg = Chat_response.Config.default
    ; tools = []
    ; tool_tbl = Hashtbl.create (module String)
    ; moderator = None
    ; safe_point_input = Some (Chat_tui.App_runtime.safe_point_input_source runtime)
    ; parallel_tool_calls = true
    ; history_compaction = false
    }
  in
  let submit : Chat_tui.App_submit.Context.t =
    { runtime; streaming; start_streaming = (fun ~history:_ ~op_id:_ -> ()) }
  in
  let context : Chat_tui.App_reducer.Context.t =
    { runtime
    ; shared
    ; submit
    ; compaction = { shared; runtime }
    ; cancelled = Chat_tui.App_streaming.Cancelled
    }
  in
  let finished, resolve_finished = Eio.Promise.create () in
  Eio.Fiber.fork ~sw:ui_sw (fun () ->
    Eio.Promise.resolve resolve_finished (Chat_tui.App_reducer.run context));
  let pump_until predicate =
    let rec loop remaining =
      if predicate ()
      then ()
      else if remaining = 0
      then failwith "timeout waiting for reducer"
      else (
        Eio.Fiber.yield ();
        loop (remaining - 1))
    in
    loop 2_000
  in
  Chat_tui.Model.set_chat_materialization_loading model;
  Eio.Stream.add input (`Key (`ASCII 'x', []));
  pump_until (fun () -> Eio.Stream.is_empty input);
  Eio.Stream.add redraw ();
  pump_until (fun () -> !drawn = 1);
  printf
    "loading input=%S frame=%d\n"
    (Chat_tui.Model.input_line model)
    (Chat_tui.Model.animation_frame model);
  for _ = 1 to 65 do
    Eio.Stream.add input (`Key (`ASCII 'y', []))
  done;
  Chat_tui.Model.set_chat_materialization_warm model;
  while not (Eio.Stream.is_empty input) do
    ignore (Eio.Stream.take_nonblocking input : Chat_tui.App_events.input_event option)
  done;
  Chat_tui.Model.set_normal_input_enabled model true;
  pump_until (fun () ->
    Eio.Stream.is_empty input && Chat_tui.Model.normal_input_is_enabled model);
  printf "warm input=%S\n" (Chat_tui.Model.input_line model);
  let op_id = Chat_tui.App_runtime.alloc_op_id runtime in
  runtime.op <- Some (Starting_streaming { id = op_id });
  Chat_tui.Model.set_activity model (Some (Assistant Thinking));
  Eio.Stream.add internal `Redraw;
  pump_until (fun () -> !drawn = 2);
  Chat_tui.Redraw_throttle.tick throttler;
  printf
    "active frame=%d enqueued=%d drawn=%d\n"
    (Chat_tui.Model.animation_frame model)
    !enqueued
    !drawn;
  Eio.Stream.add internal (`Streaming_done (op_id, []));
  pump_until (fun () -> Option.is_none runtime.op);
  let frame = Chat_tui.Model.animation_frame model in
  let drawn_before = !drawn in
  Eio.Stream.add internal `Redraw;
  pump_until (fun () -> !drawn > drawn_before);
  Chat_tui.Redraw_throttle.tick throttler;
  printf
    "done active=%b frame-stable=%b enqueued=%d\n"
    (Option.is_some (Chat_tui.Model.activity model))
    (Int.equal frame (Chat_tui.Model.animation_frame model))
    !enqueued;
  Chat_tui.Model.set_mode model Normal;
  Eio.Stream.add input (`Key (`Escape, []));
  ignore (Eio.Promise.await finished : bool);
  [%expect
    {|
    loading input="" frame=1
    warm input=""
    active frame=1 enqueued=1 drawn=2
    done active=false frame-stable=true enqueued=1
    |}]
;;

let%expect_test "only final assistant writing defers eager history warming" =
  let model = make_model () in
  let runtime = Chat_tui.App_runtime.create ~model () in
  let should_warm () =
    Chat_tui.App.For_testing.should_warm_history_before_redraw ~runtime ~model
  in
  printf "idle=%b\n" (should_warm ());
  Chat_tui.Model.set_activity model (Some (Assistant Thinking));
  printf "thinking=%b\n" (should_warm ());
  Chat_tui.Model.set_activity model (Some (Assistant Working));
  printf "working=%b\n" (should_warm ());
  Chat_tui.Model.set_activity model (Some (Assistant Writing));
  printf "writing=%b\n" (should_warm ());
  Chat_tui.Model.set_activity model None;
  printf "finished=%b\n" (should_warm ());
  [%expect
    {|
    idle=true
    thinking=true
    working=true
    writing=false
    finished=true
    |}]
;;
