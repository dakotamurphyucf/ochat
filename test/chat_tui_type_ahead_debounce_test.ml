open Core
open Eio.Std

let make_model ?(input_line = "") ?(cursor_pos = 0) ?(mode = Chat_tui.Model.Insert) ()
  : Chat_tui.Model.t
  =
  let open Chat_tui in
  let scroll_box = Notty_scroll_box.create Notty.I.empty in
  Model.create
    ~history_items:[]
    ~messages:[]
    ~input_line
    ~auto_follow:true
    ~msg_buffers:(Hashtbl.create (module String))
    ~function_name_by_id:(Hashtbl.create (module String))
    ~reasoning_idx_by_id:(Hashtbl.create (module String))
    ~tool_output_by_index:(Hashtbl.create (module Int))
    ~tasks:[]
    ~kv_store:(Hashtbl.create (module String))
    ~fetch_sw:None
    ~scroll_box
    ~cursor_pos
    ~selection_anchor:None
    ~mode
    ~draft_mode:Model.Plain
    ~selected_msg:None
    ~undo_stack:[]
    ~redo_stack:[]
    ~cmdline:""
    ~cmdline_cursor:0
;;

let set_relevant_completion (model : Chat_tui.Model.t) ~(text : string) : unit =
  let open Chat_tui in
  let completion : Model.typeahead_completion =
    { text
    ; base_input = Model.input_line model
    ; base_cursor = Model.cursor_pos model
    ; generation = 0
    }
  in
  Model.set_typeahead_completion model (Some completion)
;;

let with_reducer ~model f =
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
  let system_stream : string Eio.Stream.t = Eio.Stream.create 16 in
  let streams : Chat_tui.App_context.Streams.t =
    { input = input_stream; internal = internal_stream; system = system_stream }
  in
  let redraw () = () in
  let throttler =
    Chat_tui.Redraw_throttle.create ~fps:60. ~enqueue_redraw:(fun () -> ())
  in
  let dummy_term : Notty_eio.Term.t = Obj.magic 0 in
  let ui : Chat_tui.App_context.Ui.t =
    { term = dummy_term; throttler; redraw; redraw_immediate = redraw }
  in
  let cwd = Eio.Stdenv.cwd env in
  let cache = Chat_response.Cache.create ~max_size:1 () in
  let services : Chat_tui.App_context.Services.t =
    { env; ui_sw; cwd; cache; datadir = cwd; session = None }
  in
  let shared : Chat_tui.App_context.Resources.t = { services; streams; ui } in
  let runtime = Chat_tui.App_runtime.create ~model in
  let streaming : Chat_tui.App_streaming.Context.t =
    { shared
    ; cfg = Chat_response.Config.default
    ; tools = []
    ; tool_tbl = Hashtbl.create (module String)
    ; parallel_tool_calls = true
    ; history_compaction = false
    }
  in
  let submit : Chat_tui.App_submit.Context.t = { runtime; streaming } in
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
  let send_input (ev : Chat_tui.App_events.input_event) =
    Eio.Stream.add input_stream ev
  in
  let send_internal (ev : Chat_tui.App_events.internal_event) =
    Eio.Stream.add internal_stream ev
  in
  let stop () =
    Chat_tui.Model.set_mode model Chat_tui.Model.Normal;
    Eio.Stream.add input_stream (`Key (`Escape, []));
    Promise.await finished
  in
  f ~runtime ~send_input ~send_internal ~pump_until ~stop
;;

let%expect_test "Typeahead_done applies only when op id and snapshot match" =
  let model = make_model ~input_line:"hi" ~cursor_pos:2 ~mode:Chat_tui.Model.Insert () in
  with_reducer ~model (fun ~runtime ~send_input:_ ~send_internal ~pump_until ~stop ->
    runtime.Chat_tui.App_runtime.typeahead_op
    <- Some (Chat_tui.App_runtime.Starting_typeahead { id = 1 });
    send_internal
      (`Typeahead_done
          (1, { generation = 0; base_input = "hi"; base_cursor = 2; text = " there" }));
    pump_until (fun () -> Option.is_none runtime.Chat_tui.App_runtime.typeahead_op);
    let completion = Chat_tui.Model.typeahead_completion model in
    printf
      "completion_present=%b relevant=%b op_cleared=%b\n"
      (Option.is_some completion)
      (Chat_tui.Model.typeahead_is_relevant model)
      (Option.is_none runtime.Chat_tui.App_runtime.typeahead_op);
    ignore (stop () : bool));
  [%expect {| completion_present=true relevant=true op_cleared=true |}]
;;

let%expect_test "Typeahead_done is ignored for stale op ids" =
  let model = make_model ~input_line:"hi" ~cursor_pos:2 ~mode:Chat_tui.Model.Insert () in
  with_reducer ~model (fun ~runtime ~send_input:_ ~send_internal ~pump_until ~stop ->
    runtime.Chat_tui.App_runtime.typeahead_op
    <- Some (Chat_tui.App_runtime.Starting_typeahead { id = 1 });
    send_internal
      (`Typeahead_done
          ( 2
          , { generation = 0
            ; base_input = "hi"
            ; base_cursor = 2
            ; text = " STALE – should not apply"
            } ));
    send_internal
      (`Typeahead_done
          (1, { generation = 0; base_input = "hi"; base_cursor = 2; text = " OK" }));
    pump_until (fun () -> Option.is_none runtime.Chat_tui.App_runtime.typeahead_op);
    let completion_text =
      Option.value_map
        (Chat_tui.Model.typeahead_completion model)
        ~default:"<none>"
        ~f:(fun c -> c.Chat_tui.Model.text)
    in
    printf
      "completion=%S op_cleared=%b\n"
      completion_text
      (Option.is_none runtime.Chat_tui.App_runtime.typeahead_op);
    ignore (stop () : bool));
  [%expect {| completion=" OK" op_cleared=true |}]
;;

let%expect_test
    "Typeahead_done is ignored for stale generation/base snapshot and clears op"
  =
  let model = make_model ~input_line:"hi" ~cursor_pos:2 ~mode:Chat_tui.Model.Insert () in
  with_reducer ~model (fun ~runtime ~send_input:_ ~send_internal ~pump_until ~stop ->
    runtime.Chat_tui.App_runtime.typeahead_op
    <- Some (Chat_tui.App_runtime.Starting_typeahead { id = 1 });
    send_internal
      (`Typeahead_done
          (1, { generation = 999; base_input = "hi"; base_cursor = 2; text = " there" }));
    pump_until (fun () -> Option.is_none runtime.Chat_tui.App_runtime.typeahead_op);
    printf
      "applied=%b op_cleared=%b\n"
      (Option.is_some (Chat_tui.Model.typeahead_completion model))
      (Option.is_none runtime.Chat_tui.App_runtime.typeahead_op);
    ignore (stop () : bool));
  [%expect {| applied=false op_cleared=true |}]
;;

let%expect_test "Typeahead_done is ignored outside Insert mode (stale applicability)" =
  let model = make_model ~input_line:"hi" ~cursor_pos:2 ~mode:Chat_tui.Model.Normal () in
  with_reducer ~model (fun ~runtime ~send_input:_ ~send_internal ~pump_until ~stop ->
    runtime.Chat_tui.App_runtime.typeahead_op
    <- Some (Chat_tui.App_runtime.Starting_typeahead { id = 1 });
    send_internal
      (`Typeahead_done
          (1, { generation = 0; base_input = "hi"; base_cursor = 2; text = " there" }));
    pump_until (fun () -> Option.is_none runtime.Chat_tui.App_runtime.typeahead_op);
    printf
      "applied=%b mode=%s\n"
      (Option.is_some (Chat_tui.Model.typeahead_completion model))
      (match Chat_tui.Model.mode model with
       | Insert -> "Insert"
       | Normal -> "Normal"
       | Cmdline -> "Cmdline");
    ignore (stop () : bool));
  [%expect {| applied=false mode=Normal |}]
;;

let%expect_test "Cursor-only movement clears completion + closes preview (reducer policy)"
  =
  let model = make_model ~input_line:"hi" ~cursor_pos:2 ~mode:Chat_tui.Model.Insert () in
  set_relevant_completion model ~text:" there";
  Chat_tui.Model.set_typeahead_preview_open model true;
  with_reducer ~model (fun ~runtime:_ ~send_input ~send_internal:_ ~pump_until ~stop ->
    send_input (`Key (`Arrow `Left, []));
    pump_until (fun () -> Int.equal (Chat_tui.Model.cursor_pos model) 1);
    printf
      "input=%S cursor=%d preview_open=%b completion_present=%b\n"
      (Chat_tui.Model.input_line model)
      (Chat_tui.Model.cursor_pos model)
      (Chat_tui.Model.typeahead_preview_open model)
      (Option.is_some (Chat_tui.Model.typeahead_completion model));
    ignore (stop () : bool));
  [%expect {| input="hi" cursor=1 preview_open=false completion_present=false |}]
;;
