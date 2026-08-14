open Core
module Res = Openai.Responses
module Geometry = Chat_tui.Renderer_virtual_list.Geometry
module Viewport = Chat_tui.Renderer_virtual_list.Viewport

let make_model ~messages ~auto_follow =
  Chat_tui.Model.create
    ~history_items:[]
    ~messages
    ~input_line:""
    ~auto_follow
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

let event sequence type_ =
  Res.Response_stream.Unknown
    (`Object
        [ "sequence_number", `Number (Int.to_string sequence); "type", `String type_ ])
;;

let completion sequence =
  Res.Response_stream.Response_completed
    (Res.Response_stream.Response_completed.t_of_jsonaf
       (`Object
           [ "type", `String "response.completed"
           ; ( "response"
             , `Object
                 [ "id", `String "response"
                 ; "object", `String "response"
                 ; "created_at", `Number "0"
                 ; "model", `String "gpt-4.1"
                 ; "output", `Array []
                 ; "parallel_tool_calls", `False
                 ; "tool_choice", `String "none"
                 ; "tools", `Array []
                 ; "temperature", `Number "0"
                 ; "top_p", `Number "1"
                 ; "status", `String "completed"
                 ; "usage", `Null
                 ; "user", `Null
                 ; "error", `Null
                 ; "incomplete_details", `Null
                 ; "instructions", `Null
                 ; "max_output_tokens", `Null
                 ; "metadata", `Object []
                 ; "previous_response_id", `Null
                 ; "reasoning", `Null
                 ; "service_tier", `String "default"
                 ; "store", `False
                 ; "text", `Object [ "format", `Object [ "type", `String "text" ] ]
                 ; "truncation", `String "disabled"
                 ] )
           ; "sequence_number", `Number (Int.to_string sequence)
           ]))
;;

let consume stream = Seq.iter (fun _ -> ()) stream

let%expect_test "response stream requires one successful terminal event" =
  let show events =
    try
      Res.validate_response_stream (Stdlib.List.to_seq events) |> consume;
      print_endline "completed"
    with
    | Res.Response_stream_terminated_without_completion ->
      print_endline "missing completion"
    | Res.Response_stream_terminal_error _ -> print_endline "terminal error"
  in
  show [ event 0 "response.created" ];
  show [ completion 0 ];
  show [ completion 0; event 1 "response.output_text.delta" ];
  [%expect
    {|
    missing completion
    completed
    terminal error
    |}]
;;

let%expect_test "OpenAI idle deadline resets after each streamed event" =
  Eio_main.run
  @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let source =
    let next = ref 0 in
    let rec stream () =
      match !next with
      | 0 | 1 ->
        Int.incr next;
        Eio.Time.sleep clock 0.001;
        Seq.Cons (!next, stream)
      | _ ->
        Eio.Time.sleep clock 60.;
        Seq.Nil
    in
    stream
  in
  let seen = ref [] in
  let timed_out =
    try
      Chat_response.In_memory_stream.For_testing.with_stream_idle_timeout
        ~clock
        ~seconds:0.01
        source
      |> Seq.iter (fun event -> seen := event :: !seen);
      false
    with
    | Chat_response.In_memory_stream.Openai_stream_idle_timeout _ -> true
  in
  print_s [%sexp (List.rev !seen : int list), (timed_out : bool)];
  [%expect {| ((1 2) true) |}]
;;

let%expect_test "sourced deltas update the live model and schedule redraw" =
  let model = make_model ~messages:[] ~auto_follow:true in
  let runtime = Chat_tui.App_runtime.create ~model () in
  let enqueued = ref 0 in
  let throttler =
    Chat_tui.Redraw_throttle.create ~fps:60. ~enqueue_redraw:(fun () -> incr enqueued)
  in
  let entry_id =
    History_entry.Id.create ~namespace:"realtime" ~sequence:0 |> Result.ok_or_failwith
  in
  let event =
    Res.Response_stream.Output_text_delta
      { content_index = 0
      ; delta = "visible before completion"
      ; item_id = "provider-message"
      ; output_index = 0
      ; type_ = "response.output_text.delta"
      }
  in
  Chat_tui.App_stream_apply.apply_sourced_stream_event
    runtime
    throttler
    ~viewport_height:20
    (Chat_response.Sourced_response_event.outer ~entry_id event);
  Chat_tui.Redraw_throttle.tick throttler;
  print_s
    [%sexp (Chat_tui.Model.messages model : (string * string) list), (!enqueued : int)];
  [%expect {| (((assistant "visible before completion")) 1) |}]
;;

let%expect_test "manual viewport follows stable row identity through reorder" =
  let model =
    make_model
      ~messages:[ "assistant", "one"; "assistant", "two"; "assistant", "three" ]
      ~auto_follow:false
  in
  let ids =
    List.init 3 ~f:(fun sequence ->
      History_entry.Id.create ~namespace:"scroll" ~sequence |> Result.ok_or_failwith)
  in
  let rows =
    List.map2_exn ids [ "one"; "two"; "three" ] ~f:(fun id text ->
      Chat_tui.Projected_message.canonical_row ~entry_id:id ("assistant", text))
  in
  Chat_tui.Model.reconcile_projected_rows model rows;
  Chat_tui.Model.reconcile_messages model (List.map rows ~f:(fun row -> row.message));
  let geometry = Chat_tui.Model.chat_render_geometry model in
  Geometry.rebuild geometry ~length:3 ~height_at_index:(fun index -> index + 2);
  Notty_scroll_box.scroll_to (Chat_tui.Model.scroll_box model) 2;
  let reordered = [ List.nth_exn rows 1; List.nth_exn rows 0; List.nth_exn rows 2 ] in
  Chat_tui.Model.reconcile_projected_rows model reordered;
  Chat_tui.Model.reconcile_messages model (List.map reordered ~f:(fun row -> row.message));
  let viewport =
    Viewport.compute
      ~geometry
      ~requested_scroll:(Notty_scroll_box.scroll (Chat_tui.Model.scroll_box model))
      ~height:2
      ~follow_bottom:false
  in
  let first_id =
    Viewport.first_visible viewport
    |> Option.bind ~f:(fun idx ->
      Chat_tui.Model.render_row_identity model ~idx
      |> Option.map ~f:(fun (id, _) -> Chat_tui.Projected_message.Id.to_string id))
  in
  print_s
    [%sexp
      (first_id : string option)
    , (Notty_scroll_box.scroll (Chat_tui.Model.scroll_box model) : int)];
  [%expect {| ((6:scroll:1) 0) |}]
;;

let%expect_test "stream finalization preserves a manually scrolled row anchor" =
  let model = make_model ~messages:[] ~auto_follow:false in
  let id sequence =
    History_entry.Id.create ~namespace:"stream-finish-scroll" ~sequence
    |> Result.ok_or_failwith
  in
  let canonical sequence text =
    Chat_tui.Projected_message.canonical_row ~entry_id:(id sequence) ("assistant", text)
  in
  let a = canonical 0 "a" in
  let b_id = id 1 in
  let streaming_b : Chat_tui.Projected_message.t =
    { id = Chat_tui.Projected_message.Id.canonical b_id
    ; entry_id = Some b_id
    ; message = "assistant", "streaming"
    ; provenance = Streaming
    ; source =
        Streaming
          { entry_id = b_id; provider_item_id = Some "provider-b"; call_id = None }
    ; revision = 0
    }
  in
  let c = canonical 2 "c" in
  let initial = [ a; streaming_b; c ] in
  Chat_tui.Model.reconcile_projected_messages
    model
    ~rows:initial
    ~messages:(List.map initial ~f:(fun row -> row.message));
  let geometry = Chat_tui.Model.chat_render_geometry model in
  Geometry.rebuild geometry ~length:3 ~height_at_index:(Array.get [| 3; 20; 4 |]);
  Notty_scroll_box.scroll_to (Chat_tui.Model.scroll_box model) 7;
  let final_b = canonical 1 "streaming complete" in
  let prepend = canonical 3 "prepended" in
  let final = [ prepend; a; final_b; c ] in
  Chat_tui.Model.reconcile_projected_messages
    model
    ~rows:final
    ~messages:(List.map final ~f:(fun row -> row.message));
  let viewport =
    Viewport.compute
      ~geometry
      ~requested_scroll:(Notty_scroll_box.scroll (Chat_tui.Model.scroll_box model))
      ~height:1
      ~follow_bottom:false
  in
  let first_id =
    Viewport.first_visible viewport
    |> Option.bind ~f:(fun idx ->
      Chat_tui.Model.render_row_identity model ~idx
      |> Option.map ~f:(fun (id, _) -> Chat_tui.Projected_message.Id.to_string id))
  in
  print_s
    [%sexp
      (first_id : string option)
    , (Notty_scroll_box.scroll (Chat_tui.Model.scroll_box model) : int)];
  [%expect {| ((20:stream-finish-scroll:1) 14) |}]
;;

let%expect_test "completion cleanup preserves Chat geometry before finalization" =
  let model = make_model ~messages:[] ~auto_follow:false in
  let id sequence =
    History_entry.Id.create ~namespace:"completion-cleanup" ~sequence
    |> Result.ok_or_failwith
  in
  let row sequence text =
    Chat_tui.Projected_message.canonical_row ~entry_id:(id sequence) ("assistant", text)
  in
  let rows = [ row 0 "first"; row 1 "streaming"; row 2 "last" ] in
  Chat_tui.Model.reconcile_projected_messages
    model
    ~rows
    ~messages:(List.map rows ~f:(fun row -> row.message));
  let geometry = Chat_tui.Model.chat_render_geometry model in
  Geometry.rebuild geometry ~length:3 ~height_at_index:(Array.get [| 3; 20; 4 |]);
  Notty_scroll_box.scroll_to (Chat_tui.Model.scroll_box model) 8;
  let before =
    Geometry.heights geometry, Notty_scroll_box.scroll (Chat_tui.Model.scroll_box model)
  in
  Chat_tui.Model.clear_agent_calls model;
  let after_clear =
    Geometry.heights geometry, Notty_scroll_box.scroll (Chat_tui.Model.scroll_box model)
  in
  Chat_tui.Model.reconcile_projected_messages
    model
    ~rows
    ~messages:(List.map rows ~f:(fun row -> row.message));
  let viewport =
    Viewport.compute
      ~geometry
      ~requested_scroll:(Notty_scroll_box.scroll (Chat_tui.Model.scroll_box model))
      ~height:1
      ~follow_bottom:false
  in
  let first_id =
    Viewport.first_visible viewport
    |> Option.bind ~f:(fun idx ->
      Chat_tui.Model.render_row_identity model ~idx
      |> Option.map ~f:(fun (id, _) -> Chat_tui.Projected_message.Id.to_string id))
  in
  print_s
    [%sexp
      (before : int array * int)
    , (after_clear : int array * int)
    , (first_id : string option)];
  [%expect
    {|
    (((3 20 4) 8) ((3 20 4) 8) (18:completion-cleanup:1))
    |}]
;;

let%expect_test "auto-follow tracks streamed row growth without stale offsets" =
  let model =
    make_model
      ~messages:[ "assistant", "one"; "assistant", "streaming" ]
      ~auto_follow:true
  in
  let ids =
    List.init 2 ~f:(fun sequence ->
      History_entry.Id.create ~namespace:"follow" ~sequence |> Result.ok_or_failwith)
  in
  let rows =
    List.map2_exn ids [ "one"; "streaming" ] ~f:(fun id text ->
      Chat_tui.Projected_message.canonical_row ~entry_id:id ("assistant", text))
  in
  Chat_tui.Model.reconcile_projected_rows model rows;
  Chat_tui.Model.reconcile_messages model (List.map rows ~f:(fun row -> row.message));
  let geometry = Chat_tui.Model.chat_render_geometry model in
  Geometry.rebuild geometry ~length:2 ~height_at_index:(Array.get [| 2; 3 |]);
  let initial =
    Viewport.compute ~geometry ~requested_scroll:0 ~height:3 ~follow_bottom:true
  in
  Geometry.update_height geometry ~index:1 ~height:9;
  let grown =
    Viewport.compute
      ~geometry
      ~requested_scroll:(Viewport.scroll initial)
      ~height:3
      ~follow_bottom:true
  in
  let resized =
    Viewport.compute
      ~geometry
      ~requested_scroll:(Viewport.scroll grown)
      ~height:5
      ~follow_bottom:true
  in
  print_s
    [%sexp
      (Viewport.scroll initial : int)
    , (Viewport.scroll grown : int)
    , (Viewport.max_scroll grown : int)
    , (Viewport.scroll resized : int)
    , (Viewport.max_scroll resized : int)];
  [%expect {| (2 8 8 6 6) |}]
;;

let%expect_test "returning to the virtual bottom restores stable streamed auto-follow" =
  let model =
    make_model
      ~messages:[ "assistant", "one"; "assistant", "streaming" ]
      ~auto_follow:true
  in
  let geometry = Chat_tui.Model.chat_render_geometry model in
  Geometry.rebuild geometry ~length:2 ~height_at_index:(Array.get [| 2; 9 |]);
  let viewport_height = 3 in
  Chat_tui.Model.follow_chat_bottom model ~viewport_height;
  let initial = Notty_scroll_box.scroll (Chat_tui.Model.scroll_box model) in
  Chat_tui.Model.set_auto_follow model false;
  let at_bottom_after_up = Chat_tui.Model.scroll_chat_by model ~viewport_height (-1) in
  let after_up = Notty_scroll_box.scroll (Chat_tui.Model.scroll_box model) in
  let at_bottom_after_down = Chat_tui.Model.scroll_chat_by model ~viewport_height 1 in
  let after_down = Notty_scroll_box.scroll (Chat_tui.Model.scroll_box model) in
  Geometry.update_height geometry ~index:1 ~height:13;
  let grown =
    Viewport.compute
      ~geometry
      ~requested_scroll:(Notty_scroll_box.scroll (Chat_tui.Model.scroll_box model))
      ~height:viewport_height
      ~follow_bottom:(Chat_tui.Model.auto_follow model)
  in
  Notty_scroll_box.scroll_to (Chat_tui.Model.scroll_box model) (Viewport.scroll grown);
  print_s
    [%sexp
      (( initial
       , at_bottom_after_up
       , after_up
       , at_bottom_after_down
       , after_down
       , (Viewport.scroll grown : int)
       , (Viewport.max_scroll grown : int)
       , (Chat_tui.Model.auto_follow model : bool) )
       : int * bool * int * bool * int * int * int * bool)];
  [%expect {| (8 false 7 true 8 12 12 true) |}]
;;

let%expect_test "new streamed rows preserve manual review" =
  let model = make_model ~messages:[ "assistant", "old" ] ~auto_follow:false in
  let geometry = Chat_tui.Model.chat_render_geometry model in
  Geometry.rebuild geometry ~length:1 ~height_at_index:(fun _ -> 8);
  Notty_scroll_box.scroll_to (Chat_tui.Model.scroll_box model) 2;
  let before = Notty_scroll_box.scroll (Chat_tui.Model.scroll_box model) in
  ignore
    (Chat_tui.Model.apply_patch
       model
       (Chat_tui.Types.Ensure_buffer { id = "new-assistant"; role = "assistant" })
     : Chat_tui.Model.t);
  let after = Notty_scroll_box.scroll (Chat_tui.Model.scroll_box model) in
  print_s
    [%sexp
      (( Chat_tui.Model.auto_follow model
       , before
       , after
       , Array.length (Chat_tui.Model.render_messages model)
       , Geometry.length geometry )
       : bool * int * int * int * int)];
  [%expect {| (false 2 2 2 2) |}]
;;

let%expect_test "offscreen sourced delta updates the model without scheduling redraw" =
  let model = make_model ~messages:[ "assistant", "old" ] ~auto_follow:true in
  let entry_id =
    History_entry.Id.create ~namespace:"offscreen" ~sequence:0 |> Result.ok_or_failwith
  in
  let id = History_entry.Id.to_string entry_id in
  let runtime = Chat_tui.App_runtime.create ~model () in
  let enqueued = ref 0 in
  let throttler =
    Chat_tui.Redraw_throttle.create ~fps:60. ~enqueue_redraw:(fun () -> incr enqueued)
  in
  let event delta =
    Res.Response_stream.Output_text_delta
      { content_index = 0
      ; delta
      ; item_id = id
      ; output_index = 0
      ; type_ = "response.output_text.delta"
      }
  in
  Chat_tui.App_stream_apply.apply_sourced_stream_event
    runtime
    throttler
    ~viewport_height:5
    (Chat_response.Sourced_response_event.outer ~entry_id (event "streaming"));
  Chat_tui.Redraw_throttle.tick throttler;
  enqueued := 0;
  Chat_tui.Model.set_auto_follow model false;
  Chat_tui.Model.set_activity
    model
    (Some (Chat_tui.Model.Assistant Chat_tui.Model.Writing));
  let geometry = Chat_tui.Model.chat_render_geometry model in
  Geometry.rebuild geometry ~length:2 ~height_at_index:(Array.get [| 5; 5 |]);
  Notty_scroll_box.set_content (Chat_tui.Model.scroll_box model) (Notty.I.void 40 10);
  Notty_scroll_box.scroll_to (Chat_tui.Model.scroll_box model) 0;
  Chat_tui.App_stream_apply.apply_sourced_stream_event
    runtime
    throttler
    ~viewport_height:5
    (Chat_response.Sourced_response_event.outer ~entry_id (event " new"));
  Chat_tui.Redraw_throttle.tick throttler;
  print_s
    [%sexp (Chat_tui.Model.messages model : (string * string) list), (!enqueued : int)];
  [%expect {| (((assistant old) (assistant "streaming new")) 0) |}]
;;
