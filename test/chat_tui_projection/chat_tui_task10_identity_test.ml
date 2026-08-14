open Core
module Item = Openai.Responses.Item
module Stream = Openai.Responses.Response_stream

let ok_exn = function
  | Ok value -> value
  | Error error -> failwith error
;;

let allocator () =
  History_entry.Allocator.create ~namespace:"task10" ~next_sequence:0 |> ok_exn
;;

let make_model history =
  Chat_tui.Model.create
    ~history_items:history
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
    ~mode:Insert
    ~draft_mode:Plain
    ~selected_msg:None
    ~undo_stack:[]
    ~redo_stack:[]
    ~cmdline:""
    ~cmdline_cursor:0
;;

let output_message id text =
  Item.Output_message
    { role = Openai.Responses.Output_message.Assistant
    ; id
    ; content = [ { annotations = []; text; _type = "output_text" } ]
    ; status = "completed"
    ; phase = None
    ; _type = "message"
    }
;;

let stream_item = function
  | Item.Output_message message -> Stream.Item.Output_message message
  | _ -> failwith "expected output message"
;;

let runtime allocator history =
  let model = make_model history in
  Chat_tui.App_runtime.create ~history_allocator:allocator ~model ()
;;

let%expect_test "history stream is the sole canonical root finalization path" =
  let allocator = allocator () in
  let runtime = runtime allocator [] in
  let entry_id = History_entry.Allocator.allocate allocator |> ok_exn in
  let item = output_message "provider" "done" in
  let event =
    Stream.Output_item_done
      { item = stream_item item; output_index = 0; type_ = "response.output_item.done" }
  in
  let sourced = Chat_response.Sourced_response_event.outer event in
  let redraw = ref 0 in
  let throttler =
    Chat_tui.Redraw_throttle.create ~fps:60. ~enqueue_redraw:(fun () -> Int.incr redraw)
  in
  Chat_tui.App_stream_apply.apply_sourced_stream_event
    runtime
    throttler
    ~viewport_height:20
    sourced;
  let after_sourced = List.length (Chat_tui.Model.history_items runtime.model) in
  Chat_tui.App_stream_apply.apply_history_stream_event
    runtime
    Chat_response.History_stream_event.{ entry_id; source = None; event };
  let history = Chat_tui.Model.history_items runtime.model in
  print_s
    [%sexp
      (( after_sourced
       , List.map history ~f:(fun entry ->
           History_entry.Id.to_string (History_entry.id entry))
       , History_entry.Allocator.next_sequence allocator )
       : int * string list * int)];
  [%expect {| (0 (6:task10:0) 1) |}]
;;

let%expect_test "first sourced delta uses the eventual canonical entry ID" =
  let allocator = allocator () in
  let runtime = runtime allocator [] in
  let entry_id = History_entry.Allocator.allocate allocator |> ok_exn in
  let event =
    Stream.Output_text_delta
      { item_id = "provider-id"
      ; content_index = 0
      ; delta = "hello"
      ; output_index = 0
      ; type_ = "response.output_text.delta"
      }
  in
  let redraw = ref 0 in
  let throttler =
    Chat_tui.Redraw_throttle.create ~fps:60. ~enqueue_redraw:(fun () -> Int.incr redraw)
  in
  Chat_tui.App_stream_apply.apply_sourced_stream_event
    runtime
    throttler
    ~viewport_height:20
    (Chat_response.Sourced_response_event.outer ~entry_id event);
  let canonical_key = History_entry.Id.to_string entry_id in
  print_s
    [%sexp
      { canonical =
          (Hashtbl.mem (Chat_tui.Model.msg_buffers runtime.model) canonical_key : bool)
      ; provider =
          (Hashtbl.mem (Chat_tui.Model.msg_buffers runtime.model) "provider-id" : bool)
      }];
  [%expect {| ((canonical true) (provider false)) |}]
;;

let%expect_test "fork history stream identity does not enter parent history" =
  let allocator = allocator () in
  let runtime = runtime allocator [] in
  let entry_id =
    History_entry.Id.create ~namespace:"task10/child" ~sequence:0 |> ok_exn
  in
  let item = output_message "provider" "child" in
  let event =
    Stream.Output_item_done
      { item = stream_item item; output_index = 0; type_ = "response.output_item.done" }
  in
  Chat_tui.App_stream_apply.apply_history_stream_event
    runtime
    Chat_response.History_stream_event.{ entry_id; source = Some "child"; event };
  print_s [%sexp (List.length (Chat_tui.Model.history_items runtime.model) : int)];
  [%expect {| 0 |}]
;;

let function_call call_id =
  Item.Function_call
    { name = "tool"
    ; arguments = "{}"
    ; call_id
    ; _type = "function_call"
    ; id = None
    ; status = Some "completed"
    }
;;

let%expect_test "cancellation repair preserves duplicate occurrences and is idempotent" =
  let allocator = allocator () in
  let create item = History_entry.create ~allocator item |> ok_exn in
  let duplicate = output_message "same-provider" "same" in
  let first = create duplicate in
  let second = create duplicate in
  let call = create (function_call "call") in
  let entries = [ first; second; call ] in
  let first =
    Chat_tui.App_reducer.Cancellation_repair.repair ~allocator ~error:"cancelled" entries
    |> ok_exn
  in
  let watermark = History_entry.Allocator.next_sequence allocator in
  let second =
    Chat_tui.App_reducer.Cancellation_repair.repair ~allocator ~error:"cancelled" first
    |> ok_exn
  in
  let ids entries =
    List.map entries ~f:(fun entry -> History_entry.Id.to_string (History_entry.id entry))
  in
  print_s
    [%sexp
      (( ids first
       , List.equal
           History_entry.Id.equal
           (List.map first ~f:History_entry.id)
           (List.map second ~f:History_entry.id)
       , watermark
       , History_entry.Allocator.next_sequence allocator )
       : string list * bool * int * int)];
  [%expect {| ((6:task10:0 6:task10:1 6:task10:2 6:task10:3) true 4 4) |}]
;;
