open Core
module Item = Openai.Responses.Item

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

let runtime allocator history =
  let model = make_model history in
  Chat_tui.App_runtime.create ~history_allocator:allocator ~model ()
;;

let scope ?(relation = Transcript.Scope.Root) source =
  Transcript.Scope.create
    ~source:(Transcript.Source_id.of_string source |> ok_exn)
    ~attempt:(Transcript.Attempt_id.of_string "attempt" |> ok_exn)
    ~relation
  |> ok_exn
;;

let descriptor scope entry_id =
  Transcript.Item.create
    ~scope
    ~id:(Transcript.Item_id.of_string "provider-item" |> ok_exn)
    ~entry_id:(Some entry_id)
    ~header:(Some (Message Assistant))
    ~call_name:None
  |> ok_exn
;;

let observation view =
  Transcript.Stream.create view ~limits:Document_schema.Limits.default |> ok_exn
;;

let throttler () = Chat_tui.Redraw_throttle.create ~fps:60. ~enqueue_redraw:ignore

let finalized entry_id text =
  let module Payload = History_entry.Payload in
  Payload.Semantic.create
    (Message
       { form = Output
       ; role = Assistant
       ; content = [ Text { text; annotations = []; logprobs = Absent } ]
       ; phase = Absent
       })
    ~metadata:Payload.Metadata.empty
  |> ok_exn
  |> Payload.authored
  |> History_entry.create_with_id ~id:entry_id
;;

let%expect_test "actual postcommit callback alone appends finalized root history" =
  let allocator = allocator () in
  let runtime = runtime allocator [] in
  let entry_id = History_entry.Allocator.allocate allocator |> ok_exn in
  let entry = finalized entry_id "done" in
  let item = descriptor (scope "root") entry_id in
  let throttler = throttler () in
  Chat_tui.App_stream_apply.apply_transcript_event
    runtime
    throttler
    ~viewport_height:20
    (observation (Item_finalized { item; entry }))
  |> ok_exn;
  let after_draft = List.length (Chat_tui.Model.history_items runtime.model) in
  Chat_tui.App_stream_apply.apply_history_committed runtime throttler entry;
  Chat_tui.App_stream_apply.apply_history_committed runtime throttler entry;
  let history = Chat_tui.Model.history_items runtime.model in
  print_s
    [%sexp
      (( after_draft
       , List.map history ~f:(fun entry ->
           History_entry.Id.to_string (History_entry.id entry))
       , History_entry.Allocator.next_sequence allocator )
       : int * string list * int)];
  [%expect {| (0 (6:task10:0) 1) |}]
;;

let%expect_test
    "selfdescribing first delta uses actual host identity without provider key"
  =
  let allocator = allocator () in
  let runtime = runtime allocator [] in
  let entry_id = History_entry.Allocator.allocate allocator |> ok_exn in
  let item = descriptor (scope "root") entry_id in
  let part =
    Transcript.Part.create
      ~item
      ~id:(Transcript.Part_id.of_string "text" |> ok_exn)
      ~index:None
      ~kind:Text
    |> ok_exn
  in
  Chat_tui.App_stream_apply.apply_transcript_event
    runtime
    (throttler ())
    ~viewport_height:20
    (observation (Changed { target = Content part; change = Append "hello" }))
  |> ok_exn;
  assert (List.is_empty (Chat_tui.Model.history_items runtime.model));
  let row = Chat_tui.Model.projected_rows runtime.model |> Array.to_list |> List.hd_exn in
  print_s
    [%sexp
      { canonical =
          (Option.exists row.entry_id ~f:(History_entry.Id.equal entry_id) : bool)
      ; provider =
          (Hashtbl.mem (Chat_tui.Model.msg_buffers runtime.model) "provider-item" : bool)
      }];
  [%expect {| ((canonical true) (provider false)) |}]
;;

let%expect_test "nested finalized draft never enters parent writable history" =
  let allocator = allocator () in
  let runtime = runtime allocator [] in
  let parent = scope "root" in
  let child =
    scope
      "child"
      ~relation:
        (Nested
           { scope = Transcript.Scope.key parent
           ; call_entry_id = None
           ; call_alias = None
           })
  in
  let entry_id =
    History_entry.Id.create ~namespace:"task10/child" ~sequence:0 |> ok_exn
  in
  let item = descriptor child entry_id in
  let entry = finalized entry_id "child" in
  Chat_tui.App_stream_apply.apply_transcript_event
    runtime
    (throttler ())
    ~viewport_height:20
    (observation (Item_finalized { item; entry }))
  |> ok_exn;
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
  let create item = Openai.Responses_history.create ~allocator item |> ok_exn in
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
