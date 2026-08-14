open Core
module History = Chat_tui.Renderer_component_history
module Job = Chat_tui.Chat_message_render_job
module Model = Chat_tui.Model
module Projected = Chat_tui.Projected_message
module Worker = Chat_tui.Chat_render_worker

let make_model messages =
  Model.create
    ~history_items:[]
    ~messages
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
    ~mode:Normal
    ~draft_mode:Plain
    ~selected_msg:None
    ~undo_stack:[]
    ~redo_stack:[]
    ~cmdline:""
    ~cmdline_cursor:0
;;

let render_count count =
  let messages = Array.init count ~f:(fun index -> "assistant", Int.to_string index) in
  let model = make_model (Array.to_list messages) in
  let calls = ref [] in
  let plan =
    History.render
      ~model
      ~width:40
      ~height:10
      ~messages
      ~selected_idx:None
      ~render_message:(fun ~idx ~selected:_ _ ->
        calls := idx :: !calls;
        Notty.I.void 40 5)
  in
  ( List.dedup_and_sort !calls ~compare:Int.compare
  , List.length
      (Chat_tui.Renderer_virtual_list.Geometry.estimated_indices
         (Model.chat_render_geometry model))
  , plan )
;;

let short_mixed_messages =
  [ "user", "short user row"
  ; "assistant", "A longer assistant row that wraps at narrow widths."
  ; "tool", "tool result\nalpha\nbeta"
  ; "assistant", "```ocaml\nlet answer = 42\n```"
  ; "user", ""
  ; "assistant", "final row"
  ]
;;

let mixed_messages count =
  List.init count ~f:(fun index ->
    match index % 4 with
    | 0 -> "user", sprintf "user-%04d compact" index
    | 1 ->
      ( "assistant"
      , sprintf "assistant-%04d deterministic wrapping alpha beta gamma delta" index )
    | 2 -> "tool", sprintf "tool-%04d line-one\nline-two\nline-three" index
    | _ -> "assistant", sprintf "```ocaml\nlet row_%04d = %d\n```" index index)
;;

let render_and_warm ~width ~model =
  let size = width, 12 in
  ignore (Chat_tui.Renderer_page_chat.render ~size ~model : Notty.I.t * (int * int));
  Chat_tui.Renderer_page_chat.warm_history_synchronously ~size ~model
;;

let materialization_baseline messages =
  let model = make_model messages in
  render_and_warm ~width:40 ~model;
  Chat_tui.Renderer_page_chat.relayout_history_synchronously ~size:(60, 12) ~model;
  Chat_tui.Renderer_page_chat.warm_history_synchronously ~size:(60, 12) ~model;
  let (), restored =
    Chat_tui.Renderer_page_chat.For_testing.capture_materialized_indices (fun () ->
      render_and_warm ~width:40 ~model)
  in
  let (), uncached =
    Chat_tui.Renderer_page_chat.For_testing.capture_materialized_indices (fun () ->
      Chat_tui.Renderer_page_chat.relayout_history_synchronously ~size:(23, 12) ~model)
  in
  restored, uncached
;;

let row sequence =
  let entry_id =
    History_entry.Id.create ~namespace:"performance-baseline" ~sequence
    |> Result.ok_or_failwith
  in
  Projected.canonical_row ~entry_id ("assistant", Int.to_string sequence)
;;

let config =
  Chat_tui.Chat_render_worker_runtime.Config.create
    ~custom_grammars:[]
    ~theme_generation:0
    ~grammar_generation:0
;;

let job ~row_id ~index =
  Job.create
    ~transcript_generation:1
    ~row_id
    ~row_revision:0
    ~message_index:index
    ~message_revision:0
    ~width:40
    ~role:"assistant"
    ~text:"stable"
    ~tool_output:None
    ~tool_call_outcome:None
    ~theme_generation:0
    ~grammar_generation:0
    ~geometry_generation:0
    ~request_generation:0
    ~render_generation:0
    ~submission_generation:0
    ~semantic_seed:None
    ~priority:Background
;;

let%test_unit "short resize baseline distinguishes cached and uncached widths" =
  let restored, uncached = materialization_baseline short_mixed_messages in
  [%test_eq: int list] restored [];
  [%test_eq: int list] uncached (List.init (List.length short_mixed_messages) ~f:Fn.id)
;;

let%test_unit "long resize baseline bounds cached restoration" =
  let row_count = 513 in
  let restored, uncached = materialization_baseline (mixed_messages row_count) in
  [%test_eq: int list] restored [];
  [%test_eq: int] (List.length uncached) row_count;
  [%test_eq: int list] uncached (List.init row_count ~f:Fn.id)
;;

let%test_unit "identity maps preserve bounded lazy rendering and worker work" =
  let small_calls, small_estimated, _ = render_count 100 in
  let large_calls, large_estimated, _ = render_count 10_000 in
  [%test_eq: int] (List.length small_calls) (List.length large_calls);
  assert (List.length large_calls < 10_000);
  assert (small_estimated > 0);
  assert (large_estimated > small_estimated);
  let rows = Array.init 10_000 ~f:row in
  let model = make_model [] in
  Model.reconcile_projected_rows model (Array.to_list rows);
  Model.reconcile_messages
    model
    (Array.to_list rows |> List.map ~f:(fun row -> row.Projected.message));
  let rotated =
    Array.concat
      [ Array.sub rows ~pos:5_000 ~len:5_000; Array.sub rows ~pos:0 ~len:5_000 ]
  in
  Model.reconcile_projected_rows model (Array.to_list rotated);
  Model.reconcile_messages
    model
    (Array.to_list rotated |> List.map ~f:(fun row -> row.Projected.message));
  Array.iteri rotated ~f:(fun index row ->
    [%test_eq: int option] (Model.render_index_by_id model ~id:row.id) (Some index));
  let worker =
    Worker.For_testing.create_detached ~config ~queue_capacity:4 ~worker_count:2
  in
  let row_id = rows.(0).id in
  assert (Poly.equal (Worker.submit worker (job ~row_id ~index:0)) Queued);
  assert (Poly.equal (Worker.submit worker (job ~row_id ~index:9_999)) Already_pending);
  for index = 1 to 20 do
    ignore
      (Worker.submit worker (job ~row_id:rows.(index).id ~index) : Worker.submit_result)
  done;
  let stats = Worker.For_testing.stats worker in
  assert (stats.queued <= stats.queue_capacity);
  assert (stats.pending <= stats.queue_capacity + stats.worker_count)
;;

let%test_unit "first progressive resize corridor is independent of transcript length" =
  let scheduled_rows row_count =
    let geometry = Chat_tui.Renderer_virtual_list.Geometry.create () in
    Chat_tui.Renderer_virtual_list.Geometry.rebuild
      geometry
      ~length:row_count
      ~height_at_index:(Fn.const 1);
    let plan =
      Chat_tui.Prepared_corridor.plan
        ~geometry:(Chat_tui.Renderer_virtual_list.Geometry.snapshot geometry)
        ~requested_scroll:(row_count / 2)
        ~viewport_height:20
        ~follow_bottom:false
        ~direction:Toward_older
        ()
    in
    ( Chat_tui.History_chunk.Range.length plan.scheduled_rows
    , List.length
        (List.filter plan.batches ~f:(fun batch ->
           not (Poly.equal batch.Chat_tui.Prepared_corridor.class_ Remaining))) )
  in
  let small_rows, small_batches = scheduled_rows 100 in
  let large_rows, large_batches = scheduled_rows 10_000 in
  assert (small_rows <= large_rows);
  assert (small_batches <= large_batches);
  assert (large_rows <= 256);
  assert (large_batches <= 16)
;;
