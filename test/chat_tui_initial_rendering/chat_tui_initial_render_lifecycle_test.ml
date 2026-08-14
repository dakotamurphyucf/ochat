open Core
module Model = Chat_tui.Model
module Runtime = Chat_tui.App_runtime

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
    ~mode:Model.Insert
    ~draft_mode:Model.Plain
    ~selected_msg:None
    ~undo_stack:[]
    ~redo_stack:[]
    ~cmdline:""
    ~cmdline_cursor:0
;;

let config =
  Chat_tui.Chat_render_worker_runtime.Config.create
    ~custom_grammars:[]
    ~theme_generation:0
    ~grammar_generation:0
;;

let cache_is_current model idx =
  let role, text = (Model.render_messages model).(idx) in
  let id, revision = Model.render_row_identity model ~idx |> Option.value_exn in
  match Model.active_history_width model, Model.find_img_cache model ~id ~revision with
  | Some width, Some entry ->
    Int.equal width entry.width
    && String.equal role entry.role
    && String.equal text entry.text
  | None, _ | Some _, None -> false
;;

let project_messages model messages =
  let rows =
    List.mapi messages ~f:(fun sequence message ->
      let entry_id =
        History_entry.Id.create ~namespace:"startup-test" ~sequence
        |> Result.ok_or_failwith
      in
      Chat_tui.Projected_message.canonical_row ~entry_id message)
  in
  Model.reconcile_projected_messages model ~rows ~messages
;;

let%test_unit "aggregate startup renders all rows and publishes once" =
  Eio_main.run
  @@ fun env ->
  let size = 40, 12 in
  let model =
    let messages =
      List.init 80 ~f:(fun index ->
        "assistant", sprintf "message %d\n\n```ocaml\nlet x = %d\n```" index index)
    in
    let model = make_model messages in
    project_messages model messages;
    model
  in
  let runtime = Runtime.create ~model () in
  Runtime.arm_startup_render
    runtime
    ~domain_mgr:(Eio.Stdenv.domain_mgr env)
    ~config
    ~code_cache_capacity:16;
  Chat_tui.Renderer_page_chat.prepare_startup_history ~size ~model;
  let generation, domain_mgr, config, capacity, cancel, snapshot, jobs =
    Runtime.begin_startup_render runtime |> Option.value_exn
  in
  let snapshot = Option.value_exn snapshot in
  [%test_eq: int] (List.length jobs) 80;
  let outcome =
    Chat_tui.Chat_startup_render.render
      ~domain_mgr
      ~config
      ~code_cache_capacity:capacity
      ~is_cancelled:(fun () -> Atomic.get cancel)
      ~snapshot
      ~jobs
  in
  let completion =
    match outcome with
    | Completed completion -> completion
    | Failed error -> raise error
    | Cancelled -> failwith "startup render cancelled"
  in
  assert (Runtime.startup_render_accepts runtime ~generation completion.snapshot);
  assert (
    Chat_tui.App_reducer.For_testing.commit_startup_results ~model completion.results);
  assert (Chat_tui.Renderer_page_chat.publish_startup_history ~size ~model);
  Runtime.complete_startup_render runtime;
  Runtime.record_startup_publication runtime ~publication_latency:Time_ns.Span.zero;
  assert (not (Model.normal_input_is_enabled model));
  Model.set_normal_input_enabled model true;
  Array.iteri (Model.render_messages model) ~f:(fun index _ ->
    assert (cache_is_current model index));
  assert (not (Chat_tui.App_reducer.For_testing.finish_startup_progress ~runtime ~size));
  Model.clear_img_caches_preserving_heights model;
  Array.iteri (Model.render_messages model) ~f:(fun index (role, text) ->
    let id, revision = Model.render_row_identity model ~idx:index |> Option.value_exn in
    assert (Option.is_none (Model.find_img_cache model ~id ~revision));
    let semantic =
      Model.find_semantic_cache model ~id ~revision ~role ~text ~tool_output:None
      |> Option.value_exn
    in
    assert (not (List.is_empty semantic.highlights)));
  let rows = Model.projected_rows model |> Array.to_list in
  let first = List.hd_exn rows in
  let removed = List.nth_exn rows 1 in
  let revised = { first with message = "assistant", "revised" } in
  let remaining = revised :: List.drop rows 2 in
  Model.reconcile_projected_messages
    model
    ~rows:remaining
    ~messages:(List.map remaining ~f:(fun row -> row.message));
  let revised_id, revised_revision =
    Model.render_row_identity model ~idx:0 |> Option.value_exn
  in
  assert (Poly.equal revised_id first.id);
  assert (revised_revision > first.revision);
  assert (
    Option.is_none
      (Model.find_semantic_cache
         model
         ~id:revised_id
         ~revision:revised_revision
         ~role:"assistant"
         ~text:"revised"
         ~tool_output:None));
  assert (
    Option.is_none
      (Model.find_semantic_cache
         model
         ~id:removed.id
         ~revision:removed.revision
         ~role:(fst removed.message)
         ~text:(snd removed.message)
         ~tool_output:None))
;;

let%test_unit "stale aggregate is rejected before commit" =
  Eio_main.run
  @@ fun env ->
  let size = 40, 10 in
  let model =
    make_model (List.init 12 ~f:(fun index -> "assistant", Int.to_string index))
  in
  let runtime = Runtime.create ~model () in
  Runtime.arm_startup_render
    runtime
    ~domain_mgr:(Eio.Stdenv.domain_mgr env)
    ~config
    ~code_cache_capacity:8;
  Chat_tui.Renderer_page_chat.prepare_startup_history ~size ~model;
  let generation, _, _, _, _, snapshot, _ =
    Runtime.begin_startup_render runtime |> Option.value_exn
  in
  let snapshot = Option.value_exn snapshot in
  ignore
    (Model.apply_patch
       model
       (Chat_tui.Types.Add_placeholder_message { role = "assistant"; text = "changed" })
     : Model.t);
  assert (not (Runtime.startup_render_accepts runtime ~generation snapshot));
  Runtime.close_startup_render runtime;
  assert (Chat_tui.App_reducer.For_testing.finish_startup_progress ~runtime ~size);
  assert (not (Model.normal_input_is_enabled model));
  Model.set_normal_input_enabled model true;
  Array.iteri (Model.render_messages model) ~f:(fun index _ ->
    assert (cache_is_current model index))
;;
