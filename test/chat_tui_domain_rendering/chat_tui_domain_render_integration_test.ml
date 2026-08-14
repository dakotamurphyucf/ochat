open Core
module Job = Chat_tui.Chat_message_render_job
module Model = Chat_tui.Model
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
    ~mode:Model.Insert
    ~draft_mode:Model.Plain
    ~selected_msg:None
    ~undo_stack:[]
    ~redo_stack:[]
    ~cmdline:""
    ~cmdline_cursor:0
;;

let image_to_string ~width ~height image =
  let buffer = Buffer.create 4096 in
  Notty.Render.to_buffer buffer Notty.Cap.ansi (0, 0) (width, height) image;
  Buffer.contents buffer
;;

let config =
  Chat_tui.Chat_render_worker_runtime.Config.create
    ~custom_grammars:[]
    ~theme_generation:0
    ~grammar_generation:0
;;

let detached_runtime () =
  Job.Runtime.create
    ~hi_engine:(Chat_tui.Renderer_highlight_engine.get ())
    ~theme_generation:0
    ~grammar_generation:0
    ~code_cache:(Job.Code_cache.create ~capacity:32)
    ()
;;

let%test_unit "visible startup frame is synchronous full fidelity" =
  let size = 56, 18 in
  let messages =
    [ "user", "Find the matching tool output."
    ; ( "assistant"
      , "Markdown **bold** and a fenced block.\n\n```ocaml\nlet answer = 42\n```" )
    ; "tool", "read_file({\"file\":\"lib/example.ml\"})"
    ; "tool_output", "let answer = 42\n"
    ; "assistant", "The answer is 42."
    ]
  in
  let expected_model = make_model messages in
  let worker_model = make_model messages in
  Model.set_tool_output_kind
    expected_model
    ~idx:3
    (Chat_tui.Types.Read_file { path = Some "lib/example.ml" })
  |> ignore;
  Model.set_tool_output_kind
    worker_model
    ~idx:3
    (Chat_tui.Types.Read_file { path = Some "lib/example.ml" })
  |> ignore;
  Model.set_mode expected_model (Model.Search Model.Forward);
  Model.set_mode worker_model (Model.Search Model.Forward);
  Model.set_search_query expected_model "answer";
  Model.set_search_query worker_model "answer";
  Model.select_message expected_model (Some 4);
  Model.select_message worker_model (Some 4);
  let expected_image, expected_cursor =
    Chat_tui.Renderer.render_full ~size ~model:expected_model
  in
  let actual_image, actual_cursor =
    Chat_tui.Renderer.render_full ~size ~model:worker_model
  in
  let width, height = size in
  [%test_eq: string]
    (image_to_string ~width ~height expected_image)
    (image_to_string ~width ~height actual_image);
  [%test_eq: int * int] expected_cursor actual_cursor;
  [%test_eq: int array]
    (Model.msg_heights expected_model)
    (Model.msg_heights worker_model);
  [%test_eq: int array]
    (Model.height_prefix expected_model)
    (Model.height_prefix worker_model);
  assert (
    List.length
      (Chat_tui.Renderer_virtual_list.Geometry.estimated_indices
         (Model.chat_render_geometry worker_model))
    > 0)
;;

let%test_unit "ordinary rendering is independent of a closed startup worker" =
  let size = 44, 14 in
  let messages =
    [ "assistant", "```ocaml\nlet large = [ 1; 2; 3 ]\n```\nFallback text." ]
  in
  let expected_model = make_model messages in
  let fallback_model = make_model messages in
  let expected_image, expected_cursor =
    Chat_tui.Renderer.render_full ~size ~model:expected_model
  in
  let worker =
    Worker.For_testing.create_detached ~config ~queue_capacity:1 ~worker_count:1
  in
  Worker.close worker;
  let actual_image, actual_cursor =
    Chat_tui.Renderer.render_full ~size ~model:fallback_model
  in
  let width, height = size in
  [%test_eq: string]
    (image_to_string ~width ~height expected_image)
    (image_to_string ~width ~height actual_image);
  [%test_eq: int * int] expected_cursor actual_cursor;
  [%test_eq: int array]
    (Model.msg_heights expected_model)
    (Model.msg_heights fallback_model);
  let metrics = Worker.metrics_json worker |> Jsonaf.to_string in
  assert (String.is_substring metrics ~substring:{|"submitted":0|})
;;

let%test_unit "loading hides the cursor until exact history is published" =
  let size = 48, 14 in
  let width, height = size in
  let model =
    make_model [ "user", "startup question"; "assistant", "exact warm answer" ]
  in
  ignore (Chat_tui.Renderer.render_full ~size ~model : Notty.I.t * (int * int));
  Model.set_chat_materialization_loading model;
  let layout =
    Chat_tui.Chat_page_layout.compute ~screen_w:width ~screen_h:height ~model
  in
  let loading_image, loading_cursor =
    Chat_tui.Renderer_page_chat.render_with_layout ~size ~layout ~model
  in
  let loading_buffer = Buffer.create 4096 in
  Notty.Render.to_buffer
    loading_buffer
    Notty.Cap.dumb
    (0, 0)
    (width, height)
    loading_image;
  let loading = Buffer.contents loading_buffer in
  assert (String.is_substring loading ~substring:"Preparing conversation");
  assert (not (String.is_substring loading ~substring:"exact warm answer"));
  [%test_eq: int * int] loading_cursor (0, 0);
  [%test_eq: (int * int) option]
    (Chat_tui.App.For_testing.cursor_for_frame ~model loading_cursor)
    None;
  Chat_tui.Renderer_page_chat.warm_history_synchronously ~size ~model;
  assert (Chat_tui.Renderer_page_chat.publish_startup_history ~size ~model);
  Model.set_chat_materialization_warm model;
  let resized_image, resized_cursor =
    Chat_tui.Renderer_page_chat.render_with_layout ~size ~layout ~model
  in
  let expected_image, expected_cursor = Chat_tui.Renderer.render_full ~size ~model in
  [%test_eq: string]
    (image_to_string ~width ~height resized_image)
    (image_to_string ~width ~height expected_image);
  [%test_eq: int * int] resized_cursor expected_cursor;
  [%test_eq: (int * int) option]
    (Chat_tui.App.For_testing.cursor_for_frame ~model resized_cursor)
    (Some expected_cursor)
;;

let%test_unit "resizing uses the snake loader and corridor restores the cursor" =
  let size = 48, 14 in
  let width, height = size in
  let model = make_model [ "assistant", "old width transcript" ] in
  Model.set_chat_materialization_resizing model;
  let layout =
    Chat_tui.Chat_page_layout.compute ~screen_w:width ~screen_h:height ~model
  in
  let image, cursor =
    Chat_tui.Renderer_page_chat.render_with_layout ~size ~layout ~model
  in
  let buffer = Buffer.create 1024 in
  Notty.Render.to_buffer buffer Notty.Cap.dumb (0, 0) (width, height) image;
  assert (String.is_substring (Buffer.contents buffer) ~substring:"Resizing conversation");
  [%test_eq: (int * int) option]
    (Chat_tui.App.For_testing.cursor_for_frame ~model cursor)
    None;
  Model.set_chat_materialization_corridor model;
  [%test_eq: (int * int) option]
    (Chat_tui.App.For_testing.cursor_for_frame ~model (3, 4))
    (Some (3, 4))
;;

let%test_unit "production runtime renders off-screen startup messages on worker domains" =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let size = 52, 16 in
  let messages =
    List.init 40 ~f:(fun idx ->
      ( "assistant"
      , sprintf "```ocaml\nlet domain_value = %d\n```\nRendered off-screen." idx ))
  in
  let expected_model = make_model messages in
  let worker_model = make_model messages in
  let expected_image, expected_cursor =
    Chat_tui.Renderer.render_full ~size ~model:expected_model
  in
  let results = Eio.Stream.create 16 in
  let worker =
    Worker.For_testing.create
      ~sw
      ~domain_mgr:(Eio.Stdenv.domain_mgr env)
      ~config
      ~worker_count:2
      ~queue_capacity:64
      ~render:(fun () -> Worker.For_testing.runtime_renderer ~code_cache_capacity:16)
      ~on_result:(Eio.Stream.add results)
      ~on_error:(fun _ error -> raise error)
      ~now:(fun () -> Eio.Time.Mono.now (Eio.Stdenv.mono_clock env))
      ()
  in
  ignore
    (Chat_tui.Renderer.render_full ~size ~model:worker_model : Notty.I.t * (int * int));
  let jobs =
    Chat_tui.Renderer_page_chat.startup_background_jobs
      ~theme_generation:(Worker.theme_generation worker)
      ~grammar_generation:(Worker.grammar_generation worker)
      ~model:worker_model
  in
  assert (not (List.is_empty jobs));
  let submitted =
    List.filter_map jobs ~f:(fun job ->
      match Worker.submit worker job with
      | Worker.Queued -> Some job
      | Already_pending | Rejected -> None)
  in
  assert (not (List.is_empty submitted));
  List.iter submitted ~f:(fun _ ->
    let result = Eio.Stream.take results in
    assert (
      Chat_tui.App_reducer.For_testing.commit_startup_results
        ~model:worker_model
        [ result ]));
  let actual_image, actual_cursor =
    Chat_tui.Renderer.render_full ~size ~model:worker_model
  in
  let width, height = size in
  [%test_eq: string]
    (image_to_string ~width ~height expected_image)
    (image_to_string ~width ~height actual_image);
  [%test_eq: int * int] expected_cursor actual_cursor;
  Worker.close worker
;;

let%test_unit "startup publication matches an exact synchronous viewport" =
  let size = 48, 16 in
  let messages =
    List.init 50 ~f:(fun index ->
      if index mod 7 = 0
      then
        ( "assistant"
        , sprintf
            "long row %d\n\n```ocaml\nlet value = %d\nlet doubled = value * 2\n```"
            index
            index )
      else "assistant", sprintf "short row %d" index)
  in
  let model = make_model messages in
  let expected_image, expected_cursor = Chat_tui.Renderer.render_full ~size ~model in
  Model.set_chat_materialization_loading model;
  let worker =
    Worker.For_testing.create_detached ~config ~queue_capacity:64 ~worker_count:1
  in
  let runtime = detached_runtime () in
  Chat_tui.Renderer_page_chat.startup_background_jobs
    ~theme_generation:(Worker.theme_generation worker)
    ~grammar_generation:(Worker.grammar_generation worker)
    ~model
  |> List.iter ~f:(fun job ->
    let result = Chat_tui.Renderer_component_message.render_detached ~runtime job in
    assert (Model.commit_startup_render_result model result));
  assert (Chat_tui.Renderer_page_chat.publish_startup_history ~size ~model);
  Model.set_chat_materialization_warm model;
  let warm_image, warm_cursor = Chat_tui.Renderer.render_full ~size ~model in
  let warm_scroll = Notty_scroll_box.scroll (Model.scroll_box model) in
  let width, height = size in
  [%test_eq: string]
    (image_to_string ~width ~height expected_image)
    (image_to_string ~width ~height warm_image);
  [%test_eq: int * int] expected_cursor warm_cursor;
  [%test_eq: int]
    warm_scroll
    (Chat_tui.Model.chat_max_scroll
       model
       ~viewport_height:
         (Chat_tui.Chat_page_layout.compute ~screen_w:width ~screen_h:height ~model)
           .scroll_height);
  Worker.close worker
;;
