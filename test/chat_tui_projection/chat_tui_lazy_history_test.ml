open Core
module History = Chat_tui.Renderer_component_history
module Model = Chat_tui.Model

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

let renderer calls ~idx ~selected:_ _message =
  calls := idx :: !calls;
  Notty.I.void 40 5
;;

let first_visible model ~height =
  Chat_tui.Renderer_virtual_list.Viewport.compute
    ~geometry:(Model.chat_render_geometry model)
    ~requested_scroll:(Notty_scroll_box.scroll (Model.scroll_box model))
    ~height
    ~follow_bottom:false
  |> Chat_tui.Renderer_virtual_list.Viewport.first_visible
;;

let resize_history_id sequence =
  History_entry.Id.create ~namespace:"resize-anchor" ~sequence |> Result.ok_or_failwith
;;

let resize_row sequence text =
  Chat_tui.Projected_message.canonical_row
    ~entry_id:(resize_history_id sequence)
    ("assistant", text)
;;

let install_resize_rows model rows =
  Model.reconcile_projected_messages
    model
    ~rows
    ~messages:(List.map rows ~f:(fun row -> row.Chat_tui.Projected_message.message))
;;

let%expect_test "estimated bottom-following history renders only the demanded viewport" =
  let messages = Array.init 1_000 ~f:(fun index -> "assistant", Int.to_string index) in
  let model = make_model (Array.to_list messages) in
  let calls = ref [] in
  let plan =
    History.render
      ~model
      ~width:40
      ~height:10
      ~messages
      ~selected_idx:None
      ~render_message:(renderer calls)
  in
  let rendered = List.dedup_and_sort !calls ~compare:Int.compare in
  print_s
    [%sexp
      (List.length rendered : int)
    , (List.hd rendered : int option)
    , (List.last rendered : int option)
    , (Array.length (Model.msg_heights model) : int)
    , (List.length
         (Chat_tui.Renderer_virtual_list.Geometry.estimated_indices
            (Model.chat_render_geometry model))
       : int)];
  ignore (plan.image : Notty.I.t);
  [%expect {| (2 (998) (999) 1000 998) |}]
;;

let%expect_test "directional prefetch covers three viewports ahead and one behind" =
  let messages = List.init 30 ~f:(fun index -> "assistant", Int.to_string index) in
  let model = make_model messages in
  Model.set_auto_follow model false;
  let geometry = Model.chat_render_geometry model in
  Chat_tui.Renderer_virtual_list.Geometry.initialize_estimated
    geometry
    ~length:30
    ~estimated_height_at_index:(fun _ -> 5);
  let viewport =
    Chat_tui.Renderer_virtual_list.Viewport.compute
      ~geometry
      ~requested_scroll:50
      ~height:10
      ~follow_bottom:false
  in
  let candidates direction =
    Model.set_chat_scroll_direction model direction;
    History.prefetch_candidate_indices ~model ~viewport ~height:10
  in
  print_s
    [%sexp
      (candidates Model.Toward_older : int list), (candidates Toward_newer : int list)];
  [%expect {| ((8 9 6 7 4 5 12 13) (12 13 14 15 16 17 8 9)) |}]
;;

let%expect_test "history reconciliation preserves a duplicate-message manual anchor" =
  let duplicate = "assistant", "same" in
  let old_messages =
    [ "assistant", "zero"
    ; duplicate
    ; "assistant", "middle"
    ; duplicate
    ; "assistant", "transient"
    ]
  in
  let model = make_model old_messages in
  let geometry = Model.chat_render_geometry model in
  Chat_tui.Renderer_virtual_list.Geometry.rebuild
    geometry
    ~length:5
    ~height_at_index:(Array.get [| 2; 3; 4; 5; 6 |]);
  Model.set_auto_follow model false;
  Notty_scroll_box.scroll_to (Model.scroll_box model) 10;
  Model.reconcile_messages model (List.drop_last_exn old_messages);
  print_s
    [%sexp
      (first_visible model ~height:3 : int option)
    , (Notty_scroll_box.scroll (Model.scroll_box model) : int)
    , (Model.msg_heights model : int array)
    , (Chat_tui.Renderer_virtual_list.Geometry.estimated_indices geometry : int list)];
  [%expect {| ((3) 10 (2 3 4 5) ()) |}]
;;

let%expect_test "stable resize anchors preserve manual content and bottom follow" =
  let rows =
    [ resize_row 0 "zero"; resize_row 1 "one"; resize_row 2 "two"; resize_row 3 "three" ]
  in
  let model = make_model [] in
  install_resize_rows model rows;
  let geometry = Model.chat_render_geometry model in
  Chat_tui.Renderer_virtual_list.Geometry.rebuild
    geometry
    ~length:4
    ~height_at_index:(Array.get [| 2; 3; 4; 5 |]);
  Model.set_auto_follow model false;
  Notty_scroll_box.scroll_to (Model.scroll_box model) 6;
  let manual = Model.capture_resize_anchor model ~viewport_height:3 in
  let generation = Chat_tui.Renderer_virtual_list.Geometry.generation geometry in
  ignore
    (Chat_tui.Renderer_virtual_list.Geometry.apply_exact_batch
       geometry
       ~expected_generation:generation
       ~start_index:0
       ~heights:[| 5; 6 |]
     : Chat_tui.Renderer_virtual_list.Geometry.batch_result);
  let manual_resolution = Model.restore_resize_anchor model ~viewport_height:3 manual in
  let manual_scroll = Notty_scroll_box.scroll (Model.scroll_box model) in
  Model.set_auto_follow model true;
  let bottom = Model.capture_resize_anchor model ~viewport_height:3 in
  let generation = Chat_tui.Renderer_virtual_list.Geometry.generation geometry in
  ignore
    (Chat_tui.Renderer_virtual_list.Geometry.apply_exact_batch
       geometry
       ~expected_generation:generation
       ~start_index:2
       ~heights:[| 7; 8 |]
     : Chat_tui.Renderer_virtual_list.Geometry.batch_result);
  let bottom_resolution = Model.restore_resize_anchor model ~viewport_height:3 bottom in
  print_s
    [%sexp
      (( manual_resolution
       , (manual_scroll : int)
       , bottom_resolution
       , (Model.auto_follow model : bool)
       , (Model.chat_max_scroll model ~viewport_height:3 : int) )
       : Model.Resize_anchor.resolution
         * int
         * Model.Resize_anchor.resolution
         * bool
         * int)];
  [%expect {| (Preserved 12 Followed_bottom true 23) |}]
;;

let%expect_test "stable resize anchors reject revised rows and repair older first" =
  let a = resize_row 10 "a" in
  let b = resize_row 11 "b" in
  let c = resize_row 12 "c" in
  let model = make_model [] in
  install_resize_rows model [ a; b; c ];
  let geometry = Model.chat_render_geometry model in
  Chat_tui.Renderer_virtual_list.Geometry.rebuild
    geometry
    ~length:3
    ~height_at_index:(fun _ -> 3);
  Model.set_auto_follow model false;
  Notty_scroll_box.scroll_to (Model.scroll_box model) 3;
  let anchor = Model.capture_resize_anchor model ~viewport_height:2 in
  install_resize_rows model [ a; { b with message = "assistant", "revised" }; c ];
  Chat_tui.Renderer_virtual_list.Geometry.rebuild
    geometry
    ~length:3
    ~height_at_index:(fun _ -> 3);
  let resolution = Model.restore_resize_anchor model ~viewport_height:2 anchor in
  install_resize_rows model [ a; c ];
  Chat_tui.Renderer_virtual_list.Geometry.rebuild
    geometry
    ~length:2
    ~height_at_index:(fun _ -> 3);
  let removed_resolution = Model.restore_resize_anchor model ~viewport_height:2 anchor in
  print_s
    [%sexp
      (( resolution
       , removed_resolution
       , (Notty_scroll_box.scroll (Model.scroll_box model) : int)
       , (first_visible model ~height:2 : int option)
       , (Model.auto_follow model : bool) )
       : Model.Resize_anchor.resolution
         * Model.Resize_anchor.resolution
         * int
         * int option
         * bool)];
  [%expect {| (Repaired Repaired 2 (0) false) |}]
;;

let%expect_test "history reconciliation preserves compatible prefix geometry" =
  let old_messages = [ "assistant", "zero"; "assistant", "one"; "assistant", "old" ] in
  let model = make_model old_messages in
  let geometry = Model.chat_render_geometry model in
  Chat_tui.Renderer_virtual_list.Geometry.rebuild
    geometry
    ~length:3
    ~height_at_index:(Array.get [| 2; 3; 9 |]);
  let cache idx text height =
    let id, revision = Model.render_row_identity model ~idx |> Option.value_exn in
    Model.set_img_cache
      model
      ~id
      { row_revision = revision
      ; width = 40
      ; role = "assistant"
      ; text
      ; image = Notty.I.void 40 height
      ; height
      ; layout = Chat_tui.Chat_message_render_job.Layout.{ width = 40; lines = [] }
      ; layout_plan = Chat_tui.Chat_message_render_job.Layout_plan.unknown
      }
  in
  cache 0 "zero" 2;
  cache 1 "one" 3;
  cache 2 "old" 9;
  Model.reconcile_messages
    model
    [ "assistant", "zero"; "assistant", "one"; "assistant", "new"; "assistant", "tail" ];
  let cache_present idx =
    let id, revision = Model.render_row_identity model ~idx |> Option.value_exn in
    Option.is_some (Model.find_img_cache model ~id ~revision)
  in
  print_s
    [%sexp
      (Model.msg_heights model : int array)
    , (Chat_tui.Renderer_virtual_list.Geometry.estimated_indices geometry : int list)
    , (cache_present 0 : bool)
    , (cache_present 1 : bool)
    , (cache_present 2 : bool)];
  [%expect {| ((2 3 5 5) (2 3) true true false) |}]
;;

let%expect_test "bottom-follow reconciliation remains at the new bottom" =
  let model = make_model [ "assistant", "zero"; "assistant", "one" ] in
  let geometry = Model.chat_render_geometry model in
  Chat_tui.Renderer_virtual_list.Geometry.rebuild
    geometry
    ~length:2
    ~height_at_index:(Array.get [| 4; 6 |]);
  Notty_scroll_box.scroll_to (Model.scroll_box model) 7;
  Model.reconcile_messages
    model
    [ "assistant", "zero"; "assistant", "one"; "assistant", "new" ];
  let viewport =
    Chat_tui.Renderer_virtual_list.Viewport.compute
      ~geometry
      ~requested_scroll:(Notty_scroll_box.scroll (Model.scroll_box model))
      ~height:3
      ~follow_bottom:(Model.auto_follow model)
  in
  print_s
    [%sexp
      (Chat_tui.Renderer_virtual_list.Viewport.scroll viewport : int)
    , (Chat_tui.Renderer_virtual_list.Viewport.max_scroll viewport : int)];
  [%expect {| (12 12) |}]
;;

let%expect_test "old history is measured on demand and manual position remains stable" =
  let messages = Array.init 50 ~f:(fun index -> "assistant", Int.to_string index) in
  let model = make_model (Array.to_list messages) in
  let calls = ref [] in
  ignore
    (History.render
       ~model
       ~width:40
       ~height:10
       ~messages
       ~selected_idx:None
       ~render_message:(renderer calls)
     : History.render_plan);
  Model.set_auto_follow model false;
  Notty_scroll_box.scroll_to (Model.scroll_box model) 0;
  calls := [];
  let plan =
    History.render
      ~model
      ~width:40
      ~height:10
      ~messages
      ~selected_idx:None
      ~render_message:(renderer calls)
  in
  print_s
    [%sexp
      (List.dedup_and_sort !calls ~compare:Int.compare : int list)
    , (Chat_tui.Renderer_virtual_list.Viewport.scroll plan.viewport : int)
    , (Chat_tui.Renderer_virtual_list.Viewport.estimated_visible_indices
         ~geometry:(Model.chat_render_geometry model)
         plan.viewport
       : int list)];
  [%expect {| ((0 1) 0 ()) |}]
;;

let%expect_test "first row above a cold bottom stays within a tall final message" =
  let messages = Array.init 30 ~f:(fun index -> "assistant", Int.to_string index) in
  let model = make_model (Array.to_list messages) in
  let heights = Array.init 30 ~f:(fun index -> if Int.equal index 29 then 30 else 5) in
  let render ~idx ~selected:_ _ = Notty.I.void 40 heights.(idx) in
  let commit plan =
    let scroll_box = Model.scroll_box model in
    Notty_scroll_box.set_content scroll_box plan.History.image;
    Notty_scroll_box.scroll_to
      scroll_box
      (Chat_tui.Renderer_virtual_list.Viewport.scroll plan.viewport)
  in
  let initial =
    History.render
      ~model
      ~width:40
      ~height:10
      ~messages
      ~selected_idx:None
      ~render_message:render
  in
  commit initial;
  let initial_scroll = Notty_scroll_box.scroll (Model.scroll_box model) in
  Model.set_auto_follow model false;
  Notty_scroll_box.scroll_by (Model.scroll_box model) ~height:10 (-1);
  let after =
    History.render
      ~model
      ~width:40
      ~height:10
      ~messages
      ~selected_idx:None
      ~render_message:render
  in
  commit after;
  let geometry = Model.chat_render_geometry model in
  let first =
    Chat_tui.Renderer_virtual_list.Viewport.first_visible after.viewport
    |> Option.value_exn
  in
  let first_start =
    Chat_tui.Renderer_virtual_list.Geometry.item_start geometry ~index:first
    |> Option.value_exn
  in
  let intra_row =
    Chat_tui.Renderer_virtual_list.Viewport.scroll after.viewport - first_start
  in
  print_s
    [%sexp
      (( initial_scroll
       , (Chat_tui.Renderer_virtual_list.Viewport.max_scroll initial.viewport : int)
       , (Chat_tui.Renderer_virtual_list.Viewport.scroll after.viewport : int)
       , (Notty_scroll_box.scroll (Model.scroll_box model) : int)
       , first
       , intra_row
       , (heights.(first) : int) )
       : int * int * int * int * int * int * int)];
  [%expect {| (165 165 164 164 29 19 30) |}]
;;

let%expect_test "first rows into an estimated tall predecessor keep end distance" =
  let messages = Array.init 30 ~f:(fun index -> "assistant", Int.to_string index) in
  let run delta =
    let model = make_model (Array.to_list messages) in
    let heights =
      Array.init 30 ~f:(fun index ->
        if index = 19 then 30 else if index >= 20 then 1 else 5)
    in
    let render ~idx ~selected:_ _ = Notty.I.void 40 heights.(idx) in
    let commit plan =
      let scroll_box = Model.scroll_box model in
      Notty_scroll_box.set_content scroll_box plan.History.image;
      Notty_scroll_box.scroll_to
        scroll_box
        (Chat_tui.Renderer_virtual_list.Viewport.scroll plan.viewport)
    in
    let initial =
      History.render
        ~model
        ~width:40
        ~height:10
        ~messages
        ~selected_idx:None
        ~render_message:render
    in
    commit initial;
    let geometry = Model.chat_render_geometry model in
    let was_estimated =
      not (Chat_tui.Renderer_virtual_list.Geometry.is_exact geometry ~index:19)
    in
    Model.set_auto_follow model false;
    Notty_scroll_box.scroll_by (Model.scroll_box model) ~height:10 delta;
    let after =
      History.render
        ~model
        ~width:40
        ~height:10
        ~messages
        ~selected_idx:None
        ~render_message:render
    in
    commit after;
    let first =
      Chat_tui.Renderer_virtual_list.Viewport.first_visible after.viewport
      |> Option.value_exn
    in
    let first_start =
      Chat_tui.Renderer_virtual_list.Geometry.item_start geometry ~index:first
      |> Option.value_exn
    in
    ( was_estimated
    , first
    , Chat_tui.Renderer_virtual_list.Viewport.scroll after.viewport - first_start
    , Notty_scroll_box.scroll (Model.scroll_box model) )
  in
  print_s
    [%sexp ((run (-1), run (-3)) : (bool * int * int * int) * (bool * int * int * int))];
  [%expect {| ((true 19 29 124) (true 19 27 122)) |}]
;;

let%expect_test "search reveal measures and centers an estimated target" =
  let messages = Array.init 100 ~f:(fun index -> "assistant", Int.to_string index) in
  let model = make_model (Array.to_list messages) in
  let calls = ref [] in
  ignore
    (History.render
       ~model
       ~width:40
       ~height:10
       ~messages
       ~selected_idx:None
       ~render_message:(renderer calls)
     : History.render_plan);
  Model.set_auto_follow model false;
  let reveal_id = Model.render_row_identity model ~idx:10 |> Option.value_exn |> fst in
  Model.request_projected_reveal model ~id:reveal_id;
  calls := [];
  let plan =
    History.render
      ~model
      ~width:40
      ~height:10
      ~messages
      ~selected_idx:(Some 10)
      ~render_message:(renderer calls)
  in
  print_s
    [%sexp
      (List.mem !calls 10 ~equal:Int.equal : bool)
    , (Chat_tui.Renderer_virtual_list.Geometry.is_exact
         (Model.chat_render_geometry model)
         ~index:10
       : bool)
    , (Chat_tui.Renderer_virtual_list.Viewport.visible_indices plan.viewport : int list)];
  [%expect {| (true true (9 10)) |}]
;;

let%expect_test "dirty correction above a manual viewport preserves its anchor" =
  let messages = Array.init 20 ~f:(fun index -> "assistant", Int.to_string index) in
  let model = make_model (Array.to_list messages) in
  let heights = Array.create ~len:20 5 in
  let render ~idx ~selected:_ _ = Notty.I.void 40 heights.(idx) in
  ignore
    (History.render
       ~model
       ~width:40
       ~height:10
       ~messages
       ~selected_idx:None
       ~render_message:render
     : History.render_plan);
  Model.set_auto_follow model false;
  Notty_scroll_box.scroll_to (Model.scroll_box model) 70;
  let before =
    History.render
      ~model
      ~width:40
      ~height:10
      ~messages
      ~selected_idx:None
      ~render_message:render
  in
  heights.(0) <- 10;
  Model.invalidate_img_cache_index model ~idx:0;
  let after =
    History.render
      ~model
      ~width:40
      ~height:10
      ~messages
      ~selected_idx:None
      ~render_message:render
  in
  print_s
    [%sexp
      (Chat_tui.Renderer_virtual_list.Viewport.first_visible before.viewport : int option)
    , (Chat_tui.Renderer_virtual_list.Viewport.first_visible after.viewport : int option)
    , (Chat_tui.Renderer_virtual_list.Viewport.scroll before.viewport : int)
    , (Chat_tui.Renderer_virtual_list.Viewport.scroll after.viewport : int)];
  [%expect {| ((14) (14) 70 75) |}]
;;

let%expect_test "presentation invalidation preserves manual geometry lazily" =
  let messages = Array.init 40 ~f:(fun index -> "assistant", Int.to_string index) in
  let model = make_model (Array.to_list messages) in
  let heights = Array.init 40 ~f:(fun index -> 2 + (index mod 4)) in
  let calls = ref [] in
  let render ~idx ~selected:_ _ =
    calls := idx :: !calls;
    Notty.I.void 40 heights.(idx)
  in
  ignore
    (History.render
       ~model
       ~width:40
       ~height:10
       ~messages
       ~selected_idx:None
       ~render_message:render
     : History.render_plan);
  Model.set_auto_follow model false;
  Notty_scroll_box.scroll_to (Model.scroll_box model) 130;
  let before =
    History.render
      ~model
      ~width:40
      ~height:10
      ~messages
      ~selected_idx:None
      ~render_message:render
  in
  let before_heights = Array.copy (Model.msg_heights model) in
  let before_prefix = Array.copy (Model.height_prefix model) in
  calls := [];
  Model.clear_img_caches_preserving_heights model;
  let provisional =
    Chat_tui.Renderer_virtual_list.Geometry.estimated_indices
      (Model.chat_render_geometry model)
  in
  let after =
    History.render
      ~model
      ~width:40
      ~height:10
      ~messages
      ~selected_idx:None
      ~render_message:render
  in
  print_s
    [%sexp
      (Array.equal Int.equal before_heights (Model.msg_heights model) : bool)
    , (Array.equal Int.equal before_prefix (Model.height_prefix model) : bool)
    , (List.length provisional : int)
    , (List.dedup_and_sort !calls ~compare:Int.compare : int list)
    , (Chat_tui.Renderer_virtual_list.Viewport.first_visible before.viewport : int option)
    , (Chat_tui.Renderer_virtual_list.Viewport.first_visible after.viewport : int option)
    , (Chat_tui.Renderer_virtual_list.Viewport.scroll before.viewport : int)
    , (Chat_tui.Renderer_virtual_list.Viewport.scroll after.viewport : int)];
  [%expect {| (true true 40 (26 27 28) (26) (26) 130 130) |}]
;;

let%expect_test "first scroll survives presentation invalidation" =
  let messages = Array.init 30 ~f:(fun index -> "assistant", Int.to_string index) in
  let model = make_model (Array.to_list messages) in
  let heights =
    Array.init 30 ~f:(fun index ->
      if index = 19 then 30 else if index >= 20 then 1 else 5)
  in
  let render ~idx ~selected:_ _ = Notty.I.void 40 heights.(idx) in
  let commit plan =
    let scroll_box = Model.scroll_box model in
    Notty_scroll_box.set_content scroll_box plan.History.image;
    Notty_scroll_box.scroll_to
      scroll_box
      (Chat_tui.Renderer_virtual_list.Viewport.scroll plan.viewport)
  in
  let initial =
    History.render
      ~model
      ~width:40
      ~height:10
      ~messages
      ~selected_idx:None
      ~render_message:render
  in
  commit initial;
  Model.set_auto_follow model false;
  Notty_scroll_box.scroll_by (Model.scroll_box model) ~height:10 (-1);
  Model.clear_img_caches_preserving_heights model;
  let after =
    History.render
      ~model
      ~width:40
      ~height:10
      ~messages
      ~selected_idx:None
      ~render_message:render
  in
  commit after;
  let geometry = Model.chat_render_geometry model in
  let first =
    Chat_tui.Renderer_virtual_list.Viewport.first_visible after.viewport
    |> Option.value_exn
  in
  let first_start =
    Chat_tui.Renderer_virtual_list.Geometry.item_start geometry ~index:first
    |> Option.value_exn
  in
  print_s
    [%sexp
      (first : int)
    , (Chat_tui.Renderer_virtual_list.Viewport.scroll after.viewport - first_start : int)
    , (Notty_scroll_box.scroll (Model.scroll_box model) : int)];
  [%expect {| (19 29 124) |}]
;;

let%expect_test "manual anchor survives width-style global invalidation" =
  let messages = Array.init 40 ~f:(fun index -> "assistant", Int.to_string index) in
  let model = make_model (Array.to_list messages) in
  let heights = Array.init 40 ~f:(fun index -> 2 + (index mod 5)) in
  let render ~idx ~selected:_ _ = Notty.I.void 40 heights.(idx) in
  ignore
    (History.render
       ~model
       ~width:159
       ~height:10
       ~messages
       ~selected_idx:None
       ~render_message:render
     : History.render_plan);
  Model.set_auto_follow model false;
  Notty_scroll_box.scroll_to (Model.scroll_box model) 130;
  let before =
    History.render
      ~model
      ~width:159
      ~height:10
      ~messages
      ~selected_idx:None
      ~render_message:render
  in
  let geometry = Model.chat_render_geometry model in
  let anchor =
    Chat_tui.Renderer_virtual_list.Anchor.create
      ~geometry
      ~viewport:before.viewport
      ~screen_row:0
    |> Option.value_exn
  in
  let before_first =
    Chat_tui.Renderer_virtual_list.Viewport.first_visible before.viewport
  in
  Model.clear_img_caches_preserving_heights model;
  let after =
    History.render_with_anchor
      ~initial_anchor:anchor
      ~model
      ~width:161
      ~height:10
      ~messages
      ~selected_idx:None
      ~render_message:render
  in
  print_s
    [%sexp
      (before_first : int option)
    , (Chat_tui.Renderer_virtual_list.Viewport.first_visible after.viewport : int option)
    , (Chat_tui.Renderer_virtual_list.Viewport.scroll before.viewport : int)
    , (Chat_tui.Renderer_virtual_list.Viewport.scroll after.viewport : int)];
  [%expect {| ((26) (26) 130 130) |}]
;;

let%expect_test "width-style anchor clamps when its message becomes shorter" =
  let messages = [| "assistant", "message" |] in
  let model = make_model (Array.to_list messages) in
  let height = ref 30 in
  let render ~idx:_ ~selected:_ _ = Notty.I.void 40 !height in
  ignore
    (History.render
       ~model
       ~width:159
       ~height:10
       ~messages
       ~selected_idx:None
       ~render_message:render
     : History.render_plan);
  Model.set_auto_follow model false;
  Notty_scroll_box.scroll_to (Model.scroll_box model) 20;
  let before =
    History.render
      ~model
      ~width:159
      ~height:10
      ~messages
      ~selected_idx:None
      ~render_message:render
  in
  let anchor =
    Chat_tui.Renderer_virtual_list.Anchor.create
      ~geometry:(Model.chat_render_geometry model)
      ~viewport:before.viewport
      ~screen_row:0
    |> Option.value_exn
  in
  height := 8;
  Model.clear_img_caches_preserving_heights model;
  let after =
    History.render_with_anchor
      ~initial_anchor:anchor
      ~model
      ~width:161
      ~height:10
      ~messages
      ~selected_idx:None
      ~render_message:render
  in
  print_s
    [%sexp
      (Chat_tui.Renderer_virtual_list.Viewport.scroll after.viewport : int)
    , (Model.msg_heights model : int array)];
  [%expect {| (0 (8)) |}]
;;

let%expect_test "bottom following remains at bottom across width-style invalidation" =
  let messages = Array.init 20 ~f:(fun index -> "assistant", Int.to_string index) in
  let model = make_model (Array.to_list messages) in
  let height = ref 5 in
  let render ~idx:_ ~selected:_ _ = Notty.I.void 40 !height in
  ignore
    (History.render
       ~model
       ~width:159
       ~height:10
       ~messages
       ~selected_idx:None
       ~render_message:render
     : History.render_plan);
  height := 7;
  Model.clear_img_caches_preserving_heights model;
  let after =
    History.render
      ~model
      ~width:161
      ~height:10
      ~messages
      ~selected_idx:None
      ~render_message:render
  in
  print_s
    [%sexp
      (Chat_tui.Renderer_virtual_list.Viewport.scroll after.viewport : int)
    , (Chat_tui.Renderer_virtual_list.Viewport.max_scroll after.viewport : int)
    , (Model.auto_follow model : bool)];
  [%expect {| (94 94 true) |}]
;;

let%expect_test "late selection overlay preserves base geometry" =
  let messages = [| "assistant", "selected" |] in
  let model = make_model (Array.to_list messages) in
  let plan =
    History.render
      ~model
      ~width:40
      ~height:10
      ~messages
      ~selected_idx:(Some 0)
      ~render_message:(fun ~idx:_ ~selected _ ->
        Notty.I.void 40 (if selected then 8 else 5))
  in
  print_s
    [%sexp
      (Model.msg_heights model : int array)
    , (Notty.I.height plan.image : int)
    , (Chat_tui.Renderer_virtual_list.Geometry.total_height
         (Model.chat_render_geometry model)
       : int)];
  [%expect {| ((5) 5 5) |}]
;;

let history_id sequence =
  History_entry.Id.create ~namespace:"projected-test" ~sequence |> Result.ok_or_failwith
;;

let projected sequence text =
  Chat_tui.Projected_message.canonical_row
    ~entry_id:(history_id sequence)
    ("assistant", text)
;;

let%expect_test "projected rows reconcile by identity across duplicate text and reorder" =
  let model = make_model [] in
  let a = projected 0 "same" in
  let b = projected 1 "same" in
  let c = projected 2 "other" in
  Model.reconcile_projected_rows model [ a; b; c ];
  Model.select_projected model (Some b.id);
  Model.request_projected_reveal model ~id:b.id;
  Model.set_projected_height model ~id:a.id ~height:2;
  Model.set_projected_height model ~id:b.id ~height:7;
  Model.set_projected_height model ~id:c.id ~height:4;
  Model.reconcile_projected_rows model [ c; b; a ];
  print_s
    [%sexp
      (( Model.projected_index model ~id:a.id
       , Model.projected_index model ~id:b.id
       , Model.projected_index model ~id:c.id
       , Model.projected_height model ~id:a.id
       , Model.projected_height model ~id:b.id
       , Model.projected_height model ~id:c.id
       , Option.map
           (Model.selected_projected_id model)
           ~f:Chat_tui.Projected_message.Id.to_string
       , Option.map
           (Model.take_projected_reveal_request model)
           ~f:Chat_tui.Projected_message.Id.to_string )
       : int option
         * int option
         * int option
         * int option
         * int option
         * int option
         * string option
         * string option)];
  [%expect {| ((2) (1) (0) (2) (7) (4) (14:projected-test:1) (14:projected-test:1)) |}]
;;

let%expect_test
    "same projected ID changes revision while equal text with another ID stays distinct"
  =
  let model = make_model [] in
  let first = projected 0 "same" in
  let other = projected 1 "same" in
  Model.reconcile_projected_rows model [ first; other ];
  let changed = { first with message = "assistant", "changed" } in
  Model.reconcile_projected_rows model [ other; changed ];
  let rows = Model.projected_rows model in
  print_s
    [%sexp
      (Array.map rows ~f:(fun row ->
         Chat_tui.Projected_message.Id.to_string row.id, row.revision)
       : (string * int) array)];
  [%expect {| ((14:projected-test:1 0) (14:projected-test:0 1)) |}]
;;

let cache_entry revision text height =
  { Model.row_revision = revision
  ; width = 40
  ; role = "assistant"
  ; text
  ; image = Notty.I.void 40 height
  ; height
  ; layout = Chat_tui.Chat_message_render_job.Layout.{ width = 40; lines = [] }
  ; layout_plan = Chat_tui.Chat_message_render_job.Layout_plan.unknown
  }
;;

let%expect_test "projected image caches follow IDs and reject changed revisions" =
  let model = make_model [] in
  let a = projected 0 "same" in
  let b = projected 1 "same" in
  Model.reconcile_projected_rows model [ a; b ];
  Model.set_img_cache model ~id:a.id (cache_entry a.revision "same" 2);
  Model.set_img_cache model ~id:b.id (cache_entry b.revision "same" 7);
  Model.reconcile_projected_rows model [ b; a ];
  let height id revision =
    Model.find_img_cache model ~id ~revision |> Option.map ~f:(fun cache -> cache.height)
  in
  print_s
    [%sexp ((height a.id a.revision, height b.id b.revision) : int option * int option)];
  let changed = { a with message = "assistant", "changed" } in
  Model.reconcile_projected_rows model [ b; changed ];
  let changed = Model.projected_row model ~id:a.id |> Option.value_exn in
  print_s
    [%sexp
      (( height a.id a.revision
       , height a.id changed.revision
       , Model.take_and_clear_dirty_height_rows model
         |> List.map ~f:(fun (id, revision) ->
           Chat_tui.Projected_message.Id.to_string id, revision) )
       : int option * int option * (string * int) list)];
  [%expect
    {|
    ((2) (7))
    (() ()
     ((14:projected-test:0 1) (14:projected-test:1 0) (14:projected-test:0 0)))
    |}]
;;

let%expect_test "detached render result follows a moved projected row" =
  let module Job = Chat_tui.Chat_message_render_job in
  let model = make_model [] in
  let a = projected 0 "a" in
  let b = projected 1 "b" in
  Model.reconcile_projected_rows model [ a; b ];
  Model.reconcile_messages model [ a.message; b.message ];
  Model.set_active_history_width model (Some 40);
  let geometry = Model.chat_render_geometry model in
  Chat_tui.Renderer_virtual_list.Geometry.rebuild
    geometry
    ~length:2
    ~height_at_index:(fun _ -> 5);
  let generation = Chat_tui.Renderer_virtual_list.Geometry.generation geometry in
  let job =
    Job.create
      ~transcript_generation:(Model.transcript_generation model)
      ~row_id:b.id
      ~row_revision:b.revision
      ~message_index:1
      ~message_revision:0
      ~width:40
      ~role:"assistant"
      ~text:"b"
      ~tool_output:None
      ~tool_call_outcome:None
      ~theme_generation:0
      ~grammar_generation:0
      ~geometry_generation:generation
      ~request_generation:0
      ~render_generation:(Model.render_generation model)
      ~submission_generation:0
      ~semantic_seed:None
      ~priority:Background
  in
  Model.reconcile_projected_rows model [ b; a ];
  Model.reconcile_messages model [ b.message; a.message ];
  let accepted =
    let layout = Job.Layout.{ width = 40; lines = [] } in
    Model.commit_render_result model (Job.result job ~layout ~image:(Notty.I.void 40 9))
  in
  let cached =
    Model.find_img_cache model ~id:b.id ~revision:b.revision
    |> Option.map ~f:(fun cache -> cache.height)
  in
  print_s
    [%sexp
      ((accepted, Model.projected_index model ~id:b.id, cached)
       : bool * int option * int option)];
  [%expect {| (true (0) (9)) |}]
;;

let worker_job ~row_id ~row_revision ~index =
  Chat_tui.Chat_message_render_job.create
    ~transcript_generation:1
    ~row_id
    ~row_revision
    ~message_index:index
    ~message_revision:0
    ~width:40
    ~role:"assistant"
    ~text:"row"
    ~tool_output:None
    ~tool_call_outcome:None
    ~theme_generation:0
    ~grammar_generation:0
    ~geometry_generation:0
    ~request_generation:0
    ~render_generation:0
    ~submission_generation:0
    ~semantic_seed:None
    ~priority:Visible
;;

let%expect_test "detached worker slots use row identity and revision" =
  let module Worker = Chat_tui.Chat_render_worker in
  let config =
    Chat_tui.Chat_render_worker_runtime.Config.create
      ~custom_grammars:[]
      ~theme_generation:0
      ~grammar_generation:0
  in
  let worker =
    Worker.For_testing.create_detached ~config ~queue_capacity:4 ~worker_count:1
  in
  let row_id = (projected 0 "row").id in
  let other_id = (projected 1 "row").id in
  let submit row_id row_revision index =
    worker_job ~row_id ~row_revision ~index |> Worker.submit worker
  in
  let show = function
    | Worker.Queued -> "queued"
    | Already_pending -> "already-pending"
    | Rejected -> "rejected"
  in
  let first = submit row_id 0 1 |> show in
  let moved = submit row_id 0 3 |> show in
  let other = submit other_id 0 3 |> show in
  let revised = submit row_id 1 3 |> show in
  let queued = (Worker.For_testing.stats worker).queued in
  print_s
    [%sexp
      ((first, moved, other, revised, queued) : string * string * string * string * int)];
  [%expect {| (queued already-pending queued queued 2) |}]
;;

let%expect_test "recent width snapshot restores exact images and geometry" =
  let model = make_model [ "assistant", "zero"; "assistant", "one" ] in
  Model.set_active_history_width model (Some 40);
  let geometry = Model.chat_render_geometry model in
  Chat_tui.Renderer_virtual_list.Geometry.rebuild
    geometry
    ~length:2
    ~height_at_index:(Array.get [| 3; 7 |]);
  Array.iteri (Model.render_messages model) ~f:(fun idx (role, text) ->
    let id, row_revision = Model.render_row_identity model ~idx |> Option.value_exn in
    Model.set_img_cache
      model
      ~id
      { row_revision
      ; width = 40
      ; role
      ; text
      ; image = Notty.I.void 40 (if Int.equal idx 0 then 3 else 7)
      ; height = (if Int.equal idx 0 then 3 else 7)
      ; layout = Chat_tui.Chat_message_render_job.Layout.{ width = 40; lines = [] }
      ; layout_plan = Chat_tui.Chat_message_render_job.Layout_plan.unknown
      });
  Model.set_history_image_cache
    model
    (Some
       { Model.Chat_page_state.width = 40
       ; transcript_generation = Model.transcript_generation model
       ; render_generation = Model.render_generation model
       ; chunks = [||]
       ; chunk_tree = [| Notty.I.empty; Notty.I.void 40 10 |]
       ; chunk_tree_base = 1
       ; image = Notty.I.void 40 10
       });
  Model.remember_current_width model;
  Model.clear_all_img_caches model;
  Model.set_active_history_width model (Some 60);
  let restored = Model.restore_width model ~width:40 in
  print_s
    [%sexp
      (restored : bool)
    , (Model.active_history_width model : int option)
    , (Model.msg_heights model : int array)
    , (Chat_tui.Renderer_virtual_list.Geometry.all_exact geometry : bool)
    , (Option.is_some (Model.history_image_cache model) : bool)];
  [%expect {| (true (40) (3 7) true true) |}]
;;

let%expect_test "recent width snapshots evict the least recently used width" =
  let model = make_model [ "assistant", "row" ] in
  let id, row_revision = Model.render_row_identity model ~idx:0 |> Option.value_exn in
  let remember width =
    Model.set_active_history_width model (Some width);
    Chat_tui.Renderer_virtual_list.Geometry.rebuild
      (Model.chat_render_geometry model)
      ~length:1
      ~height_at_index:(Fn.const 1);
    Model.set_img_cache
      model
      ~id
      { Model.row_revision
      ; width
      ; role = "assistant"
      ; text = "row"
      ; image = Notty.I.void width 1
      ; height = 1
      ; layout = Chat_tui.Chat_message_render_job.Layout.{ width; lines = [] }
      ; layout_plan = Chat_tui.Chat_message_render_job.Layout_plan.unknown
      };
    Model.set_history_image_cache
      model
      (Some
         { Model.Chat_page_state.width
         ; transcript_generation = Model.transcript_generation model
         ; render_generation = Model.render_generation model
         ; chunks = [||]
         ; chunk_tree = [| Notty.I.empty; Notty.I.void width 1 |]
         ; chunk_tree_base = 1
         ; image = Notty.I.void width 1
         });
    Model.remember_current_width model
  in
  List.iter [ 40; 50; 60 ] ~f:remember;
  assert (Model.restore_width model ~width:40);
  remember 70;
  print_s
    [%sexp
      (Model.restore_width model ~width:50 : bool)
    , (Model.restore_width model ~width:40 : bool)
    , (Model.restore_width model ~width:60 : bool)
    , (Model.restore_width model ~width:70 : bool)];
  [%expect {| (false true true true) |}]
;;

let%expect_test "width preparation is isolated from active exact rendering" =
  let model = make_model [ "assistant", "row" ] in
  Model.set_active_history_width model (Some 40);
  let geometry = Model.chat_render_geometry model in
  Chat_tui.Renderer_virtual_list.Geometry.rebuild
    geometry
    ~length:1
    ~height_at_index:(Fn.const 3);
  let id, row_revision = Model.render_row_identity model ~idx:0 |> Option.value_exn in
  let active_entry =
    { Model.row_revision
    ; width = 40
    ; role = "assistant"
    ; text = "row"
    ; image = Notty.I.void 40 3
    ; height = 3
    ; layout = Chat_tui.Chat_message_render_job.Layout.{ width = 40; lines = [] }
    ; layout_plan = Chat_tui.Chat_message_render_job.Layout_plan.unknown
    }
  in
  Model.set_img_cache model ~id active_entry;
  let history_cache =
    { Model.Chat_page_state.width = 40
    ; transcript_generation = Model.transcript_generation model
    ; render_generation = Model.render_generation model
    ; chunks = [||]
    ; chunk_tree = [| Notty.I.empty; Notty.I.void 40 3 |]
    ; chunk_tree_base = 1
    ; image = Notty.I.void 40 3
    }
  in
  Model.set_history_image_cache model (Some history_cache);
  let anchor = Model.capture_resize_anchor model ~viewport_height:3 in
  Model.start_width_preparation
    model
    ~request_generation:7
    ~terminal_size:(60, 24)
    ~layout:
      { Model.Chat_page_state.input_box_height = 3
      ; history_height = 20
      ; sticky_height = 0
      ; scroll_height = 20
      }
    ~theme_generation:2
    ~grammar_generation:3
    ~anchor;
  let target_entry =
    { active_entry with
      width = 60
    ; image = Notty.I.void 60 4
    ; height = 4
    ; layout = { active_entry.layout with width = 60 }
    }
  in
  let stored =
    Model.set_width_preparation_row model ~request_generation:7 ~id target_entry
  in
  let wrong_width =
    Model.set_width_preparation_row
      model
      ~request_generation:7
      ~id
      { target_entry with width = 59 }
  in
  let preparation = Model.width_preparation model |> Option.value_exn in
  let active_unchanged =
    Option.value_exn (Model.find_img_cache model ~id ~revision:row_revision)
    |> fun (entry : Model.msg_img_cache) ->
    Int.equal entry.width 40 && Int.equal entry.height 3
  in
  print_s
    [%sexp
      (Model.active_history_width model : int option)
    , (Model.msg_heights model : int array)
    , (Option.is_some (Model.history_image_cache model) : bool)
    , (stored : bool)
    , (wrong_width : bool)
    , (active_unchanged : bool)
    , (Model.width_preparation_target_width preparation : int)
    , (Model.width_preparation_row_count preparation : int)
    , (Model.width_preparation_highlight_generations preparation : int * int)
    , (Option.map
         (Model.find_width_preparation_row model ~request_generation:7 ~id)
         ~f:(fun (entry : Model.msg_img_cache) -> entry.width, entry.height)
       : (int * int) option)];
  [%expect {| ((40) (3) true true false true 60 1 (2 3) ((60 4))) |}]
;;

let%expect_test "cancelling isolates preparations and transcript changes retain row work" =
  let model = make_model [ "assistant", "row" ] in
  Model.set_active_history_width model (Some 40);
  let anchor = Model.capture_resize_anchor model ~viewport_height:3 in
  let start generation =
    Model.start_width_preparation
      model
      ~request_generation:generation
      ~terminal_size:(60, 24)
      ~layout:
        { Model.Chat_page_state.input_box_height = 3
        ; history_height = 20
        ; sticky_height = 0
        ; scroll_height = 20
        }
      ~theme_generation:0
      ~grammar_generation:0
      ~anchor
  in
  start 1;
  let first =
    Model.width_preparation model
    |> Option.value_exn
    |> Model.width_preparation_request_generation
  in
  start 2;
  let stale_row_rejected =
    let id, row_revision = Model.render_row_identity model ~idx:0 |> Option.value_exn in
    Model.set_width_preparation_row
      model
      ~request_generation:1
      ~id
      { Model.row_revision
      ; width = 60
      ; role = "assistant"
      ; text = "row"
      ; image = Notty.I.void 60 1
      ; height = 1
      ; layout = Chat_tui.Chat_message_render_job.Layout.{ width = 60; lines = [] }
      ; layout_plan = Chat_tui.Chat_message_render_job.Layout_plan.unknown
      }
  in
  let wrong_cancel = Model.cancel_width_preparation model ~request_generation:0 in
  let cancelled = Model.cancel_width_preparation model ~request_generation:2 in
  let active_after_cancel = Model.active_history_width model in
  start 2;
  Model.set_messages model (Model.messages model);
  let stale_cleared = Option.is_none (Model.width_preparation model) in
  print_s
    [%sexp
      (wrong_cancel : bool)
    , (first : int)
    , (stale_row_rejected : bool)
    , (cancelled : bool)
    , (active_after_cancel : int option)
    , (stale_cleared : bool)];
  [%expect {| (false 1 false true (40) false) |}]
;;

let%expect_test "row revision invalidates only its prepared target entry" =
  let model = make_model [ "assistant", "a"; "assistant", "b" ] in
  let anchor = Model.capture_resize_anchor model ~viewport_height:3 in
  Model.start_width_preparation
    model
    ~request_generation:12
    ~terminal_size:(60, 24)
    ~layout:
      { Model.Chat_page_state.input_box_height = 3
      ; history_height = 20
      ; sticky_height = 0
      ; scroll_height = 3
      }
    ~theme_generation:0
    ~grammar_generation:0
    ~anchor;
  let store index =
    let id, row_revision =
      Model.render_row_identity model ~idx:index |> Option.value_exn
    in
    let role, text = (Model.render_messages model).(index) in
    let entry =
      { Model.row_revision
      ; width = 60
      ; role
      ; text
      ; image = Notty.I.void 60 1
      ; height = 1
      ; layout = Chat_tui.Chat_message_render_job.Layout.{ width = 60; lines = [] }
      ; layout_plan = Chat_tui.Chat_message_render_job.Layout_plan.unknown
      }
    in
    assert (Model.set_width_preparation_row model ~request_generation:12 ~id entry);
    id
  in
  let a = store 0 in
  let b = store 1 in
  Model.invalidate_width_preparation_row model ~id:b;
  let preparation = Model.width_preparation model |> Option.value_exn in
  print_s
    [%sexp
      (Model.width_preparation_row_count preparation : int)
    , (Option.is_some
         (Model.find_width_preparation_row model ~request_generation:12 ~id:a)
       : bool)
    , (Option.is_some
         (Model.find_width_preparation_row model ~request_generation:12 ~id:b)
       : bool)];
  [%expect {| (1 true false) |}]
;;

let%expect_test "exact prefix classifies below rows with partial geometry" =
  let model =
    make_model
      [ "assistant", "a"
      ; "assistant", "b"
      ; "assistant", "c"
      ; "assistant", "d"
      ; "assistant", "e"
      ]
  in
  Model.set_auto_follow model false;
  let geometry = Model.chat_render_geometry model in
  Chat_tui.Renderer_virtual_list.Geometry.initialize_estimated
    geometry
    ~length:5
    ~estimated_height_at_index:(Fn.const 2);
  List.iter [ 0; 1; 2 ] ~f:(fun index ->
    Chat_tui.Renderer_virtual_list.Geometry.mark_exact geometry ~index ~height:2);
  let exact_prefix =
    Chat_tui.Renderer_virtual_list.Geometry.exact_prefix_length geometry
  in
  let relation = function
    | Model.Above -> "above"
    | Visible -> "visible"
    | Below -> "below"
    | Unknown -> "unknown"
  in
  print_s
    [%sexp
      (Chat_tui.Renderer_virtual_list.Geometry.all_exact geometry : bool)
    , (exact_prefix : int)
    , (relation (Model.relation_at_index model ~viewport_height:4 ~index:3) : string)
    , (relation (Model.relation_at_index model ~viewport_height:4 ~index:4) : string)];
  [%expect {| (false 3 below unknown) |}]
;;

let%expect_test "history chunks and render batches preserve exact boundaries" =
  let module Chunk = Chat_tui.History_chunk in
  let summarize row_count =
    let full_range = Chunk.Range.create_exn ~first:0 ~past:row_count in
    let canonical_ranges =
      Chunk.canonical_indices_intersecting ~row_count full_range
      |> List.filter_map ~f:(fun chunk_index ->
        Chunk.canonical_range ~row_count ~chunk_index)
    in
    ( row_count
    , Chunk.foreground_batch_count ~row_count
    , Chunk.canonical_count ~row_count
    , canonical_ranges )
  in
  List.map [ 0; 1; 15; 16; 63; 64; 65 ] ~f:summarize
  |> List.iter ~f:(fun summary ->
    print_s [%sexp (summary : int * int * int * Chunk.Range.t list)]);
  [%expect
    {|
    (0 0 0 ())
    (1 1 1 (((first 0) (past 1))))
    (15 1 1 (((first 0) (past 15))))
    (16 1 1 (((first 0) (past 16))))
    (63 4 1 (((first 0) (past 63))))
    (64 4 1 (((first 0) (past 64))))
    (65 5 2 (((first 0) (past 64)) ((first 64) (past 65))))
    |}]
;;

let%expect_test "history ranges expand at 16- and 64-row boundaries" =
  let module Chunk = Chat_tui.History_chunk in
  let show range =
    print_s
      [%sexp
        (Chunk.foreground_batch_indices_intersecting ~row_count:65 range : int list)
      , (Chunk.canonical_indices_intersecting ~row_count:65 range : int list)]
  in
  show (Chunk.Range.create_exn ~first:0 ~past:0);
  show (Chunk.Range.create_exn ~first:0 ~past:16);
  show (Chunk.Range.create_exn ~first:15 ~past:17);
  show (Chunk.Range.create_exn ~first:63 ~past:65);
  [%expect
    {|
    (() ())
    ((0) (0))
    ((0 1) (0))
    ((3 4) (0 1))
    |}]
;;

let%expect_test "history range distance ordering is deterministic" =
  let module Chunk = Chat_tui.History_chunk in
  let range first past = Chunk.Range.create_exn ~first ~past in
  let viewport = range 32 48 in
  [ range 64 65; range 48 64; range 16 32; range 0 16; range 40 41 ]
  |> List.sort ~compare:(Chunk.compare_by_distance ~viewport)
  |> List.iter ~f:(fun range -> printf "%d-%d\n" range.first range.past);
  [%expect
    {|
    16-32
    40-41
    48-64
    0-16
    64-65
    |}]
;;

let%expect_test "prepared corridor is bounded, directional, and batch aligned" =
  let geometry = Chat_tui.Renderer_virtual_list.Geometry.create () in
  Chat_tui.Renderer_virtual_list.Geometry.rebuild
    geometry
    ~length:1_000
    ~height_at_index:(fun index -> if Int.equal index 500 then 25 else 1);
  let snapshot = Chat_tui.Renderer_virtual_list.Geometry.snapshot geometry in
  let show direction =
    let plan =
      Chat_tui.Prepared_corridor.plan
        ~geometry:snapshot
        ~requested_scroll:500
        ~viewport_height:10
        ~follow_bottom:false
        ~direction
        ()
    in
    let classes =
      List.take plan.batches 8
      |> List.map ~f:(fun batch ->
        ( batch.index
        , match batch.class_ with
          | Visible -> "visible"
          | Directional -> "directional"
          | Guard -> "guard"
          | Remaining -> "remaining" ))
    in
    print_s
      [%sexp
        (plan.visible_rows : Chat_tui.History_chunk.Range.t)
      , (plan.desired_rows : Chat_tui.History_chunk.Range.t)
      , (plan.scheduled_rows : Chat_tui.History_chunk.Range.t)
      , (classes : (int * string) list)]
  in
  show Toward_older;
  show Toward_newer;
  [%expect
    {|
    (((first 500) (past 501)) ((first 450) (past 516)) ((first 448) (past 528))
     ((31 visible) (30 directional) (29 directional) (28 directional)
      (27 directional) (32 guard) (33 guard) (34 remaining)))
    (((first 500) (past 501)) ((first 470) (past 536)) ((first 464) (past 544))
     ((31 visible) (32 directional) (33 directional) (34 directional) (30 guard)
      (29 guard) (28 guard) (27 remaining)))
    |}]
;;

let%expect_test "auto-follow ranks newest visible batch first" =
  let geometry = Chat_tui.Renderer_virtual_list.Geometry.create () in
  Chat_tui.Renderer_virtual_list.Geometry.rebuild
    geometry
    ~length:513
    ~height_at_index:(Fn.const 1);
  let plan =
    Chat_tui.Prepared_corridor.plan
      ~geometry:(Chat_tui.Renderer_virtual_list.Geometry.snapshot geometry)
      ~requested_scroll:0
      ~viewport_height:20
      ~follow_bottom:true
      ~direction:Toward_newer
      ()
  in
  print_s
    [%sexp
      (plan.visible_rows : Chat_tui.History_chunk.Range.t)
    , (plan.scheduled_rows : Chat_tui.History_chunk.Range.t)
    , (List.take plan.batches 3
       |> List.map ~f:(fun batch ->
         ( batch.index
         , match batch.class_ with
           | Visible -> "visible"
           | Directional -> "directional"
           | Guard -> "guard"
           | Remaining -> "remaining" ))
       : (int * string) list)];
  [%expect
    {|
    (((first 493) (past 513)) ((first 432) (past 513))
     ((32 visible) (31 visible) (30 visible)))
    |}]
;;

let%expect_test "runtime submits the bounded initial corridor to the injected worker" =
  let model =
    List.init 100 ~f:(fun index -> "assistant", Int.to_string index) |> make_model
  in
  Chat_tui.Renderer_virtual_list.Geometry.rebuild
    (Model.chat_render_geometry model)
    ~length:100
    ~height_at_index:(Fn.const 1);
  let anchor = Model.capture_resize_anchor model ~viewport_height:10 in
  Model.start_width_preparation
    model
    ~request_generation:17
    ~terminal_size:(60, 24)
    ~layout:
      { Model.Chat_page_state.input_box_height = 3
      ; history_height = 20
      ; sticky_height = 0
      ; scroll_height = 10
      }
    ~theme_generation:0
    ~grammar_generation:0
    ~anchor;
  let config =
    Chat_tui.Chat_render_worker_runtime.Config.create
      ~custom_grammars:[]
      ~theme_generation:0
      ~grammar_generation:0
  in
  let worker =
    Chat_tui.Chat_render_worker.For_testing.create_detached
      ~config
      ~queue_capacity:64
      ~worker_count:2
  in
  let runtime = Chat_tui.App_runtime.create ~chat_render_worker:worker ~model () in
  Chat_tui.App_runtime.submit_initial_target_width_batches runtime;
  let stats = Chat_tui.Chat_render_worker.For_testing.stats worker in
  let preparation = Model.width_preparation model |> Option.value_exn in
  print_s
    [%sexp
      (stats.queued : int)
    , (Model.width_preparation_corridors preparation
       : Chat_tui.History_chunk.Range.t option * Chat_tui.History_chunk.Range.t option)];
  [%expect {| (64 ((((first 48) (past 100))) ())) |}]
;;

let%expect_test "warm history materializes only dirty chunks" =
  let messages = List.init 130 ~f:(fun index -> "assistant", Int.to_string index) in
  let model = make_model messages in
  ignore
    (Chat_tui.Renderer_page_chat.render ~size:(40, 12) ~model : Notty.I.t * (int * int));
  Chat_tui.Renderer_page_chat.warm_history_synchronously ~size:(40, 12) ~model;
  let row_id, _ = Model.render_row_identity model ~idx:70 |> Option.value_exn in
  Model.mark_history_row_dirty model ~id:row_id;
  let (), indices =
    Chat_tui.Renderer_page_chat.For_testing.capture_materialized_indices (fun () ->
      Chat_tui.Renderer_page_chat.For_testing.warm_dirty_chunks_synchronously
        ~size:(40, 12)
        ~model)
  in
  print_s [%sexp (indices : int list)];
  [%expect
    {|
    (64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88
     89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104 105 106 107 108 109 110
     111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127)
    |}]
;;

let%expect_test "uncached width reuses only a proven layout interval" =
  let model = make_model [ "assistant", "abc" ] in
  ignore
    (Chat_tui.Renderer_page_chat.render ~size:(5, 12) ~model : Notty.I.t * (int * int));
  Chat_tui.Renderer_page_chat.warm_history_synchronously ~size:(5, 12) ~model;
  print_s
    [%sexp
      (( Model.reusable_layout_width model ~width:4
       , Model.reusable_layout_width model ~width:3
       , Model.reusable_layout_width model ~width:2 )
       : int option * int option * int option)];
  [%expect {| ((5) (5) ()) |}]
;;

let%expect_test "appending rows extends geometry and the canonical chunk image" =
  let model =
    List.init 64 ~f:(fun index -> "assistant", Int.to_string index) |> make_model
  in
  ignore
    (Chat_tui.Renderer_page_chat.render ~size:(40, 12) ~model : Notty.I.t * (int * int));
  Chat_tui.Renderer_page_chat.warm_history_synchronously ~size:(40, 12) ~model;
  let before_chunks = Chat_tui.Renderer_page_chat.For_testing.history_chunk_count model in
  Model.set_auto_follow model false;
  ignore
    (Model.apply_patch model (Chat_tui.Types.Add_user_message { text = "submitted" })
     : Model.t);
  let after_append =
    ( Array.length (Model.render_messages model)
    , Chat_tui.Renderer_virtual_list.Geometry.length (Model.chat_render_geometry model)
    , Model.auto_follow model )
  in
  let _, materialized =
    Chat_tui.Renderer_page_chat.For_testing.capture_materialized_indices (fun () ->
      Chat_tui.Renderer_page_chat.warm_history_synchronously ~size:(40, 12) ~model)
  in
  let after_chunks = Chat_tui.Renderer_page_chat.For_testing.history_chunk_count model in
  print_s
    [%sexp
      ((before_chunks, after_append, materialized, after_chunks)
       : int * (int * int * bool) * int list * int)];
  [%expect {| (1 (65 65 false) (64) 2) |}]
;;
