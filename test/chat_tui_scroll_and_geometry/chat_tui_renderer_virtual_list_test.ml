open Core
module Geometry = Chat_tui.Renderer_virtual_list.Geometry
module Viewport = Chat_tui.Renderer_virtual_list.Viewport

let show_viewport viewport =
  print_s
    [%sexp
      (( Viewport.scroll viewport
       , Viewport.max_scroll viewport
       , Viewport.first_visible viewport
       , Viewport.last_visible viewport
       , Viewport.top_spacer viewport
       , Viewport.bottom_spacer viewport
       , Viewport.visible_indices viewport )
       : int * int * int option * int option * int * int * int list)]
;;

let%expect_test "empty, clamped, boundary, and bottom-follow viewports" =
  let geometry = Geometry.create () in
  show_viewport
    (Viewport.compute ~geometry ~requested_scroll:20 ~height:5 ~follow_bottom:false);
  Geometry.rebuild geometry ~length:4 ~height_at_index:(Array.get [| 2; 3; 1; 4 |]);
  show_viewport
    (Viewport.compute ~geometry ~requested_scroll:2 ~height:0 ~follow_bottom:false);
  List.iter
    [ -2, 3, false; 2, 3, false; 50, 3, false; 0, 3, true ]
    ~f:(fun (requested_scroll, height, follow_bottom) ->
      show_viewport (Viewport.compute ~geometry ~requested_scroll ~height ~follow_bottom));
  [%expect
    {|
    (0 0 () () 0 0 ())
    (2 10 () () 0 10 ())
    (0 7 (0) (1) 0 5 (0 1))
    (2 7 (1) (1) 2 5 (1))
    (7 7 (3) (3) 6 0 (3))
    (7 7 (3) (3) 6 0 (3))
    |}]
;;

let%test_unit "negative item heights are rejected" =
  let geometry = Geometry.create () in
  [%test_result: bool]
    (try
       Geometry.rebuild geometry ~length:1 ~height_at_index:(fun _ -> -1);
       false
     with
     | Invalid_argument _ -> true)
    ~expect:true;
  Geometry.rebuild geometry ~length:1 ~height_at_index:(fun _ -> 1);
  [%test_result: bool]
    (try
       Geometry.update_height geometry ~index:0 ~height:(-1);
       false
     with
     | Invalid_argument _ -> true)
    ~expect:true
;;

let%expect_test "incremental heights update suffix geometry" =
  let geometry = Geometry.create () in
  Geometry.rebuild geometry ~length:4 ~height_at_index:(Array.get [| 2; 3; 1; 4 |]);
  Geometry.update_height geometry ~index:1 ~height:6;
  print_s
    [%sexp
      (Geometry.heights geometry : int array)
    , (Geometry.prefix geometry : int array)
    , (Geometry.total_height geometry : int)
    , (Geometry.item_start geometry ~index:2 : int option)];
  [%expect {| ((2 6 1 4) (0 2 8 9 13) 13 (8)) |}]
;;

let%expect_test "marking geometry estimated preserves established dimensions" =
  let geometry = Geometry.create () in
  Geometry.rebuild geometry ~length:3 ~height_at_index:(Array.get [| 2; 7; 3 |]);
  let generation = Geometry.generation geometry in
  Geometry.mark_all_estimated geometry;
  print_s
    [%sexp
      (Geometry.heights geometry : int array)
    , (Geometry.prefix geometry : int array)
    , (Geometry.total_height geometry : int)
    , (Geometry.estimated_indices geometry : int list)
    , (Geometry.generation geometry - generation : int)];
  [%expect {| ((2 7 3) (0 2 9 12) 12 (0 1 2) 1) |}]
;;

let%expect_test "geometry snapshots are immutable and range exactness is local" =
  let geometry = Geometry.create () in
  Geometry.initialize_estimated geometry ~length:4 ~estimated_height_at_index:(fun _ -> 2);
  Geometry.mark_exact geometry ~index:1 ~height:3;
  Geometry.mark_exact geometry ~index:2 ~height:4;
  let snapshot = Geometry.snapshot geometry in
  let snapshot_heights = Geometry.Snapshot.heights snapshot in
  snapshot_heights.(1) <- 99;
  Geometry.mark_exact geometry ~index:0 ~height:5;
  print_s
    [%sexp
      (Geometry.Snapshot.heights snapshot : int array)
    , (Geometry.Snapshot.prefix snapshot : int array)
    , (Geometry.Snapshot.exactness snapshot : bool array)
    , (Geometry.Snapshot.range_is_exact snapshot ~first:1 ~past:3 : bool)
    , (Geometry.Snapshot.range_is_exact snapshot ~first:0 ~past:3 : bool)
    , (Geometry.range_is_exact geometry ~first:0 ~past:3 : bool)];
  [%expect {| ((2 3 4 2) (0 2 5 9 11) (false true true false) true false true) |}]
;;

let%expect_test "exact height batches publish once and match a full rebuild" =
  let batched = Geometry.create () in
  Geometry.initialize_estimated batched ~length:5 ~estimated_height_at_index:(fun _ -> 2);
  Geometry.mark_exact batched ~index:0 ~height:1;
  let generation = Geometry.generation batched in
  let applied =
    Geometry.apply_exact_batch
      batched
      ~expected_generation:generation
      ~start_index:1
      ~heights:[| 3; 4; 5 |]
  in
  let stale =
    Geometry.apply_exact_batch
      batched
      ~expected_generation:generation
      ~start_index:4
      ~heights:[| 8 |]
  in
  let rebuilt = Geometry.create () in
  Geometry.rebuild rebuilt ~length:5 ~height_at_index:(Array.get [| 1; 3; 4; 5; 2 |]);
  print_s
    [%sexp
      (( applied
       , stale
       , (Geometry.generation batched - generation : int)
       , (Geometry.heights batched : int array)
       , (Geometry.prefix batched : int array)
       , (Geometry.prefix rebuilt : int array)
       , (Geometry.estimated_indices batched : int list) )
       : Geometry.batch_result
         * Geometry.batch_result
         * int
         * int array
         * int array
         * int array
         * int list)];
  [%expect
    {| (Applied Stale_generation 1 (1 3 4 5 2) (0 1 4 8 13 15) (0 1 4 8 13 15) (4)) |}]
;;

let%test_unit "invalid exact height batches are rejected atomically" =
  let geometry = Geometry.create () in
  Geometry.initialize_estimated geometry ~length:3 ~estimated_height_at_index:(fun _ -> 2);
  let generation = Geometry.generation geometry in
  let heights = Array.copy (Geometry.heights geometry) in
  let prefix = Array.copy (Geometry.prefix geometry) in
  let rejected f =
    match f () with
    | _ -> false
    | exception Invalid_argument _ -> true
  in
  assert (
    rejected (fun () ->
      Geometry.apply_exact_batch
        geometry
        ~expected_generation:generation
        ~start_index:2
        ~heights:[| 3; 4 |]));
  assert (
    rejected (fun () ->
      Geometry.apply_exact_batch
        geometry
        ~expected_generation:generation
        ~start_index:0
        ~heights:[| -1 |]));
  assert (
    rejected (fun () ->
      Geometry.apply_exact_batch
        geometry
        ~expected_generation:generation
        ~start_index:0
        ~heights:[||]));
  [%test_result: int] (Geometry.generation geometry) ~expect:generation;
  [%test_result: int array] (Geometry.heights geometry) ~expect:heights;
  [%test_result: int array] (Geometry.prefix geometry) ~expect:prefix
;;

let%expect_test "sparse rendering requests only visible items" =
  let geometry = Geometry.create () in
  Geometry.rebuild geometry ~length:5 ~height_at_index:(fun _ -> 2);
  let viewport =
    Viewport.compute ~geometry ~requested_scroll:4 ~height:3 ~follow_bottom:false
  in
  let rendered = ref [] in
  let image =
    Chat_tui.Renderer_virtual_list.render ~viewport ~width:8 ~image_at_index:(fun index ->
      rendered := index :: !rendered;
      Notty.I.void 8 2)
  in
  print_s
    [%sexp
      (List.rev !rendered : int list)
    , (Notty.I.width image : int)
    , (Notty.I.height image : int)];
  [%expect {| ((2 3) 8 10) |}]
;;

let%expect_test "estimated geometry supports bottom-up exact measurement" =
  let geometry = Geometry.create () in
  Geometry.initialize_estimated geometry ~length:6 ~estimated_height_at_index:(fun _ -> 2);
  let generation = Geometry.generation geometry in
  let candidates = Viewport.bottom_up_candidates ~geometry ~height:5 ~overscan_rows:2 in
  List.iter candidates ~f:(fun index ->
    Geometry.mark_exact geometry ~index ~height:(if index = 5 then 4 else 2));
  let viewport =
    Viewport.compute ~geometry ~requested_scroll:0 ~height:5 ~follow_bottom:true
  in
  print_s
    [%sexp
      (( generation
       , candidates
       , (Geometry.total_height geometry : int)
       , (Viewport.visible_indices viewport : int list)
       , (Viewport.estimated_visible_indices ~geometry viewport : int list)
       , (Geometry.estimated_indices geometry : int list) )
       : int * int list * int * int list * int list * int list)];
  [%expect {| (1 (5 4 3 2) 14 (4 5) () (0 1)) |}]
;;

let%expect_test "viewport exactness ignores estimated offscreen rows" =
  let geometry = Geometry.create () in
  Geometry.initialize_estimated geometry ~length:5 ~estimated_height_at_index:(fun _ -> 2);
  Geometry.mark_exact geometry ~index:1 ~height:2;
  Geometry.mark_exact geometry ~index:2 ~height:2;
  let viewport =
    Viewport.compute ~geometry ~requested_scroll:2 ~height:4 ~follow_bottom:false
  in
  print_s
    [%sexp
      (Viewport.visible_indices viewport : int list)
    , (Viewport.is_exact ~geometry viewport : bool)
    , (Geometry.all_exact geometry : bool)];
  [%expect {| ((1 2) true false) |}]
;;

let%expect_test "anchors preserve a manual viewport across earlier corrections" =
  let geometry = Geometry.create () in
  Geometry.initialize_estimated geometry ~length:4 ~estimated_height_at_index:(fun _ -> 3);
  let viewport =
    Viewport.compute ~geometry ~requested_scroll:7 ~height:4 ~follow_bottom:false
  in
  let anchor =
    Chat_tui.Renderer_virtual_list.Anchor.create ~geometry ~viewport ~screen_row:0
    |> Option.value_exn
  in
  Geometry.mark_exact geometry ~index:0 ~height:6;
  let corrected =
    Chat_tui.Renderer_virtual_list.Anchor.corrected_scroll anchor ~geometry
  in
  print_s
    [%sexp
      (Viewport.visible_indices viewport : int list)
    , (corrected : int option)
    , (Geometry.prefix geometry : int array)];
  [%expect {| ((2 3) (10) (0 6 9 12 15)) |}]
;;

let%expect_test "estimated anchors preserve distance from the item end" =
  let geometry = Geometry.create () in
  Geometry.initialize_estimated
    geometry
    ~length:3
    ~estimated_height_at_index:(fun index -> if index = 0 then 5 else 1);
  Geometry.mark_exact geometry ~index:1 ~height:1;
  Geometry.mark_exact geometry ~index:2 ~height:1;
  let corrected_scroll requested_scroll =
    let viewport =
      Viewport.compute ~geometry ~requested_scroll ~height:2 ~follow_bottom:false
    in
    let anchor =
      Chat_tui.Renderer_virtual_list.Anchor.create ~geometry ~viewport ~screen_row:0
      |> Option.value_exn
    in
    Geometry.mark_exact geometry ~index:0 ~height:30;
    Chat_tui.Renderer_virtual_list.Anchor.corrected_scroll anchor ~geometry
    |> Option.value_exn
  in
  let final_row = corrected_scroll 4 in
  Geometry.initialize_estimated
    geometry
    ~length:3
    ~estimated_height_at_index:(fun index -> if index = 0 then 5 else 1);
  Geometry.mark_exact geometry ~index:1 ~height:1;
  Geometry.mark_exact geometry ~index:2 ~height:1;
  let three_rows_from_end = corrected_scroll 2 in
  print_s [%sexp ((final_row, three_rows_from_end) : int * int)];
  [%expect {| (29 27) |}]
;;

let%expect_test "exact anchors remain relative to the item start" =
  let geometry = Geometry.create () in
  Geometry.rebuild geometry ~length:1 ~height_at_index:(fun _ -> 5);
  let viewport =
    Viewport.compute ~geometry ~requested_scroll:4 ~height:1 ~follow_bottom:false
  in
  let anchor =
    Chat_tui.Renderer_virtual_list.Anchor.create ~geometry ~viewport ~screen_row:0
    |> Option.value_exn
  in
  Geometry.mark_exact geometry ~index:0 ~height:30;
  print_s
    [%sexp
      (Chat_tui.Renderer_virtual_list.Anchor.corrected_scroll anchor ~geometry
       : int option)];
  [%expect {| (4) |}]
;;

let%expect_test "exact and estimated geometry converge to the same viewport" =
  let exact = Geometry.create () in
  let lazy_geometry = Geometry.create () in
  let heights = [| 2; 5; 1; 4 |] in
  Geometry.rebuild exact ~length:4 ~height_at_index:(Array.get heights);
  Geometry.initialize_estimated
    lazy_geometry
    ~length:4
    ~estimated_height_at_index:(fun _ -> 3);
  Array.iteri heights ~f:(fun index height ->
    Geometry.mark_exact lazy_geometry ~index ~height);
  let view geometry =
    Viewport.compute ~geometry ~requested_scroll:5 ~height:4 ~follow_bottom:false
    |> Viewport.visible_indices
  in
  print_s
    [%sexp
      (Geometry.all_exact lazy_geometry : bool)
    , (Geometry.prefix exact : int array)
    , (Geometry.prefix lazy_geometry : int array)
    , (view exact : int list)
    , (view lazy_geometry : int list)];
  [%expect {| (true (0 2 7 8 12) (0 2 7 8 12) (1 2 3) (1 2 3)) |}]
;;
