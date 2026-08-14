open Core
module Range = History_chunk.Range
module Snapshot = Renderer_virtual_list.Geometry.Snapshot
module Viewport = Renderer_virtual_list.Viewport

type direction =
  | Toward_older
  | Toward_newer

type policy =
  { older_viewports : int
  ; newer_viewports : int
  }

type batch_class =
  | Visible
  | Directional
  | Guard
  | Remaining

type batch =
  { index : int
  ; rows : Range.t
  ; class_ : batch_class
  ; distance : int
  }

type t =
  { viewport : Viewport.t
  ; visible_rows : Range.t
  ; desired_rows : Range.t
  ; scheduled_rows : Range.t
  ; batches : batch list
  }

let max_viewports_per_side = 8

let create_policy ?(older_viewports = 5) ?(newer_viewports = 3) () =
  let validate name value =
    if value < 0 || value > max_viewports_per_side
    then invalid_arg ("Prepared_corridor.create_policy: " ^ name);
    value
  in
  { older_viewports = validate "older_viewports" older_viewports
  ; newer_viewports = validate "newer_viewports" newer_viewports
  }
;;

let default_policy = create_policy ()

let range_of_viewport viewport =
  match Viewport.first_visible viewport, Viewport.last_visible viewport with
  | Some first, Some last -> Range.create_exn ~first ~past:(last + 1)
  | None, None | None, Some _ | Some _, None -> Range.create_exn ~first:0 ~past:0
;;

let distance viewport rows =
  if rows.Range.past <= viewport.Range.first
  then viewport.first - rows.past
  else if viewport.past <= rows.first
  then rows.first - viewport.past
  else 0
;;

let class_rank = function
  | Visible -> 0
  | Directional -> 1
  | Guard -> 2
  | Remaining -> 3
;;

let classify ~direction ~visible ~scheduled rows =
  if distance visible rows = 0
  then Visible
  else if distance scheduled rows > 0
  then Remaining
  else (
    let is_older = rows.Range.past <= visible.Range.first in
    match direction, is_older with
    | Toward_older, true | Toward_newer, false -> Directional
    | Toward_older, false | Toward_newer, true -> Guard)
;;

let compare_batch ~follow_bottom left right =
  match Int.compare (class_rank left.class_) (class_rank right.class_) with
  | 0 ->
    (match Int.compare left.distance right.distance with
     | 0 ->
       if follow_bottom && Poly.(left.class_ = Visible)
       then Int.compare right.index left.index
       else Int.compare left.index right.index
     | result -> result)
  | result -> result
;;

let plan
      ?(policy = default_policy)
      ~geometry
      ~requested_scroll
      ~viewport_height
      ~follow_bottom
      ~direction
      ()
  =
  let row_count = Snapshot.length geometry in
  let viewport =
    Viewport.compute_snapshot
      ~geometry
      ~requested_scroll
      ~height:viewport_height
      ~follow_bottom
  in
  let visible_rows = range_of_viewport viewport in
  if Range.is_empty visible_rows
  then
    { viewport
    ; visible_rows
    ; desired_rows = visible_rows
    ; scheduled_rows = visible_rows
    ; batches = []
    }
  else (
    let older_weight, newer_weight =
      match direction with
      | Toward_older -> policy.older_viewports, policy.newer_viewports
      | Toward_newer -> policy.newer_viewports, policy.older_viewports
    in
    let height = Int.max 0 viewport_height in
    let first_scroll = Int.max 0 (Viewport.scroll viewport - (older_weight * height)) in
    let total_height = Snapshot.total_height geometry in
    let past_scroll =
      Int.min total_height (Viewport.scroll viewport + height + (newer_weight * height))
    in
    let expanded_viewport =
      Viewport.compute_snapshot
        ~geometry
        ~requested_scroll:first_scroll
        ~height:(past_scroll - first_scroll)
        ~follow_bottom:false
    in
    let desired_rows = range_of_viewport expanded_viewport in
    let scheduled_rows =
      History_chunk.expand_to_foreground_batches ~row_count desired_rows
    in
    let batches =
      List.init (History_chunk.foreground_batch_count ~row_count) ~f:(fun index ->
        let rows =
          History_chunk.foreground_batch_range ~row_count ~batch_index:index
          |> Option.value_exn
        in
        let class_ =
          classify ~direction ~visible:visible_rows ~scheduled:scheduled_rows rows
        in
        { index; rows; class_; distance = distance visible_rows rows })
      |> List.sort ~compare:(compare_batch ~follow_bottom)
    in
    { viewport; visible_rows; desired_rows; scheduled_rows; batches })
;;
