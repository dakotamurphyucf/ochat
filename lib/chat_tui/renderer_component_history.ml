open Core
open Notty
open Types

type render_plan =
  { image : I.t
  ; viewport : Renderer_virtual_list.Viewport.t
  ; top_visible_idx : int option
  ; prefetch_indices : int list
  }

type rendered =
  | Ready of I.t
  | Pending of I.t

let cache_matches entry ~width (role, text) =
  entry.Model.width = width
  && String.equal entry.role role
  && String.equal entry.text text
;;

let cache_entry ~row_revision ~width (role, text) image =
  { Model.row_revision
  ; width
  ; role
  ; text
  ; image
  ; height = I.height image
  ; layout = Chat_message_render_job.Layout.{ width; lines = [] }
  ; layout_plan = Chat_message_render_job.Layout_plan.unknown
  }
;;

let get_height ~model ~width ~render_message ~idx msg =
  let id, revision = Model.render_row_identity model ~idx |> Option.value_exn in
  match Model.find_img_cache model ~id ~revision with
  | Some entry when cache_matches entry ~width msg -> entry.height, true
  | Some _ | None ->
    (match render_message ~idx ~selected:false msg with
     | Pending image -> I.height image, false
     | Ready image ->
       (match Model.find_img_cache model ~id ~revision with
        | Some entry when cache_matches entry ~width msg -> entry.height, true
        | Some _ | None ->
          let entry = cache_entry ~row_revision:revision ~width msg image in
          Model.set_img_cache model ~id entry;
          entry.height, true))
;;

let get_image ~model ~width ~render_message ~idx msg ~selected:_ =
  let id, revision = Model.render_row_identity model ~idx |> Option.value_exn in
  let entry, unselected_pending =
    match Model.find_img_cache model ~id ~revision with
    | Some entry when cache_matches entry ~width msg -> Some entry, None
    | Some _ | None ->
      (match render_message ~idx ~selected:false msg with
       | Pending image -> None, Some image
       | Ready image ->
         (match Model.find_img_cache model ~id ~revision with
          | Some entry when cache_matches entry ~width msg -> Some entry, None
          | Some _ | None ->
            let entry = cache_entry ~row_revision:revision ~width msg image in
            Model.set_img_cache model ~id entry;
            Some entry, None))
  in
  match entry, unselected_pending with
  | None, Some image -> image, false
  | None, None -> I.empty, false
  | Some entry, _ -> entry.image, true
;;

let estimated_height ~width:_ _ = 5

let initialize_geometry ~geometry ~messages ~width =
  let length = Array.length messages in
  let current = Renderer_virtual_list.Geometry.length geometry in
  if Int.equal current 0
  then
    Renderer_virtual_list.Geometry.initialize_estimated
      geometry
      ~length
      ~estimated_height_at_index:(fun index -> estimated_height ~width messages.(index))
  else if current < length
  then
    Renderer_virtual_list.Geometry.extend_estimated
      geometry
      ~length
      ~estimated_height_at_index:(fun index -> estimated_height ~width messages.(index))
  else if not (Int.equal current length)
  then
    Renderer_virtual_list.Geometry.initialize_estimated
      geometry
      ~length
      ~estimated_height_at_index:(fun index -> estimated_height ~width messages.(index))
;;

let measure_index ~model ~messages ~width ~render_message ~selected_idx index =
  let selected = Option.value_map selected_idx ~default:false ~f:(Int.equal index) in
  let image, ready =
    get_image ~model ~width ~render_message ~idx:index messages.(index) ~selected
  in
  if ready
  then
    Renderer_virtual_list.Geometry.mark_exact
      (Model.chat_render_geometry model)
      ~index
      ~height:(I.height image);
  ready
;;

let measure_indices ~model ~messages ~width ~render_message ~selected_idx indices =
  List.fold indices ~init:false ~f:(fun any index ->
    measure_index ~model ~messages ~width ~render_message ~selected_idx index || any)
;;

let viewport ~model ~height =
  Renderer_virtual_list.Viewport.compute
    ~geometry:(Model.chat_render_geometry model)
    ~requested_scroll:(Notty_scroll_box.scroll (Model.scroll_box model))
    ~height
    ~follow_bottom:(Model.auto_follow model)
;;

let restore_anchor ~model anchor =
  Option.bind anchor ~f:(fun anchor ->
    Renderer_virtual_list.Anchor.corrected_scroll
      anchor
      ~geometry:(Model.chat_render_geometry model))
  |> Option.iter ~f:(Notty_scroll_box.scroll_to (Model.scroll_box model))
;;

let measure_initial_region ~model ~messages ~width ~height ~render_message ~selected_idx =
  let geometry = Model.chat_render_geometry model in
  let indices =
    if Model.auto_follow model
    then
      Renderer_virtual_list.Viewport.bottom_up_candidates
        ~geometry
        ~height
        ~overscan_rows:0
    else
      viewport ~model ~height
      |> Renderer_virtual_list.Viewport.estimated_visible_indices ~geometry
  in
  ignore
    (measure_indices ~model ~messages ~width ~render_message ~selected_idx indices : bool)
;;

let rec converge_viewport
          ~fuel
          ~anchor
          ~model
          ~messages
          ~width
          ~height
          ~render_message
          ~selected_idx
  =
  let geometry = Model.chat_render_geometry model in
  let view = viewport ~model ~height in
  match fuel, Renderer_virtual_list.Viewport.estimated_visible_indices ~geometry view with
  | fuel, indices when fuel > 0 && not (List.is_empty indices) ->
    let progressed =
      measure_indices ~model ~messages ~width ~render_message ~selected_idx indices
    in
    if not progressed
    then view
    else (
      restore_anchor ~model anchor;
      converge_viewport
        ~fuel:(fuel - 1)
        ~anchor
        ~model
        ~messages
        ~width
        ~height
        ~render_message
        ~selected_idx)
  | _ -> view
;;

let apply_dirty_indices ~model ~messages ~width ~height ~render_message ~selected_idx =
  let last_visible =
    viewport ~model ~height |> Renderer_virtual_list.Viewport.last_visible
  in
  let dirty =
    Model.take_and_clear_dirty_height_rows model
    |> List.dedup_and_sort ~compare:(fun (left, _) (right, _) ->
      Projected_message.Id.compare left right)
  in
  let deferred =
    List.filter dirty ~f:(fun (id, revision) ->
      match Model.render_index_by_id model ~id with
      | Some index
        when Option.equal
               Int.equal
               (Model.render_row_identity model ~idx:index |> Option.map ~f:snd)
               (Some revision)
             && index < Array.length messages ->
        if Option.value_map last_visible ~default:false ~f:(fun last -> index <= last)
        then (
          ignore
            (measure_index ~model ~messages ~width ~render_message ~selected_idx index
             : bool);
          false)
        else true
      | None | Some _ -> false)
  in
  Model.defer_dirty_height_rows model deferred
;;

let apply_reveal_request ~model ~messages ~width ~height ~render_message ~selected_idx =
  match Model.take_projected_reveal_request model with
  | None -> false
  | Some id ->
    (match Model.render_index_by_id model ~id with
     | None -> false
     | Some index when index < 0 || index >= Array.length messages -> false
     | Some index ->
       ignore
         (measure_index ~model ~messages ~width ~render_message ~selected_idx index
          : bool);
       let geometry = Model.chat_render_geometry model in
       let desired =
         Renderer_virtual_list.Geometry.item_start geometry ~index
         |> Option.value_map ~default:0 ~f:(fun start -> Int.max 0 (start - (height / 2)))
       in
       let max_scroll =
         Int.max 0 (Renderer_virtual_list.Geometry.total_height geometry - height)
       in
       Notty_scroll_box.scroll_to (Model.scroll_box model) (Int.min desired max_scroll);
       true)
;;

let top_visible_index ~geometry ~viewport =
  match Renderer_virtual_list.Viewport.first_visible viewport with
  | None -> None
  | Some index ->
    let message_start =
      Renderer_virtual_list.Geometry.item_start geometry ~index |> Option.value_exn
    in
    let header_position =
      message_start + 1 - Renderer_virtual_list.Viewport.scroll viewport
    in
    if header_position >= 0 && header_position < 2 then None else Some index
;;

let prefetch_candidate_indices ~model ~viewport ~height =
  let geometry = Model.chat_render_geometry model in
  let height = Int.max 0 height in
  let scroll = Renderer_virtual_list.Viewport.scroll viewport in
  let visible =
    Renderer_virtual_list.Viewport.visible_indices viewport |> Int.Hash_set.of_list
  in
  let offsets =
    match Model.chat_scroll_direction model with
    | Model.Toward_older -> [ -height; -(2 * height); -(3 * height); height ]
    | Toward_newer -> [ height; 2 * height; 3 * height; -height ]
  in
  let seen = Int.Hash_set.create () in
  List.concat_map offsets ~f:(fun offset ->
    Renderer_virtual_list.Viewport.compute
      ~geometry
      ~requested_scroll:(scroll + offset)
      ~height
      ~follow_bottom:false
    |> Renderer_virtual_list.Viewport.visible_indices)
  |> List.filter ~f:(fun index ->
    (not (Hash_set.mem visible index))
    && (not (Renderer_virtual_list.Geometry.is_exact geometry ~index))
    && (not (Hash_set.mem seen index))
    &&
    (Hash_set.add seen index;
     true))
;;

let render_with_initial_anchor
      ~initial_anchor
      ~model
      ~width
      ~height
      ~messages
      ~selected_idx
      ~render_message
  =
  let geometry = Model.chat_render_geometry model in
  Live_scroll_trace.emit
    ~phase:"history_render_enter"
    [ "width", `Number (Int.to_string width)
    ; "height", `Number (Int.to_string height)
    ; "message_count", `Number (Int.to_string (Array.length messages))
    ; ( "requested_scroll"
      , `Number (Int.to_string (Notty_scroll_box.scroll (Model.scroll_box model))) )
    ; ("auto_follow", if Model.auto_follow model then `True else `False)
    ; ( "geometry_generation"
      , `Number (Int.to_string (Renderer_virtual_list.Geometry.generation geometry)) )
    ; ( "geometry_total"
      , `Number (Int.to_string (Renderer_virtual_list.Geometry.total_height geometry)) )
    ];
  initialize_geometry ~geometry ~messages ~width;
  let anchor =
    match initial_anchor with
    | Some anchor -> Some anchor
    | None ->
      if Model.auto_follow model
      then None
      else
        Renderer_virtual_list.Anchor.create
          ~geometry
          ~viewport:(viewport ~model ~height)
          ~screen_row:0
  in
  apply_dirty_indices ~model ~messages ~width ~height ~render_message ~selected_idx;
  restore_anchor ~model anchor;
  measure_initial_region ~model ~messages ~width ~height ~render_message ~selected_idx;
  restore_anchor ~model anchor;
  let revealed =
    apply_reveal_request ~model ~messages ~width ~height ~render_message ~selected_idx
  in
  let anchor =
    if revealed
    then
      Renderer_virtual_list.Anchor.create
        ~geometry
        ~viewport:(viewport ~model ~height)
        ~screen_row:0
    else anchor
  in
  let viewport =
    converge_viewport
      ~fuel:(Array.length messages + 1)
      ~anchor
      ~model
      ~messages
      ~width
      ~height
      ~render_message
      ~selected_idx
  in
  let image =
    Renderer_virtual_list.render ~viewport ~width ~image_at_index:(fun index ->
      let selected = Option.value_map selected_idx ~default:false ~f:(Int.equal index) in
      get_image ~model ~width ~render_message ~idx:index messages.(index) ~selected |> fst)
  in
  let image_height = I.height image in
  let geometry_total = Renderer_virtual_list.Geometry.total_height geometry in
  Live_scroll_trace.emit
    ~phase:"history_render_exit"
    [ ( "viewport_scroll"
      , `Number (Int.to_string (Renderer_virtual_list.Viewport.scroll viewport)) )
    ; ( "viewport_max"
      , `Number (Int.to_string (Renderer_virtual_list.Viewport.max_scroll viewport)) )
    ; ( "first_visible"
      , match Renderer_virtual_list.Viewport.first_visible viewport with
        | None -> `Null
        | Some index -> `Number (Int.to_string index) )
    ; ( "last_visible"
      , match Renderer_virtual_list.Viewport.last_visible viewport with
        | None -> `Null
        | Some index -> `Number (Int.to_string index) )
    ; ( "geometry_generation"
      , `Number (Int.to_string (Renderer_virtual_list.Geometry.generation geometry)) )
    ; "geometry_total", `Number (Int.to_string geometry_total)
    ; "image_height", `Number (Int.to_string image_height)
    ; ( "height_matches_geometry"
      , if Int.equal image_height geometry_total then `True else `False )
    ];
  { image
  ; viewport
  ; top_visible_idx = top_visible_index ~geometry ~viewport
  ; prefetch_indices = prefetch_candidate_indices ~model ~viewport ~height
  }
;;

let render_async ~model ~width ~height ~messages ~selected_idx ~render_message =
  render_with_initial_anchor
    ~initial_anchor:None
    ~model
    ~width
    ~height
    ~messages
    ~selected_idx
    ~render_message
;;

let render_async_with_anchor
      ~initial_anchor
      ~model
      ~width
      ~height
      ~messages
      ~selected_idx
      ~render_message
  =
  render_with_initial_anchor
    ~initial_anchor:(Some initial_anchor)
    ~model
    ~width
    ~height
    ~messages
    ~selected_idx
    ~render_message
;;

let render ~model ~width ~height ~messages ~selected_idx ~render_message =
  render_async
    ~model
    ~width
    ~height
    ~messages
    ~selected_idx
    ~render_message:(fun ~idx ~selected message ->
      Ready (render_message ~idx ~selected message))
;;

let render_with_anchor
      ~initial_anchor
      ~model
      ~width
      ~height
      ~messages
      ~selected_idx
      ~render_message
  =
  render_async_with_anchor
    ~initial_anchor
    ~model
    ~width
    ~height
    ~messages
    ~selected_idx
    ~render_message:(fun ~idx ~selected message ->
      Ready (render_message ~idx ~selected message))
;;
