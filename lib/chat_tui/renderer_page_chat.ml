(** Chat page renderer used by {!Chat_tui.Renderer_pages}. *)

open Core
open Notty
open Types

module Compose = struct
  let render_input_box ~(w : int) ~(layout : Chat_page_layout.t) ~(model : Model.t) =
    Renderer_component_input_box.render
      ~width:w
      ~max_height:layout.input_box_height
      ~model
  ;;

  let typeahead_preview_popup
        ~(model : Model.t)
        ~(w : int)
        ~(history_height : int)
        ~(max_popup_h : int)
    =
    let is_visible =
      match Model.mode model with
      | Insert -> Model.typeahead_is_relevant model && Model.typeahead_preview_open model
      | Normal | Cmdline | Search _ -> false
    in
    if not is_visible
    then None
    else (
      match Model.typeahead_completion model with
      | None -> None
      | Some completion ->
        let popup_h_max = Int.min history_height max_popup_h in
        if popup_h_max <= 0
        then None
        else (
          let completion_text = Util.sanitize ~strip:false completion.text in
          let lines = String.split ~on:'\n' completion_text in
          let header_h = 1 in
          let footer_h = if popup_h_max >= 3 then 1 else 0 in
          let body_h = Int.max 0 (popup_h_max - header_h - footer_h) in
          let max_scroll =
            if body_h <= 0 then 0 else Int.max 0 (List.length lines - body_h)
          in
          let preview_scroll =
            Model.typeahead_preview_scroll model
            |> Int.max 0
            |> fun s -> Int.min s max_scroll
          in
          let visible_lines =
            List.drop lines preview_scroll
            |> fun ls ->
            List.take ls body_h
            |> fun ls ->
            let missing = body_h - List.length ls in
            if missing <= 0 then ls else ls @ List.init missing ~f:(fun _ -> "")
          in
          let popup_bg = A.(bg (gray 3)) in
          let title_attr = A.(popup_bg ++ fg (gray 15)) in
          let body_attr = A.(popup_bg ++ fg (gray 10)) in
          let footer_attr = A.(popup_bg ++ fg (gray 12)) in
          let title_img =
            I.string title_attr "completion preview" |> I.hsnap ~align:`Left w
          in
          let body_imgs =
            List.map visible_lines ~f:(fun line ->
              I.string body_attr line |> I.hsnap ~align:`Left w)
          in
          let footer_imgs =
            if footer_h <= 0
            then []
            else [ I.string footer_attr "↑/↓ scroll" |> I.hsnap ~align:`Left w ]
          in
          let text_layer = I.vcat ((title_img :: body_imgs) @ footer_imgs) in
          let popup_h = I.height text_layer in
          let bg = I.char popup_bg ' ' (Int.max 0 w) (Int.max 0 popup_h) in
          let popup_img = Notty.Infix.(text_layer </> bg) |> I.hsnap w in
          let popup_y = Int.max 0 (history_height - I.height popup_img) in
          Some (I.pad ~t:popup_y popup_img)))
  ;;

  let history_layout ~(w : int) ~(h : int) ~(model : Model.t) =
    Chat_page_layout.compute ~screen_w:w ~screen_h:h ~model
  ;;

  let history_anchor ~(model : Model.t) ~scroll_height =
    let geometry = Model.chat_render_geometry model in
    if Model.auto_follow model || Renderer_virtual_list.Geometry.length geometry = 0
    then None
    else
      Renderer_virtual_list.Viewport.compute
        ~geometry
        ~requested_scroll:(Notty_scroll_box.scroll (Model.scroll_box model))
        ~height:scroll_height
        ~follow_bottom:false
      |> fun viewport ->
      Renderer_virtual_list.Anchor.create ~geometry ~viewport ~screen_row:0
  ;;

  let ensure_history_width ~(model : Model.t) ~(w : int) ~scroll_height =
    match Model.active_history_width model with
    | Some prev when Int.equal prev w -> None
    | None ->
      Model.clear_all_img_caches model;
      Model.set_active_history_width model (Some w);
      None
    | Some _ ->
      let anchor = history_anchor ~model ~scroll_height in
      Model.remember_current_width model;
      let exact_width_hit = Model.restore_width model ~width:w in
      Live_scroll_trace.emit
        ~phase:"resize_exact_width_lookup"
        [ "width", `Number (Int.to_string w)
        ; "hit", `String (Bool.to_string exact_width_hit)
        ];
      if not exact_width_hit
      then (
        let restored_layout =
          Model.reusable_layout_width model ~width:w
          |> Option.exists ~f:(fun source_width ->
            Model.restore_layout_width model ~source_width ~width:w)
        in
        if not restored_layout
        then (
          Model.clear_img_caches_preserving_heights model;
          Model.set_active_history_width model (Some w)));
      anchor
  ;;

  let make_render_job
        ~(model : Model.t)
        ~(w : int)
        ?(request_generation = 0)
        ?render_generation
        ~theme_generation
        ~grammar_generation
        ~priority
        ~idx
        ~selected:_
        ((role, text) : message)
    =
    let row_id, row_revision = Model.render_row_identity model ~idx |> Option.value_exn in
    let tool_output = Model.tool_output_for_row model ~id:row_id in
    let tool_call_outcome =
      if String.equal role "tool"
      then Model.tool_call_outcome_for_row model ~id:row_id
      else None
    in
    let semantic_seed =
      Model.find_semantic_cache
        model
        ~id:row_id
        ~revision:row_revision
        ~role
        ~text
        ~tool_output
      |> Option.map ~f:(fun cached ->
        Chat_message_render_job.
          { prepared = cached.prepared; highlights = cached.highlights })
    in
    Chat_message_render_job.create
      ~transcript_generation:(Model.transcript_generation model)
      ~row_id
      ~row_revision
      ~message_index:idx
      ~message_revision:(Option.value_exn (Model.message_revision model ~idx))
      ~width:w
      ~role
      ~text
      ~tool_output
      ~tool_call_outcome
      ~theme_generation
      ~grammar_generation
      ~geometry_generation:
        (Renderer_virtual_list.Geometry.generation (Model.chat_render_geometry model))
      ~request_generation
      ~render_generation:
        (Option.value render_generation ~default:(Model.render_generation model))
      ~submission_generation:0
      ~semantic_seed
      ~priority
  ;;

  let render_message_fn ~(model : Model.t) ~(w : int) ~hi_engine =
    let grammar_generation = Highlight_registry.generation () in
    fun ~idx ~selected message ->
      let job =
        make_render_job
          ~model
          ~w
          ~theme_generation:0
          ~grammar_generation
          ~priority:Visible
          ~idx
          ~selected
          message
      in
      Model.find_semantic_cache
        model
        ~id:job.key.row_id
        ~revision:job.key.row_revision
        ~role:job.key.role
        ~text:job.key.text
        ~tool_output:job.key.tool_output
      |> Option.iter ~f:(fun cached ->
        Renderer_component_message.install_prepared
          ~row_id:job.key.row_id
          ~row_revision:job.key.row_revision
          ~role:job.key.role
          ~text:job.key.text
          ~tool_output:job.key.tool_output
          cached.prepared;
        Renderer_component_message.install_highlights cached.highlights);
      let result : Chat_message_render_job.result =
        Renderer_component_message.render_synchronously ~hi_engine job
      in
      ignore (Model.commit_render_result model result : bool);
      result.image
  ;;

  let history_plan
        ~initial_anchor
        ~(model : Model.t)
        ~(w : int)
        ~(scroll_height : int)
        ~(messages : message array)
        ~render_message
    =
    let selected_idx = Model.selected_msg model in
    match initial_anchor with
    | None ->
      Renderer_component_history.render
        ~model
        ~width:w
        ~height:scroll_height
        ~messages
        ~selected_idx
        ~render_message
    | Some initial_anchor ->
      Renderer_component_history.render_with_anchor
        ~initial_anchor
        ~model
        ~width:w
        ~height:scroll_height
        ~messages
        ~selected_idx
        ~render_message
  ;;

  let update_scroll_box ~(model : Model.t) ~scroll_height plan =
    let scroll_box = Model.scroll_box model in
    let before = Notty_scroll_box.scroll scroll_box in
    Notty_scroll_box.set_content scroll_box plan.Renderer_component_history.image;
    Notty_scroll_box.scroll_to
      scroll_box
      (Renderer_virtual_list.Viewport.scroll plan.viewport);
    let physical_max = Notty_scroll_box.max_scroll scroll_box ~height:scroll_height in
    let viewport_max = Renderer_virtual_list.Viewport.max_scroll plan.viewport in
    Live_scroll_trace.emit
      ~phase:"page_plan_commit"
      [ "scroll_before", `Number (Int.to_string before)
      ; ( "planned_scroll"
        , `Number (Int.to_string (Renderer_virtual_list.Viewport.scroll plan.viewport)) )
      ; "committed_scroll", `Number (Int.to_string (Notty_scroll_box.scroll scroll_box))
      ; "physical_max", `Number (Int.to_string physical_max)
      ; "viewport_max", `Number (Int.to_string viewport_max)
      ; ("maxima_match", if Int.equal physical_max viewport_max then `True else `False)
      ]
  ;;

  let current_history_image ~(model : Model.t) ~(w : int) =
    match Model.history_image_cache model with
    | Some cache
      when Int.equal cache.width w
           && Int.equal cache.transcript_generation (Model.transcript_generation model)
           && Int.equal cache.render_generation (Model.render_generation model) ->
      Some cache.image
    | None | Some _ -> None
  ;;

  let exact_history_plan ~(model : Model.t) ~scroll_height ~image =
    let geometry = Model.chat_render_geometry model in
    let viewport =
      Renderer_virtual_list.Viewport.compute
        ~geometry
        ~requested_scroll:(Notty_scroll_box.scroll (Model.scroll_box model))
        ~height:scroll_height
        ~follow_bottom:(Model.auto_follow model)
    in
    let top_visible_idx =
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
    in
    { Renderer_component_history.image; viewport; top_visible_idx; prefetch_indices = [] }
  ;;

  let sticky_header_blank ~(w : int) = I.hsnap ~align:`Left w (I.string A.empty "")

  let sticky_header_for_index
        ~(model : Model.t)
        ~(w : int)
        ~hi_engine
        ~messages
        ~selected_idx
        idx
    =
    let role, _ = messages.(idx) in
    let selected = Option.value_map selected_idx ~default:false ~f:(Int.equal idx) in
    let search_query =
      match Model.mode model with
      | Model.Search _ -> Some (Model.search_query model)
      | _ -> None
    in
    Renderer_component_message.render_header_line
      ~width:w
      ~selected
      ~role
      ~hi_engine
      ~search_query
      ()
  ;;

  let sticky_header
        ~(model : Model.t)
        ~(w : int)
        ~hi_engine
        ~messages
        ~selected_idx
        ~sticky_height
        ~top_visible_idx
    =
    if sticky_height <= 0
    then I.empty
    else (
      match top_visible_idx with
      | None -> sticky_header_blank ~w
      | Some idx ->
        sticky_header_for_index ~model ~w ~hi_engine ~messages ~selected_idx idx)
  ;;

  let prepare_history ~(model : Model.t) ~(w : int) ~hi_engine ~scroll_height =
    let initial_anchor = ensure_history_width ~model ~w ~scroll_height in
    let render_message = render_message_fn ~model ~w ~hi_engine in
    let messages = Model.render_messages model in
    let plan =
      match current_history_image ~model ~w with
      | Some image
        when Renderer_virtual_list.Geometry.all_exact (Model.chat_render_geometry model)
        -> exact_history_plan ~model ~scroll_height ~image
      | None | Some _ ->
        history_plan ~initial_anchor ~model ~w ~scroll_height ~messages ~render_message
    in
    update_scroll_box ~model ~scroll_height plan;
    messages, plan
  ;;

  let render_full_with_layout
        ~(size : int * int)
        ~(layout : Chat_page_layout.t)
        ~(model : Model.t)
    : I.t * (int * int)
    =
    let w, h = size in
    let input_img, (cursor_x, cursor_y_in_box) = render_input_box ~w ~layout ~model in
    let history_height = layout.history_height in
    let sticky_height = layout.sticky_height in
    let scroll_height = layout.scroll_height in
    let hi_engine = Renderer_highlight_engine.get () in
    let messages, plan = prepare_history ~model ~w ~hi_engine ~scroll_height in
    let scroll_view =
      Notty_scroll_box.render (Model.scroll_box model) ~width:w ~height:scroll_height
    in
    let sticky_header_img =
      sticky_header
        ~model
        ~w
        ~hi_engine
        ~messages
        ~selected_idx:(Model.selected_msg model)
        ~sticky_height
        ~top_visible_idx:plan.top_visible_idx
    in
    let history_view =
      if sticky_height <= 0
      then scroll_view
      else I.vcat [ sticky_header_img; scroll_view ]
    in
    let status = Renderer_component_status_bar.render ~width:w ~model in
    let full_img = Notty.Infix.(history_view <-> status <-> input_img) in
    let full_img =
      match typeahead_preview_popup ~model ~w ~history_height ~max_popup_h:10 with
      | None -> full_img
      | Some popup -> Notty.Infix.(popup </> full_img)
    in
    full_img, (cursor_x, history_height + 1 + cursor_y_in_box)
  ;;

  let render_full ~size:((w, h) as size) ~model =
    let layout = history_layout ~w ~h ~model in
    render_full_with_layout ~size ~layout ~model
  ;;

  let render_loading ~size:(w, h) ~model =
    let image =
      Renderer_component_loader.render
        ~base_attr:A.(fg (gray 12))
        ~frame:(Model.animation_frame model)
        "Preparing conversation"
      |> I.hsnap ~align:`Middle w
      |> I.vsnap ~align:`Middle h
    in
    image, (0, 0)
  ;;

  let render_resizing ~size:(w, h) ~model =
    let label =
      Model.width_preparation model
      |> Option.bind ~f:Model.width_preparation_destination
      |> Option.value_map ~default:"Resizing conversation" ~f:(fun destination ->
        match destination.reason with
        | Model.Chat_page_state.Destination.Earlier_conversation ->
          "Preparing earlier conversation"
        | Search_result -> "Preparing search result"
        | Latest_conversation -> "Preparing latest conversation")
    in
    let image =
      Renderer_component_loader.render
        ~base_attr:A.(fg (gray 12))
        ~frame:(Model.animation_frame model)
        label
      |> I.hsnap ~align:`Middle w
      |> I.vsnap ~align:`Middle h
    in
    image, (0, 0)
  ;;

  let corridor_history_plan ~(model : Model.t) ~scroll_height ~image =
    exact_history_plan ~model ~scroll_height ~image
  ;;

  let refresh_corridor_overlays ~model cache =
    let messages = Model.render_messages model in
    let row_count = Array.length messages in
    let selected_id = Model.selected_projected_id model in
    let search_query = Model.last_search_query model in
    let geometry = Model.chat_render_geometry model in
    let prefix = Renderer_virtual_list.Geometry.prefix geometry in
    let rows_and_images =
      List.init
        (History_chunk.Range.length cache.Model.Chat_page_state.rows)
        ~f:(fun offset ->
          let index = cache.rows.first + offset in
          let role, text = messages.(index) in
          let id, revision =
            Model.render_row_identity model ~idx:index |> Option.value_exn
          in
          Model.find_img_cache model ~id ~revision
          |> Option.bind ~f:(fun entry ->
            if
              Int.equal entry.width cache.width
              && String.equal entry.role role
              && String.equal entry.text text
            then (
              let selected =
                Option.value_map
                  selected_id
                  ~default:false
                  ~f:(Projected_message.Id.equal id)
              in
              Some
                (Renderer_component_message.apply_overlay
                   ~selected
                   ~search_query:(if selected then search_query else None)
                   entry.layout))
            else None))
      |> Option.all
    in
    Option.iter rows_and_images ~f:(fun row_images ->
      let image =
        I.vcat
          [ I.void cache.width prefix.(cache.rows.first)
          ; I.vcat row_images
          ; I.void cache.width (prefix.(row_count) - prefix.(cache.rows.past))
          ]
      in
      Model.set_corridor_history_image model image)
  ;;

  let render_corridor_with_layout ~size ~layout ~model =
    match Model.corridor_history_cache model with
    | None -> render_resizing ~size ~model
    | Some cache ->
      refresh_corridor_overlays ~model cache;
      let w, _ = size in
      let input_img, (cursor_x, cursor_y_in_box) = render_input_box ~w ~layout ~model in
      let plan =
        corridor_history_plan
          ~model
          ~scroll_height:layout.scroll_height
          ~image:cache.image
      in
      update_scroll_box ~model ~scroll_height:layout.scroll_height plan;
      let scroll_view =
        Notty_scroll_box.render
          (Model.scroll_box model)
          ~width:w
          ~height:layout.scroll_height
      in
      let hi_engine = Renderer_highlight_engine.get () in
      let messages = Model.render_messages model in
      let sticky_header_img =
        sticky_header
          ~model
          ~w
          ~hi_engine
          ~messages
          ~selected_idx:(Model.selected_msg model)
          ~sticky_height:layout.sticky_height
          ~top_visible_idx:plan.top_visible_idx
      in
      let history_view =
        if layout.sticky_height <= 0
        then scroll_view
        else I.vcat [ sticky_header_img; scroll_view ]
      in
      let status = Renderer_component_status_bar.render ~width:w ~model in
      let full_img = Notty.Infix.(history_view <-> status <-> input_img) in
      full_img, (cursor_x, layout.history_height + 1 + cursor_y_in_box)
  ;;
end

let render ~size ~model =
  match Model.chat_materialization model with
  | Loading -> Compose.render_loading ~size ~model
  | Resizing -> Compose.render_resizing ~size ~model
  | Corridor ->
    let w, h = size in
    let layout = Compose.history_layout ~w ~h ~model in
    Compose.render_corridor_with_layout ~size ~layout ~model
  | Warm -> Compose.render_full ~size ~model
;;

let render_with_layout ~size ~layout ~model =
  match Model.chat_materialization model with
  | Loading -> Compose.render_loading ~size ~model
  | Resizing -> Compose.render_resizing ~size ~model
  | Corridor -> Compose.render_corridor_with_layout ~size ~layout ~model
  | Warm -> Compose.render_full_with_layout ~size ~layout ~model
;;

let prepare_startup_history ~size:(w, h) ~model =
  let layout = Compose.history_layout ~w ~h ~model in
  ignore
    (Compose.ensure_history_width ~model ~w ~scroll_height:layout.scroll_height
     : Renderer_virtual_list.Anchor.t option);
  Renderer_component_history.initialize_geometry
    ~geometry:(Model.chat_render_geometry model)
    ~messages:(Model.render_messages model)
    ~width:w
;;

let complete_history_cache_is_current ~w ~model =
  match Model.history_image_cache model with
  | None -> false
  | Some cache ->
    Int.equal cache.width w
    && Int.equal cache.transcript_generation (Model.transcript_generation model)
    && Int.equal cache.render_generation (Model.render_generation model)
;;

let next_power_of_two value =
  let rec loop power = if power >= Int.max 1 value then power else loop (power * 2) in
  loop 1
;;

let compose_history_chunks
      ?old_cache
      ~dirty
      (chunks : Model.Chat_page_state.history_chunk array)
  =
  let chunk_count = Array.length chunks in
  let base = next_power_of_two chunk_count in
  let tree =
    match old_cache with
    | Some (cache : Model.Chat_page_state.history_image_cache)
      when Int.equal cache.chunk_tree_base base && Array.length cache.chunks = chunk_count
      -> Array.copy cache.chunk_tree
    | None | Some _ -> Array.create ~len:(base * 2) I.empty
  in
  let dirty =
    match old_cache with
    | Some cache
      when Int.equal cache.chunk_tree_base base && Array.length cache.chunks = chunk_count
      -> dirty
    | None | Some _ -> List.init chunk_count ~f:Fn.id
  in
  let parents = Int.Hash_set.create () in
  List.iter dirty ~f:(fun index ->
    if index >= 0 && index < chunk_count
    then (
      let leaf = base + index in
      tree.(leaf) <- chunks.(index).image;
      let rec mark parent =
        if parent > 0
        then (
          Hash_set.add parents parent;
          mark (parent / 2))
      in
      mark (leaf / 2)));
  Hash_set.to_list parents
  |> List.sort ~compare:(fun left right -> Int.compare right left)
  |> List.iter ~f:(fun index ->
    tree.(index) <- I.vcat [ tree.(index * 2); tree.((index * 2) + 1) ]);
  tree, base, tree.(1)
;;

let dirty_chunks ~w ~model =
  let messages = Model.render_messages model in
  let chunk_count = History_chunk.canonical_count ~row_count:(Array.length messages) in
  match
    Model.history_image_cache model
    |> Option.filter ~f:(fun cache -> Int.equal cache.width w)
  with
  | None -> List.init chunk_count ~f:Fn.id
  | Some cache ->
    let dirty = Model.take_dirty_history_chunks model in
    if Array.length cache.chunks > chunk_count
    then List.dedup_and_sort ~compare:Int.compare (dirty @ List.init chunk_count ~f:Fn.id)
    else dirty
;;

let complete_history_cache_with_dirty ~w ~model ~dirty =
  let messages = Model.render_messages model in
  let selected_id = Model.selected_projected_id model in
  let search_query = Model.last_search_query model in
  let old_cache =
    Model.history_image_cache model
    |> Option.filter ~f:(fun cache -> Int.equal cache.width w)
  in
  let chunk_count = History_chunk.canonical_count ~row_count:(Array.length messages) in
  let row_and_image idx =
    let role, text = messages.(idx) in
    let id, revision = Model.render_row_identity model ~idx |> Option.value_exn in
    Model.find_img_cache model ~id ~revision
    |> Option.bind ~f:(fun entry ->
      if
        Int.equal entry.width w
        && String.equal entry.role role
        && String.equal entry.text text
      then (
        let selected =
          Option.value_map selected_id ~default:false ~f:(Projected_message.Id.equal id)
        in
        let image =
          Renderer_component_message.apply_overlay
            ~selected
            ~search_query:(if selected then search_query else None)
            entry.layout
        in
        Some ({ Model.Chat_page_state.row_id = id; row_revision = revision }, image))
      else None)
  in
  let chunks =
    match old_cache with
    | Some cache when Array.length cache.chunks = chunk_count -> Array.copy cache.chunks
    | Some cache when Array.length cache.chunks < chunk_count ->
      Array.init chunk_count ~f:(fun index ->
        if index < Array.length cache.chunks
        then cache.chunks.(index)
        else { Model.Chat_page_state.rows = [||]; image = I.empty; height = 0 })
    | None | Some _ ->
      Array.create
        ~len:chunk_count
        { Model.Chat_page_state.rows = [||]; image = I.empty; height = 0 }
  in
  let failed = ref [] in
  List.iter dirty ~f:(fun chunk_index ->
    match
      History_chunk.canonical_range ~row_count:(Array.length messages) ~chunk_index
    with
    | None -> ()
    | Some range ->
      let length = History_chunk.Range.length range in
      let rows_and_images =
        List.init length ~f:(fun offset -> row_and_image (range.first + offset))
        |> Option.all
      in
      (match rows_and_images with
       | None -> failed := chunk_index :: !failed
       | Some rows_and_images ->
         let rows = List.map rows_and_images ~f:fst |> Array.of_list in
         let image = List.map rows_and_images ~f:snd |> I.vcat in
         chunks.(chunk_index)
         <- { Model.Chat_page_state.rows; image; height = I.height image }));
  if List.is_empty !failed
  then (
    let chunk_tree, chunk_tree_base, image =
      compose_history_chunks ?old_cache ~dirty chunks
    in
    Model.set_history_image_cache
      model
      (Some
         { Model.Chat_page_state.width = w
         ; transcript_generation = Model.transcript_generation model
         ; render_generation = Model.render_generation model
         ; chunks
         ; chunk_tree
         ; chunk_tree_base
         ; image
         }))
  else Model.defer_dirty_history_chunks model !failed
;;

let complete_history_cache ~size:(w, _h) ~model =
  if
    (not (complete_history_cache_is_current ~w ~model))
    || Model.has_dirty_history_chunks model
  then (
    let dirty = dirty_chunks ~w ~model in
    complete_history_cache_with_dirty ~w ~model ~dirty)
;;

let rebuild_exact_geometry_from_cache ~model =
  let messages = Model.render_messages model in
  let heights =
    Array.mapi messages ~f:(fun index _ ->
      let id, revision = Model.render_row_identity model ~idx:index |> Option.value_exn in
      let entry = Model.find_img_cache model ~id ~revision |> Option.value_exn in
      entry.height)
  in
  let prefix = Array.create ~len:(Array.length heights + 1) 0 in
  Array.iteri heights ~f:(fun index height ->
    prefix.(index + 1) <- prefix.(index) + height);
  Model.set_chat_render_geometry model ~heights ~prefix
;;

let publish_startup_history ~size ~model =
  rebuild_exact_geometry_from_cache ~model;
  Model.mark_all_history_chunks_dirty model;
  Model.set_history_image_cache model None;
  complete_history_cache ~size ~model;
  Option.is_some (Model.history_image_cache model)
;;

module Materialization_trace = struct
  let enabled = ref false
  let indices = ref []
  let note index = if !enabled then indices := index :: !indices

  let capture f =
    enabled := true;
    indices := [];
    match f () with
    | value ->
      enabled := false;
      value, List.rev !indices
    | exception exn ->
      enabled := false;
      raise exn
  ;;
end

let materialize_history_indices_synchronously ~w ~model indices =
  let hi_engine = Renderer_highlight_engine.get () in
  let grammar_generation = Highlight_registry.generation () in
  let messages = Model.render_messages model in
  List.count indices ~f:(fun idx ->
    if idx >= 0 && idx < Array.length messages
    then (
      Materialization_trace.note idx;
      let role, text = messages.(idx) in
      let message = role, text in
      let id, revision = Model.render_row_identity model ~idx |> Option.value_exn in
      let render ~selected =
        let job =
          Compose.make_render_job
            ~model
            ~w
            ~theme_generation:0
            ~grammar_generation
            ~priority:Visible
            ~idx
            ~selected
            message
        in
        let result =
          Model.find_semantic_cache
            model
            ~id:job.key.row_id
            ~revision:job.key.row_revision
            ~role:job.key.role
            ~text:job.key.text
            ~tool_output:job.key.tool_output
          |> Option.iter ~f:(fun cached ->
            Renderer_component_message.install_prepared
              ~row_id:job.key.row_id
              ~row_revision:job.key.row_revision
              ~role:job.key.role
              ~text:job.key.text
              ~tool_output:job.key.tool_output
              cached.prepared;
            Renderer_component_message.install_highlights cached.highlights);
          Renderer_component_message.render_synchronously ~hi_engine job
        in
        ignore (Model.commit_render_result model result : bool)
      in
      let entry = Model.find_img_cache model ~id ~revision in
      let base_is_ready =
        Option.exists entry ~f:(fun entry ->
          Int.equal entry.width w
          && String.equal entry.role role
          && String.equal entry.text text
          && Chat_message_render_job.Layout_plan.allows entry.layout_plan ~width:w)
      in
      if not base_is_ready then render ~selected:false;
      true)
    else false)
;;

let indices_of_chunks ~message_count chunks =
  List.concat_map chunks ~f:(fun chunk_index ->
    match History_chunk.canonical_range ~row_count:message_count ~chunk_index with
    | None -> []
    | Some range ->
      List.init (History_chunk.Range.length range) ~f:(fun offset -> range.first + offset))
;;

let materialize_history_chunks_synchronously ~w ~model chunks =
  let message_count = Array.length (Model.render_messages model) in
  indices_of_chunks ~message_count chunks
  |> materialize_history_indices_synchronously ~w ~model
;;

let materialize_history_synchronously ~w ~model =
  let message_count = Array.length (Model.render_messages model) in
  List.init message_count ~f:Fn.id |> materialize_history_indices_synchronously ~w ~model
;;

let warm_history_synchronously ~size:(w, _h) ~model =
  if not (complete_history_cache_is_current ~w ~model)
  then (
    let dirty = dirty_chunks ~w ~model in
    ignore (materialize_history_chunks_synchronously ~w ~model dirty : int);
    complete_history_cache_with_dirty ~w ~model ~dirty)
;;

module For_testing = struct
  let capture_materialized_indices f = Materialization_trace.capture f

  let warm_dirty_chunks_synchronously ~size:(w, _h) ~model =
    let dirty = dirty_chunks ~w ~model in
    ignore (materialize_history_chunks_synchronously ~w ~model dirty : int);
    complete_history_cache_with_dirty ~w ~model ~dirty
  ;;

  let history_chunk_count model =
    Model.history_image_cache model
    |> Option.value_map ~default:0 ~f:(fun cache -> Array.length cache.chunks)
  ;;
end

let relayout_history_with_layout_synchronously
      ~width:w
      ~(layout : Chat_page_layout.t)
      ~model
  =
  Live_scroll_trace.emit
    ~phase:"resize_relayout_begin"
    [ "width", `Number (Int.to_string w) ];
  ignore
    (Compose.ensure_history_width ~model ~w ~scroll_height:layout.scroll_height
     : Renderer_virtual_list.Anchor.t option);
  let rows_materialized = materialize_history_synchronously ~w ~model in
  Live_scroll_trace.emit
    ~phase:"resize_relayout_end"
    [ "width", `Number (Int.to_string w)
    ; "rows_materialized", `Number (Int.to_string rows_materialized)
    ]
;;

let relayout_history_synchronously ~size:(w, h) ~model =
  let layout = Compose.history_layout ~w ~h ~model in
  relayout_history_with_layout_synchronously ~width:w ~layout ~model
;;

let startup_background_jobs ~theme_generation ~grammar_generation ~model =
  match Model.active_history_width model with
  | None -> []
  | Some w ->
    let messages = Model.render_messages model in
    Array.filter_mapi messages ~f:(fun idx ((role, text) as message) ->
      let id, revision = Model.render_row_identity model ~idx |> Option.value_exn in
      match Model.find_img_cache model ~id ~revision with
      | Some entry
        when Int.equal entry.width w
             && String.equal entry.role role
             && String.equal entry.text text -> None
      | Some _ | None ->
        Some
          (Compose.make_render_job
             ~model
             ~w
             ~theme_generation
             ~grammar_generation
             ~priority:Background
             ~idx
             ~selected:false
             message))
    |> Array.to_list
;;

let target_width_jobs ~indices ~priority ~model =
  match Model.width_preparation model with
  | None -> []
  | Some preparation ->
    let request_generation = Model.width_preparation_request_generation preparation in
    let target_width = Model.width_preparation_target_width preparation in
    let _, render_generation = Model.width_preparation_generations preparation in
    let theme_generation, grammar_generation =
      Model.width_preparation_highlight_generations preparation
    in
    let messages = Model.render_messages model in
    List.filter_map indices ~f:(fun idx ->
      if idx < 0 || idx >= Array.length messages
      then None
      else (
        let role, text = messages.(idx) in
        let id, revision = Model.render_row_identity model ~idx |> Option.value_exn in
        match Model.find_width_preparation_row model ~request_generation ~id with
        | Some _ -> None
        | None ->
          (match Model.find_cached_width_row model ~width:target_width ~id ~revision with
           | Some entry ->
             ignore
               (Model.set_width_preparation_row model ~request_generation ~id entry
                : bool);
             None
           | None ->
             let job =
               Compose.make_render_job
                 ~model
                 ~w:target_width
                 ~request_generation
                 ~render_generation
                 ~theme_generation
                 ~grammar_generation
                 ~priority
                 ~idx
                 ~selected:false
                 (role, text)
             in
             (match
                Model.find_compatible_layout_row model ~width:target_width ~id ~revision
              with
              | None -> Some job
              | Some entry ->
                let layout =
                  Chat_message_render_job.Layout.
                    { entry.layout with width = target_width }
                in
                let image = Notty.I.hsnap ~align:`Left target_width entry.image in
                let result =
                  Chat_message_render_job.result
                    job
                    ~layout
                    ~image
                    ~layout_plan:entry.layout_plan
                in
                ignore
                  (Model.commit_width_preparation_result
                     model
                     ~theme_generation
                     ~grammar_generation
                     result
                   : bool);
                None))))
;;

let ranked_target_width_batches ~plan ~model =
  List.filter_map plan.Prepared_corridor.batches ~f:(fun batch ->
    match batch.class_ with
    | Prepared_corridor.Remaining -> None
    | Visible | Directional | Guard ->
      let indices =
        List.init (History_chunk.Range.length batch.rows) ~f:(fun offset ->
          batch.rows.first + offset)
      in
      let priority =
        match batch.class_ with
        | Visible -> Chat_message_render_job.Priority.Visible
        | Directional | Guard -> Prefetch
        | Remaining -> Background
      in
      let direction =
        match batch.class_ with
        | Visible -> Chat_render_worker.Neutral
        | Directional -> Preferred
        | Guard -> Opposite
        | Remaining -> Neutral
      in
      let ranked =
        target_width_jobs ~indices ~priority ~model
        |> List.map ~f:(fun job ->
          Chat_render_worker.{ job; distance = batch.distance; direction })
      in
      Some (batch.index, ranked))
;;

let all_ranked_target_width_batches ~plan ~model =
  List.map plan.Prepared_corridor.batches ~f:(fun batch ->
    let indices =
      List.init (History_chunk.Range.length batch.rows) ~f:(fun offset ->
        batch.rows.first + offset)
    in
    let priority, direction =
      match batch.class_ with
      | Prepared_corridor.Visible ->
        Chat_message_render_job.Priority.Visible, Chat_render_worker.Neutral
      | Directional -> Prefetch, Preferred
      | Guard -> Prefetch, Opposite
      | Remaining -> Background, Neutral
    in
    let jobs =
      target_width_jobs ~indices ~priority ~model
      |> List.map ~f:(fun job ->
        Chat_render_worker.{ job; distance = batch.distance; direction })
    in
    batch.index, jobs)
;;

let initial_target_width_batches ?policy ~model () =
  match Model.width_preparation model with
  | None -> None
  | Some preparation ->
    let geometry = Model.width_preparation_active_geometry preparation in
    let requested_scroll, follow_bottom =
      Model.width_preparation_viewport_intent model preparation
    in
    let direction =
      match Model.width_preparation_scroll_direction preparation with
      | Model.Toward_older -> Prepared_corridor.Toward_older
      | Toward_newer -> Toward_newer
    in
    let viewport_height = (Model.width_preparation_layout preparation).scroll_height in
    let plan =
      Prepared_corridor.plan
        ?policy
        ~geometry
        ~requested_scroll
        ~viewport_height
        ~follow_bottom
        ~direction
        ()
    in
    let request_generation = Model.width_preparation_request_generation preparation in
    ignore
      (Model.set_width_preparation_corridors
         model
         ~request_generation
         ~desired:(Some plan.scheduled_rows)
         ~published:None
       : bool);
    Some (plan, ranked_target_width_batches ~plan ~model)
;;

let current_target_width_batches ?policy ~model () =
  match Model.width_preparation model with
  | None -> None
  | Some preparation ->
    let geometry =
      Renderer_virtual_list.Geometry.snapshot (Model.chat_render_geometry model)
    in
    let direction =
      match Model.chat_scroll_direction model with
      | Model.Toward_older -> Prepared_corridor.Toward_older
      | Toward_newer -> Toward_newer
    in
    let viewport_height = (Model.width_preparation_layout preparation).scroll_height in
    let plan =
      Prepared_corridor.plan
        ?policy
        ~geometry
        ~requested_scroll:(Notty_scroll_box.scroll (Model.scroll_box model))
        ~viewport_height
        ~follow_bottom:(Model.auto_follow model)
        ~direction
        ()
    in
    let request_generation = Model.width_preparation_request_generation preparation in
    let desired, published = Model.width_preparation_corridors preparation in
    let desired =
      match desired with
      | None -> plan.scheduled_rows
      | Some desired ->
        History_chunk.Range.create_exn
          ~first:(Int.min desired.first plan.scheduled_rows.first)
          ~past:(Int.max desired.past plan.scheduled_rows.past)
    in
    ignore
      (Model.set_width_preparation_corridors
         model
         ~request_generation
         ~desired:(Some desired)
         ~published
       : bool);
    Some (plan, ranked_target_width_batches ~plan ~model)
;;

let destination_target_width_batches ?policy ~model () =
  match Model.width_preparation model with
  | None -> None
  | Some preparation ->
    (match Model.width_preparation_destination preparation with
     | None -> None
     | Some destination ->
       (match Model.render_index_by_id model ~id:destination.id with
        | None -> None
        | Some index ->
          let _, revision =
            Model.render_row_identity model ~idx:index |> Option.value_exn
          in
          if not (Int.equal revision destination.revision)
          then None
          else (
            let geometry =
              Renderer_virtual_list.Geometry.snapshot (Model.chat_render_geometry model)
            in
            let prefix = Renderer_virtual_list.Geometry.Snapshot.prefix geometry in
            let viewport_height =
              (Model.width_preparation_layout preparation).scroll_height
            in
            let requested_scroll, follow_bottom =
              match destination.placement with
              | Model.Chat_page_state.Destination.Top -> prefix.(index), false
              | Center -> Int.max 0 (prefix.(index) - (viewport_height / 2)), false
              | Bottom ->
                ( prefix.(index + 1) - viewport_height |> Int.max 0
                , Poly.(destination.reason = Latest_conversation) )
            in
            let direction =
              match destination.reason with
              | Earlier_conversation -> Prepared_corridor.Toward_older
              | Search_result ->
                (match Model.chat_scroll_direction model with
                 | Model.Toward_older -> Toward_older
                 | Toward_newer -> Toward_newer)
              | Latest_conversation -> Toward_newer
            in
            let plan =
              Prepared_corridor.plan
                ?policy
                ~geometry
                ~requested_scroll
                ~viewport_height
                ~follow_bottom
                ~direction
                ()
            in
            let request_generation =
              Model.width_preparation_request_generation preparation
            in
            let _, published = Model.width_preparation_corridors preparation in
            ignore
              (Model.set_width_preparation_corridors
                 model
                 ~request_generation
                 ~desired:(Some plan.scheduled_rows)
                 ~published
               : bool);
            Some (plan, ranked_target_width_batches ~plan ~model))))
;;

let remaining_target_width_batches ?policy ~model () =
  match Model.width_preparation model with
  | None -> None
  | Some preparation ->
    let geometry =
      Renderer_virtual_list.Geometry.snapshot (Model.chat_render_geometry model)
    in
    let direction =
      match Model.chat_scroll_direction model with
      | Model.Toward_older -> Prepared_corridor.Toward_older
      | Toward_newer -> Toward_newer
    in
    let viewport_height = (Model.width_preparation_layout preparation).scroll_height in
    let plan =
      Prepared_corridor.plan
        ?policy
        ~geometry
        ~requested_scroll:(Notty_scroll_box.scroll (Model.scroll_box model))
        ~viewport_height
        ~follow_bottom:(Model.auto_follow model)
        ~direction
        ()
    in
    Some (all_ranked_target_width_batches ~plan ~model)
;;

let promote_width_preparation ~size:((w, _) as size) ~model ~request_generation =
  let scroll_height =
    Model.width_preparation model
    |> Option.map ~f:(fun preparation ->
      (Model.width_preparation_layout preparation).scroll_height)
  in
  let anchor =
    Option.map scroll_height ~f:(fun viewport_height ->
      Model.capture_resize_anchor model ~viewport_height)
  in
  if Model.promote_width_preparation_rows model ~request_generation
  then (
    Model.mark_all_history_chunks_dirty model;
    Model.set_history_image_cache model None;
    complete_history_cache ~size ~model;
    match Model.history_image_cache model with
    | None -> false
    | Some _ ->
      Option.iter (Option.both scroll_height anchor) ~f:(fun (viewport_height, anchor) ->
        ignore
          (Model.restore_resize_anchor model ~viewport_height anchor
           : Model.Resize_anchor.resolution));
      Model.finish_width_preparation_promotion model ~request_generation)
  else false
;;
