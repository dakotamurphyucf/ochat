(** Chat page renderer used by {!Chat_tui.Renderer_pages}. *)

open Core
open Notty
open Types

module Compose = struct
  let render_input_box ~(w : int) ~(model : Model.t) =
    Renderer_component_input_box.render ~width:w ~model
  ;;

  let history_layout ~(h : int) ~(input_img : I.t) =
    let history_height = Int.max 1 (h - I.height input_img - 1) in
    let sticky_height = if history_height > 1 then 1 else 0 in
    let scroll_height = history_height - sticky_height in
    history_height, sticky_height, scroll_height
  ;;

  let ensure_history_width ~(model : Model.t) ~(w : int) =
    match Model.last_history_width model with
    | Some prev when Int.equal prev w -> ()
    | _ ->
      Model.clear_all_img_caches model;
      Model.set_last_history_width model (Some w)
  ;;

  let render_message_fn ~(model : Model.t) ~(w : int) ~hi_engine =
    let tool_outputs = Model.tool_output_by_index model in
    fun ~idx ~selected ((role, text) : message) ->
      let tool_output = Hashtbl.find tool_outputs idx in
      Renderer_component_message.render_message
        ~width:w
        ~selected
        ~tool_output
        ~role
        ~text
        ~hi_engine
  ;;

  let history_img
        ~(model : Model.t)
        ~(w : int)
        ~(scroll_height : int)
        ~(messages : message list)
        ~render_message
    =
    Renderer_component_history.render
      ~model
      ~width:w
      ~height:scroll_height
      ~messages
      ~selected_idx:(Model.selected_msg model)
      ~render_message
  ;;

  let update_scroll_box ~(model : Model.t) ~scroll_height history_img =
    Notty_scroll_box.set_content (Model.scroll_box model) history_img;
    if Model.auto_follow model
    then Notty_scroll_box.scroll_to_bottom (Model.scroll_box model) ~height:scroll_height
  ;;

  let sticky_header_blank ~(w : int) = I.hsnap ~align:`Left w (I.string A.empty "")

  let sticky_header_for_index ~(model : Model.t) ~(w : int) ~hi_engine ~messages idx =
    let role, _ = List.nth_exn messages idx in
    let selected =
      Option.value_map (Model.selected_msg model) ~default:false ~f:(Int.equal idx)
    in
    Renderer_component_message.render_header_line ~width:w ~selected ~role ~hi_engine
  ;;

  let sticky_header
        ~(model : Model.t)
        ~(w : int)
        ~hi_engine
        ~messages
        ~sticky_height
        ~top_visible_idx
    =
    if sticky_height <= 0
    then I.empty
    else (
      match top_visible_idx with
      | None -> sticky_header_blank ~w
      | Some idx -> sticky_header_for_index ~model ~w ~hi_engine ~messages idx)
  ;;

  let prepare_history ~(model : Model.t) ~(w : int) ~hi_engine ~scroll_height =
    ensure_history_width ~model ~w;
    let render_message = render_message_fn ~model ~w ~hi_engine in
    let messages = Model.messages model in
    let history_img = history_img ~model ~w ~scroll_height ~messages ~render_message in
    update_scroll_box ~model ~scroll_height history_img;
    messages
  ;;

  let render_full ~(size : int * int) ~(model : Model.t) : I.t * (int * int) =
    let w, h = size in
    let input_img, (cursor_x, cursor_y_in_box) = render_input_box ~w ~model in
    let history_height, sticky_height, scroll_height = history_layout ~h ~input_img in
    let hi_engine = Renderer_highlight_engine.get () in
    let messages = prepare_history ~model ~w ~hi_engine ~scroll_height in
    let top_visible_idx =
      Renderer_component_history.top_visible_index ~model ~scroll_height ~messages
    in
    let scroll_view =
      Notty_scroll_box.render (Model.scroll_box model) ~width:w ~height:scroll_height
    in
    let sticky_header_img =
      sticky_header ~model ~w ~hi_engine ~messages ~sticky_height ~top_visible_idx
    in
    let history_view =
      if sticky_height <= 0
      then scroll_view
      else I.vcat [ sticky_header_img; scroll_view ]
    in
    let status = Renderer_component_status_bar.render ~width:w ~model in
    let full_img = Notty.Infix.(history_view <-> status <-> input_img) in
    full_img, (cursor_x, history_height + 1 + cursor_y_in_box)
  ;;
end

let render ~size ~model = Compose.render_full ~size ~model
