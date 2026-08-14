open Core

let layout ~size ~model =
  let screen_w, screen_h = size () in
  Agent_page_layout.compute ~screen_w ~screen_h ~model
;;

let return_to_chat model =
  Model.set_active_page model Model.Page_id.Chat;
  Controller_types.Redraw
;;

let select model f =
  f model;
  Model.set_agent_auto_follow model true;
  Controller_types.Redraw
;;

let scroll model ~height delta =
  let scroll_box = Model.agent_scroll_box model in
  Model.set_agent_auto_follow model false;
  Notty_scroll_box.scroll_by scroll_box ~height delta;
  if Notty_scroll_box.scroll scroll_box = Notty_scroll_box.max_scroll scroll_box ~height
  then Model.set_agent_auto_follow model true;
  Controller_types.Redraw
;;

let handle_key_with_size ~model ~size = function
  | `Key (`ASCII ('g' | 'G'), [ `Ctrl ]) | `Key (`ASCII '\007', []) | `Key (`Escape, _) ->
    return_to_chat model
  | `Key (`ASCII 'j', []) -> select model Model.select_next_agent_call
  | `Key (`ASCII 'k', []) -> select model Model.select_previous_agent_call
  | `Key (`Arrow `Down, mods)
    when List.is_empty mods || List.mem mods `Ctrl ~equal:Poly.equal ->
    let height = (layout ~size ~model).scroll_height in
    scroll model ~height 1
  | `Key (`Arrow `Up, mods)
    when List.is_empty mods || List.mem mods `Ctrl ~equal:Poly.equal ->
    let height = (layout ~size ~model).scroll_height in
    scroll model ~height (-1)
  | `Mouse (`Press (`Scroll direction), (_x, _y), _mods) ->
    let height = (layout ~size ~model).scroll_height in
    let delta =
      match direction with
      | `Up -> -1
      | `Down -> 1
    in
    scroll model ~height delta
  | `Key (`Page `Down, _) | `Key (`ASCII 'f', [ `Ctrl ]) ->
    let height = (layout ~size ~model).scroll_height in
    scroll model ~height height
  | `Key (`Page `Up, _) | `Key (`ASCII 'b', [ `Ctrl ]) ->
    let height = (layout ~size ~model).scroll_height in
    scroll model ~height (-height)
  | `Key (`Home, _) ->
    Model.set_agent_auto_follow model false;
    Notty_scroll_box.scroll_to_top (Model.agent_scroll_box model);
    Controller_types.Redraw
  | `Key (`End, _) ->
    let height = (layout ~size ~model).scroll_height in
    Model.set_agent_auto_follow model true;
    Notty_scroll_box.scroll_to_bottom (Model.agent_scroll_box model) ~height;
    Controller_types.Redraw
  | _ -> Controller_types.Unhandled
;;

let handle_key ~model ~term =
  handle_key_with_size ~model ~size:(fun () -> Notty_eio.Term.size term)
;;

module For_testing = struct
  let handle_key = handle_key_with_size
end
