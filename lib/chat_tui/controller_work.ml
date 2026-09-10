open Core

let handle_key_with_size ~model ~size event =
  let count =
    Option.value_map (Model.session_work model) ~default:0 ~f:(fun view ->
      Array.length view.Agent_work_view.rows)
  in
  let _, height = size () in
  let page = Renderer_page_work.visible_rows height in
  let last = Int.max 0 (count - page) in
  let move offset =
    Model.set_work_offset model (Int.max 0 (Int.min last offset));
    Controller_types.Redraw
  in
  match event with
  | `Key (`Escape, _) ->
    Model.set_active_page model Model.Page_id.Chat;
    Controller_types.Redraw
  | `Key (`ASCII 'j', []) | `Key (`Arrow `Down, []) -> move (Model.work_offset model + 1)
  | `Key (`ASCII 'k', []) | `Key (`Arrow `Up, []) -> move (Model.work_offset model - 1)
  | `Key (`Page `Down, _) -> move (Model.work_offset model + page)
  | `Key (`Page `Up, _) -> move (Model.work_offset model - page)
  | `Key (`Home, _) -> move 0
  | `Key (`End, _) -> move last
  | `Mouse (`Press (`Scroll `Down), _, _) -> move (Model.work_offset model + 1)
  | `Mouse (`Press (`Scroll `Up), _, _) -> move (Model.work_offset model - 1)
  | _ -> Controller_types.Unhandled
;;

let handle_key ~model ~term =
  handle_key_with_size ~model ~size:(fun () -> Notty_eio.Term.size term)
;;

module For_testing = struct
  let handle_key = handle_key_with_size
end
