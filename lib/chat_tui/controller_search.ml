open Core
open Controller_types

let insert_text model text =
  let buf = Model.search_query model in
  let pos = Model.search_cursor model in
  let before = String.sub buf ~pos:0 ~len:pos in
  let after = String.sub buf ~pos ~len:(String.length buf - pos) in
  Model.set_search_query model (before ^ text ^ after);
  Model.set_search_cursor model (pos + String.length text)
;;

let backspace model =
  let buf = Model.search_query model in
  let pos = Model.search_cursor model in
  if pos > 0
  then (
    let previous = Utf8_edit.previous buf pos in
    let before = String.sub buf ~pos:0 ~len:previous in
    let after = String.sub buf ~pos ~len:(String.length buf - pos) in
    Model.set_search_query model (before ^ after);
    Model.set_search_cursor model previous)
;;

let execute_search ~(model : Model.t) ~term (dir : Model.search_dir) : reaction =
  let q = Model.search_query model in
  (* leave search mode regardless *)
  Model.set_mode model Model.Normal;
  Model.set_search_cursor model 0;
  match Controller_history_search.find_next ~model ~query:q ~dir with
  | None -> Redraw
  | Some id ->
    Model.set_last_search model ~query:q ~dir;
    Controller_history_search.select_and_reveal ~model ~term ~id
;;

let handle_key_search ~(model : Model.t) ~term (ev : Notty.Unescape.event) : reaction =
  let dir =
    match Model.mode model with
    | Model.Search d -> d
    | _ -> Model.Forward
  in
  match ev with
  | `Key (`Escape, _) ->
    Model.set_mode model Model.Normal;
    Redraw
  | `Key (`Enter, _) -> execute_search ~model ~term dir
  | `Key (`Backspace, _) ->
    backspace model;
    Redraw
  | `Key (`Arrow `Left, _) ->
    let pos = Model.search_cursor model in
    Model.set_search_cursor model (Utf8_edit.previous (Model.search_query model) pos);
    Redraw
  | `Key (`Arrow `Right, _) ->
    let pos = Model.search_cursor model in
    Model.set_search_cursor model (Utf8_edit.next (Model.search_query model) pos);
    Redraw
  | `Key (`ASCII c, mods) when List.is_empty mods && Char.to_int c >= 0x20 ->
    insert_text model (String.of_char c);
    Redraw
  | `Key (`Uchar u, []) ->
    insert_text model (Utf8_edit.uchar u);
    Redraw
  | _ -> Unhandled
;;
