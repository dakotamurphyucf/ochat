open Core
open Controller_types
module Scroll_box = Notty_scroll_box

let insert_char model c =
  let buf = Model.search_query model in
  let pos = Model.search_cursor model in
  let before = String.sub buf ~pos:0 ~len:pos in
  let after = String.sub buf ~pos ~len:(String.length buf - pos) in
  Model.set_search_query model (before ^ String.of_char c ^ after);
  Model.set_search_cursor model (pos + 1)
;;

let backspace model =
  let buf = Model.search_query model in
  let pos = Model.search_cursor model in
  if pos > 0
  then (
    let before = String.sub buf ~pos:0 ~len:(pos - 1) in
    let after = String.sub buf ~pos ~len:(String.length buf - pos) in
    Model.set_search_query model (before ^ after);
    Model.set_search_cursor model (pos - 1))
;;

let scroll_to_message ~(model : Model.t) ~term ~(idx : int) : unit =
  (* Best-effort centering using cached heights/prefix arrays.
     Assumes height_prefix.(i) = cumulative height before message i.
     If caches are missing/out of date, we still select the message. *)
  Model.set_auto_follow model false;
  let prefix = Model.height_prefix model in
  let screen_w, screen_h = Notty_eio.Term.size term in
  let layout = Chat_page_layout.compute ~screen_w ~screen_h ~model in
  let height = layout.scroll_height in
  if idx < 0 || idx >= Array.length prefix
  then ()
  else (
    let msg_top = prefix.(idx) in
    let desired = Int.max 0 (msg_top - (height / 2)) in
    let cur = Scroll_box.scroll (Model.scroll_box model) in
    let maxs = Scroll_box.max_scroll (Model.scroll_box model) ~height in
    let desired = Int.min desired maxs in
    Scroll_box.scroll_by (Model.scroll_box model) ~height (desired - cur))
;;

let find_in_messages ~(model : Model.t) ~(query : string) ~(dir : Model.search_dir)
  : int option
  =
  if String.is_empty query
  then None
  else (
    let q = String.lowercase query in
    let msgs = Model.messages model in
    let n = List.length msgs in
    if n = 0
    then None
    else (
      let start =
        match Model.selected_msg model with
        | None ->
          (match dir with
           | Forward -> 0
           | Backward -> n - 1)
        | Some i ->
          (match dir with
           | Forward -> Int.min (n - 1) (i + 1)
           | Backward -> Int.max 0 (i - 1))
      in
      let text_at i =
        match List.nth msgs i with
        | None -> ""
        | Some (_role, txt) -> txt
      in
      let matches i = String.is_substring (String.lowercase (text_at i)) ~substring:q in
      let rec scan i steps_left =
        if steps_left <= 0
        then None
        else if matches i
        then Some i
        else (
          let next =
            match dir with
            | Forward -> (i + 1) mod n
            | Backward -> (i - 1 + n) mod n
          in
          scan next (steps_left - 1))
      in
      scan start n))
;;

let execute_search ~(model : Model.t) ~term (dir : Model.search_dir) : reaction =
  let q = Model.search_query model in
  (* leave search mode regardless *)
  Model.set_mode model Model.Normal;
  Model.set_search_cursor model 0;
  match Controller_history_search.find_next ~model ~query:q ~dir with
  | None -> Redraw
  | Some idx ->
    Model.set_last_search model ~query:q ~dir;
    Controller_history_search.select_and_reveal ~model ~term ~idx;
    Redraw
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
    if pos > 0 then Model.set_search_cursor model (pos - 1);
    Redraw
  | `Key (`Arrow `Right, _) ->
    let pos = Model.search_cursor model in
    if pos < String.length (Model.search_query model)
    then Model.set_search_cursor model (pos + 1);
    Redraw
  | `Key (`ASCII c, mods) when List.is_empty mods && Char.to_int c >= 0x20 ->
    insert_char model c;
    Redraw
  | _ -> Unhandled
;;
