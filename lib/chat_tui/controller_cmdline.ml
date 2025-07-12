open Core
open Controller_types

let insert_char model c =
  let buf = Model.cmdline model in
  let pos = Model.cmdline_cursor model in
  let before = String.sub buf ~pos:0 ~len:pos in
  let after = String.sub buf ~pos ~len:(String.length buf - pos) in
  Model.set_cmdline model (before ^ String.of_char c ^ after);
  Model.set_cmdline_cursor model (pos + 1)
;;

let backspace model =
  let buf = Model.cmdline model in
  let pos = Model.cmdline_cursor model in
  if pos > 0
  then (
    let before = String.sub buf ~pos:0 ~len:(pos - 1) in
    let after = String.sub buf ~pos ~len:(String.length buf - pos) in
    Model.set_cmdline model (before ^ after);
    Model.set_cmdline_cursor model (pos - 1))
;;

let execute_command model line : reaction =
  let open String in
  let cmd = lowercase (strip line) in
  (* Leave command-line mode regardless of command *)
  Model.set_mode model Model.Normal;
  Model.set_cmdline model "";
  Model.set_cmdline_cursor model 0;
  match cmd with
  | "q" | "quit" -> Quit
  | "w" -> Submit_input
  | "wq" -> Quit
  | "delete" | "d" ->
    (match Model.selected_msg model with
     | None -> Redraw
     | Some sel_idx ->
       let msgs = Model.messages model in
       if Int.(sel_idx < 0) || Int.(sel_idx >= List.length msgs)
       then Redraw
       else (
         let new_msgs = List.filteri msgs ~f:(fun i _ -> Int.(i <> sel_idx)) in
         Model.set_messages model new_msgs;
         (* Adjust selection to previous message or None *)
         let new_len = List.length new_msgs in
         if Int.(new_len = 0)
         then Model.select_message model None
         else Model.select_message model (Some (Int.min (new_len - 1) sel_idx));
         Redraw))
  | "edit" | "e" ->
    (match Model.selected_msg model with
     | None -> Redraw
     | Some sel_idx ->
       (match List.nth (Model.messages model) sel_idx with
        | None -> Redraw
        | Some (_role, txt) ->
          Model.set_input_line model txt;
          Model.set_cursor_pos model (String.length txt);
          Model.set_mode model Model.Insert;
          (* Enable Raw mode for safe editing as per design.*)
          Model.set_draft_mode model Model.Raw_xml;
          Redraw))
  | _ -> Redraw
;;

let handle_key_cmdline ~(model : Model.t) ~term:_ (ev : Notty.Unescape.event) : reaction =
  match ev with
  | `Key (`Enter, _) -> execute_command model (Model.cmdline model)
  | `Key (`Escape, _) ->
    Model.set_mode model Model.Normal;
    Redraw
  | `Key (`Backspace, _) ->
    backspace model;
    Redraw
  | `Key (`ASCII c, mods) when List.is_empty mods ->
    insert_char model c;
    Redraw
  | `Key (`Arrow `Left, _) ->
    let pos = Model.cmdline_cursor model in
    if pos > 0 then Model.set_cmdline_cursor model (pos - 1);
    Redraw
  | `Key (`Arrow `Right, _) ->
    let pos = Model.cmdline_cursor model in
    if pos < String.length (Model.cmdline model)
    then Model.set_cmdline_cursor model (pos + 1);
    Redraw
  | _ -> Unhandled
;;
