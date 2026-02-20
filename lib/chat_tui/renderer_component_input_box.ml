(** Input box renderer shared by chat and future pages. *)

open Core
open Notty

let ghost_attr = A.(bg (gray 2) ++ fg (gray 15))

let safe_string attr s =
  match I.string attr s with
  | img -> img
  | exception _e -> I.string attr "[error: invalid input]"
;;

let prompt_prefix_and_indent ~(model : Model.t) =
  Input_display.prompt_prefix_and_indent ~model
;;

let is_selection_active ~(model : Model.t) =
  match Model.mode model with
  | Cmdline -> false
  | _ -> Model.selection_active model
;;

let selection_overlap_for_line ~(model : Model.t) ~line_start ~line_end =
  match Model.selection_anchor model with
  | None -> None
  | Some anchor ->
    let cur = Model.cursor_pos model in
    let sel_start = Int.min anchor cur in
    let sel_end = Int.max anchor cur in
    let overlap_start = Int.max sel_start line_start in
    let overlap_end = Int.min sel_end line_end in
    if overlap_start < overlap_end then Some (overlap_start, overlap_end) else None
;;

let content_img_for_line
      ~(text_attr : A.t)
      ~(sel_attr : A.t)
      ~(line_prefix : string)
      ~line
      ~line_start
      ~overlap
  =
  match overlap with
  | None -> safe_string text_attr (line_prefix ^ line)
  | Some (ov_s, ov_e) ->
    let line_len = String.length line in
    let local_start = ov_s - line_start in
    let local_end = ov_e - line_start in
    let before = String.sub line ~pos:0 ~len:local_start in
    let selected = String.sub line ~pos:local_start ~len:(local_end - local_start) in
    let after = String.sub line ~pos:local_end ~len:(line_len - local_end) in
    I.hcat
      [ I.string text_attr line_prefix
      ; I.string text_attr before
      ; I.string sel_attr selected
      ; I.string text_attr after
      ]
;;

let typeahead_ghost_text ~(model : Model.t) : string option =
  if not (Model.typeahead_is_relevant model)
  then None
  else (
    match Model.typeahead_completion model with
    | None -> None
    | Some completion ->
      let completion_text = Util.sanitize ~strip:false completion.text in
      let lines = String.split ~on:'\n' completion_text in
      let first_line =
        match lines with
        | [] -> ""
        | l :: _ -> l
      in
      let hidden_lines = Int.max 0 (List.length lines - 1) in
      let indicator =
        if hidden_lines <= 0
        then ""
        else (
          let ind = "… (+" ^ Int.to_string hidden_lines ^ " more lines)" in
          if String.is_empty first_line then ind else " " ^ ind)
      in
      let ghost = first_line ^ indicator in
      if String.is_empty ghost then None else Some ghost)
;;

let content_img_for_cursor_line
      ~(text_attr : A.t)
      ~(sel_attr : A.t)
      ~(line_prefix : string)
      ~line
      ~line_start
      ~overlap
      ~cursor_byte
      ~(ghost : string)
  =
  let line_len = String.length line in
  let cursor_byte = Int.min cursor_byte line_len in
  let local_overlap =
    match overlap with
    | None -> None
    | Some (ov_s, ov_e) -> Some (ov_s - line_start, ov_e - line_start)
  in
  let breakpoints =
    (match local_overlap with
     | None -> [ 0; cursor_byte; line_len ]
     | Some (ov_s, ov_e) -> [ 0; cursor_byte; ov_s; ov_e; line_len ])
    |> List.filter ~f:(fun i -> i >= 0 && i <= line_len)
    |> List.dedup_and_sort ~compare:Int.compare
  in
  let segments =
    match breakpoints with
    | [] | [ _ ] -> []
    | _ ->
      List.zip_exn (List.drop_last_exn breakpoints) (List.tl_exn breakpoints)
      |> List.filter ~f:(fun (a, b) -> a < b)
  in
  let is_selected (a, b) =
    match local_overlap with
    | None -> false
    | Some (ov_s, ov_e) -> a >= ov_s && b <= ov_e
  in
  let segment_img (a, b) =
    let attr = if is_selected (a, b) then sel_attr else text_attr in
    let s = String.sub line ~pos:a ~len:(b - a) in
    I.string attr s
  in
  let before, after = List.partition_tf segments ~f:(fun (_a, b) -> b <= cursor_byte) in
  I.hcat
    ([ I.string text_attr line_prefix ]
     @ List.map before ~f:segment_img
     @ [ I.string ghost_attr ghost ]
     @ List.map after ~f:segment_img)
;;

let framed_row ~(border_attr : A.t) ~inside =
  Notty.Infix.(I.string border_attr "│" <|> inside <|> I.string border_attr "│")
;;

let render_row
      ~(w : int)
      ~(border_attr : A.t)
      ~(text_attr : A.t)
      ~(sel_attr : A.t)
      ~(model : Model.t)
      ~sel_active
      ~prefix
      ~indent
      ~cursor_row
      ~cursor_byte
      ~ghost_text
      ~idx
      (row : Input_display.row)
  =
  let line_prefix = if idx = 0 then prefix else indent in
  let line = row.text in
  let line_start = row.start in
  let line_end = row.stop in
  let overlap =
    if sel_active then selection_overlap_for_line ~model ~line_start ~line_end else None
  in
  let content_img =
    match idx = cursor_row, ghost_text with
    | true, Some ghost ->
      content_img_for_cursor_line
        ~text_attr
        ~sel_attr
        ~line_prefix
        ~line
        ~line_start
        ~overlap
        ~cursor_byte
        ~ghost
    | false, _ | true, None ->
      content_img_for_line ~text_attr ~sel_attr ~line_prefix ~line ~line_start ~overlap
  in
  let inside = content_img |> I.hsnap ~align:`Left (w - 2) in
  framed_row ~border_attr ~inside
;;

let render_rows
      ~(w : int)
      ~(border_attr : A.t)
      ~(text_attr : A.t)
      ~(sel_attr : A.t)
      ~(model : Model.t)
      ~sel_active
      ~prefix
      ~indent
      ~cursor_row
      ~cursor_byte
      ~ghost_text
      (rows : Input_display.row list)
  =
  let render_row =
    render_row
      ~w
      ~border_attr
      ~text_attr
      ~sel_attr
      ~model
      ~sel_active
      ~prefix
      ~indent
      ~cursor_row
      ~cursor_byte
      ~ghost_text
  in
  List.mapi rows ~f:(fun idx row -> render_row ~idx row)
;;

let hline ~(border_attr : A.t) len =
  let seg = "─" in
  String.concat ~sep:"" (List.init len ~f:(fun _ -> seg)) |> I.string border_attr
;;

let top_border ~(w : int) ~(border_attr : A.t) =
  Notty.Infix.(
    I.string border_attr "┌" <|> hline ~border_attr (w - 2) <|> I.string border_attr "┐")
;;

let bottom_border ~(w : int) ~(border_attr : A.t) =
  Notty.Infix.(
    I.string border_attr "└" <|> hline ~border_attr (w - 2) <|> I.string border_attr "┘")
;;

let cursor_position ~line_prefix ~row ~col =
  let cursor_x = 1 + String.length line_prefix + col in
  let cursor_y = 1 + row in
  cursor_x, cursor_y
;;

module Scrollable = struct
  let clamp_scroll ~content_height ~viewport_height scroll =
    let max_scroll = Int.max 0 (content_height - viewport_height) in
    scroll |> Int.max 0 |> fun s -> Int.min s max_scroll
  ;;

  let scroll_to_make_row_visible ~content_height ~viewport_height ~row =
    if viewport_height <= 0
    then 0
    else (
      let desired = Int.max 0 (row - viewport_height + 1) in
      clamp_scroll ~content_height ~viewport_height desired)
  ;;

  let window rows ~scroll ~height =
    rows |> fun rs -> List.drop rs scroll |> fun rs -> List.take rs height
  ;;
end

let render ~width ~max_height ~(model : Model.t) : I.t * (int * int) =
  let w = width in
  let max_height = Int.max 0 max_height in
  if max_height <= 0
  then I.empty, (0, 0)
  else if max_height < 3
  then (
    let img =
      I.hsnap ~align:`Left w (I.string A.empty "") |> I.vsnap ~align:`Top max_height
    in
    img, (0, 0))
  else (
    let prefix, indent = prompt_prefix_and_indent ~model in
    let ({ rows; cursor } : Input_display.t) =
      Input_display.layout_for_render ~box_width:w ~model
    in
    let cursor_row = cursor.row in
    let cursor_byte = cursor.byte_in_row in
    let cursor_col = cursor.col in
    let ghost_text = typeahead_ghost_text ~model in
    let border_attr = A.(fg (rgb ~r:1 ~g:4 ~b:5)) in
    let text_attr = A.empty in
    let sel_attr = A.(text_attr ++ st reverse) in
    let sel_active = is_selection_active ~model in
    let rendered_rows =
      render_rows
        ~w
        ~border_attr
        ~text_attr
        ~sel_attr
        ~model
        ~sel_active
        ~prefix
        ~indent
        ~cursor_row
        ~cursor_byte
        ~ghost_text
        rows
    in
    let content_height = List.length rendered_rows in
    let viewport_height =
      let total_height = Int.min (content_height + 2) max_height in
      Int.max 0 (total_height - 2)
    in
    let scroll =
      Scrollable.scroll_to_make_row_visible
        ~content_height
        ~viewport_height
        ~row:cursor_row
    in
    let visible_rows = Scrollable.window rendered_rows ~scroll ~height:viewport_height in
    let img =
      I.vcat
        ((top_border ~w ~border_attr :: visible_rows) @ [ bottom_border ~w ~border_attr ])
    in
    let visible_cursor_row = Int.max 0 (cursor_row - scroll) in
    let cursor_prefix = if cursor_row = 0 then prefix else indent in
    ( img
    , cursor_position ~line_prefix:cursor_prefix ~row:visible_cursor_row ~col:cursor_col ))
;;
