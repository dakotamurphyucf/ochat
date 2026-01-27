(** Input box renderer shared by chat and future pages. *)

open Core
open Notty

let input_lines ~(model : Model.t) =
  match Model.mode model with
  | Cmdline -> [ Model.cmdline model ]
  | _ ->
    (match String.split ~on:'\n' (Model.input_line model) with
     | [] -> [ "" ]
     | ls -> ls)
;;

let prompt_prefix_and_indent ~(model : Model.t) =
  match Model.mode model with
  | Cmdline -> ":", ""
  | _ ->
    let prefix = "> " in
    prefix, String.make (String.length prefix) ' '
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
  | None -> I.string text_attr (line_prefix ^ line)
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
      ~idx
      ~abs_off
      ~line
  =
  let line_prefix = if idx = 0 then prefix else indent in
  let line_len = String.length line in
  let line_start = abs_off in
  let line_end = abs_off + line_len in
  let overlap =
    if sel_active then selection_overlap_for_line ~model ~line_start ~line_end else None
  in
  let content_img =
    content_img_for_line ~text_attr ~sel_attr ~line_prefix ~line ~line_start ~overlap
  in
  let inside = content_img |> I.hsnap ~align:`Left (w - 2) in
  let next_abs_off = line_end + 1 in
  framed_row ~border_attr ~inside, next_abs_off
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
      lines
  =
  let render_row =
    render_row ~w ~border_attr ~text_attr ~sel_attr ~model ~sel_active ~prefix ~indent
  in
  let _, rows_rev =
    List.foldi lines ~init:(0, []) ~f:(fun idx (abs_off, acc) line ->
      let row, next_abs_off = render_row ~idx ~abs_off ~line in
      next_abs_off, row :: acc)
  in
  List.rev rows_rev
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

let total_index ~(model : Model.t) =
  match Model.mode model with
  | Cmdline -> Model.cmdline_cursor model
  | _ -> Model.cursor_pos model
;;

let rec row_and_col_in_line lines ~total_index ~offset ~row =
  match lines with
  | [] -> row, 0
  | line :: rest ->
    let len = String.length line in
    if total_index <= offset + len
    then row, total_index - offset
    else row_and_col_in_line rest ~total_index ~offset:(offset + len + 1) ~row:(row + 1)
;;

let cursor_position ~(model : Model.t) ~prefix lines =
  let total_index = total_index ~model in
  let row, col_in_line = row_and_col_in_line lines ~total_index ~offset:0 ~row:0 in
  let cursor_x = 1 + String.length prefix + col_in_line in
  let cursor_y = 1 + row in
  cursor_x, cursor_y
;;

let render ~width ~(model : Model.t) : I.t * (int * int) =
  let w = width in
  let lines = input_lines ~model in
  let prefix, indent = prompt_prefix_and_indent ~model in
  let border_attr = A.(fg (rgb ~r:1 ~g:4 ~b:5)) in
  let text_attr = A.empty in
  let sel_attr = A.(text_attr ++ st reverse) in
  let sel_active = is_selection_active ~model in
  let rows =
    render_rows
      ~w
      ~border_attr
      ~text_attr
      ~sel_attr
      ~model
      ~sel_active
      ~prefix
      ~indent
      lines
  in
  let img =
    I.vcat ((top_border ~w ~border_attr :: rows) @ [ bottom_border ~w ~border_attr ])
  in
  img, cursor_position ~model ~prefix lines
;;
