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

let render ~width ~(model : Model.t) : I.t * (int * int) =
  let w = width in
  let input_lines = input_lines ~model in
  let border_attr = A.(fg (rgb ~r:1 ~g:4 ~b:5)) in
  let bg_attr = A.empty in
  let selection_attr base = A.(base ++ st reverse) in
  let prefix, indent =
    match Model.mode model with
    | Cmdline -> ":", ""
    | _ ->
      let p = "> " in
      p, String.make (String.length p) ' '
  in
  let sel_active =
    match Model.mode model with
    | Cmdline -> false
    | _ -> Model.selection_active model
  in
  let rows =
    let text_attr = bg_attr in
    let sel_attr = selection_attr text_attr in
    let rec build lines idx abs_off acc =
      match lines with
      | [] -> List.rev acc
      | line :: rest ->
        let line_prefix = if idx = 0 then prefix else indent in
        let line_len = String.length line in
        let line_start = abs_off in
        let line_end = abs_off + line_len in
        let overlap_start, overlap_end =
          if not sel_active
          then None, None
          else (
            match Model.selection_anchor model with
            | None -> None, None
            | Some anchor ->
              let cur = Model.cursor_pos model in
              let sel_start = Int.min anchor cur in
              let sel_end = Int.max anchor cur in
              let ov_start = Int.max sel_start line_start in
              let ov_end = Int.min sel_end line_end in
              if ov_start < ov_end then Some ov_start, Some ov_end else None, None)
        in
        let content_img =
          match overlap_start, overlap_end with
          | None, _ | _, None -> I.string text_attr (line_prefix ^ line)
          | Some ov_s, Some ov_e ->
            let local_start = ov_s - line_start in
            let local_end = ov_e - line_start in
            let before = String.sub line ~pos:0 ~len:local_start in
            let selected =
              String.sub line ~pos:local_start ~len:(local_end - local_start)
            in
            let after = String.sub line ~pos:local_end ~len:(line_len - local_end) in
            I.hcat
              [ I.string text_attr line_prefix
              ; I.string text_attr before
              ; I.string sel_attr selected
              ; I.string text_attr after
              ]
        in
        let inside = content_img |> I.hsnap ~align:`Left (w - 2) in
        let row_img =
          Notty.Infix.(I.string border_attr "│" <|> inside <|> I.string border_attr "│")
        in
        let next_abs = line_end + 1 in
        build rest (idx + 1) next_abs (row_img :: acc)
    in
    build input_lines 0 0 []
  in
  let hline len =
    let seg = "─" in
    String.concat ~sep:"" (List.init len ~f:(fun _ -> seg)) |> I.string border_attr
  in
  let top_border =
    Notty.Infix.(I.string border_attr "┌" <|> hline (w - 2) <|> I.string border_attr "┐")
  in
  let bottom_border =
    Notty.Infix.(I.string border_attr "└" <|> hline (w - 2) <|> I.string border_attr "┘")
  in
  let img = I.vcat ((top_border :: rows) @ [ bottom_border ]) in
  let total_index =
    match Model.mode model with
    | Cmdline -> Model.cmdline_cursor model
    | _ -> Model.cursor_pos model
  in
  let rec row_col lines offset row =
    match lines with
    | [] -> row, 0
    | l :: ls ->
      let len = String.length l in
      if total_index <= offset + len
      then row, total_index - offset
      else row_col ls (offset + len + 1) (row + 1)
  in
  let row, col_in_line = row_col input_lines 0 0 in
  let cursor_x =
    (match Model.mode model with
     | Cmdline -> 2
     | _ -> 3)
    + col_in_line
  in
  let cursor_y = 1 + row in
  img, (cursor_x, cursor_y)
;;
