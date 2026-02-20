open Core
open Notty

let safe_string ?(fallback = "") attr s =
  match I.string attr s with
  | img -> img
  | exception _ -> I.string attr fallback
;;

let width_of_piece piece =
  (* Measure everything via Notty to avoid drifting assumptions about width. *)
  I.width (safe_string ~fallback:"�" A.empty piece)
;;

let display_width s = I.width (safe_string ~fallback:"�" A.empty s)

let utf8_len byte =
  if byte land 0x80 = 0
  then 1
  else if byte land 0xE0 = 0xC0
  then 2
  else if byte land 0xF0 = 0xE0
  then 3
  else if byte land 0xF8 = 0xF0
  then 4
  else 1
;;

let next_piece s pos =
  if pos >= String.length s
  then None
  else (
    let code = Char.to_int (String.unsafe_get s pos) in
    let len = utf8_len code |> Int.max 1 in
    let len = Int.min len (String.length s - pos) in
    Some (String.sub s ~pos ~len, pos + len))
;;

type break_kind =
  [ `Newline
  | `Wrap
  | `Eof
  ]

type row =
  { start : int
  ; stop : int (* where to continue splitting from in the source text *)
  ; next_pos : int (* exclusive upper bound for "cursor_pos is on this row" *)
  ; cursor_limit : int
  ; text : string
  ; width : int
  ; break_kind : break_kind
  }

type cursor =
  { row : int
  ; byte_in_row : int
  ; col : int
  }

type t =
  { rows : row list
  ; cursor : cursor
  }

let prompt_prefix_and_indent ~(model : Model.t) =
  match Model.mode model with
  | Cmdline -> ":", ""
  | Insert | Normal ->
    let prefix = "> " in
    prefix, String.make (String.length prefix) ' '
;;

let active_text_and_cursor ~(model : Model.t) =
  match Model.mode model with
  | Cmdline -> Model.cmdline model, Model.cmdline_cursor model
  | Insert | Normal -> Model.input_line model, Model.cursor_pos model
;;

let inside_width ~box_width = Int.max 0 (box_width - 2)

let avail_width_for_row ~inside ~prefix ~indent ~row_index =
  let reserved = if row_index = 0 then display_width prefix else display_width indent in
  Int.max 1 (inside - reserved)
;;

let is_space_piece piece =
  (* Conservative: treat only single-byte ASCII whitespace (except newline)
     as a wrap opportunity. Tabs are said to be converted upstream. *)
  String.length piece = 1
  && Char.is_whitespace piece.[0]
  && not (Char.equal piece.[0] '\n')
;;

let width_of_prefix s ~stop =
  let stop = Int.min stop (String.length s) in
  let rec loop pos acc =
    if pos >= stop
    then acc
    else (
      match next_piece s pos with
      | None -> acc
      | Some (piece, next_pos) ->
        let next_pos = Int.min next_pos stop in
        let w = width_of_piece piece in
        loop next_pos (acc + w))
  in
  loop 0 0
;;

let break_row ~text ~start ~limit : int * int * [ `Newline | `Wrap | `Eof ] =
  let len = String.length text in
  let limit = Int.max 1 limit in
  let rec loop pos cur_w last_space =
    if pos >= len
    then pos, cur_w, `Eof
    else if Char.equal (String.get text pos) '\n'
    then pos, cur_w, `Newline
    else (
      match next_piece text pos with
      | None -> pos, cur_w, `Eof
      | Some (piece, next_pos) ->
        let piece_w = width_of_piece piece in
        if cur_w + piece_w > limit
        then (
          match last_space with
          | Some (space_stop, space_w) when space_stop > start ->
            space_stop, space_w, `Wrap
          | _ ->
            if Int.equal pos start then next_pos, piece_w, `Wrap else pos, cur_w, `Wrap)
        else (
          let last_space =
            if is_space_piece piece then Some (next_pos, cur_w + piece_w) else last_space
          in
          loop next_pos (cur_w + piece_w) last_space))
  in
  loop start 0 None
;;

let next_pos_of_break ~stop : break_kind -> int = function
  | `Newline -> stop + 1
  | `Wrap | `Eof -> stop
;;

let cursor_limit_of_break ~stop : break_kind -> int = function
  | `Wrap -> stop
  | `Newline | `Eof -> stop + 1
;;

let rec build_rows ~box_width ~prefix ~indent ~text ~row_index ~pos acc =
  if pos > String.length text
  then List.rev acc
  else if pos = String.length text
  then (
    let ends_with_newline =
      (not (String.is_empty text))
      && Char.equal (String.get text (String.length text - 1)) '\n'
    in
    (* If the text is empty, or it ends with a newline, show an empty trailing row. *)
    if List.is_empty acc || ends_with_newline
    then (
      let row =
        { start = pos
        ; stop = pos
        ; next_pos = pos
        ; cursor_limit = pos + 1
        ; text = ""
        ; width = 0
        ; break_kind = `Newline
        }
      in
      List.rev (row :: acc))
    else List.rev acc)
  else (
    let inside = inside_width ~box_width in
    let limit = avail_width_for_row ~inside ~prefix ~indent ~row_index in
    let stop, width, reason = break_row ~text ~start:pos ~limit in
    let row_text = String.sub text ~pos ~len:(stop - pos) in
    let next_pos = next_pos_of_break ~stop reason in
    let cursor_limit = cursor_limit_of_break ~stop reason in
    let row =
      { start = pos
      ; stop
      ; next_pos
      ; cursor_limit
      ; text = row_text
      ; width
      ; break_kind = reason
      }
    in
    build_rows
      ~box_width
      ~prefix
      ~indent
      ~text
      ~row_index:(row_index + 1)
      ~pos:next_pos
      (row :: acc))
;;

let rows ~box_width ~(model : Model.t) =
  let prefix, indent = prompt_prefix_and_indent ~model in
  let text, _cursor_pos = active_text_and_cursor ~model in
  build_rows ~box_width ~prefix ~indent ~text ~row_index:0 ~pos:0 []
;;

let content_row_count ~box_width ~model = rows ~box_width ~model |> List.length

let find_cursor_row ~cursor_pos rows =
  let rec loop idx = function
    | [] -> None
    | row :: rest ->
      if cursor_pos < row.cursor_limit then Some (idx, row) else loop (idx + 1) rest
  in
  loop 0 rows
;;

let cursor_for_row ~cursor_pos ~(row : row) ~row_index =
  let byte_in_row =
    cursor_pos - row.start |> Int.max 0 |> fun x -> Int.min x (String.length row.text)
  in
  let col = width_of_prefix row.text ~stop:byte_in_row in
  { row = row_index; byte_in_row; col }
;;

let layout_for_render ~box_width ~(model : Model.t) : t =
  let prefix, indent = prompt_prefix_and_indent ~model in
  let text, cursor_pos = active_text_and_cursor ~model in
  let rows = build_rows ~box_width ~prefix ~indent ~text ~row_index:0 ~pos:0 [] in
  let cursor =
    match find_cursor_row ~cursor_pos rows with
    | Some (row_index, row) -> cursor_for_row ~cursor_pos ~row ~row_index
    | None -> { row = 0; byte_in_row = 0; col = 0 }
  in
  { rows; cursor }
;;

let byte_offset_at_col s ~col =
  let col = Int.max 0 col in
  let rec loop pos cur_w =
    if pos >= String.length s
    then pos
    else if cur_w >= col
    then pos
    else (
      match next_piece s pos with
      | None -> pos
      | Some (piece, next_pos) ->
        let w = width_of_piece piece in
        if cur_w + w > col then pos else loop next_pos (cur_w + w))
  in
  loop 0 0
;;

let cursor_pos_after_vertical_move ~box_width ~(model : Model.t) ~dir =
  match Model.mode model with
  | Cmdline -> None
  | Insert | Normal ->
    let ({ rows; cursor } : t) = layout_for_render ~box_width ~model in
    let target_row = cursor.row + dir in
    if target_row < 0 || target_row >= List.length rows
    then None
    else (
      let row = List.nth_exn rows target_row in
      let local = byte_offset_at_col row.text ~col:cursor.col in
      Some (row.start + local))
;;
