open Core
open Notty
open Types

(* ------------------------------------------------------------------------- *)
(*  Colour scheme                                                            *)
(* ------------------------------------------------------------------------- *)

let attr_of_role = function
  | "assistant" -> A.(fg lightcyan)
  (* User messages are highlighted in yellow to distinguish them from assistant responses. *)
  | "user" -> A.(fg yellow)
  | "developer" -> A.(fg red)
  | "tool" -> A.(fg lightmagenta)
  | "reasoning" -> A.(fg lightblue)
  (* System messages (model instructions, meta information, …) are rendered
     in a dimmed grey so that they stand out from regular assistant output
     without being too prominent. *)
  | "system" -> A.(fg lightblack)
  | "tool_output" -> A.(fg lightgreen)
  | _ -> A.empty
;;

(* ------------------------------------------------------------------------- *)
(*  Word-wrapping & layout                                                   *)
(* ------------------------------------------------------------------------- *)

(* Render a single chat [message] into a boxed [Notty] image.  We mimic the
   "box" helper from [prompts/example_notty_eio.ml]: the textual content is
   word-wrapped, then displayed on top of a coloured background with a one
  -column/row margin on every side.  This yields a cleaner look than the old
   ASCII/Unicode line drawing frame while still keeping messages visually
   separate. *)

let message_to_image ~max_width ((role, text) : message) : I.t =
  (* Skip entirely blank messages (after trimming whitespace). *)
  let trimmed = String.strip text in
  if String.is_empty trimmed then I.empty
  else (
    (* ------------------------------------------------------------------- *)
    (*  Colours                                                            *)
    (* ------------------------------------------------------------------- *)
    let content_attr = attr_of_role role in

    (* ------------------------------------------------------------------- *)
    (*  Word-wrapping                                                     *)
    (* ------------------------------------------------------------------- *)
    (* We leave two columns for interior padding (a single space on each
       side), plus the border itself.  Therefore the usable width for the
       actual content is [max_width - 4]. *)
    let content_width = Int.max 1 (max_width - 4) in

    let prefix = role ^ ": " in
    let indent = String.make (String.length prefix) ' ' in

    let make_lines body pref =
      let limit = Int.max 1 (content_width - String.length pref) in
      match Util.wrap_line ~limit body with
      | [] -> [ pref ]
      | first :: rest -> (pref ^ first) :: List.map rest ~f:(fun l -> indent ^ l)
    in

    let lines =
      String.split_lines text
      |> List.concat_mapi ~f:(fun idx para ->
           let pref = if idx = 0 then prefix else indent in
           make_lines para pref)
    in

    (* Pre-create image per line to get display width. *)
    let line_imgs = List.map lines ~f:(fun l -> I.string content_attr l) in

    let line_widths = List.map line_imgs ~f:I.width in
    let max_line_w = List.fold line_widths ~init:0 ~f:Int.max in

    (* Borders reuse content colour. *)
    let border_attr = content_attr in

    (* Helper: horizontal rule of box-drawing chars, sized by cells. *)
    let hline len =
      let seg = "─" in
      let line = String.concat ~sep:"" (List.init len ~f:(fun _ -> seg)) in
      I.string border_attr line
    in

    let top_border =
      let left = I.string border_attr "┌" in
      let mid = hline (max_line_w + 2) in
      let right = I.string border_attr "┐" in
      Notty.Infix.(left <|> mid <|> right)
    in

    let bottom_border =
      let left = I.string border_attr "└" in
      let mid = hline (max_line_w + 2) in
      let right = I.string border_attr "┘" in
      Notty.Infix.(left <|> mid <|> right)
    in

    (* Build each interior row: │ <space><text><padding><space> │ *)
    let build_row (img, w) =
      let pad_w = max_line_w - w in
      let padding = if pad_w = 0 then I.empty else I.char A.empty ' ' pad_w 1 in
      Notty.Infix.(
        I.string border_attr "│ "
        <|> img
        <|> padding
        <|> I.string border_attr " │")
    in

    let content_rows =
      List.map (List.zip_exn line_imgs line_widths) ~f:build_row |> I.vcat
    in

    Notty.I.vcat [ top_border; content_rows; bottom_border ])
;;

let history_image ~width ~(messages : message list) : I.t =
  messages |> List.map ~f:(message_to_image ~max_width:width) |> I.vcat
;;

(* ------------------------------------------------------------------------- *)
(*  High-level compositor – history viewport + multi-line prompt             *)
(* ------------------------------------------------------------------------- *)

let render_full ~(size : int * int) ~(model : Model.t) : I.t * (int * int) =
  let open Notty in
  let w, h = size in
  (* Prepare / update history image inside the scroll box. *)
  let history_img = history_image ~width:w ~messages:!(Model.messages model) in
  Notty_scroll_box.set_content model.scroll_box history_img;
  (* Keep bottom aligned if [auto_follow] is enabled.  The scroll helpers are
     effect-free apart from mutating the scroll box’s internal offset, which
     is an intended side effect that belongs to the model. *)
  let input_lines =
    match String.split ~on:'\n' !(Model.input_line model) with
    | [] -> [ "" ]
    | ls -> ls
  in
  let input_height = List.length input_lines in
  let history_height = Int.max 1 (h - input_height) in
  if !(Model.auto_follow model)
  then Notty_scroll_box.scroll_to_bottom model.scroll_box ~height:history_height;
  let history_view =
    Notty_scroll_box.render model.scroll_box ~width:w ~height:history_height
  in
  (* Build the multi-line input editor (prefix on first row only). *)
  let input_img =
    let open I in
    let prefix = "> " in
    let indent = String.make (String.length prefix) ' ' in
    let rows =
      List.mapi input_lines ~f:(fun idx line ->
        let txt = if idx = 0 then prefix ^ line else indent ^ line in
        string A.empty txt |> hsnap ~align:`Left w)
    in
    I.vcat rows
  in
  let full_img = Notty.Infix.(history_view <-> input_img) in
  (* Compute cursor position. *)
  let total_index = !(Model.cursor_pos model) in
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
    2 + col_in_line
    (* length of "> " prefix *)
  in
  let cursor_y = history_height + row in
  full_img, (cursor_x, cursor_y)
;;
