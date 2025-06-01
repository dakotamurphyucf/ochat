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
  if String.is_empty trimmed
  then I.empty
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
    let line_imgs =
      List.map lines ~f:(fun l ->
        match I.string content_attr l with
        | s -> s
        | exception e ->
          I.string content_attr (Printf.sprintf "[error: %s]" (Exn.to_string e)))
    in
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
        I.string border_attr "│ " <|> img <|> padding <|> I.string border_attr " │")
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
  let history_img = history_image ~width:w ~messages:(Model.messages model) in
  Notty_scroll_box.set_content (Model.scroll_box model) history_img;
  (* Keep bottom aligned if [auto_follow] is enabled.  The scroll helpers are
     effect-free apart from mutating the scroll box’s internal offset, which
     is an intended side effect that belongs to the model. *)
  let input_lines =
    match String.split ~on:'\n' (Model.input_line model) with
    | [] -> [ "" ]
    | ls -> ls
  in
  (* ---------------------------- Dimensions ---------------------------- *)
  let input_content_height = List.length input_lines in
  let border_rows =
    2
    (* top & bottom of input box *)
  in
  let history_height = Int.max 1 (h - input_content_height - border_rows) in
  if Model.auto_follow model
  then Notty_scroll_box.scroll_to_bottom (Model.scroll_box model) ~height:history_height;
  let history_view =
    Notty_scroll_box.render (Model.scroll_box model) ~width:w ~height:history_height
  in
  (* ------------------------ Input box & BG --------------------------- *)
  let border_attr = A.(fg lightblue) in
  let bg_attr = A.(bg (rgb ~r:1 ~g:1 ~b:2)) in
  (* ---------------- Selection attributes --------------------------- *)
  let selection_attr base = A.(base ++ st reverse) in
  let input_img =
    let open I in
    let prefix = "> " in
    let indent = String.make (String.length prefix) ' ' in
    let sel_active = Model.selection_active model in
    (* We'll iterate lines keeping track of absolute offset *)
    let rows =
      let text_attr = bg_attr in
      let sel_attr = selection_attr text_attr in
      let rec build_rows lines idx abs_off acc =
        match lines with
        | [] -> List.rev acc
        | line :: rest ->
          let line_prefix = if idx = 0 then prefix else indent in
          let line_len = String.length line in
          let line_start = abs_off in
          let line_end = abs_off + line_len in
          (* Determine selection overlap in input coordinates *)
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
          (* Build content segments *)
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
          let row_text_img = content_img |> I.hsnap ~align:`Left (w - 2) in
          let bg = I.char bg_attr ' ' (w - 2) 1 in
          let inside = Notty.Infix.(row_text_img </> bg) in
          let row_img =
            Notty.Infix.(I.string border_attr "│" <|> inside <|> I.string border_attr "│")
          in
          let next_abs =
            line_end + 1
            (* newline *)
          in
          build_rows rest (idx + 1) next_abs (row_img :: acc)
      in
      build_rows input_lines 0 0 []
    in
    let hline len =
      let seg = "─" in
      String.concat ~sep:"" (List.init len ~f:(fun _ -> seg)) |> string border_attr
    in
    let top_border =
      Notty.Infix.(string border_attr "┌" <|> hline (w - 2) <|> string border_attr "┐")
    in
    let bottom_border =
      Notty.Infix.(string border_attr "└" <|> hline (w - 2) <|> string border_attr "┘")
    in
    I.vcat ((top_border :: rows) @ [ bottom_border ])
  in
  let full_img = Notty.Infix.(history_view <-> input_img) in
  (* Compute cursor position. *)
  let total_index = Model.cursor_pos model in
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
  let cursor_x = 3 + col_in_line in
  let cursor_y =
    history_height + 1 + row
    (* +1 for top border *)
  in
  full_img, (cursor_x, cursor_y)
;;
