open Core
open Notty
open Types

(* ------------------------------------------------------------------------- *)
(*  Colour scheme                                                            *)
(* ------------------------------------------------------------------------- *)

let attr_of_role = function
  | "assistant" -> A.(fg lightcyan)
  | "user" -> A.(fg yellow)
  | "developer" -> A.(fg red)
  | "tool" -> A.(fg lightmagenta)
  | "reasoning" -> A.(fg lightblue)
  | "tool_output" -> A.(fg lightgreen)
  | _ -> A.empty
;;

(* ------------------------------------------------------------------------- *)
(*  Word-wrapping & layout                                                   *)
(* ------------------------------------------------------------------------- *)

let message_to_image ~max_width ((role, text) : message) : I.t =
  let attr = attr_of_role role in
  let prefix = role ^ ": " in
  let indent = String.make (String.length prefix) ' ' in
  let make_lines body pref indent limit =
    match Util.wrap_line ~limit body with
    | [] -> [ pref ]
    | first :: rest -> (pref ^ first) :: List.map rest ~f:(fun l -> indent ^ l)
  in
  let paragraphs = String.split_lines text in
  let lines =
    List.concat_mapi paragraphs ~f:(fun idx para ->
      let pref = if idx = 0 then prefix else indent in
      let limit = Int.max 1 (max_width - String.length pref) in
      make_lines para pref indent limit)
  in
  I.vcat (List.map lines ~f:(fun l -> I.string attr l))
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
