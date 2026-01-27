(** Status bar renderer shared by chat and future pages. *)

open Core
open Notty

let render ~width ~(model : Model.t) =
  let bar_attr = A.(bg (gray 2) ++ fg (gray 15)) in
  let mode_txt =
    match Model.mode model with
    | Insert -> "-- INSERT --"
    | Normal -> "-- NORMAL --"
    | Cmdline -> "-- CMD --"
  in
  let raw_txt =
    match Model.draft_mode model with
    | Model.Raw_xml -> " -- RAW --"
    | Model.Plain -> ""
  in
  let text = mode_txt ^ raw_txt in
  let text_img = I.string bar_attr text in
  let pad_w = Int.max 0 (width - I.width text_img) in
  let pad = I.string bar_attr (String.make pad_w ' ') in
  Notty.Infix.(text_img <|> pad)
;;
