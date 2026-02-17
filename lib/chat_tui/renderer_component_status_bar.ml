(** Status bar renderer shared by chat and future pages. *)

open Core
open Notty

let render ~width ~(model : Model.t) =
  let bar_attr = A.(bg (gray 2) ++ fg (gray 15)) in
  let hint_text =
    "[Tab accept all] [Shift+Tab accept line] [Ctrl+Space preview] [Esc dismiss]"
  in
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
  let text =
    let base = mode_txt ^ raw_txt in
    if Model.typeahead_is_relevant model then base ^ "  " ^ hint_text else base
  in
  let width = Int.max 0 width in
  I.string bar_attr text |> I.hsnap ~align:`Left width
;;
