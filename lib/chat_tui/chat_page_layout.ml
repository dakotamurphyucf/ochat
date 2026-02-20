open Core
open Core

type t =
  { input_box_height : int
  ; history_height : int
  ; sticky_height : int
  ; scroll_height : int
  }

let max_input_box_height ~screen_h =
  let min_history_height = 1 in
  let status_height = 1 in
  Int.max 0 (screen_h - min_history_height - status_height)
;;

let input_display_row_count ~screen_w ~(model : Model.t) =
  let n = Input_display.content_row_count ~box_width:screen_w ~model in
  Int.max 1 n
;;

let compute ~screen_w ~screen_h ~(model : Model.t) =
  let max_input_h = max_input_box_height ~screen_h in
  let uncapped_input_h = input_display_row_count ~screen_w ~model + 2 in
  let input_box_height = Int.min uncapped_input_h max_input_h in
  let status_height = 1 in
  let history_height = Int.max 1 (screen_h - input_box_height - status_height) in
  let sticky_height = if history_height > 1 then 1 else 0 in
  let scroll_height = history_height - sticky_height in
  { input_box_height; history_height; sticky_height; scroll_height }
;;
