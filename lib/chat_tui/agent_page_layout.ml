open Core

type t =
  { header_height : int
  ; selector_height : int
  ; footer_height : int
  ; scroll_height : int
  }

let reserve remaining wanted =
  let height = Int.min remaining wanted in
  height, remaining - height
;;

let compute ~screen_w ~screen_h ~model =
  let remaining = Int.max 0 screen_h in
  let header_height, remaining = reserve remaining 1 in
  let selector_height, remaining =
    reserve remaining (Agent_page_selector.height ~width:screen_w model)
  in
  let footer_height, remaining = reserve remaining 1 in
  { header_height; selector_height; footer_height; scroll_height = remaining }
;;
