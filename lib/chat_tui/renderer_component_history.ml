open Core
open Notty
open Types

let bsearch_first_gt arr ~len ~target =
  let lo = ref 0 in
  let hi = ref len in
  while !lo < !hi do
    let mid = (!lo + !hi) lsr 1 in
    if arr.(mid) <= target then lo := mid + 1 else hi := mid
  done;
  !lo
;;

let bsearch_first_ge arr ~len ~target =
  let lo = ref 0 in
  let hi = ref len in
  while !lo < !hi do
    let mid = (!lo + !hi) lsr 1 in
    if arr.(mid) < target then lo := mid + 1 else hi := mid
  done;
  !lo
;;

let clamp_scroll ~max_scroll scroll = Int.max 0 (Int.min scroll max_scroll)

let scroll_and_max_scroll ~(model : Model.t) ~total_height ~height =
  let max_scroll = Int.max 0 (total_height - height) in
  let scroll =
    if Model.auto_follow model
    then max_scroll
    else clamp_scroll ~max_scroll (Notty_scroll_box.scroll (Model.scroll_box model))
  in
  scroll, max_scroll
;;

let get_height
  ~(model : Model.t)
  ~width
  ~(render_message : idx:int -> selected:bool -> message -> I.t)
  ~idx
  msg
  =
  match Model.find_img_cache model ~idx with
  | Some entry when entry.width = width && String.equal entry.text (snd msg) ->
    entry.height_unselected
  | _ ->
    let img_unselected = render_message ~idx ~selected:false msg in
    let h = I.height img_unselected in
    let entry =
      { Model.width
      ; text = snd msg
      ; img_unselected
      ; height_unselected = h
      ; img_selected = None
      ; height_selected = None
      }
    in
    Model.set_img_cache model ~idx entry;
    h
;;

let recompute_height_arrays
  ~(model : Model.t)
  ~len
  ~(messages : message list)
  ~width
  ~(render_message : idx:int -> selected:bool -> message -> I.t)
  =
  let heights = Array.create ~len 0 in
  let prefix = Array.create ~len:(len + 1) 0 in
  List.iteri messages ~f:(fun idx msg ->
    let h = get_height ~model ~width ~render_message ~idx msg in
    heights.(idx) <- h;
    prefix.(idx + 1) <- prefix.(idx) + h);
  Model.set_msg_heights model heights;
  Model.set_height_prefix model prefix
;;

let apply_delta_to_prefix prefix ~start ~delta =
  let last = Array.length prefix - 1 in
  for j = start to last do
    prefix.(j) <- prefix.(j) + delta
  done
;;

let update_height_for_index
  ~(model : Model.t)
  ~len
  ~(messages : message list)
  ~width
  ~(render_message : idx:int -> selected:bool -> message -> I.t)
  ~(heights : int array)
  ~(prefix : int array)
  ~idx
  =
  if idx >= 0 && idx < len
  then (
    let msg = List.nth_exn messages idx in
    let old_h = heights.(idx) in
    let new_h = get_height ~model ~width ~render_message ~idx msg in
    let delta = new_h - old_h in
    if not (Int.equal delta 0)
    then (
      heights.(idx) <- new_h;
      apply_delta_to_prefix prefix ~start:(idx + 1) ~delta))
;;

let ensure_height_arrays
  ~(model : Model.t)
  ~len
  ~(messages : message list)
  ~width
  ~(render_message : idx:int -> selected:bool -> message -> I.t)
  =
  let heights = Model.msg_heights model in
  let prefix = Model.height_prefix model in
  if Array.length heights <> len || Array.length prefix <> len + 1
  then recompute_height_arrays ~model ~len ~messages ~width ~render_message
  else (
    match Model.take_and_clear_dirty_height_indices model with
    | [] -> ()
    | dirty ->
      let dirty = List.dedup_and_sort ~compare:Int.compare dirty in
      List.iter dirty ~f:(fun idx ->
        update_height_for_index
          ~model
          ~len
          ~messages
          ~width
          ~render_message
          ~heights
          ~prefix
          ~idx))
;;

let visible_indices ~(prefix : int array) ~len ~total_height ~scroll ~height =
  let start_k = bsearch_first_gt prefix ~len:(len + 1) ~target:scroll in
  let start_idx = Int.max 0 (start_k - 1) in
  let end_pos = Int.min total_height (scroll + height) in
  let end_k = bsearch_first_ge prefix ~len:(len + 1) ~target:end_pos in
  let last_idx = Int.max 0 (Int.min (len - 1) (end_k - 1)) in
  start_idx, last_idx
;;

let top_and_bottom_blank ~(prefix : int array) ~len ~total_height ~start_idx ~last_idx =
  if Int.equal len 0
  then 0, 0
  else (
    let top_blank = prefix.(start_idx) in
    let bottom_blank = total_height - prefix.(last_idx + 1) in
    top_blank, bottom_blank)
;;

let cache_entry_for_miss
  ~(model : Model.t)
  ~width
  ~(render_message : idx:int -> selected:bool -> message -> I.t)
  ~idx
  msg
  =
  let img_unselected = render_message ~idx ~selected:false msg in
  let h = I.height img_unselected in
  let entry =
    { Model.width
    ; text = snd msg
    ; img_unselected
    ; height_unselected = h
    ; img_selected = None
    ; height_selected = None
    }
  in
  Model.set_img_cache model ~idx entry;
  entry
;;

let img_from_cache_hit
  ~(model : Model.t)
  ~(render_message : idx:int -> selected:bool -> message -> I.t)
  ~idx
  msg
  ~selected
  (entry : Model.msg_img_cache)
  =
  match selected, entry.img_selected with
  | false, _ -> entry.img_unselected
  | true, Some img -> img
  | true, None ->
    let img = render_message ~idx ~selected:true msg in
    let entry' =
      { entry with img_selected = Some img; height_selected = Some (I.height img) }
    in
    Model.set_img_cache model ~idx entry';
    img
;;

let get_img ~(model : Model.t) ~width ~render_message ~idx msg ~selected =
  match Model.find_img_cache model ~idx with
  | Some entry when entry.width = width && String.equal entry.text (snd msg) ->
    img_from_cache_hit ~model ~render_message ~idx msg ~selected entry
  | _ ->
    let entry = cache_entry_for_miss ~model ~width ~render_message ~idx msg in
    if selected then render_message ~idx ~selected:true msg else entry.img_unselected
;;

let body_img
  ~(model : Model.t)
  ~width
  ~(messages : message list)
  ~start_idx
  ~last_idx
  ~(selected_idx : int option)
  ~(render_message : idx:int -> selected:bool -> message -> I.t)
  =
  if last_idx < start_idx
  then I.empty
  else (
    let imgs =
      List.init
        (last_idx - start_idx + 1)
        ~f:(fun off ->
          let idx = start_idx + off in
          let msg = List.nth_exn messages idx in
          let selected = Option.value_map selected_idx ~default:false ~f:(Int.equal idx) in
          get_img ~model ~width ~render_message ~idx msg ~selected)
    in
    I.vcat imgs)
;;

let render
      ~(model : Model.t)
      ~(width : int)
      ~(height : int)
      ~(messages : message list)
      ~(selected_idx : int option)
      ~(render_message : idx:int -> selected:bool -> message -> I.t)
  : I.t
  =
  let len = List.length messages in
  ensure_height_arrays ~model ~len ~messages ~width ~render_message;
  let prefix = Model.height_prefix model in
  let total_height = prefix.(len) in
  let scroll, _max_scroll = scroll_and_max_scroll ~model ~total_height ~height in
  let start_idx, last_idx = visible_indices ~prefix ~len ~total_height ~scroll ~height in
  let top_blank, bottom_blank =
    top_and_bottom_blank ~prefix ~len ~total_height ~start_idx ~last_idx
  in
  let body =
    if Int.equal len 0
    then I.empty
    else
      body_img ~model ~width ~messages ~start_idx ~last_idx ~selected_idx ~render_message
  in
  I.vcat [ I.void width top_blank; body; I.void width bottom_blank ]
;;

let top_visible_index ~(model : Model.t) ~(scroll_height : int) ~(messages : message list)
  : int option
  =
  let len = List.length messages in
  if Int.equal len 0 then None else (
    let prefix = Model.height_prefix model in
    if Array.length prefix < len + 1 then None else (
      let total_height = prefix.(len) in
      let scroll, _max_scroll =
        scroll_and_max_scroll ~model ~total_height ~height:scroll_height
      in
      let k = bsearch_first_gt prefix ~len:(len + 1) ~target:scroll in
      let idx = Int.max 0 (k - 1) in
      if idx >= len then None else (
        let message_start_y = prefix.(idx) in
        let header_vpos = (message_start_y + 1) - scroll in
        if header_vpos >= 0 && header_vpos < 2 then None else Some idx)))
;;
