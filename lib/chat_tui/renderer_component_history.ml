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
  let heights = Model.msg_heights model in
  let prefix = Model.height_prefix model in
  let get_height idx msg =
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
  in
  let ensure_arrays () =
    if Array.length heights <> len || Array.length prefix <> len + 1
    then (
      let heights' = Array.create ~len 0 in
      let prefix' = Array.create ~len:(len + 1) 0 in
      let rec fill i msgs =
        match msgs with
        | [] -> ()
        | msg :: rest ->
          let h = get_height i msg in
          heights'.(i) <- h;
          prefix'.(i + 1) <- prefix'.(i) + h;
          fill (i + 1) rest
      in
      fill 0 messages;
      Model.set_msg_heights model heights';
      Model.set_height_prefix model prefix')
    else (
      match Model.take_and_clear_dirty_height_indices model with
      | [] -> ()
      | dirty ->
        let dirty = List.dedup_and_sort ~compare:Int.compare dirty in
        List.iter dirty ~f:(fun idx ->
          if idx >= 0 && idx < len
          then (
            let msg = List.nth_exn messages idx in
            let old_h = heights.(idx) in
            let new_h = get_height idx msg in
            let delta = new_h - old_h in
            if not (Int.equal delta 0)
            then (
              heights.(idx) <- new_h;
              let n = Array.length prefix - 1 in
              for j = idx + 1 to n do
                prefix.(j) <- prefix.(j) + delta
              done))))
  in
  ensure_arrays ();
  let prefix = Model.height_prefix model in
  let total_height = prefix.(len) in
  let max_scroll = Int.max 0 (total_height - height) in
  let scroll =
    if Model.auto_follow model
    then max_scroll
    else (
      let s = Notty_scroll_box.scroll (Model.scroll_box model) in
      Int.max 0 (Int.min s max_scroll))
  in
  let start_k = bsearch_first_gt prefix ~len:(len + 1) ~target:scroll in
  let start_idx = Int.max 0 (start_k - 1) in
  let end_pos = Int.min total_height (scroll + height) in
  let end_k = bsearch_first_ge prefix ~len:(len + 1) ~target:end_pos in
  let last_idx = Int.max 0 (Int.min (len - 1) (end_k - 1)) in
  let top_blank = if len = 0 then 0 else prefix.(start_idx) in
  let bottom_blank = if len = 0 then 0 else total_height - prefix.(last_idx + 1) in
  let get_img idx msg ~selected =
    match Model.find_img_cache model ~idx with
    | Some entry when entry.width = width && String.equal entry.text (snd msg) ->
      (match selected, entry.img_selected with
       | true, Some img -> img
       | true, None ->
         let img = render_message ~idx ~selected:true msg in
         let entry' =
           { entry with img_selected = Some img; height_selected = Some (I.height img) }
         in
         Model.set_img_cache model ~idx entry';
         img
       | false, _ -> entry.img_unselected)
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
      if Option.value_map selected_idx ~default:false ~f:(Int.equal idx)
      then render_message ~idx ~selected:true msg
      else img_unselected
  in
  let body =
    if len = 0 || last_idx < start_idx
    then I.empty
    else (
      let imgs =
        List.init
          (last_idx - start_idx + 1)
          ~f:(fun off ->
            let idx = start_idx + off in
            let msg = List.nth_exn messages idx in
            let sel = Option.value_map selected_idx ~default:false ~f:(Int.equal idx) in
            get_img idx msg ~selected:sel)
      in
      I.vcat imgs)
  in
  let top_pad = I.void width top_blank in
  let bot_pad = I.void width bottom_blank in
  I.vcat [ top_pad; body; bot_pad ]
;;

let top_visible_index ~(model : Model.t) ~(scroll_height : int) ~(messages : message list)
  : int option
  =
  let len = List.length messages in
  if Int.equal len 0
  then None
  else (
    let prefix = Model.height_prefix model in
    if Array.length prefix < len + 1
    then None
    else (
      let total_height = prefix.(len) in
      let max_scroll = Int.max 0 (total_height - scroll_height) in
      let scroll =
        if Model.auto_follow model
        then max_scroll
        else (
          let s = Notty_scroll_box.scroll (Model.scroll_box model) in
          Int.max 0 (Int.min s max_scroll))
      in
      let k = bsearch_first_gt prefix ~len:(len + 1) ~target:scroll in
      let idx = Int.max 0 (k - 1) in
      if idx >= len
      then None
      else (
        let message_start_y = prefix.(idx) in
        let header_y = message_start_y + 1 in
        let header_vpos = header_y - scroll in
        let exclusion_band = 2 in
        if header_vpos >= 0 && header_vpos < exclusion_band then None else Some idx)))
;;
