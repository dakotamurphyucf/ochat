open Core
module Scroll_box = Notty_scroll_box

(* History search is over Model.messages (role, text).
   We do case-insensitive substring match for now. *)

let normalize_query (q : string) = String.strip q
let normalize_haystack (s : string) = String.lowercase s
let normalize_needle (s : string) = String.lowercase s

let message_text_at ~(model : Model.t) (i : int) : string option =
  match List.nth (Model.messages model) i with
  | None -> None
  | Some (_role, txt) -> Some txt
;;

let matches_query ~(model : Model.t) ~(query : string) (i : int) : bool =
  match message_text_at ~model i with
  | None -> false
  | Some txt ->
    let needle = normalize_needle query in
    String.is_substring (normalize_haystack txt) ~substring:needle
;;

let next_index ~n ~dir i =
  match (dir : Model.search_dir) with
  | Forward -> (i + 1) mod n
  | Backward -> (i - 1 + n) mod n
;;

(* Determine where to start searching.
   Forward: start after selected msg (or 0 if none)
   Backward: start before selected msg (or last if none) *)
let start_index ~(model : Model.t) ~(dir : Model.search_dir) : int option =
  let n = List.length (Model.messages model) in
  if n = 0
  then None
  else (
    match Model.selected_msg model with
    | None ->
      Some
        (match dir with
         | Forward -> 0
         | Backward -> n - 1)
    | Some i ->
      Some
        (match dir with
         | Forward -> Int.min (n - 1) (i + 1)
         | Backward -> Int.max 0 (i - 1)))
;;

let find_next ~(model : Model.t) ~(query : string) ~(dir : Model.search_dir) : int option =
  let query = normalize_query query in
  let n = List.length (Model.messages model) in
  if n = 0 || String.is_empty query
  then None
  else (
    match start_index ~model ~dir with
    | None -> None
    | Some start ->
      (* Wrap-around scan at most n messages *)
      let rec loop i steps_left =
        if steps_left <= 0
        then None
        else if matches_query ~model ~query i
        then Some i
        else loop (next_index ~n ~dir i) (steps_left - 1)
      in
      loop start n)
;;

let scroll_to_message ~(model : Model.t) ~term ~(idx : int) : unit =
  (* Best-effort scroll using cached per-message prefix heights if available.
     If cache isn't ready, selection will still move; user can scroll manually. *)
  Model.set_auto_follow model false;
  let screen_w, screen_h = Notty_eio.Term.size term in
  let layout = Chat_page_layout.compute ~screen_w ~screen_h ~model in
  let height = layout.scroll_height in
  let prefix = Model.height_prefix model in
  if idx < 0 || idx >= Array.length prefix
  then ()
  else (
    let msg_top = prefix.(idx) in
    (* Center the target message in the viewport if possible *)
    let desired = Int.max 0 (msg_top - (height / 2)) in
    let maxs = Scroll_box.max_scroll (Model.scroll_box model) ~height in
    let desired = Int.min desired maxs in
    let cur = Scroll_box.scroll (Model.scroll_box model) in
    Scroll_box.scroll_by (Model.scroll_box model) ~height (desired - cur))
;;

let select_and_reveal ~(model : Model.t) ~term ~(idx : int) : unit =
  Model.select_message model (Some idx);
  scroll_to_message ~model ~term ~idx
;;

let repeat_last ~(model : Model.t) ~term ~(reverse : bool) : bool =
  match Model.last_search_query model, Model.last_search_dir model with
  | None, _ | _, None -> false
  | Some q, Some dir0 ->
    let dir =
      if reverse
      then (
        match dir0 with
        | Forward -> Model.Backward
        | Backward -> Model.Forward)
      else dir0
    in
    (match find_next ~model ~query:q ~dir with
     | None -> false
     | Some idx ->
       (* Note: keep last_search as the original query+dir0; that's Vim behavior:
         'n' repeats same direction, 'N' repeats opposite without overwriting. *)
       select_and_reveal ~model ~term ~idx;
       true)
;;
