open Core

(* History search is over Model.messages (role, text).
   We do case-insensitive substring match for now. *)

let normalize_query (q : string) = String.strip q
let normalize_haystack (s : string) = String.lowercase s
let normalize_needle (s : string) = String.lowercase s

let message_text_at ~(messages : Types.message array) (i : int) : string option =
  if i < 0 || i >= Array.length messages
  then None
  else (
    let _role, text = messages.(i) in
    Some text)
;;

let matches_query ~messages ~(query : string) (i : int) : bool =
  match message_text_at ~messages i with
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
let start_index ~(model : Model.t) ~(dir : Model.search_dir) ~length : int option =
  let n = length in
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

let find_next ~(model : Model.t) ~(query : string) ~(dir : Model.search_dir)
  : Projected_message.Id.t option
  =
  let query = normalize_query query in
  let messages = Model.render_messages model in
  let n = Array.length messages in
  if n = 0 || String.is_empty query
  then None
  else (
    match start_index ~model ~dir ~length:n with
    | None -> None
    | Some start ->
      (* Wrap-around scan at most n messages *)
      let rec loop i steps_left =
        if steps_left <= 0
        then None
        else if matches_query ~messages ~query i
        then Model.render_row_identity model ~idx:i |> Option.map ~f:fst
        else loop (next_index ~n ~dir i) (steps_left - 1)
      in
      loop start n)
;;

let select_and_reveal ~(model : Model.t) ~term:_ ~(id : Projected_message.Id.t) =
  let idx = Model.render_index_by_id model ~id in
  let direction =
    match Model.selected_msg model, idx with
    | Some current, Some idx when idx < current -> Model.Toward_older
    | Some _, Some _ -> Toward_newer
    | None, _ | _, None ->
      (match Model.last_search_dir model with
       | Some Model.Backward -> Toward_older
       | Some Forward | None -> Toward_newer)
  in
  Model.set_chat_scroll_direction model direction;
  Model.select_projected model (Some id);
  Model.set_auto_follow model false;
  match Model.chat_materialization model, Model.width_preparation model with
  | Model.Chat_page_state.Corridor, Some preparation ->
    let viewport_height = (Model.width_preparation_layout preparation).scroll_height in
    if
      Model.reveal_prepared_row
        model
        ~viewport_height
        ~id
        ~placement:Model.Chat_page_state.Destination.Center
    then Controller_types.Chat_scrolled true
    else Prepare_chat_destination (Search_result id)
  | (Loading | Resizing | Warm), _ | Corridor, None ->
    Model.request_projected_reveal model ~id;
    Redraw
;;

let repeat_last ~(model : Model.t) ~term ~(reverse : bool) =
  match Model.last_search_query model, Model.last_search_dir model with
  | None, _ | _, None -> None
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
     | None -> None
     | Some id ->
       (* Note: keep last_search as the original query+dir0; that's Vim behavior:
         'n' repeats same direction, 'N' repeats opposite without overwriting. *)
       Some (select_and_reveal ~model ~term ~id))
;;
