open Core
module Model = Model
module Redraw_throttle = Redraw_throttle
module Stream_handler = Stream
module Res = Openai.Responses
module Res_item = Res.Item
module Res_stream = Res.Response_stream

let append_history_item_if_output_done (model : Model.t) (ev : Res_stream.t) : unit =
  match ev with
  | Res_stream.Output_item_done { item; _ } ->
    (match item with
     | Res_stream.Item.Output_message om ->
       ignore (Model.add_history_item model (Res_item.Output_message om))
     | Res_stream.Item.Reasoning r ->
       ignore (Model.add_history_item model (Res_item.Reasoning r))
     | Res_stream.Item.Function_call fc ->
       ignore (Model.add_history_item model (Res_item.Function_call fc))
     | Res_stream.Item.Custom_function ct ->
       ignore (Model.add_history_item model (Res_item.Custom_tool_call ct))
     | _ -> ())
  | _ -> ()
;;

let coalesce_stream_patches (patches : Types.patch list) : Types.patch list =
  let weight = function
    | Types.Ensure_buffer _ -> 0
    | Types.Set_function_name _ -> 1
    | Types.Update_reasoning_idx _ -> 1
    | Types.Append_text _ -> 2
    | _ -> 3
  in
  let stable_sorted =
    List.mapi patches ~f:(fun i p -> i, p)
    |> List.stable_sort ~compare:(fun (i1, p1) (i2, p2) ->
      match Int.compare (weight p1) (weight p2) with
      | 0 -> Int.compare i1 i2
      | c -> c)
    |> List.map ~f:snd
  in
  let rec coalesce acc = function
    | [] -> List.rev acc
    | Types.Append_text a1 :: Types.Append_text a2 :: rest
      when String.equal a1.id a2.id && String.equal a1.role a2.role ->
      let merged = Types.Append_text { a1 with text = a1.text ^ a2.text } in
      coalesce acc (merged :: rest)
    | p :: rest -> coalesce (p :: acc) rest
  in
  coalesce [] stable_sorted
;;

let apply_stream_event model throttler ev =
  let patches = Stream_handler.handle_event ~model ev in
  ignore (Model.apply_patches model patches);
  append_history_item_if_output_done model ev;
  Redraw_throttle.request_redraw throttler
;;

let apply_stream_batch model throttler items =
  let patches =
    List.concat_map items ~f:(fun ev -> Stream_handler.handle_event ~model ev)
  in
  let patches = coalesce_stream_patches patches in
  ignore (Model.apply_patches model patches);
  List.iter items ~f:(append_history_item_if_output_done model);
  Redraw_throttle.request_redraw throttler
;;

let apply_tool_output model throttler item =
  let patches = Stream_handler.handle_tool_out ~model item in
  ignore (Model.apply_patches model patches);
  ignore (Model.add_history_item model item);
  Redraw_throttle.request_redraw throttler
;;

let replace_history model redraw_immediate items =
  Model.set_history_items model items;
  Model.set_messages model (Conversation.of_history (Model.history_items model));
  Model.rebuild_tool_output_index model;
  redraw_immediate ()
;;
