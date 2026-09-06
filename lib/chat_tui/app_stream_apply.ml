open Core
module Model = Model
module Redraw_throttle = Redraw_throttle
module Stream_handler = Stream
module Res = Openai.Responses
module Res_item = Res.Item
module Res_stream = Res.Response_stream
module Sourced = Chat_response.Sourced_response_event
module History_stream = Chat_response.History_stream_event

let append_history_item_if_output_done
      (runtime : App_runtime.t)
      (history_event : History_stream.t)
  : unit
  =
  let model = runtime.model in
  let add item =
    let entry = History_entry.create_with_id ~id:history_event.entry_id item in
    ignore (Model.add_history_item model entry)
  in
  match history_event.source, history_event.event with
  | Some _, _ -> ()
  | None, Res_stream.Output_item_done { item; _ } ->
    (match item with
     | Res_stream.Item.Output_message om -> add (Res_item.Output_message om)
     | Res_stream.Item.Reasoning r -> add (Res_item.Reasoning r)
     | Res_stream.Item.Function_call fc -> add (Res_item.Function_call fc)
     | Res_stream.Item.Custom_function ct -> add (Res_item.Custom_tool_call ct)
     | _ -> ())
  | None, _ -> ()
;;

let apply_history_stream_event runtime history_event =
  append_history_item_if_output_done runtime history_event
;;

let apply_history_stream_batch runtime history_events =
  List.iter history_events ~f:(append_history_item_if_output_done runtime)
;;

let append_raw_history_item_if_output_done (runtime : App_runtime.t) (ev : Res_stream.t) =
  let add item =
    History_entry.create ~allocator:runtime.history_allocator item
    |> Result.ok_or_failwith
    |> Model.add_history_item runtime.model
    |> ignore
  in
  match ev with
  | Res_stream.Output_item_done { item; _ } ->
    (match item with
     | Res_stream.Item.Output_message om -> add (Res_item.Output_message om)
     | Res_stream.Item.Reasoning r -> add (Res_item.Reasoning r)
     | Res_stream.Item.Function_call fc -> add (Res_item.Function_call fc)
     | Res_stream.Item.Custom_function ct -> add (Res_item.Custom_tool_call ct)
     | _ -> ())
  | _ -> ()
;;

let coalesce_stream_patches (patches : Types.patch list) : Types.patch list =
  let weight = function
    | Types.Ensure_buffer _ -> 0
    | Types.Associate_tool_call _ -> 1
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

let note_assistant_activity model patches =
  let has_running_tool =
    Model.active_agent_calls model
    |> List.exists ~f:(fun call -> Option.is_none (Model.agent_call_outcome call))
  in
  let activity =
    List.fold patches ~init:None ~f:(fun activity -> function
      | Types.Append_text { role = "assistant"; text; _ } when not (String.is_empty text)
        -> Some (if has_running_tool then Model.Working else Model.Writing)
      | Types.Append_text { role = "tool" | "fork"; text; _ }
        when not (String.is_empty text) -> Some Model.Working
      | _ -> activity)
  in
  Option.iter activity ~f:(fun activity ->
    Model.set_activity model (Some (Model.Assistant activity)))
;;

let changed_row_ids model patches =
  List.filter_map patches ~f:(function
    | Types.Ensure_buffer { id; _ }
    | Types.Append_text { id; _ }
    | Types.Set_function_output { id; _ }
    | Types.Update_reasoning_idx { id; _ } -> Model.buffer_row_id model id
    | Types.Associate_tool_call { item_id; _ } -> Model.buffer_row_id model item_id
    | Types.Set_function_name _
    | Types.Add_user_message _
    | Types.Add_placeholder_message _ -> None)
  |> List.dedup_and_sort ~compare:Projected_message.Id.compare
;;

let request_stream_redraw ~model ~viewport_height ~activity_before ~changed_rows throttler
  =
  let activity_changed = not (Poly.equal activity_before (Model.activity model)) in
  let history_is_visible =
    Model.auto_follow model
    || List.is_empty changed_rows
    || List.exists changed_rows ~f:(fun id ->
      match Model.row_viewport_relation model ~viewport_height ~id with
      | Model.Below -> false
      | Above | Visible | Unknown -> true)
  in
  if activity_changed || history_is_visible then Redraw_throttle.request_redraw throttler
;;

let reconcile_target_width runtime changed_rows =
  let model = runtime.App_runtime.model in
  if Option.is_some (Model.width_preparation model)
  then (
    List.iter changed_rows ~f:(fun id -> Model.invalidate_width_preparation_row model ~id);
    Model.reconcile_width_preparation model;
    App_runtime.reprioritize_target_width_batches runtime;
    App_runtime.pump_target_width_completion runtime)
;;

let apply_sourced_stream_event runtime throttler ~viewport_height (sourced : Sourced.t) =
  let model = runtime.App_runtime.model in
  let activity_before = Model.activity model in
  let patches =
    Stream_handler.handle_event
      ~model
      ~parent_call_id:sourced.parent_call_id
      ?entry_id:sourced.entry_id
      sourced.event
  in
  note_assistant_activity model patches;
  ignore (Model.apply_patches model patches);
  let changed_rows = changed_row_ids model patches in
  reconcile_target_width runtime changed_rows;
  request_stream_redraw ~model ~viewport_height ~activity_before ~changed_rows throttler
;;

let apply_sourced_stream_batch runtime throttler ~viewport_height items =
  let model = runtime.App_runtime.model in
  let activity_before = Model.activity model in
  let patches =
    List.concat_map items ~f:(fun (sourced : Sourced.t) ->
      Stream_handler.handle_event
        ~model
        ~parent_call_id:sourced.parent_call_id
        ?entry_id:sourced.entry_id
        sourced.event)
  in
  let patches = coalesce_stream_patches patches in
  note_assistant_activity model patches;
  ignore (Model.apply_patches model patches);
  let changed_rows = changed_row_ids model patches in
  reconcile_target_width runtime changed_rows;
  request_stream_redraw ~model ~viewport_height ~activity_before ~changed_rows throttler
;;

let apply_stream_event runtime throttler ~viewport_height event =
  apply_sourced_stream_event runtime throttler ~viewport_height (Sourced.outer event);
  append_raw_history_item_if_output_done runtime event
;;

let apply_stream_batch runtime throttler ~viewport_height events =
  apply_sourced_stream_batch
    runtime
    throttler
    ~viewport_height
    (List.map events ~f:Sourced.outer);
  List.iter events ~f:(append_raw_history_item_if_output_done runtime)
;;

let apply_tool_output runtime throttler item =
  let model = runtime.App_runtime.model in
  let patches =
    Stream_handler.handle_tool_out
      ~model
      ~entry_id:(History_entry.id item)
      (History_entry.item item)
  in
  ignore (Model.apply_patches model patches);
  let changed_rows = changed_row_ids model patches in
  reconcile_target_width runtime changed_rows;
  ignore (Model.add_history_item model item);
  Redraw_throttle.request_redraw throttler
;;

let replace_history runtime redraw_immediate items =
  let model = runtime.App_runtime.model in
  Model.set_history_items model items;
  ignore (App_runtime.refresh_messages runtime : Model.projection_damage);
  if Option.is_some (Model.width_preparation model)
  then (
    Model.reconcile_width_preparation model;
    App_runtime.reprioritize_target_width_batches runtime;
    App_runtime.pump_target_width_completion runtime);
  redraw_immediate ()
;;
