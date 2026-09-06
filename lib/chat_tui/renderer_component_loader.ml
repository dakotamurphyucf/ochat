open Core
open Notty

let has_running_tool model =
  Model.active_agent_calls model
  |> List.exists ~f:(fun call -> Option.is_none (Model.agent_call_outcome call))
;;

let status_text model =
  let label =
    match Model.activity model with
    | None -> None
    | Some Model.Compacting -> Some "Compacting"
    | Some (Model.Assistant assistant_activity) ->
      if has_running_tool model
      then Some "Working"
      else (
        match assistant_activity with
        | Model.Thinking -> Some "Thinking"
        | Model.Writing -> Some "Writing"
        | Model.Working -> Some "Working")
  in
  label
;;

let ease progress = (1. -. Float.cos (progress *. Float.pi)) /. 2.

let shimmer_range ~frame ~length =
  if length <= 1
  then 0, 0
  else (
    let half_cycle_frames = length * 3 in
    let cycle_frames = half_cycle_frames * 2 in
    let cycle_frame = frame mod cycle_frames in
    if cycle_frame < half_cycle_frames
    then (
      let progress = Float.of_int cycle_frame /. Float.of_int (half_cycle_frames - 1) in
      0, Int.of_float ((ease progress *. Float.of_int (length - 1)) +. 0.5))
    else (
      let progress =
        Float.of_int (cycle_frame - half_cycle_frames)
        /. Float.of_int (half_cycle_frames - 1)
      in
      Int.of_float ((ease progress *. Float.of_int (length - 1)) +. 0.5), length - 1))
;;

let%test_unit "shimmer range grows fully before its tail advances" =
  [%test_result: int * int] (shimmer_range ~frame:0 ~length:8) ~expect:(0, 0);
  [%test_result: int * int] (shimmer_range ~frame:24 ~length:8) ~expect:(0, 7);
  [%test_result: int * int] (shimmer_range ~frame:47 ~length:8) ~expect:(7, 7)
;;

let render ~base_attr ~frame text =
  let length = String.length text in
  let first_highlight, last_highlight = shimmer_range ~frame ~length in
  String.to_list text
  |> List.mapi ~f:(fun index char ->
    let attr =
      if index >= first_highlight && index <= last_highlight
      then A.(base_attr ++ fg lightwhite ++ st bold)
      else if index = first_highlight - 1 || index = last_highlight + 1
      then A.(base_attr ++ fg (gray 18))
      else base_attr
    in
    I.string attr (String.of_char char))
  |> I.hcat
;;
