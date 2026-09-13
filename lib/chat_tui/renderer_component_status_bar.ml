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
    | Search Forward -> "/" ^ Model.search_query model
    | Search Backward -> "?" ^ Model.search_query model
  in
  let raw_txt =
    match Model.draft_mode model with
    | Model.Raw_xml -> " -- RAW --"
    | Model.Plain -> ""
  in
  let connection_txt =
    match Model.connection_status model with
    | None -> ""
    | Some { Connection_status.phase = Connected; _ } -> "  [connected]"
    | Some { phase = Reconnecting { attempt }; _ } ->
      Printf.sprintf "  [reconnecting %d]" attempt
    | Some { phase = Disconnected; _ } -> "  [disconnected]"
    | Some { phase = Failed failure; _ } ->
      "  [connection failed: " ^ Agent_protocol.Error.code_to_string failure.code ^ "]"
  in
  let base = I.string bar_attr (mode_txt ^ raw_txt ^ connection_txt) in
  let activity =
    match Renderer_component_loader.status_text model with
    | None -> I.empty
    | Some text ->
      I.hcat
        [ I.string bar_attr "  "
        ; Renderer_component_loader.render
            ~base_attr:bar_attr
            ~frame:(Model.animation_frame model)
            text
        ]
  in
  let hint =
    if Model.typeahead_is_relevant model
    then I.string bar_attr ("  " ^ hint_text)
    else I.empty
  in
  let width = Int.max 0 width in
  let typeahead =
    Option.value_map (Model.typeahead_status model) ~default:I.empty ~f:(fun status ->
      I.string bar_attr ("  [" ^ status ^ "]"))
  in
  let work =
    match Model.session_work model with
    | None -> I.empty
    | Some view ->
      I.string bar_attr (Printf.sprintf "  [%d jobs · :work]" view.active_jobs)
  in
  I.hcat [ base; activity; work; typeahead; hint ] |> I.hsnap ~align:`Left width
;;
