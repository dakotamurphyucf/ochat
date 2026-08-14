open Core
open Notty

let selected_attr = A.(bg (gray 5) ++ Highlight_styles.fg_hex "#13A3F2" ++ st bold)
let muted_attr = A.(fg (gray 12))

let safe_string attr text =
  match I.string attr (Util.sanitize ~strip:false text) with
  | image -> image
  | exception _ -> I.string attr ""
;;

let short_id call_id =
  let length = String.length call_id in
  if length <= 8 then call_id else String.suffix call_id 8
;;

let status_glyph = function
  | None -> ""
  | Some Ochat_function.Trace.Returned -> " ✓"
  | Some Raised -> " ✗"
  | Some Cancelled -> " ⊘"
;;

let selected_id model =
  Option.map (Model.selected_agent_call model) ~f:Model.agent_call_id
;;

let cell ~selected ~width call =
  let id = Model.agent_call_id call in
  let is_selected = Option.value_map selected ~default:false ~f:(String.equal id) in
  let marker = if is_selected then ">" else " " in
  let attr = if is_selected then selected_attr else muted_attr in
  let image =
    safe_string
      attr
      (Printf.sprintf
         "%s %s:%s%s"
         marker
         (Model.agent_call_name call)
         (short_id id)
         (status_glyph (Model.agent_call_outcome call)))
  in
  if I.width image > width then I.hsnap ~align:`Left width image else image
;;

let wrapped_rows ~width model =
  let width = Int.max 0 width in
  let separator = safe_string muted_attr "  " in
  let separator_width = I.width separator in
  let flush row rows =
    if List.is_empty row then rows else I.hcat (List.rev row) :: rows
  in
  let rec loop row row_width rows = function
    | [] -> List.rev (flush row rows)
    | image :: rest ->
      let image_width = I.width image in
      let added_width = image_width + if List.is_empty row then 0 else separator_width in
      if (not (List.is_empty row)) && row_width + added_width > width
      then loop [] 0 (flush row rows) (image :: rest)
      else (
        let row = if List.is_empty row then image :: row else image :: separator :: row in
        loop row (row_width + added_width) rows rest)
  in
  let selected = selected_id model in
  Model.active_agent_calls model |> List.map ~f:(cell ~selected ~width) |> loop [] 0 []
;;

let render ~width model =
  match wrapped_rows ~width model with
  | [] -> safe_string muted_attr "" |> I.hsnap ~align:`Left (Int.max 0 width)
  | rows -> List.map rows ~f:(I.hsnap ~align:`Left (Int.max 0 width)) |> I.vcat
;;

let height ~width model = I.height (render ~width model)
