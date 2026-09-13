open Core
open Notty

let chrome = A.(bg (gray 2) ++ fg (gray 15))
let muted = A.(fg (gray 12))
let active = A.(Highlight_styles.fg_hex "#13A3F2" ++ st bold)

let row ~width attr text =
  I.string attr (Util.sanitize ~strip:false text)
  |> I.hsnap ~align:`Left (Int.max 0 width)
;;

let body_height height = Int.max 0 (height - 2)
let visible_rows height = Int.max 1 ((body_height height + 1) / 2)

let render ~size:(width, height) ~model =
  let width = Int.max 0 width in
  let height = Int.max 0 height in
  let view = Model.session_work model in
  let header =
    match view with
    | None -> "Session work"
    | Some view ->
      Printf.sprintf
        "Session work · %d active jobs · %d completions pending"
        view.active_jobs
        view.pending_completions
  in
  let header =
    match Model.connection_status model with
    | Some { phase = Reconnecting _ | Disconnected | Failed _; _ } ->
      header ^ " · last known state (offline)"
    | None | Some { phase = Connected; _ } -> header
  in
  let content =
    match view with
    | None -> row ~width muted "Work status is available when attached to a session."
    | Some view when Array.is_empty view.rows ->
      row ~width muted "No work is visible for this session."
    | Some view ->
      let count = Array.length view.rows in
      let offset =
        Int.min (Model.work_offset model) (Int.max 0 (count - visible_rows height))
        |> Int.max 0
      in
      Model.set_work_offset model offset;
      Array.sub
        view.rows
        ~pos:offset
        ~len:(Int.min (visible_rows height) (count - offset))
      |> Array.to_list
      |> List.concat_map ~f:(fun item ->
        let attr =
          match item.Agent_work_view.active with
          | true -> active
          | false -> A.empty
        in
        [ row ~width attr (item.label ^ " · " ^ item.status)
        ; row ~width muted ("  " ^ item.key)
        ])
      |> I.vcat
  in
  let footer = row ~width chrome "j/k ↑↓ scroll · PgUp/PgDn page · Esc chat" in
  let regions =
    match height with
    | 0 -> []
    | 1 -> [ row ~width chrome header ]
    | _ ->
      [ row ~width chrome header
      ; I.vsnap ~align:`Top (body_height height) content
      ; footer
      ]
  in
  I.vcat regions |> I.hsnap width |> I.vsnap height, (0, 0)
;;
