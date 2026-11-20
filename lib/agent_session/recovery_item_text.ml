open! Core
module P = Agent_protocol
module Source = Recovery_source_reader
module Work = Recovery_source_work
module Payload = History_entry.Payload
module D = Document_schema

type selection =
  | Canonical of P.Recovery_window.Citation.t
  | Prepared of
      { citation : P.Recovery_window.Citation.t
      ; source_revision : int64
      }

type view = P.Recovery_history_request.view =
  | Ordinary
  | Selected_developer
  | Selected_tool_result
[@@deriving equal]

type role = P.Recovery_history_request.role =
  | User
  | Assistant
  | Developer
  | Tool_result
[@@deriving equal, sexp_of]

type t =
  { role : role
  ; parts : P.History_text.t list
  }

let role t = t.role
let parts t = t.parts
let max_item_text_bytes = 1_048_576

let admit_part work ~index ~remaining text =
  let bytes = String.length text in
  if bytes > remaining
  then Error Source.Limit
  else (
    Work.charge_selected work bytes;
    P.History_text.create ~index ~text |> Result.map_error ~f:(fun _ -> Source.Corrupt))
;;

let text_parts work parts ~max_text_bytes =
  let rec loop parts ~index ~remaining ~reversed =
    match parts with
    | [] -> Ok (List.rev reversed)
    | _ :: _ when index >= 4096 -> Error Source.Limit
    | part :: rest ->
      Work.charge_source work 1;
      (match part with
       | Payload.Content.Text { text; _ } | Refusal text ->
         (match admit_part work ~index ~remaining text with
          | Error _ as error -> error
          | Ok selected ->
            loop
              rest
              ~index:(index + 1)
              ~remaining:(remaining - String.length text)
              ~reversed:(selected :: reversed))
       | Image _ | Unknown _ -> loop rest ~index:(index + 1) ~remaining ~reversed)
  in
  loop parts ~index:0 ~remaining:max_text_bytes ~reversed:[]
;;

let classify work ~limits ~view (row : P.History.entry) =
  let open Result.Let_syntax in
  let%bind () =
    match row.provenance with
    | Runtime_notification _ | Runtime_authoring _ -> Error Source.Unavailable
    | Canonical | Moderator_inserted | Moderator_replaced _ -> Ok ()
  in
  let%bind bytes =
    Work.measure_json work ~limits row.payload
    |> Result.map_error ~f:(function
      | D.Error.Limit_exceeded _ -> Source.Limit
      | _ -> Corrupt)
  in
  Work.charge_source work bytes;
  let%bind entry =
    History_codec.of_protocol row |> Result.map_error ~f:(fun _ -> Source.Corrupt)
  in
  if Context_compaction.History_view.is_reminder entry
  then Error Source.Unavailable
  else (
    match
      view, Payload.Semantic.view (Payload.semantic (History_entry.payload entry))
    with
    | Ordinary, Message { role = User; content; _ } ->
      Ok (User, Payload.Output.Content content)
    | Ordinary, Message { role = Assistant; content; _ } -> Ok (Assistant, Content content)
    | Selected_developer, Message { role = Developer; content; _ } ->
      Ok (Developer, Content content)
    | Selected_tool_result, Result { output; _ } -> Ok (Tool_result, output)
    | _, (Message _ | Call _ | Result _ | Reasoning _ | Unknown _) ->
      Error Source.Unavailable)
;;

let project work ~limits ~view ~max_text_bytes row =
  let open Result.Let_syntax in
  let%bind role, output = classify work ~limits ~view row in
  let%map parts =
    match output with
    | Payload.Output.Content values -> text_parts work values ~max_text_bytes
    | Text value ->
      Result.map (admit_part work ~index:0 ~remaining:max_text_bytes value) ~f:List.return
  in
  { role; parts }
;;

let resolve_selected
      ?allowance
      ?(max_text_bytes = max_item_text_bytes)
      store
      ~handle
      ~current
      ~max_frame_bytes
      ~max_payload_bytes
      selection
      ~view
  =
  if max_payload_bytes <= 0 || max_text_bytes < 0
  then Error Source.Limit
  else
    let open Result.Let_syntax in
    let%bind limits =
      D.Limits.create
        ~max_bytes:(Int.min max_payload_bytes (4 * 1024 * 1024))
        ~max_fields:65_536
        ~max_nodes:131_072
        ~max_depth:64
      |> Result.map_error ~f:(fun _ -> Source.Limit)
    in
    let citation =
      match selection with
      | Canonical c | Prepared { citation = c; _ } -> c
    in
    let current () = current selection view in
    let visible (state : Session_state.t) =
      not
        (Option.exists state.recovery_windows ~f:(fun ledger ->
           Recovery_window_ledger.is_derived_summary ledger citation))
    in
    let resolve allowance =
      let project =
        project
          allowance
          ~limits
          ~view
          ~max_text_bytes:(Int.min max_item_text_bytes max_text_bytes)
      in
      let selected row =
        match project row with
        | Ok projected -> Ok (Some projected)
        | Error Source.Unavailable -> Ok None
        | Error _ as error -> error
      in
      match selection with
      | Canonical citation ->
        Source.read
          ~allowance
          ~visible
          store
          ~handle
          ~current
          ~max_frame_bytes
          citation
          ~project:selected
      | Prepared { citation; source_revision } ->
        let original_selected = ref true in
        let check_replaced_source row =
          Result.map (selected row) ~f:(fun source ->
            original_selected := Option.is_some source)
        in
        Source.read_effective
          ~allowance
          ~visible
          ~check_replaced_source
          store
          ~handle
          ~current
          ~max_frame_bytes
          ~source_revision
          citation
          ~project:(fun row -> if !original_selected then selected row else Ok None)
    in
    match allowance with
    | Some allowance -> resolve allowance
    | None -> Source.with_allowance store ~handle ~f:resolve
;;

let resolve
      ?allowance
      ?max_text_bytes
      store
      ~handle
      ~current
      ~max_frame_bytes
      ~max_payload_bytes
      selection
      ~view
  =
  let open Result.Let_syntax in
  let%bind selected =
    resolve_selected
      ?allowance
      ?max_text_bytes
      store
      ~handle
      ~current
      ~max_frame_bytes
      ~max_payload_bytes
      selection
      ~view
  in
  Result.of_option selected ~error:Source.Unavailable
;;
