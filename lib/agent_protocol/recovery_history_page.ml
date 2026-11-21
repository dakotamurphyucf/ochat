open! Core
module J = Json_codec
module W = Recovery_wire
module Q = Recovery_history_request
module Citation = Recovery_window.Citation
module Scope = Recovery_window.Scope

type projection =
  | Text_parts of int list
  | Match of
      { part : int
      ; start : int
      ; stop : int
      ; snippet : Search_snippet.t
      }

type row =
  { citation : Citation.t
  ; preparation_revision : int64 option
  ; ordinal : int
  ; role : Q.role
  ; attempt : Recovery_attempt_evidence.t option
  ; projection : projection
  }

type coverage =
  | Complete
  | Source_unavailable
  | Row_limit
  | Position_limit
  | Work_limit
  | Reply_limit

type t =
  { rows : row list
  ; next : Page.Cursor.t option
  ; coverage : coverage
  }

let invalid message = Error (Protocol_error.invalid_request message)
let num n = `Number (Int.to_string n)
let part_index = J.bounded_int ~min:0 ~max:4095
let offset = J.bounded_int ~min:0 ~max:(1024 * 1024)

let snippet_of_json json =
  let open Result.Let_syntax in
  let%bind _ =
    W.fields
      json
      [ "text"
      ; "highlight_start"
      ; "highlight_length"
      ; "truncated_before"
      ; "truncated_after"
      ]
  in
  let%bind s = Search_snippet.of_json json in
  if String.length (Search_snippet.text s) <= 512
  then Ok s
  else invalid "oversized recovery snippet"
;;

let projection_of_fields f =
  let open Result.Let_syntax in
  match J.optional f "text_parts" with
  | Some json ->
    if
      List.exists [ "part"; "start"; "stop"; "snippet" ] ~f:(fun k ->
        Option.is_some (J.optional f k))
    then invalid "conflicting recovery row projections"
    else (
      let%bind parts = J.list part_index json in
      if List.is_sorted_strictly parts ~compare:Int.compare
      then Ok (Text_parts parts)
      else invalid "unordered recovery text parts")
  | None ->
    let%bind part = J.required_as f "part" part_index in
    let%bind start = J.required_as f "start" offset in
    let%bind stop = J.required_as f "stop" offset in
    let%bind snippet = J.required_as f "snippet" snippet_of_json in
    if stop - start = Search_snippet.highlight_length snippet
    then Ok (Match { part; start; stop; snippet })
    else invalid "inconsistent recovery match bounds"
;;

let row_of_json json =
  let open Result.Let_syntax in
  let%bind f =
    W.fields
      json
      [ "citation"
      ; "preparation_revision"
      ; "ordinal"
      ; "role"
      ; "attempt"
      ; "text_parts"
      ; "part"
      ; "start"
      ; "stop"
      ; "snippet"
      ]
  in
  let%bind citation = J.required_as f "citation" Citation.of_json in
  let%bind preparation_revision =
    J.optional_as f "preparation_revision" W.revision_of_json
  in
  let%bind ordinal =
    J.required_as f "ordinal" (J.bounded_int ~min:0 ~max:Int.max_value)
  in
  let%bind role = J.required_as f "role" Q.role_of_json in
  let%bind attempt = J.optional_as f "attempt" Recovery_attempt_evidence.of_json in
  let%bind projection = projection_of_fields f in
  match Citation.version citation, preparation_revision, attempt with
  | Canonical _, None, None | Effective _, Some _, _ ->
    Ok { citation; preparation_revision; ordinal; role; attempt; projection }
  | _ -> invalid "inconsistent recovery item provenance"
;;

let row_to_json r =
  let projection =
    match r.projection with
    | Text_parts parts -> [ "text_parts", `Array (List.map parts ~f:num) ]
    | Match { part; start; stop; snippet } ->
      [ "part", num part
      ; "start", num start
      ; "stop", num stop
      ; "snippet", Search_snippet.to_json snippet
      ]
  in
  `Object
    ([ "citation", Citation.to_json r.citation ]
     @ Option.to_list
         (Option.map r.preparation_revision ~f:(fun n ->
            "preparation_revision", W.revision_to_json n))
     @ [ "ordinal", num r.ordinal; "role", `String (Q.role_name r.role) ]
     @ Option.to_list
         (Option.map r.attempt ~f:(fun a ->
            "attempt", Recovery_attempt_evidence.to_json a))
     @ projection)
;;

let coverage_name = function
  | Complete -> "complete"
  | Source_unavailable -> "source_unavailable"
  | Row_limit -> "row_limit"
  | Position_limit -> "position_limit"
  | Work_limit -> "work_limit"
  | Reply_limit -> "reply_limit"
;;

let coverage_values =
  [ Complete; Source_unavailable; Row_limit; Position_limit; Work_limit; Reply_limit ]
;;

let to_json t =
  `Object
    [ "rows", `Array (List.map t.rows ~f:row_to_json)
    ; "next", Option.value_map t.next ~default:`Null ~f:Page.Cursor.to_json
    ; "coverage", `String (coverage_name t.coverage)
    ]
;;

let limits =
  W.limits ~max_bytes:Recovery_query.max_page_bytes ~max_fields:2048 ~max_nodes:65536
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind _ = W.measure ~limits json in
  let%bind f = W.fields json [ "rows"; "next"; "coverage" ] in
  let%bind rows =
    J.required_as f "rows" (function
      | `Array values when List.length values <= Recovery_query.max_page_size ->
        List.map values ~f:row_of_json |> Result.all
      | _ -> invalid "invalid recovery row count")
  in
  let%bind next =
    J.required_as f "next" (function
      | `Null -> Ok None
      | j -> Result.map (Page.Cursor.of_json j) ~f:Option.some)
  in
  let%bind coverage =
    J.required_as
      f
      "coverage"
      (J.enum
         ~name:"recovery coverage"
         (List.map coverage_values ~f:(fun v -> coverage_name v, v)))
  in
  match next, coverage with
  | None, (Complete | Source_unavailable)
  | Some _, (Row_limit | Position_limit | Work_limit | Reply_limit) ->
    Ok { rows; next; coverage }
  | _ -> invalid "inconsistent recovery page continuation"
;;

let validate t ~(request : Q.t) =
  let matches row =
    let scope = Citation.scope row.citation in
    Session_ref.equal request.session (Scope.session scope)
    && Option.for_all request.window ~f:(Scope.equal scope)
    && Option.for_all request.role ~f:(Q.equal_role row.role)
    && Option.for_all request.ordinals ~f:(fun (lo, hi) ->
      lo <= row.ordinal && row.ordinal < hi)
    && (match request.view, row.role with
        | Ordinary, (User | Assistant)
        | Selected_developer, Developer
        | Selected_tool_result, Tool_result -> true
        | _ -> false)
    && (match request.family, Citation.version row.citation with
        | Canonical, Canonical _ | Effective, Effective _ -> true
        | _ -> false)
    &&
    match request.action, row.projection with
    | List_items, Text_parts _ -> true
    | Search term, Match { snippet; _ } ->
      let selected =
        String.sub
          (Search_snippet.text snippet)
          ~pos:(Search_snippet.highlight_start snippet)
          ~len:(Search_snippet.highlight_length snippet)
      in
      String.Caseless.equal selected (Search_term.text term)
    | _ -> false
  in
  if
    List.length t.rows <= request.page.limit
    && List.for_all t.rows ~f:matches
    && String.length (Jsonaf.to_string (to_json t)) <= request.max_reply_bytes
  then Ok ()
  else invalid "recovery page does not match the requested query"
;;
