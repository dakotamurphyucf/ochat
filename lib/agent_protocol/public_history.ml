open Core
module P = History_entry.Payload
module J = Json_codec

let checked = Projection_codec.string_result
let optional = Projection_codec.optional

let presence name value =
  match value with
  | P.Presence.Absent -> []
  | Null -> [ name, `Null ]
  | Value s -> [ name, `String s ]
;;

let decode_presence fields name =
  match J.optional fields name with
  | None -> Ok P.Presence.Absent
  | Some `Null -> Ok P.Presence.Null
  | Some json -> Result.map (J.string json) ~f:(fun s -> P.Presence.Value s)
;;

module Visible = struct
  type part =
    | Text of string
    | Refusal of string
    | Image of
        { uri : string
        ; detail : string P.Presence.t
        }
    | Redacted_part of { kind : string }
  [@@deriving equal, sexp_of]

  type t =
    | Message of
        { form : P.Semantic.message_form
        ; role : P.Role.t
        ; content : part list
        ; phase : string P.Presence.t
        }
    | Reasoning of { readable_summary : string list }
  [@@deriving equal, sexp_of]

  let of_semantic semantic =
    match P.Semantic.view semantic with
    | Message { form; role; content; phase } ->
      Some
        (Message
           { form
           ; role
           ; phase
           ; content =
               List.map content ~f:(function
                 | P.Content.Text { text; annotations = _; logprobs = _ } -> Text text
                 | Refusal text -> Refusal text
                 | Image { uri; detail } -> Image { uri; detail }
                 | Unknown { kind; raw = _ } -> Redacted_part { kind })
           })
    | Reasoning { readable_summary } -> Some (Reasoning { readable_summary })
    | Call _ | Result _ | Unknown _ -> None
  ;;

  let header = function
    | Message { role; _ } -> Transcript.Header.Message role
    | Reasoning _ -> Transcript.Header.Reasoning
  ;;

  let part_to_json = function
    | Text text -> `Object [ "type", `String "text"; "text", `String text ]
    | Refusal text -> `Object [ "type", `String "refusal"; "text", `String text ]
    | Image { uri; detail } ->
      `Object ([ "type", `String "image"; "uri", `String uri ] @ presence "detail" detail)
    | Redacted_part { kind } ->
      `Object [ "type", `String "redacted"; "kind", `String kind ]
  ;;

  let to_json = function
    | Message { form; role; content; phase } ->
      `Object
        ([ "type", `String "message"
         ; "header", Transcript.Header.to_json (Transcript.Header.Message role)
         ; ( "form"
           , `String
               (match form with
                | Input -> "input"
                | Output -> "output") )
         ; "content", `Array (List.map content ~f:part_to_json)
         ]
         @ presence "phase" phase)
    | Reasoning { readable_summary } ->
      `Object
        [ "type", `String "reasoning"
        ; "readable_summary", `Array (List.map readable_summary ~f:(fun s -> `String s))
        ]
  ;;

  let part_of_json json =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    let%bind kind = J.required_as fields "type" J.string in
    match kind with
    | "text" ->
      let%map text = J.required_as fields "text" J.string in
      Text text
    | "refusal" ->
      let%map text = J.required_as fields "text" J.string in
      Refusal text
    | "image" ->
      let%bind uri = J.required_as fields "uri" J.string in
      let%map detail = decode_presence fields "detail" in
      Image { uri; detail }
    | "redacted" ->
      let%map kind = J.required_as fields "kind" J.string in
      Redacted_part { kind }
    | _ -> Error (Protocol_error.invalid_request "unknown visible history part")
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind () = Projection_codec.validate json in
    let%bind fields = J.fields json in
    let%bind kind = J.required_as fields "type" J.string in
    match kind with
    | "message" ->
      let%bind header =
        J.required_as fields "header" (fun json ->
          checked (Transcript.Header.of_json json))
      in
      let%bind role =
        match header with
        | Transcript.Header.Message role -> Ok role
        | Call _ | Result _ | Reasoning | Unknown _ ->
          Error
            (Protocol_error.invalid_request "visible message requires a message header")
      in
      let%bind form =
        J.required_as
          fields
          "form"
          (J.enum
             ~name:"message form"
             [ "input", P.Semantic.Input; "output", P.Semantic.Output ])
      in
      let%bind content = J.required_as fields "content" (J.list part_of_json) in
      let%bind phase = decode_presence fields "phase" in
      if P.Semantic.equal_message_form form Output && not (P.Role.equal role Assistant)
      then
        Error
          (Protocol_error.invalid_request
             "visible output message must have assistant role")
      else Ok (Message { form; role; content; phase })
    | "reasoning" ->
      let%map readable_summary =
        J.required_as fields "readable_summary" (J.list J.string)
      in
      Reasoning { readable_summary }
    | _ -> Error (Protocol_error.invalid_request "unknown visible history body")
  ;;
end

module Redaction = struct
  type t = { disclosed_header : Transcript.Header.t option } [@@deriving equal, sexp_of]

  let create ~disclosed_header = { disclosed_header }
end

type body =
  | Full of P.t
  | Visible of Visible.t
  | Redacted of Redaction.t
[@@deriving sexp_of]

type t =
  { id : History.Id.t
  ; provenance : History.provenance
  ; body : body
  }
[@@deriving sexp_of]

let body_to_json = function
  | Full payload -> `Object [ "type", `String "full"; "payload", P.to_json payload ]
  | Visible visible ->
    `Object [ "type", `String "visible"; "view", Visible.to_json visible ]
  | Redacted { disclosed_header } ->
    `Object
      ([ "type", `String "redacted" ]
       @ optional "header" disclosed_header Transcript.Header.to_json)
;;

let to_json t =
  `Object
    [ "id", History.Id.to_json t.id
    ; "provenance", History.provenance_to_json t.provenance
    ; "body", body_to_json t.body
    ]
;;

let create id ~provenance body =
  let open Result.Let_syntax in
  let%bind () =
    match body with
    | Full payload -> checked (P.validate payload)
    | Redacted { disclosed_header = Some header } ->
      Result.map
        (checked (Transcript.Header.of_json (Transcript.Header.to_json header)))
        ~f:(fun _ -> ())
    | Visible _ | Redacted { disclosed_header = None } -> Ok ()
  in
  let%bind () =
    match provenance with
    | History.Runtime_authoring guidance -> Authoring_guidance.validate guidance
    | Canonical | Moderator_inserted | Moderator_replaced _ | Runtime_notification _ ->
      Ok ()
  in
  let t = { id; provenance; body } in
  let%map () = Projection_codec.validate (to_json t) in
  t
;;

let full entry ~provenance =
  create (History_entry.id entry) ~provenance (Full (History_entry.payload entry))
;;

let visible id ~provenance view = create id ~provenance (Visible view)
let redacted id ~provenance redaction = create id ~provenance (Redacted redaction)

let header t =
  match t.body with
  | Full payload -> Some (Transcript.Header.of_semantic (P.semantic payload))
  | Visible view -> Some (Visible.header view)
  | Redacted redaction -> redaction.disclosed_header
;;

let full_payload t =
  match t.body with
  | Full p -> Some p
  | Visible _ | Redacted _ -> None
;;

let equal a b =
  History.Id.equal a.id b.id
  && History.equal_provenance a.provenance b.provenance
  &&
  match a.body, b.body with
  | Full a, Full b -> Jsonaf.exactly_equal (P.to_json a) (P.to_json b)
  | Visible a, Visible b -> Visible.equal a b
  | Redacted a, Redacted b -> Redaction.equal a b
  | (Full _ | Visible _ | Redacted _), _ -> false
;;

let validate_unique_ids entries =
  match List.find_a_dup entries ~compare:(fun a b -> History.Id.compare a.id b.id) with
  | None -> Ok ()
  | Some _ -> Error (Protocol_error.invalid_request "duplicate public history identity")
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () = Projection_codec.validate json in
  let%bind fields = J.fields json in
  let%bind id = J.required_as fields "id" History.Id.of_json in
  let%bind provenance = J.required_as fields "provenance" History.provenance_of_json in
  let%bind body = J.required_as fields "body" J.fields in
  let%bind kind = J.required_as body "type" J.string in
  let%bind body =
    match kind with
    | "full" ->
      let%map payload =
        J.required_as body "payload" (fun json -> checked (P.of_json json))
      in
      Full payload
    | "visible" ->
      let%map view = J.required_as body "view" Visible.of_json in
      Visible view
    | "redacted" ->
      let%map disclosed_header =
        J.optional_as body "header" (fun json -> checked (Transcript.Header.of_json json))
      in
      Redacted (Redaction.create ~disclosed_header)
    | _ -> Error (Protocol_error.invalid_request "unknown public history body")
  in
  create id ~provenance body
;;

module Window = struct
  type entry = t [@@deriving sexp_of]

  type t =
    { entries : entry list
    ; previous_cursor : Page.Cursor.t option
    ; next_cursor : Page.Cursor.t option
    ; reached_start : bool
    ; reached_end : bool
    ; structurally_complete : bool
    }
  [@@deriving sexp_of]

  let optional_field name value encode =
    Option.map value ~f:(fun value -> name, encode value)
  ;;

  let to_json t =
    let fields =
      [ Some ("entries", `Array (List.map t.entries ~f:to_json))
      ; optional_field "previous_cursor" t.previous_cursor Page.Cursor.to_json
      ; optional_field "next_cursor" t.next_cursor Page.Cursor.to_json
      ; Some ("reached_start", if t.reached_start then `True else `False)
      ; Some ("reached_end", if t.reached_end then `True else `False)
      ; Some ("structurally_complete", if t.structurally_complete then `True else `False)
      ]
      |> List.filter_opt
    in
    `Object fields
  ;;

  let validate t =
    let open Result.Let_syntax in
    let%bind () = Projection_codec.validate (to_json t) in
    validate_unique_ids t.entries
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind () = Projection_codec.validate json in
    let%bind fields = Json_codec.fields json in
    let%bind entries =
      Json_codec.required_as fields "entries" (Json_codec.list of_json)
    in
    let%bind previous_cursor =
      Json_codec.optional_as fields "previous_cursor" Page.Cursor.of_json
    in
    let%bind next_cursor =
      Json_codec.optional_as fields "next_cursor" Page.Cursor.of_json
    in
    let%bind reached_start =
      Json_codec.required_as fields "reached_start" Json_codec.bool
    in
    let%bind reached_end = Json_codec.required_as fields "reached_end" Json_codec.bool in
    let%bind structurally_complete =
      Json_codec.required_as fields "structurally_complete" Json_codec.bool
    in
    let t =
      { entries
      ; previous_cursor
      ; next_cursor
      ; reached_start
      ; reached_end
      ; structurally_complete
      }
    in
    let%map () = validate t in
    t
  ;;
end
