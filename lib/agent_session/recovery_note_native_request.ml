open! Core
module P = Agent_protocol
module J = P.Json_codec
module Q = Recovery_note_query
module C = Recovery_notes.Catalog
module Effect = Recovery_note_access.Effect
module Share = Recovery_note_share

type t =
  | Query of Q.t
  | Shared_read of Share.Reference.t * Q.Range.t
  | Mutate of C.mutation

let invalid () = P.Error.invalid_request "invalid bounded recovery note arguments"

let limits =
  Document_schema.Limits.create
    ~max_bytes:401_408
    ~max_depth:8
    ~max_fields:48
    ~max_nodes:80
  |> function
  | Ok limits -> limits
  | Error error -> raise_s [%sexp (error : Document_schema.Error.t)]
;;

let decode_text text =
  Document_schema.Json.decode ~limits text |> Result.map_error ~f:(fun _ -> invalid ())
;;

let decode_text_exn text =
  decode_text text
  |> Result.map_error ~f:(fun failure -> failure.P.Error.message)
  |> Result.ok_or_failwith
;;

let fields json ~names =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  if
    List.for_all (J.to_alist fields) ~f:(fun (name, _) ->
      List.mem names name ~equal:String.equal)
  then Ok fields
  else Error (invalid ())
;;

let counter json ~create =
  let%bind.Result text = J.string json in
  match Int64.of_string_opt text with
  | Some value when String.equal text (Int64.to_string value) -> create value
  | Some _ | None -> Error (invalid ())
;;

let catalog_revision json = counter json ~create:C.Catalog_revision.of_int64
let id json = counter json ~create:C.Note_id.of_int64
let note_revision json = counter json ~create:C.Note_revision.of_int64
let path json = Result.bind (J.string json) ~f:C.Note_path.of_string
let content json = Result.bind (J.string json) ~f:C.Content.of_string

let selection ~names ~require_kind json =
  let open Result.Let_syntax in
  let%bind f = fields json ~names in
  let%bind () =
    if require_kind
    then J.required_as f "kind" (J.enum ~name:"note kind" [ "local", () ])
    else Ok ()
  in
  let%bind id = J.required_as f "id" id in
  let%map revision = J.required_as f "revision" note_revision in
  { C.id; revision }
;;

let exact_selection = selection ~names:[ "id"; "revision" ] ~require_kind:false
let local_selection = selection ~names:[ "kind"; "id"; "revision" ] ~require_kind:true

let range json =
  let open Result.Let_syntax in
  let%bind f = fields json ~names:[ "start"; "stop" ] in
  let byte_offset = J.bounded_int ~min:0 ~max:65_536 in
  let%bind start = J.required_as f "start" byte_offset in
  let%bind stop = J.required_as f "stop" byte_offset in
  Q.Range.create ~start ~stop
;;

let shared_reference json =
  let open Result.Let_syntax in
  let%bind f = fields json ~names:[ "source"; "nonce"; "id" ] in
  let%bind _ = J.required_as f "source" (fields ~names:[ "server_id"; "session_id" ]) in
  Share.Reference.of_json json
;;

let read_selection json range =
  let open Result.Let_syntax in
  let%bind f = J.fields json in
  let%bind kind = J.required_as f "kind" J.string in
  match kind with
  | "local" ->
    let%map selection = local_selection json in
    Query (Q.Read (selection, range))
  | "shared" ->
    let%bind f = fields json ~names:[ "kind"; "reference" ] in
    let%map reference = J.required_as f "reference" shared_reference in
    Shared_read (reference, range)
  | _ -> Error (invalid ())
;;

let cursor json =
  let open Result.Let_syntax in
  let%bind f = fields json ~names:[ "revision"; "after"; "query" ] in
  let%bind revision = J.required_as f "revision" catalog_revision in
  let%bind after = J.required_as f "after" path in
  let%bind query = J.required_as f "query" J.string in
  let%map query =
    Chatml_source.Digest.of_hex query |> Result.map_error ~f:(fun _ -> invalid ())
  in
  { Q.Page.revision; after; query }
;;

let nullable decode json =
  match json with
  | `Null -> Ok None
  | json -> Result.map (decode json) ~f:Option.some
;;

let page f =
  let open Result.Let_syntax in
  let%bind limit = J.required_as f "limit" (J.bounded_int ~min:1 ~max:16) in
  let%bind cursor = J.required_as f "cursor" (nullable cursor) in
  Q.Page.create ~limit ~cursor
;;

let write f =
  let open Result.Let_syntax in
  let%bind content = J.required_as f "content" content in
  let%bind mode =
    J.required_as
      f
      "kind"
      (J.enum ~name:"note write" [ "create", `Create; "replace", `Replace ])
  in
  let%bind expected = J.required_as f "expected" (nullable exact_selection) in
  let%bind path = J.required_as f "path" (nullable path) in
  let%bind revision =
    J.required_as f "expected_catalog_revision" (nullable catalog_revision)
  in
  match mode, expected, path, revision with
  | `Create, None, Some path, Some expected_catalog_revision ->
    Ok (Mutate (C.Create { expected_catalog_revision; path; content }))
  | `Replace, Some expected, None, None -> Ok (Mutate (C.Replace { expected; content }))
  | _ -> Error (invalid ())
;;

let fields_for_effect = function
  | Effect.Read -> [ "selection"; "range" ]
  | List -> [ "limit"; "cursor" ]
  | Search -> [ "term"; "limit"; "cursor" ]
  | Write -> [ "kind"; "expected"; "path"; "expected_catalog_revision"; "content" ]
  | Append -> [ "expected"; "content" ]
  | Delete -> [ "expected" ]
;;

let decode ~effect_kind json =
  let open Result.Let_syntax in
  let%bind () =
    Document_schema.Json.validate ~limits json
    |> Result.map_error ~f:(fun _ -> invalid ())
  in
  let%bind f = fields json ~names:(fields_for_effect effect_kind) in
  match effect_kind with
  | Effect.Read ->
    let%bind range = J.required_as f "range" range in
    J.required_as f "selection" (fun json -> read_selection json range)
  | List ->
    let%map page = page f in
    Query (Q.List page)
  | Search ->
    let%bind term = J.required_as f "term" P.Search_term.of_json in
    let%map page = page f in
    Query (Q.Search (term, page))
  | Write -> write f
  | Append ->
    let%bind expected = J.required_as f "expected" exact_selection in
    let%map content = J.required_as f "content" content in
    Mutate (C.Append { expected; content })
  | Delete ->
    let%map expected = J.required_as f "expected" exact_selection in
    Mutate (C.Delete expected)
;;

let primitive name = `Object [ "type", `String name ]
let text = primitive "string"
let integer = primitive "integer"
let optional schema = `Object [ "anyOf", `Array [ schema; primitive "null" ] ]

let enum values =
  `Object
    [ "type", `String "string"
    ; "enum", `Array (List.map values ~f:(fun value -> `String value))
    ]
;;

let object_ properties =
  `Object
    [ "type", `String "object"
    ; "properties", `Object properties
    ; "required", `Array (List.map properties ~f:(fun (name, _) -> `String name))
    ; "additionalProperties", `False
    ]
;;

let selection_fields = [ "id", text; "revision", text ]
let exact = object_ selection_fields

let read_selection_schema =
  let reference =
    object_
      [ "source", object_ [ "server_id", text; "session_id", text ]
      ; "nonce", text
      ; "id", text
      ]
  in
  `Object
    [ ( "anyOf"
      , `Array
          [ object_ (("kind", enum [ "local" ]) :: selection_fields)
          ; object_ [ "kind", enum [ "shared" ]; "reference", reference ]
          ] )
    ]
;;

let page_fields =
  [ "limit", integer
  ; "cursor", optional (object_ [ "revision", text; "after", text; "query", text ])
  ]
;;

let parameters = function
  | Effect.Read ->
    object_
      [ "selection", read_selection_schema
      ; "range", object_ [ "start", integer; "stop", integer ]
      ]
  | List -> object_ page_fields
  | Search -> object_ (("term", text) :: page_fields)
  | Write ->
    object_
      [ "kind", enum [ "create"; "replace" ]
      ; "expected", optional exact
      ; "path", optional text
      ; "expected_catalog_revision", optional text
      ; "content", text
      ]
  | Append -> object_ [ "expected", exact; "content", text ]
  | Delete -> object_ [ "expected", exact ]
;;
