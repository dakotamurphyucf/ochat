open! Core
module P = Agent_protocol
module J = P.Json_codec
module H = P.Recovery_history_request
module Window = P.Recovery_window
module Tool = Chat_response.Tool_capability.Local_history_tool

type media =
  | Image
  | Document

type t =
  | Windows of P.Page.Request.t
  | Items of H.t
  | Text of P.Recovery_text_range.t
  | Media of media * P.Recovery_asset_read.t

let invalid () = P.Error.invalid_request "invalid bounded local history arguments"

let limits =
  Document_schema.Limits.create ~max_bytes:8192 ~max_depth:12 ~max_fields:64 ~max_nodes:96
  |> function
  | Ok limits -> limits
  | Error failure -> raise_s [%sexp (failure : Document_schema.Error.t)]
;;

let decode_text_exn text =
  Document_schema.Json.decode ~limits text
  |> Result.map_error ~f:(fun _ -> (invalid ()).message)
  |> Result.ok_or_failwith
;;

let fields json names =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  if
    List.for_all (J.to_alist fields) ~f:(fun (key, _) ->
      List.mem names key ~equal:String.equal)
  then Ok fields
  else Error (invalid ())
;;

let optional fields key decode =
  match J.optional fields key with
  | None | Some `Null -> Ok None
  | Some value -> Result.map (decode value) ~f:Option.some
;;

let page fields =
  let open Result.Let_syntax in
  let%bind limit = J.required_as fields "limit" (J.bounded_int ~min:1 ~max:16) in
  let%bind cursor = optional fields "cursor" P.Page.Cursor.of_json in
  P.Page.Request.create ~limit ?cursor ()
;;

let items kind ~source json =
  let open Result.Let_syntax in
  let args = [ "window"; "family"; "role"; "limit"; "cursor" ] in
  let%bind fields =
    fields json (if Tool.equal kind Search then "term" :: args else args)
  in
  let%bind window = optional fields "window" Window.Scope.of_json in
  let%bind family =
    J.required_as
      fields
      "family"
      (J.enum ~name:"history family" [ "canonical", H.Canonical; "effective", Effective ])
  in
  let%bind role =
    optional
      fields
      "role"
      (J.enum ~name:"ordinary role" [ "user", H.User; "assistant", Assistant ])
  in
  let%bind page = page fields in
  let%bind action =
    if Tool.equal kind Search
    then
      Result.map (J.required_as fields "term" P.Search_term.of_json) ~f:(fun term ->
        H.Search term)
    else Ok List_items
  in
  let%map query =
    H.create ~session:source ?window ~family ?role ~page ~max_reply_bytes:16_384 action
  in
  Items query
;;

let text ~source json =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  let encoded =
    J.to_alist fields
    |> List.filter ~f:(function
      | ("preparation_revision" | "max_bytes"), `Null -> false
      | _ -> true)
  in
  let%bind range = P.Recovery_text_range.of_json (`Object encoded) in
  if P.Session_ref.equal source (P.Recovery_text_range.session range)
  then Ok (Text range)
  else Error (invalid ())
;;

let media ~source json =
  let open Result.Let_syntax in
  let%bind fields =
    fields json [ "citation"; "part"; "kind"; "offset"; "expected_sha256" ]
  in
  let%bind kind =
    J.required_as
      fields
      "kind"
      (J.enum ~name:"local media kind" [ "image", Image; "document", Document ])
  in
  let%bind citation = J.required_as fields "citation" Window.Citation.of_json in
  let%bind part = J.required_as fields "part" (J.bounded_int ~min:0 ~max:4095) in
  let%bind offset =
    J.required_as
      fields
      "offset"
      (J.bounded_int ~min:0 ~max:P.Recovery_asset_read.max_original_bytes)
  in
  let%bind expected_sha256 = optional fields "expected_sha256" J.string in
  let%bind request =
    P.Recovery_asset_read.create citation ~part ~offset ?expected_sha256 ()
  in
  if
    (not (P.Session_ref.equal source (P.Recovery_asset_read.session request)))
    ||
    match kind with
    | Image -> offset <> 0
    | Document -> false
  then Error (invalid ())
  else Ok (Media (kind, request))
;;

let decode kind ~source json =
  match kind with
  | Tool.List_windows ->
    let%bind.Result fields = fields json [ "limit"; "cursor" ] in
    Result.map (page fields) ~f:(fun page -> Windows page)
  | List_items | Search -> items kind ~source json
  | Read_text -> text ~source json
  | Read_media -> media ~source json
;;

let str value = `String value
let typ name = `Object [ "type", str name ]
let string = typ "string"
let integer = typ "integer"
let one_of schemas = `Object [ "anyOf", `Array schemas ]
let nullable schema = one_of [ schema; typ "null" ]

let enum options =
  `Object [ "type", str "string"; "enum", `Array (List.map options ~f:str) ]
;;

let tag value = enum [ value ]

let object_ properties =
  `Object
    [ "type", str "object"
    ; "properties", `Object properties
    ; "required", `Array (List.map properties ~f:(fun (key, _) -> str key))
    ; "additionalProperties", `False
    ]
;;

let window =
  object_
    [ "session", object_ [ "server_id", string; "session_id", string ]
    ; "generation", integer
    ; "window", string
    ]
;;

let canonical_version = object_ [ "view", tag "canonical"; "revision", string ]

let effective_version =
  let origin kind fields = object_ (("kind", tag kind) :: fields) in
  let change = [ "incarnation", string; "change_id", integer ] in
  let origins =
    [ origin "unchanged" [ "base", string ]
    ; origin "captured" [ "session_revision", string ]
    ; origin "inserted" change
    ; origin "replaced" (("base", string) :: change)
    ]
  in
  object_ [ "view", tag "effective"; "origin", one_of origins; "sha256", string ]
;;

let citation version =
  object_
    [ "scope", window; "member", object_ [ "occurrence", string; "version", version ] ]
;;

let parameters kind =
  let page = [ "limit", integer; "cursor", nullable string ] in
  match kind with
  | Tool.List_windows -> object_ page
  | List_items | Search ->
    let window = if Tool.equal kind Search then nullable window else window in
    let fields =
      [ "window", window
      ; "family", enum [ "canonical"; "effective" ]
      ; "role", nullable (enum [ "user"; "assistant" ])
      ]
      @ page
    in
    object_ (if Tool.equal kind Search then ("term", string) :: fields else fields)
  | Read_text ->
    object_
      [ "citation", citation (one_of [ canonical_version; effective_version ])
      ; "preparation_revision", nullable string
      ; "part", integer
      ; "start", integer
      ; "stop", integer
      ; "max_bytes", nullable integer
      ]
  | Read_media ->
    object_
      [ "citation", citation canonical_version
      ; "part", integer
      ; "kind", enum [ "image"; "document" ]
      ; "offset", integer
      ; "expected_sha256", nullable string
      ]
;;
