open Core

type level =
  | Info
  | Warning
  | Error
[@@deriving compare, equal, sexp]

type t =
  { sequence : int64
  ; timestamp : Timestamp.t
  ; level : level
  ; name : string
  ; session_id : Id.Session.t option
  ; principal_id : Id.Principal.t option
  ; payload : Jsonaf.t
  ; redacted : bool
  }
[@@deriving sexp]

let optional_field name value encode =
  Option.map value ~f:(fun value -> name, encode value)
;;

let level_values = [ "info", Info; "warning", Warning; "error", Error ]

let level_to_string level =
  List.Assoc.find_exn
    (List.map level_values ~f:(fun (name, level) -> level, name))
    level
    ~equal:equal_level
;;

let level_of_json = Json_codec.enum ~name:"audit level" level_values
let sequence_of_json = Json_codec.bounded_int64 ~min:Int64.zero ~max:Int64.max_value

let to_json t =
  let fields =
    [ Some ("sequence", `Number (Int64.to_string t.sequence))
    ; Some ("timestamp", Timestamp.to_json t.timestamp)
    ; Some ("level", `String (level_to_string t.level))
    ; Some ("name", `String t.name)
    ; optional_field "session_id" t.session_id Id.Session.to_json
    ; optional_field "principal_id" t.principal_id Id.Principal.to_json
    ; Some ("payload", t.payload)
    ; Some ("redacted", if t.redacted then `True else `False)
    ]
    |> List.filter_opt
  in
  `Object fields
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind sequence = Json_codec.required_as fields "sequence" sequence_of_json in
  let%bind timestamp = Json_codec.required_as fields "timestamp" Timestamp.of_json in
  let%bind level = Json_codec.required_as fields "level" level_of_json in
  let%bind name = Json_codec.required_as fields "name" Json_codec.string in
  let%bind session_id = Json_codec.optional_as fields "session_id" Id.Session.of_json in
  let%bind principal_id =
    Json_codec.optional_as fields "principal_id" Id.Principal.of_json
  in
  let%bind payload = Json_codec.required fields "payload" in
  let%bind redacted = Json_codec.required_as fields "redacted" Json_codec.bool in
  if String.is_empty name
  then Error (Protocol_error.invalid_request "audit name must be nonempty")
  else
    Ok { sequence; timestamp; level; name; session_id; principal_id; payload; redacted }
;;

module Read_request = struct
  type t =
    { page : Page.Request.t
    ; session_id : Id.Session.t option
    ; principal_id : Id.Principal.t option
    ; minimum_level : level option
    ; name_prefix : string option
    }
  [@@deriving sexp]

  let to_json t =
    let fields =
      [ optional_field "session_id" t.session_id Id.Session.to_json
      ; optional_field "principal_id" t.principal_id Id.Principal.to_json
      ; optional_field "minimum_level" t.minimum_level (fun level ->
          `String (level_to_string level))
      ; optional_field "name_prefix" t.name_prefix (fun value -> `String value)
      ]
      |> List.filter_opt
    in
    `Object (Page.Request.to_fields t.page @ fields)
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind page = Page.Request.of_fields fields in
    let%bind session_id = Json_codec.optional_as fields "session_id" Id.Session.of_json in
    let%bind principal_id =
      Json_codec.optional_as fields "principal_id" Id.Principal.of_json
    in
    let%bind minimum_level =
      Json_codec.optional_as fields "minimum_level" level_of_json
    in
    let%map name_prefix = Json_codec.optional_as fields "name_prefix" Json_codec.string in
    { page; session_id; principal_id; minimum_level; name_prefix }
  ;;
end
