open Core

type availability =
  | Available
  | Unavailable of { reason : string }
[@@deriving compare, equal, sexp]

type t =
  { id : Id.Prompt_definition.t
  ; name : string
  ; description : string option
  ; enabled : bool
  ; availability : availability
  ; current_revision : Id.Prompt_revision.t option
  ; allowed_workspaces : Id.Workspace_definition.t list
  ; permission_profile : string
  ; runtime_policy : string option
  }
[@@deriving sexp]

let optional_field name value encode =
  Option.map value ~f:(fun value -> name, encode value)
;;

let availability_fields = function
  | Available -> [ "availability", `String "available" ]
  | Unavailable { reason } ->
    [ "availability", `String "unavailable"; "unavailable_reason", `String reason ]
;;

let to_json t =
  let fields =
    [ Some ("id", Id.Prompt_definition.to_json t.id)
    ; Some ("name", `String t.name)
    ; optional_field "description" t.description (fun value -> `String value)
    ; Some ("enabled", if t.enabled then `True else `False)
    ; optional_field "current_revision" t.current_revision Id.Prompt_revision.to_json
    ; Some
        ( "allowed_workspaces"
        , `Array (List.map t.allowed_workspaces ~f:Id.Workspace_definition.to_json) )
    ; Some ("permission_profile", `String t.permission_profile)
    ; optional_field "runtime_policy" t.runtime_policy (fun value -> `String value)
    ]
    |> List.filter_opt
  in
  `Object (fields @ availability_fields t.availability)
;;

let availability_of_fields fields =
  let open Result.Let_syntax in
  let%bind encoded = Json_codec.required_as fields "availability" Json_codec.string in
  match encoded with
  | "available" -> Ok Available
  | "unavailable" ->
    let%map reason =
      Json_codec.required_as fields "unavailable_reason" Json_codec.string
    in
    Unavailable { reason }
  | _ -> Error (Protocol_error.invalid_request "unknown prompt availability")
;;

let validate t =
  let workspace_ids =
    List.map t.allowed_workspaces ~f:Id.Workspace_definition.to_string
  in
  if String.is_empty t.name
  then Error (Protocol_error.invalid_request "prompt name must be nonempty")
  else if String.is_empty t.permission_profile
  then Error (Protocol_error.invalid_request "permission profile must be nonempty")
  else if Option.is_some (List.find_a_dup workspace_ids ~compare:String.compare)
  then
    Error
      (Protocol_error.invalid_request "prompt workspace allowlist contains duplicates")
  else Ok t
;;

let decode_identity fields =
  let open Result.Let_syntax in
  let%bind id = Json_codec.required_as fields "id" Id.Prompt_definition.of_json in
  let%bind name = Json_codec.required_as fields "name" Json_codec.string in
  let%bind description = Json_codec.optional_as fields "description" Json_codec.string in
  let%map enabled = Json_codec.required_as fields "enabled" Json_codec.bool in
  id, name, description, enabled
;;

let decode_policy fields =
  let open Result.Let_syntax in
  let%bind current_revision =
    Json_codec.optional_as fields "current_revision" Id.Prompt_revision.of_json
  in
  let%bind allowed_workspaces =
    Json_codec.required_as
      fields
      "allowed_workspaces"
      (Json_codec.list Id.Workspace_definition.of_json)
  in
  let%bind permission_profile =
    Json_codec.required_as fields "permission_profile" Json_codec.string
  in
  let%bind runtime_policy =
    Json_codec.optional_as fields "runtime_policy" Json_codec.string
  in
  Ok (current_revision, allowed_workspaces, permission_profile, runtime_policy)
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind id, name, description, enabled = decode_identity fields in
  let%bind availability = availability_of_fields fields in
  let%bind current_revision, allowed_workspaces, permission_profile, runtime_policy =
    decode_policy fields
  in
  validate
    { id
    ; name
    ; description
    ; enabled
    ; availability
    ; current_revision
    ; allowed_workspaces
    ; permission_profile
    ; runtime_policy
    }
;;

module List_request = struct
  type t =
    { page : Page.Request.t
    ; enabled : bool option
    ; available : bool option
    }
  [@@deriving sexp]

  let to_json t =
    let filters =
      [ optional_field "enabled" t.enabled (fun value -> if value then `True else `False)
      ; optional_field "available" t.available (fun value ->
          if value then `True else `False)
      ]
      |> List.filter_opt
    in
    `Object (Page.Request.to_fields t.page @ filters)
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind page = Page.Request.of_fields fields in
    let%bind enabled = Json_codec.optional_as fields "enabled" Json_codec.bool in
    let%map available = Json_codec.optional_as fields "available" Json_codec.bool in
    { page; enabled; available }
  ;;
end

module Get_request = struct
  type t = { prompt_id : Id.Prompt_definition.t } [@@deriving sexp]

  let to_json t = `Object [ "prompt_id", Id.Prompt_definition.to_json t.prompt_id ]

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%map prompt_id =
      Json_codec.required_as fields "prompt_id" Id.Prompt_definition.of_json
    in
    { prompt_id }
  ;;
end
