open Core

type kind =
  | Physical
  | Temporary
[@@deriving compare, equal, sexp]

type temporary_location =
  | System_tmp
  | Session_dir
[@@deriving compare, equal, sexp]

type cleanup =
  | On_session_stop
  | On_session_delete
  | Retain
[@@deriving compare, equal, sexp]

type access =
  | Read_only
  | Shared_write
  | Exclusive
[@@deriving compare, equal, sexp]

type overflow =
  | Reject
  | Queue
[@@deriving compare, equal, sexp]

type availability =
  | Available
  | Unavailable of { reason : string }
[@@deriving compare, equal, sexp]

type prompt_limit =
  { prompt_id : Id.Prompt_definition.t
  ; max_root_agents : int
  ; overflow : overflow
  }
[@@deriving sexp]

type t =
  { id : Id.Workspace_definition.t
  ; name : string
  ; kind : kind
  ; temporary_location : temporary_location option
  ; cleanup : cleanup option
  ; access : access
  ; conflict_domain : string option
  ; prompt_limits : prompt_limit list
  ; availability : availability
  }
[@@deriving sexp]

let kind_to_string = function
  | Physical -> "physical"
  | Temporary -> "temporary"
;;

let kind_of_json =
  Json_codec.enum ~name:"workspace kind" [ "physical", Physical; "temporary", Temporary ]
;;

let location_to_string = function
  | System_tmp -> "system_tmp"
  | Session_dir -> "session_dir"
;;

let location_of_json =
  Json_codec.enum
    ~name:"temporary workspace location"
    [ "system_tmp", System_tmp; "session_dir", Session_dir ]
;;

let cleanup_to_string = function
  | On_session_stop -> "on_session_stop"
  | On_session_delete -> "on_session_delete"
  | Retain -> "retain"
;;

let cleanup_of_json =
  Json_codec.enum
    ~name:"workspace cleanup policy"
    [ "on_session_stop", On_session_stop
    ; "on_session_delete", On_session_delete
    ; "retain", Retain
    ]
;;

let access_to_string = function
  | Read_only -> "read_only"
  | Shared_write -> "shared_write"
  | Exclusive -> "exclusive"
;;

let access_of_json =
  Json_codec.enum
    ~name:"workspace access"
    [ "read_only", Read_only; "shared_write", Shared_write; "exclusive", Exclusive ]
;;

let overflow_to_string = function
  | Reject -> "reject"
  | Queue -> "queue"
;;

let overflow_of_json =
  Json_codec.enum ~name:"workspace overflow policy" [ "reject", Reject; "queue", Queue ]
;;

let optional_field name value encode =
  Option.map value ~f:(fun value -> name, encode value)
;;

let prompt_limit_to_json limit =
  `Object
    [ "prompt_id", Id.Prompt_definition.to_json limit.prompt_id
    ; "max_root_agents", `Number (Int.to_string limit.max_root_agents)
    ; "overflow", `String (overflow_to_string limit.overflow)
    ]
;;

let prompt_limit_of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind prompt_id =
    Json_codec.required_as fields "prompt_id" Id.Prompt_definition.of_json
  in
  let%bind max_root_agents =
    Json_codec.required_as
      fields
      "max_root_agents"
      (Json_codec.bounded_int ~min:1 ~max:Int.max_value)
  in
  let%map overflow = Json_codec.required_as fields "overflow" overflow_of_json in
  { prompt_id; max_root_agents; overflow }
;;

let availability_fields = function
  | Available -> [ "availability", `String "available" ]
  | Unavailable { reason } ->
    [ "availability", `String "unavailable"; "unavailable_reason", `String reason ]
;;

let to_json t =
  let fields =
    [ Some ("id", Id.Workspace_definition.to_json t.id)
    ; Some ("name", `String t.name)
    ; Some ("kind", `String (kind_to_string t.kind))
    ; optional_field "temporary_location" t.temporary_location (fun value ->
        `String (location_to_string value))
    ; optional_field "cleanup" t.cleanup (fun value -> `String (cleanup_to_string value))
    ; Some ("access", `String (access_to_string t.access))
    ; optional_field "conflict_domain" t.conflict_domain (fun value -> `String value)
    ; Some ("prompt_limits", `Array (List.map t.prompt_limits ~f:prompt_limit_to_json))
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
  | _ -> Error (Protocol_error.invalid_request "unknown workspace availability")
;;

let validate_kind_fields kind temporary_location cleanup =
  match kind, temporary_location, cleanup with
  | Physical, None, None -> Ok ()
  | Temporary, Some _, Some _ -> Ok ()
  | Physical, _, _ ->
    Error (Protocol_error.invalid_request "physical workspace has temporary fields")
  | Temporary, _, _ ->
    Error (Protocol_error.invalid_request "temporary workspace lacks creation policy")
;;

let validate t =
  let prompt_ids =
    List.map t.prompt_limits ~f:(fun limit ->
      Id.Prompt_definition.to_string limit.prompt_id)
  in
  if String.is_empty t.name
  then Error (Protocol_error.invalid_request "workspace name must be nonempty")
  else if Option.exists t.conflict_domain ~f:String.is_empty
  then Error (Protocol_error.invalid_request "conflict domain must be nonempty")
  else if Option.is_some (List.find_a_dup prompt_ids ~compare:String.compare)
  then Error (Protocol_error.invalid_request "workspace prompt limits contain duplicates")
  else Ok t
;;

let decode_source fields =
  let open Result.Let_syntax in
  let%bind id = Json_codec.required_as fields "id" Id.Workspace_definition.of_json in
  let%bind name = Json_codec.required_as fields "name" Json_codec.string in
  let%bind kind = Json_codec.required_as fields "kind" kind_of_json in
  let%bind temporary_location =
    Json_codec.optional_as fields "temporary_location" location_of_json
  in
  let%bind cleanup = Json_codec.optional_as fields "cleanup" cleanup_of_json in
  let%map () = validate_kind_fields kind temporary_location cleanup in
  id, name, kind, temporary_location, cleanup
;;

let decode_policy fields =
  let open Result.Let_syntax in
  let%bind access = Json_codec.required_as fields "access" access_of_json in
  let%bind conflict_domain =
    Json_codec.optional_as fields "conflict_domain" Json_codec.string
  in
  let%bind prompt_limits =
    Json_codec.required_as fields "prompt_limits" (Json_codec.list prompt_limit_of_json)
  in
  let%map availability = availability_of_fields fields in
  access, conflict_domain, prompt_limits, availability
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind id, name, kind, temporary_location, cleanup = decode_source fields in
  let%bind access, conflict_domain, prompt_limits, availability = decode_policy fields in
  validate
    { id
    ; name
    ; kind
    ; temporary_location
    ; cleanup
    ; access
    ; conflict_domain
    ; prompt_limits
    ; availability
    }
;;

module List_request = struct
  type t =
    { page : Page.Request.t
    ; kind : kind option
    ; access : access option
    ; available : bool option
    }
  [@@deriving sexp]

  let to_json t =
    let filters =
      [ optional_field "kind" t.kind (fun value -> `String (kind_to_string value))
      ; optional_field "access" t.access (fun value -> `String (access_to_string value))
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
    let%bind kind = Json_codec.optional_as fields "kind" kind_of_json in
    let%bind access = Json_codec.optional_as fields "access" access_of_json in
    let%map available = Json_codec.optional_as fields "available" Json_codec.bool in
    { page; kind; access; available }
  ;;
end

module Get_request = struct
  type t = { workspace_id : Id.Workspace_definition.t } [@@deriving sexp]

  let to_json t =
    `Object [ "workspace_id", Id.Workspace_definition.to_json t.workspace_id ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%map workspace_id =
      Json_codec.required_as fields "workspace_id" Id.Workspace_definition.of_json
    in
    { workspace_id }
  ;;
end
