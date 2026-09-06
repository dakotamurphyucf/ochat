open Core

type status =
  | Healthy
  | Degraded
  | Unhealthy
[@@deriving compare, equal, sexp]

let status_to_string = function
  | Healthy -> "healthy"
  | Degraded -> "degraded"
  | Unhealthy -> "unhealthy"
;;

let status_of_json =
  Json_codec.enum
    ~name:"health status"
    [ "healthy", Healthy; "degraded", Degraded; "unhealthy", Unhealthy ]
;;

module Request = struct
  type t = { include_details : bool } [@@deriving sexp]

  let to_json t =
    `Object [ ("include_details", if t.include_details then `True else `False) ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%map include_details =
      Json_codec.optional_as fields "include_details" Json_codec.bool
      |> Result.map ~f:(Option.value ~default:false)
    in
    { include_details }
  ;;
end

module Component = struct
  type t =
    { name : string
    ; status : status
    ; message : string option
    }
  [@@deriving sexp]

  let to_json t =
    let fields =
      [ "name", `String t.name; "status", `String (status_to_string t.status) ]
    in
    match t.message with
    | None -> `Object fields
    | Some message -> `Object (fields @ [ "message", `String message ])
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind name = Json_codec.required_as fields "name" Json_codec.string in
    let%bind status = Json_codec.required_as fields "status" status_of_json in
    let%bind message = Json_codec.optional_as fields "message" Json_codec.string in
    if String.is_empty name
    then Error (Protocol_error.invalid_request "health component name must be nonempty")
    else Ok { name; status; message }
  ;;
end

module Response = struct
  type t =
    { status : status
    ; ready : bool
    ; draining : bool
    ; checked_at : Timestamp.t
    ; components : Component.t list
    }
  [@@deriving sexp]

  let to_json t =
    `Object
      [ "status", `String (status_to_string t.status)
      ; ("ready", if t.ready then `True else `False)
      ; ("draining", if t.draining then `True else `False)
      ; "checked_at", Timestamp.to_json t.checked_at
      ; "components", `Array (List.map t.components ~f:Component.to_json)
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind status = Json_codec.required_as fields "status" status_of_json in
    let%bind ready = Json_codec.required_as fields "ready" Json_codec.bool in
    let%bind draining = Json_codec.required_as fields "draining" Json_codec.bool in
    let%bind checked_at = Json_codec.required_as fields "checked_at" Timestamp.of_json in
    let%map components =
      Json_codec.required_as fields "components" (Json_codec.list Component.of_json)
    in
    { status; ready; draining; checked_at; components }
  ;;
end
