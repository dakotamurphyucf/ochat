open Core

let optional_field name value encode =
  Option.map value ~f:(fun value -> name, encode value)
;;

let positive_int = Json_codec.bounded_int ~min:1 ~max:Int.max_value
let nonnegative_int = Json_codec.bounded_int ~min:0 ~max:Int.max_value
let nonnegative_int64 = Json_codec.bounded_int64 ~min:Int64.zero ~max:Int64.max_value

let validate_nonempty name value =
  if String.is_empty value
  then Error (Protocol_error.invalid_request (name ^ " must be nonempty"))
  else Ok value
;;

let validate_features features =
  let open Result.Let_syntax in
  let%bind features = Result.all (List.map features ~f:Version.validate_feature) in
  if Option.is_some (List.find_a_dup features ~compare:String.compare)
  then Error (Protocol_error.invalid_request "feature list contains duplicates")
  else Ok (List.sort features ~compare:String.compare)
;;

module Implementation = struct
  type t =
    { name : string
    ; version : string
    }
  [@@deriving sexp]

  let create ~name ~version =
    let open Result.Let_syntax in
    let%bind name = validate_nonempty "implementation name" name in
    let%map version = validate_nonempty "implementation version" version in
    { name; version }
  ;;

  let to_json t = `Object [ "name", `String t.name; "version", `String t.version ]

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind name = Json_codec.required_as fields "name" Json_codec.string in
    let%bind version = Json_codec.required_as fields "version" Json_codec.string in
    create ~name ~version
  ;;
end

type event_encoding =
  | Json
  | Ndjson
[@@deriving compare, equal, sexp]

let event_encoding_to_string = function
  | Json -> "json"
  | Ndjson -> "ndjson"
;;

let event_encoding_of_json =
  Json_codec.enum ~name:"event encoding" [ "json", Json; "ndjson", Ndjson ]
;;

let validate_event_encodings encodings =
  if List.is_empty encodings
  then Error (Protocol_error.invalid_request "event encoding list must be nonempty")
  else if Option.is_some (List.find_a_dup encodings ~compare:compare_event_encoding)
  then Error (Protocol_error.invalid_request "event encoding list contains duplicates")
  else Ok encodings
;;

module Request = struct
  type t =
    { implementation : Implementation.t
    ; protocol_min : Version.t
    ; protocol_max : Version.t
    ; features : string list
    ; event_encodings : event_encoding list
    ; max_inbound_event_bytes : int
    ; client_instance_id : string option
    }
  [@@deriving sexp]

  let create
        ~implementation
        ~protocol_min
        ~protocol_max
        ~features
        ~event_encodings
        ~max_inbound_event_bytes
        ?client_instance_id
        ()
    =
    let open Result.Let_syntax in
    let%bind features = validate_features features in
    let%bind event_encodings = validate_event_encodings event_encodings in
    let%bind client_instance_id =
      match client_instance_id with
      | None -> Ok None
      | Some id -> Result.map (validate_nonempty "client instance ID" id) ~f:Option.some
    in
    if Version.compare protocol_min protocol_max > 0
    then Error (Protocol_error.invalid_request "protocol range is inverted")
    else if max_inbound_event_bytes <= 0
    then Error (Protocol_error.invalid_request "inbound event limit must be positive")
    else
      Ok
        { implementation
        ; protocol_min
        ; protocol_max
        ; features
        ; event_encodings
        ; max_inbound_event_bytes
        ; client_instance_id
        }
  ;;

  let to_json t =
    let fields =
      [ Some ("implementation", Implementation.to_json t.implementation)
      ; Some ("protocol_min", Version.to_json t.protocol_min)
      ; Some ("protocol_max", Version.to_json t.protocol_max)
      ; Some ("features", `Array (List.map t.features ~f:(fun x -> `String x)))
      ; Some
          ( "event_encodings"
          , `Array
              (List.map t.event_encodings ~f:(fun x ->
                 `String (event_encoding_to_string x))) )
      ; Some ("max_inbound_event_bytes", `Number (Int.to_string t.max_inbound_event_bytes))
      ; optional_field "client_instance_id" t.client_instance_id (fun x -> `String x)
      ]
      |> List.filter_opt
    in
    `Object fields
  ;;

  let decode_capabilities fields =
    let open Result.Let_syntax in
    let%bind features =
      Json_codec.required_as fields "features" (Json_codec.list Json_codec.string)
    in
    let%bind event_encodings =
      Json_codec.required_as
        fields
        "event_encodings"
        (Json_codec.list event_encoding_of_json)
    in
    let%map max_inbound_event_bytes =
      Json_codec.required_as fields "max_inbound_event_bytes" positive_int
    in
    features, event_encodings, max_inbound_event_bytes
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind implementation =
      Json_codec.required_as fields "implementation" Implementation.of_json
    in
    let%bind protocol_min =
      Json_codec.required_as fields "protocol_min" Version.of_json
    in
    let%bind protocol_max =
      Json_codec.required_as fields "protocol_max" Version.of_json
    in
    let%bind features, event_encodings, max_inbound_event_bytes =
      decode_capabilities fields
    in
    let%bind client_instance_id =
      Json_codec.optional_as fields "client_instance_id" Json_codec.string
    in
    create
      ~implementation
      ~protocol_min
      ~protocol_max
      ~features
      ~event_encodings
      ~max_inbound_event_bytes
      ?client_instance_id
      ()
  ;;
end

module Limits = struct
  type t =
    { max_request_bytes : int
    ; max_event_bytes : int
    ; max_page_size : int
    ; max_attachments_per_connection : int
    }
  [@@deriving sexp]

  let to_json t =
    `Object
      [ "max_request_bytes", `Number (Int.to_string t.max_request_bytes)
      ; "max_event_bytes", `Number (Int.to_string t.max_event_bytes)
      ; "max_page_size", `Number (Int.to_string t.max_page_size)
      ; ( "max_attachments_per_connection"
        , `Number (Int.to_string t.max_attachments_per_connection) )
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind max_request_bytes =
      Json_codec.required_as fields "max_request_bytes" positive_int
    in
    let%bind max_event_bytes =
      Json_codec.required_as fields "max_event_bytes" positive_int
    in
    let%bind max_page_size = Json_codec.required_as fields "max_page_size" positive_int in
    let%map max_attachments_per_connection =
      Json_codec.required_as fields "max_attachments_per_connection" positive_int
    in
    { max_request_bytes; max_event_bytes; max_page_size; max_attachments_per_connection }
  ;;
end

module Event_retention = struct
  type t =
    { minimum_age_ms : int
    ; maximum_events : int
    ; oldest_replayable_sequence : int64 option
    }
  [@@deriving sexp]

  let to_json t =
    let fields =
      [ Some ("minimum_age_ms", `Number (Int.to_string t.minimum_age_ms))
      ; Some ("maximum_events", `Number (Int.to_string t.maximum_events))
      ; optional_field "oldest_replayable_sequence" t.oldest_replayable_sequence (fun x ->
          `Number (Int64.to_string x))
      ]
      |> List.filter_opt
    in
    `Object fields
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind minimum_age_ms =
      Json_codec.required_as fields "minimum_age_ms" nonnegative_int
    in
    let%bind maximum_events =
      Json_codec.required_as fields "maximum_events" positive_int
    in
    let%map oldest_replayable_sequence =
      Json_codec.optional_as fields "oldest_replayable_sequence" nonnegative_int64
    in
    { minimum_age_ms; maximum_events; oldest_replayable_sequence }
  ;;
end

module Timing = struct
  type t =
    { heartbeat_interval_ms : int
    ; owner_lease_duration_ms : int
    ; owner_renew_after_ms : int
    ; disconnect_grace_default_ms : int
    }
  [@@deriving sexp]

  let to_json t =
    `Object
      [ "heartbeat_interval_ms", `Number (Int.to_string t.heartbeat_interval_ms)
      ; "owner_lease_duration_ms", `Number (Int.to_string t.owner_lease_duration_ms)
      ; "owner_renew_after_ms", `Number (Int.to_string t.owner_renew_after_ms)
      ; ( "disconnect_grace_default_ms"
        , `Number (Int.to_string t.disconnect_grace_default_ms) )
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind heartbeat_interval_ms =
      Json_codec.required_as fields "heartbeat_interval_ms" positive_int
    in
    let%bind owner_lease_duration_ms =
      Json_codec.required_as fields "owner_lease_duration_ms" positive_int
    in
    let%bind owner_renew_after_ms =
      Json_codec.required_as fields "owner_renew_after_ms" positive_int
    in
    let%bind disconnect_grace_default_ms =
      Json_codec.required_as fields "disconnect_grace_default_ms" nonnegative_int
    in
    if owner_renew_after_ms >= owner_lease_duration_ms
    then Error (Protocol_error.invalid_request "owner renewal must precede lease expiry")
    else
      Ok
        { heartbeat_interval_ms
        ; owner_lease_duration_ms
        ; owner_renew_after_ms
        ; disconnect_grace_default_ms
        }
  ;;
end

module Response = struct
  type t =
    { protocol_name : string
    ; selected_version : Version.t
    ; implementation : Implementation.t
    ; server_id : Id.Server.t
    ; enabled_features : string list
    ; extensions : Extension_capabilities.t option [@sexp.option]
    ; principal : Principal.t
    ; limits : Limits.t
    ; event_retention : Event_retention.t
    ; timing : Timing.t
    ; server_time : Timestamp.t
    }
  [@@deriving sexp]

  let create
        ~protocol_name
        ~selected_version
        ~implementation
        ~server_id
        ~enabled_features
        ~extensions
        ~principal
        ~limits
        ~event_retention
        ~timing
        ~server_time
    =
    let open Result.Let_syntax in
    let%bind protocol_name = validate_nonempty "protocol name" protocol_name in
    let%bind enabled_features = validate_features enabled_features in
    let%map () =
      match extensions with
      | Some metadata
        when not
               (List.equal
                  String.equal
                  enabled_features
                  (Extension_capabilities.filter_available metadata enabled_features)) ->
        Error
          (Protocol_error.invalid_request
             "enabled extension features exceed host capabilities")
      | _ -> Ok ()
    in
    { protocol_name
    ; selected_version
    ; implementation
    ; server_id
    ; enabled_features
    ; extensions
    ; principal
    ; limits
    ; event_retention
    ; timing
    ; server_time
    }
  ;;

  let to_json t =
    `Object
      ([ "protocol_name", `String t.protocol_name
       ; "selected_version", Version.to_json t.selected_version
       ; "implementation", Implementation.to_json t.implementation
       ; "server_id", Id.Server.to_json t.server_id
       ; "enabled_features", `Array (List.map t.enabled_features ~f:(fun x -> `String x))
       ; "principal", Principal.to_json t.principal
       ; "limits", Limits.to_json t.limits
       ; "event_retention", Event_retention.to_json t.event_retention
       ; "timing", Timing.to_json t.timing
       ; "server_time", Timestamp.to_json t.server_time
       ]
       @ Option.to_list
           (optional_field "extensions" t.extensions Extension_capabilities.to_json))
  ;;

  let decode_identity fields =
    let open Result.Let_syntax in
    let%bind protocol_name =
      Json_codec.required_as fields "protocol_name" Json_codec.string
    in
    let%bind selected_version =
      Json_codec.required_as fields "selected_version" Version.of_json
    in
    let%bind implementation =
      Json_codec.required_as fields "implementation" Implementation.of_json
    in
    let%map server_id = Json_codec.required_as fields "server_id" Id.Server.of_json in
    protocol_name, selected_version, implementation, server_id
  ;;

  let decode_capabilities fields =
    let open Result.Let_syntax in
    let%bind enabled_features =
      Json_codec.required_as fields "enabled_features" (Json_codec.list Json_codec.string)
    in
    let%bind principal = Json_codec.required_as fields "principal" Principal.of_json in
    let%map limits = Json_codec.required_as fields "limits" Limits.of_json in
    enabled_features, principal, limits
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind protocol_name, selected_version, implementation, server_id =
      decode_identity fields
    in
    let%bind enabled_features, principal, limits = decode_capabilities fields in
    let%bind extensions =
      Json_codec.optional_as fields "extensions" Extension_capabilities.of_json
    in
    let%bind event_retention =
      Json_codec.required_as fields "event_retention" Event_retention.of_json
    in
    let%bind timing = Json_codec.required_as fields "timing" Timing.of_json in
    let%bind server_time =
      Json_codec.required_as fields "server_time" Timestamp.of_json
    in
    create
      ~protocol_name
      ~selected_version
      ~implementation
      ~server_id
      ~enabled_features
      ~extensions
      ~principal
      ~limits
      ~event_retention
      ~timing
      ~server_time
  ;;
end
