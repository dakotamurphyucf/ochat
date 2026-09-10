open Core

let fields json names =
  let open Result.Let_syntax in
  let%bind () =
    Json_codec.validate_limits ~max_depth:132 ~max_bytes:((1024 * 1024) + 8192) json
  in
  let%bind fields = Json_codec.fields json in
  let%bind () =
    match
      List.for_all (Json_codec.to_alist fields) ~f:(fun (name, _) ->
        List.mem ("version" :: names) name ~equal:String.equal)
    with
    | true -> Ok ()
    | false -> Error (Protocol_error.invalid_request "unsupported ingress field")
  in
  let%map _ =
    Json_codec.required_as fields "version" (Json_codec.bounded_int ~min:1 ~max:1)
  in
  fields
;;

module Submit_request = struct
  type t =
    { session_id : Id.Session.t
    ; registration_id : Id.Capability.t
    ; namespace : string
    ; idempotency_key : Idempotency_key.t
    ; payload : Jsonaf.t
    }
  [@@deriving sexp]

  let to_json t =
    `Object
      [ "version", `Number "1"
      ; "session_id", Id.Session.to_json t.session_id
      ; "registration_id", Id.Capability.to_json t.registration_id
      ; "namespace", `String t.namespace
      ; "idempotency_key", Idempotency_key.to_json t.idempotency_key
      ; "payload", t.payload
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields =
      fields
        json
        [ "session_id"; "registration_id"; "namespace"; "idempotency_key"; "payload" ]
    in
    let get name decode = Json_codec.required_as fields name decode in
    let%bind session_id = get "session_id" Id.Session.of_json in
    let%bind registration_id = get "registration_id" Id.Capability.of_json in
    let%bind namespace = get "namespace" Json_codec.string in
    let%bind () =
      match
        String.is_prefix namespace ~prefix:"external."
        && String.length namespace > 9
        && String.length namespace <= 128
      with
      | true -> Ok ()
      | false ->
        Error (Protocol_error.invalid_request "invalid external ingress namespace")
    in
    let%bind idempotency_key = get "idempotency_key" Idempotency_key.of_json in
    let%bind payload = Json_codec.required fields "payload" in
    let%map () =
      Json_codec.validate_limits ~max_depth:128 ~max_bytes:(1024 * 1024) payload
    in
    { session_id; registration_id; namespace; idempotency_key; payload }
  ;;
end

module Acknowledgement = struct
  type t =
    { session_id : Id.Session.t
    ; registration_id : Id.Capability.t
    ; event_id : Id.Ingress_event.t
    ; idempotency_key : Idempotency_key.t
    ; payload_sha256 : string
    ; accepted_at : Timestamp.t
    }
  [@@deriving equal, sexp]

  let to_json t =
    `Object
      [ "version", `Number "1"
      ; "status", `String "accepted"
      ; "session_id", Id.Session.to_json t.session_id
      ; "registration_id", Id.Capability.to_json t.registration_id
      ; "event_id", Id.Ingress_event.to_json t.event_id
      ; "idempotency_key", Idempotency_key.to_json t.idempotency_key
      ; "payload_sha256", `String t.payload_sha256
      ; "accepted_at", Timestamp.to_json t.accepted_at
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields =
      fields
        json
        [ "status"
        ; "session_id"
        ; "registration_id"
        ; "event_id"
        ; "idempotency_key"
        ; "payload_sha256"
        ; "accepted_at"
        ]
    in
    let get name decode = Json_codec.required_as fields name decode in
    let%bind _ =
      get "status" (Json_codec.enum ~name:"ingress status" [ "accepted", () ])
    in
    let%bind session_id = get "session_id" Id.Session.of_json in
    let%bind registration_id = get "registration_id" Id.Capability.of_json in
    let%bind event_id = get "event_id" Id.Ingress_event.of_json in
    let%bind idempotency_key = get "idempotency_key" Idempotency_key.of_json in
    let%bind payload_sha256 = get "payload_sha256" Json_codec.string in
    let%bind () =
      match
        String.length payload_sha256 = 64
        && String.for_all payload_sha256 ~f:(function
          | '0' .. '9' | 'a' .. 'f' -> true
          | _ -> false)
      with
      | true -> Ok ()
      | false -> Error (Protocol_error.invalid_request "invalid ingress payload digest")
    in
    let%map accepted_at = get "accepted_at" Timestamp.of_json in
    { session_id
    ; registration_id
    ; event_id
    ; idempotency_key
    ; payload_sha256
    ; accepted_at
    }
  ;;
end
