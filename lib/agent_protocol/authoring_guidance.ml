open Core

type source =
  | Installed of string
  | Authored of string
[@@deriving equal, sexp]

type purpose =
  | Primer
  | Preload
  | Reference
  | Rediscovery
[@@deriving equal, sexp]

type topic =
  { id : string
  ; document_sha256 : string
  ; source : source
  ; complete : bool
  }
[@@deriving equal, sexp]

type t =
  { version : int
  ; context_identity : string
  ; policy_fingerprint : string
  ; payload_sha256 : string
  ; purpose : purpose
  ; topics : topic list
  }
[@@deriving equal, sexp]

let digest text = Digestif.SHA256.(digest_string text |> to_hex)

let matches_payload t payload =
  String.equal t.payload_sha256 (digest (Jsonaf.to_string payload))
;;

let invalid message = Error (Protocol_error.invalid_request message)

let valid_hash value =
  String.length value = 64
  && String.for_all value ~f:(function
    | '0' .. '9' | 'a' .. 'f' -> true
    | _ -> false)
;;

let source_identity = function
  | Installed identity | Authored identity -> identity
;;

let valid_topic topic =
  String.length topic.id > 0
  && String.length topic.id <= 128
  && String.for_all topic.id ~f:(function
    | 'a' .. 'z' | '0' .. '9' | '_' | '-' | '.' | ':' | '/' -> true
    | _ -> false)
  && valid_hash topic.document_sha256
  && valid_hash (source_identity topic.source)
;;

let validate t =
  match t.version with
  | 1
    when valid_hash t.context_identity
         && valid_hash t.policy_fingerprint
         && valid_hash t.payload_sha256
         && (not (List.is_empty t.topics))
         && List.length t.topics <= 128
         && List.for_all t.topics ~f:valid_topic
         && Option.is_none
              (List.find_a_dup
                 (List.map t.topics ~f:(fun topic -> topic.id))
                 ~compare:String.compare)
         && ((not (equal_purpose t.purpose Rediscovery))
             || List.for_all t.topics ~f:(fun topic -> not topic.complete)) -> Ok ()
  | _ -> invalid "invalid or unsupported authoring guidance provenance"
;;

let create ~context_identity ~policy_fingerprint ~purpose ~topics ~payload =
  let open Result.Let_syntax in
  let t =
    { version = 1
    ; context_identity
    ; policy_fingerprint
    ; payload_sha256 = digest (Jsonaf.to_string payload)
    ; purpose
    ; topics
    }
  in
  let%map () = validate t in
  t
;;

let source_to_json source =
  let kind =
    match source with
    | Installed _ -> "installed"
    | Authored _ -> "authored"
  in
  `Object [ "kind", `String kind; "identity", `String (source_identity source) ]
;;

let source_of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind () = Extension_codec.closed fields [ "kind"; "identity" ] in
  let%bind identity = Json_codec.required_as fields "identity" Json_codec.string in
  let%bind kind = Json_codec.required_as fields "kind" Json_codec.string in
  match kind with
  | "installed" -> Ok (Installed identity)
  | "authored" -> Ok (Authored identity)
  | _ -> invalid "unknown authoring guidance source"
;;

let topic_to_json topic =
  `Object
    [ "id", `String topic.id
    ; "document_sha256", `String topic.document_sha256
    ; "source", source_to_json topic.source
    ; ("complete", if topic.complete then `True else `False)
    ]
;;

let topic_of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind () =
    Extension_codec.closed fields [ "id"; "document_sha256"; "source"; "complete" ]
  in
  let%bind id = Json_codec.required_as fields "id" Json_codec.string in
  let%bind document_sha256 =
    Json_codec.required_as fields "document_sha256" Json_codec.string
  in
  let%bind source = Json_codec.required_as fields "source" source_of_json in
  let%map complete = Json_codec.required_as fields "complete" Json_codec.bool in
  { id; document_sha256; source; complete }
;;

let purpose_values =
  [ "primer", Primer
  ; "preload", Preload
  ; "reference", Reference
  ; "rediscovery", Rediscovery
  ]
;;

let purpose_name = function
  | Primer -> "primer"
  | Preload -> "preload"
  | Reference -> "reference"
  | Rediscovery -> "rediscovery"
;;

let to_json t =
  `Object
    [ "version", `Number (Int.to_string t.version)
    ; "context_identity", `String t.context_identity
    ; "policy_fingerprint", `String t.policy_fingerprint
    ; "payload_sha256", `String t.payload_sha256
    ; "purpose", `String (purpose_name t.purpose)
    ; "topics", `Array (List.map t.topics ~f:topic_to_json)
    ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind () =
    Extension_codec.closed
      fields
      [ "version"
      ; "context_identity"
      ; "policy_fingerprint"
      ; "payload_sha256"
      ; "purpose"
      ; "topics"
      ]
  in
  let%bind version =
    Json_codec.required_as fields "version" (Json_codec.bounded_int ~min:1 ~max:1)
  in
  let%bind context_identity =
    Json_codec.required_as fields "context_identity" Json_codec.string
  in
  let%bind policy_fingerprint =
    Json_codec.required_as fields "policy_fingerprint" Json_codec.string
  in
  let%bind payload_sha256 =
    Json_codec.required_as fields "payload_sha256" Json_codec.string
  in
  let%bind purpose =
    Json_codec.required_as
      fields
      "purpose"
      (Json_codec.enum ~name:"authoring guidance purpose" purpose_values)
  in
  let%bind topics =
    Json_codec.required_as fields "topics" (Json_codec.list topic_of_json)
  in
  let t =
    { version; context_identity; policy_fingerprint; payload_sha256; purpose; topics }
  in
  let%map () = validate t in
  t
;;
