open Core

type host =
  | Daemon
  | Embedded_durable
  | Embedded_transient
  | Direct
[@@deriving compare, equal, sexp]

type journal_flush =
  | Synced
  | Buffered
  | Memory
[@@deriving compare, equal, sexp]

type t =
  { host : host
  ; journal_flush : journal_flush
  ; available_features : string list
  }
[@@deriving sexp]

let known_features =
  [ "chatml.invocations.v1"
  ; "chatml.background.v1"
  ; "chatml.notifications.v1"
  ; "agent.delegation.v1"
  ; "chatml.authoring.v1"
  ]
;;

let create ~host ~journal_flush ~available_features =
  if
    Option.is_some (List.find_a_dup available_features ~compare:String.compare)
    || not
         (List.for_all available_features ~f:(fun feature ->
            List.mem known_features feature ~equal:String.equal))
  then Error (Protocol_error.invalid_request "unknown or duplicate extension capability")
  else
    Ok
      { host
      ; journal_flush
      ; available_features = List.sort available_features ~compare:String.compare
      }
;;

let extension_namespace feature =
  List.exists [ "chatml."; "agent.delegation." ] ~f:(fun prefix ->
    String.is_prefix feature ~prefix)
;;

let filter_available t features =
  List.filter features ~f:(fun feature ->
    (not (extension_namespace feature))
    || List.mem t.available_features feature ~equal:String.equal)
;;

let hosts =
  [ "daemon", Daemon
  ; "embedded_durable", Embedded_durable
  ; "embedded_transient", Embedded_transient
  ; "direct", Direct
  ]
;;

let flushes = [ "synced", Synced; "buffered", Buffered; "memory", Memory ]

let name values equal value =
  List.find_exn values ~f:(fun (_, candidate) -> equal value candidate) |> fst
;;

let to_json t =
  `Object
    [ "version", `Number "1"
    ; "record_version", `Number "1"
    ; "host", `String (name hosts equal_host t.host)
    ; "journal_flush", `String (name flushes equal_journal_flush t.journal_flush)
    ; "known_features", `Array (List.map known_features ~f:(fun s -> `String s))
    ; "available_features", `Array (List.map t.available_features ~f:(fun s -> `String s))
    ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () = Extension_codec.validate_json ~max_bytes:4096 ~max_depth:3 json in
  let%bind fields = Json_codec.fields json in
  let%bind () =
    Extension_codec.closed
      fields
      [ "version"
      ; "record_version"
      ; "host"
      ; "journal_flush"
      ; "known_features"
      ; "available_features"
      ]
  in
  let version = Json_codec.bounded_int ~min:1 ~max:1 in
  let%bind _ = Json_codec.required_as fields "version" version in
  let%bind _ = Json_codec.required_as fields "record_version" version in
  let%bind host =
    Json_codec.required_as fields "host" (Json_codec.enum ~name:"extension host" hosts)
  in
  let%bind journal_flush =
    Json_codec.required_as
      fields
      "journal_flush"
      (Json_codec.enum ~name:"journal flush" flushes)
  in
  let%bind known =
    Json_codec.required_as fields "known_features" (Json_codec.list Json_codec.string)
  in
  let%bind available_features =
    Json_codec.required_as fields "available_features" (Json_codec.list Json_codec.string)
  in
  if
    not
      (List.equal
         String.equal
         (List.sort known ~compare:String.compare)
         (List.sort known_features ~compare:String.compare))
  then
    Error
      (Protocol_error.invalid_request
         "extension feature catalog disagrees with contract version")
  else create ~host ~journal_flush ~available_features
;;
