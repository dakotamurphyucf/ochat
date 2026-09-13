open Core
module G = Authoring_guidance
module J = Json_codec
module X = Extension_codec

type part =
  { index : int
  ; item_sha256 : string
  }
[@@deriving equal, sexp]

type topic =
  { topic : G.topic
  ; total_parts : int
  ; parts : part list
  }
[@@deriving equal, sexp]

type t =
  { version : int
  ; query_identity : string
  ; host_identity : string
  ; capability_fingerprint : string
  ; scope : string
  ; surface_id : string
  ; corpus_identity : string
  ; response_sha256 : string
  ; topics : topic list
  }
[@@deriving equal, sexp]

let invalid message = Error (Protocol_error.invalid_request message)
let digest text = Digestif.SHA256.(digest_string text |> to_hex)

let matches_response t json =
  String.equal t.response_sha256 (digest (Jsonaf.to_string json))
;;

let matches_output t = function
  | `String encoded ->
    (match Result.try_with (fun () -> Jsonaf.of_string encoded) with
     | Ok json -> matches_response t json
     | Error _ -> false)
  | json -> matches_response t json
;;

let scope_for ~session_id ~generation =
  [%sexp (session_id : Id.Session.t), (generation : int)] |> Sexp.to_string
;;

let valid_hash value =
  String.length value = 64
  && String.for_all value ~f:(function
    | '0' .. '9' | 'a' .. 'f' -> true
    | _ -> false)
;;

let integer value = `Number (Int.to_string value)

let to_json t =
  `Object
    [ "version", integer t.version
    ; "query_identity", `String t.query_identity
    ; "host_identity", `String t.host_identity
    ; "capability_fingerprint", `String t.capability_fingerprint
    ; "scope", `String t.scope
    ; "surface_id", `String t.surface_id
    ; "corpus_identity", `String t.corpus_identity
    ; "response_sha256", `String t.response_sha256
    ; ( "topics"
      , `Array
          (List.map t.topics ~f:(fun topic ->
             `Object
               [ "topic", G.topic_to_json topic.topic
               ; "total_parts", integer topic.total_parts
               ; ( "parts"
                 , `Array
                     (List.map topic.parts ~f:(fun part ->
                        `Object
                          [ "index", integer part.index
                          ; "item_sha256", `String part.item_sha256
                          ])) )
               ])) )
    ]
;;

let validate t =
  let open Result.Let_syntax in
  let%bind () = X.text ~name:"authoring reference scope" ~max:1024 t.scope in
  let%bind () =
    match
      t.version = 1
      && List.for_all
           [ t.query_identity
           ; t.host_identity
           ; t.capability_fingerprint
           ; t.corpus_identity
           ; t.response_sha256
           ]
           ~f:valid_hash
      && List.mem
           [ "one_off_v1"; "tool_v1"; "moderator_v1"; "delegated_moderator_v1" ]
           t.surface_id
           ~equal:String.equal
      && (not (List.is_empty t.topics))
      && List.length t.topics <= 1024
      && Option.is_none
           (List.find_a_dup t.topics ~compare:(fun a b ->
              String.compare a.topic.id b.topic.id))
    with
    | true -> Ok ()
    | false -> invalid "invalid authoring reference identity or topic set"
  in
  let%bind _ =
    List.fold_result t.topics ~init:0 ~f:(fun used topic ->
      let count = List.length topic.parts in
      let%bind () =
        match
          G.valid_topic topic.topic
          && topic.total_parts > 0
          && topic.total_parts <= 4096
          && count > 0
          && count <= topic.total_parts
          && used <= 4096 - count
          && Bool.equal topic.topic.complete (count = topic.total_parts)
        with
        | true -> Ok ()
        | false -> invalid "invalid authoring reference fragment coverage"
      in
      let%map _ =
        List.fold_result topic.parts ~init:(-1) ~f:(fun previous part ->
          match
            part.index > previous
            && part.index < topic.total_parts
            && valid_hash part.item_sha256
          with
          | true -> Ok part.index
          | false -> invalid "invalid or repeated authoring reference fragment")
      in
      used + count)
  in
  X.validate_json ~max_bytes:(1024 * 1024) ~max_depth:16 (to_json t)
;;

let create
      ~query_identity
      ~host_identity
      ~capability_fingerprint
      ~scope
      ~surface_id
      ~corpus_identity
      ~response_sha256
      ~topics
  =
  let open Result.Let_syntax in
  let t =
    { version = 1
    ; query_identity
    ; host_identity
    ; capability_fingerprint
    ; scope
    ; surface_id
    ; corpus_identity
    ; response_sha256
    ; topics
    }
  in
  let%map () = validate t in
  t
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () = X.validate_json ~max_bytes:(1024 * 1024) ~max_depth:16 json in
  let%bind fields = J.fields json in
  let%bind () =
    X.closed
      fields
      [ "version"
      ; "query_identity"
      ; "host_identity"
      ; "capability_fingerprint"
      ; "scope"
      ; "surface_id"
      ; "corpus_identity"
      ; "response_sha256"
      ; "topics"
      ]
  in
  let%bind version = J.required_as fields "version" (J.bounded_int ~min:1 ~max:1) in
  let%bind query_identity = J.required_as fields "query_identity" J.string in
  let%bind host_identity = J.required_as fields "host_identity" J.string in
  let%bind capability_fingerprint =
    J.required_as fields "capability_fingerprint" J.string
  in
  let%bind scope = J.required_as fields "scope" J.string in
  let%bind surface_id = J.required_as fields "surface_id" J.string in
  let%bind corpus_identity = J.required_as fields "corpus_identity" J.string in
  let%bind response_sha256 = J.required_as fields "response_sha256" J.string in
  let%bind topics =
    J.required_as
      fields
      "topics"
      (J.list (fun json ->
         let%bind fields = J.fields json in
         let%bind () = X.closed fields [ "topic"; "total_parts"; "parts" ] in
         let%bind topic = J.required_as fields "topic" G.topic_of_json in
         let%bind total_parts =
           J.required_as fields "total_parts" (J.bounded_int ~min:1 ~max:4096)
         in
         let%map parts =
           J.required_as
             fields
             "parts"
             (J.list (fun json ->
                let%bind fields = J.fields json in
                let%bind () = X.closed fields [ "index"; "item_sha256" ] in
                let%bind index =
                  J.required_as fields "index" (J.bounded_int ~min:0 ~max:4095)
                in
                let%map item_sha256 = J.required_as fields "item_sha256" J.string in
                { index; item_sha256 }))
         in
         { topic; total_parts; parts }))
  in
  let t =
    { version
    ; query_identity
    ; host_identity
    ; capability_fingerprint
    ; scope
    ; surface_id
    ; corpus_identity
    ; response_sha256
    ; topics
    }
  in
  let%map () = validate t in
  t
;;
