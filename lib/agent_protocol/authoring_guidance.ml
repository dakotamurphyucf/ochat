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
  ; fragments : fragment list [@sexp.list]
  ; surface_id : string option [@sexp.option]
  }

and part =
  { index : int
  ; item_sha256 : string
  }

and fragment =
  { topic_id : string
  ; total_parts : int
  ; parts : part list
  }
[@@deriving equal, sexp]

(* Fragment coverage is separate from [topic] so existing version-1 provenance
   and reference topic identities retain their wire and source contracts. *)

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
  let open Result.Let_syntax in
  let%bind () =
    match t.version, t.surface_id with
    | (1 | 2), None -> Ok ()
    | 3, Some ("one_off_v1" | "tool_v1" | "moderator_v1" | "delegated_moderator_v1") ->
      Ok ()
    | _ -> invalid "invalid authoring guidance surface"
  in
  let%bind () =
    match t.version with
    | (1 | 2 | 3)
      when valid_hash t.context_identity
           && valid_hash t.policy_fingerprint
           && valid_hash t.payload_sha256
           && (not (List.is_empty t.topics))
           && (List.length t.topics
               <=
               if t.version >= 2 && equal_purpose t.purpose Reference then 1024 else 128)
           && List.for_all t.topics ~f:valid_topic
           && Option.is_none
                (List.find_a_dup
                   (List.map t.topics ~f:(fun topic -> topic.id))
                   ~compare:String.compare)
           && ((not (equal_purpose t.purpose Rediscovery))
               || List.for_all t.topics ~f:(fun topic -> not topic.complete)) -> Ok ()
    | _ -> invalid "invalid or unsupported authoring guidance provenance"
  in
  match t.version, t.fragments, t.purpose with
  | 1, [], _ -> Ok ()
  | 3, [], Rediscovery -> Ok ()
  | (2 | 3), (_ :: _ as fragments), Reference
    when List.length fragments = List.length t.topics ->
    let%bind () =
      match
        List.find_a_dup fragments ~compare:(fun a b ->
          String.compare a.topic_id b.topic_id)
      with
      | None -> Ok ()
      | Some _ -> invalid "duplicate authoring fragment topic"
    in
    let%map _ =
      List.fold_result fragments ~init:0 ~f:(fun used fragment ->
        let%bind topic =
          List.find t.topics ~f:(fun topic -> String.equal topic.id fragment.topic_id)
          |> Result.of_option
               ~error:(Protocol_error.invalid_request "fragment topic is not in guidance")
        in
        let count = List.length fragment.parts in
        let%bind () =
          match
            fragment.total_parts > 0
            && fragment.total_parts <= 4096
            && count > 0
            && count <= fragment.total_parts
            && used <= 4096 - count
            && Bool.equal topic.complete (count = fragment.total_parts)
          with
          | true -> Ok ()
          | false -> invalid "invalid authoring fragment coverage"
        in
        let%map _ =
          List.fold_result fragment.parts ~init:(-1) ~f:(fun previous part ->
            match
              part.index > previous
              && part.index < fragment.total_parts
              && valid_hash part.item_sha256
            with
            | true -> Ok part.index
            | false -> invalid "invalid or repeated authoring fragment index")
        in
        used + count)
    in
    ()
  | _ -> invalid "invalid authoring fragment purpose or version"
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
    ; fragments = []
    ; surface_id = None
    }
  in
  let%map () = validate t in
  t
;;

let create_reference ~context_identity ~policy_fingerprint ~topics ~fragments ~payload =
  let t =
    { version = 2
    ; context_identity
    ; policy_fingerprint
    ; payload_sha256 = digest (Jsonaf.to_string payload)
    ; purpose = Reference
    ; topics
    ; fragments
    ; surface_id = None
    }
  in
  Result.map (validate t) ~f:(fun () -> t)
;;

let create_surface_reference
      ~surface_id
      ~context_identity
      ~policy_fingerprint
      ~topics
      ~fragments
      ~payload
  =
  let open Result.Let_syntax in
  let%bind t =
    create_reference ~context_identity ~policy_fingerprint ~topics ~fragments ~payload
  in
  let t = { t with version = 3; surface_id = Some surface_id } in
  let%map () = validate t in
  t
;;

let create_surface_rediscovery
      ~surface_id
      ~context_identity
      ~policy_fingerprint
      ~topics
      ~payload
  =
  let open Result.Let_syntax in
  let%bind t =
    create ~context_identity ~policy_fingerprint ~purpose:Rediscovery ~topics ~payload
  in
  let t = { t with version = 3; surface_id = Some surface_id } in
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
    ([ "version", `Number (Int.to_string t.version)
     ; "context_identity", `String t.context_identity
     ; "policy_fingerprint", `String t.policy_fingerprint
     ; "payload_sha256", `String t.payload_sha256
     ; "purpose", `String (purpose_name t.purpose)
     ; "topics", `Array (List.map t.topics ~f:topic_to_json)
     ]
     @ Option.to_list (Option.map t.surface_id ~f:(fun id -> "surface_id", `String id))
     @
     match t.fragments with
     | [] when t.version = 1 -> []
     | fragments ->
       [ ( "fragments"
         , `Array
             (List.map fragments ~f:(fun fragment ->
                `Object
                  [ "topic_id", `String fragment.topic_id
                  ; "total_parts", `Number (Int.to_string fragment.total_parts)
                  ; ( "parts"
                    , `Array
                        (List.map fragment.parts ~f:(fun part ->
                           `Object
                             [ "index", `Number (Int.to_string part.index)
                             ; "item_sha256", `String part.item_sha256
                             ])) )
                  ])) )
       ])
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind version =
    Json_codec.required_as fields "version" (Json_codec.bounded_int ~min:1 ~max:3)
  in
  let%bind () =
    Extension_codec.closed
      fields
      ([ "version"
       ; "context_identity"
       ; "policy_fingerprint"
       ; "payload_sha256"
       ; "purpose"
       ; "topics"
       ]
       @ (if version >= 2 then [ "fragments" ] else [])
       @ if version = 3 then [ "surface_id" ] else [])
  in
  let%bind surface_id =
    match version with
    | 3 ->
      Json_codec.required_as fields "surface_id" Json_codec.string
      |> Result.map ~f:Option.some
    | _ -> Ok None
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
  let%bind fragments =
    match version with
    | 1 -> Ok []
    | _ ->
      Json_codec.required_as
        fields
        "fragments"
        (Json_codec.list (fun json ->
           let%bind fields = Json_codec.fields json in
           let%bind () =
             Extension_codec.closed fields [ "topic_id"; "total_parts"; "parts" ]
           in
           let%bind topic_id =
             Json_codec.required_as fields "topic_id" Json_codec.string
           in
           let%bind total_parts =
             Json_codec.required_as
               fields
               "total_parts"
               (Json_codec.bounded_int ~min:1 ~max:4096)
           in
           let%map parts =
             Json_codec.required_as
               fields
               "parts"
               (Json_codec.list (fun json ->
                  let%bind fields = Json_codec.fields json in
                  let%bind () =
                    Extension_codec.closed fields [ "index"; "item_sha256" ]
                  in
                  let%bind index =
                    Json_codec.required_as
                      fields
                      "index"
                      (Json_codec.bounded_int ~min:0 ~max:4095)
                  in
                  let%map item_sha256 =
                    Json_codec.required_as fields "item_sha256" Json_codec.string
                  in
                  { index; item_sha256 }))
           in
           { topic_id; total_parts; parts }))
  in
  let t =
    { version
    ; context_identity
    ; policy_fingerprint
    ; payload_sha256
    ; purpose
    ; topics
    ; fragments
    ; surface_id
    }
  in
  let%map () = validate t in
  t
;;
