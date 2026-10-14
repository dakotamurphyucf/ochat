open! Core
module D = Document_schema
module F = Document_fields
module P = Agent_protocol
module B = P.Authoring_bundle.Bundle

let ( let* ) result f = Result.bind result ~f
let document_limit ~max_bytes = F.limits ~max_bytes |> F.store
let invalid field message = F.invalid field message

let exact_fields json allowed =
  match json with
  | `Object fields ->
    if
      List.length fields <> List.length allowed
      || List.exists fields ~f:(fun (name, _) ->
        not (List.mem allowed name ~equal:String.equal))
    then invalid "payload" "unexpected durable upload fields"
    else Ok ()
  | _ -> invalid "payload" "durable upload payload is not an object"
;;

let exact_envelope document =
  exact_fields
    (D.Document.json document)
    [ "format"; "schema_version"; "kind"; "payload" ]
;;

let encode ~kind ~max_bytes payload =
  let* limits = document_limit ~max_bytes in
  let* document = D.Document.create ~limits ~kind ~version:2 ~payload |> F.store in
  Ok (D.Document.to_string document)
;;

let decode ~kind ~max_bytes bytes =
  let* limits = document_limit ~max_bytes in
  let* document = D.Document.decode ~limits bytes |> F.store in
  let* () = exact_envelope document |> F.store in
  if not (String.equal (D.Document.kind document) kind)
  then Error (Store_error.Corrupt "wrong durable upload document kind")
  else if D.Document.version document <> 2
  then Error (Store_error.Corrupt "unsupported durable upload document version")
  else Ok (D.Document.payload document)
;;

let id decode json = F.protocol (decode json)
let timestamp json = id P.Timestamp.of_json json

let digest json =
  let* hex = F.digest json in
  match Chatml_source.Digest.of_hex hex with
  | Ok value -> Ok value
  | Error _ -> invalid "digest" "invalid source digest"
;;

let field json name decode = F.required json name decode

let optional json name decode =
  match D.Json.field json ~name with
  | Absent -> Ok None
  | Null -> invalid name "optional durable upload field must be absent"
  | Value value -> Result.map (decode value) ~f:Option.some
;;

let int json =
  let* value = F.decimal json in
  if Int64.(value < zero || value > of_int Int.max_value)
  then invalid "number" "durable upload integer outside range"
  else Ok (Int64.to_int_exn value)
;;

let int_json value = F.decimal_json (Int64.of_int value)

let optional_field name value encode =
  Option.to_list (Option.map value ~f:(fun x -> name, encode x))
;;

module Manifest = struct
  type t =
    { owner : B.Owner.t
    ; upload_id : P.Id.Authoring_upload.t
    ; begin_key : P.Idempotency_key.t
    ; manifest : B.Manifest.t
    ; parent_context : P.Authoring_context.Context.Expected.t option
    ; created_at : P.Timestamp.t
    ; stage_deadline : P.Timestamp.t
    }

  let kind = "store.authoring_upload.manifest"

  let to_payload t =
    `Object
      ([ "owner", B.Owner.to_json t.owner
       ; "upload_id", P.Id.Authoring_upload.to_json t.upload_id
       ; "begin_key", P.Idempotency_key.to_json t.begin_key
       ; "manifest", B.Manifest.to_json t.manifest
       ; "created_at", P.Timestamp.to_json t.created_at
       ; "stage_deadline", P.Timestamp.to_json t.stage_deadline
       ]
       @ optional_field
           "parent_context"
           t.parent_context
           P.Authoring_context.Context.Expected.to_json)
  ;;

  let to_bytes t ~max_bytes = encode ~kind ~max_bytes (to_payload t)

  let of_bytes bytes ~max_bytes =
    let* json = decode ~kind ~max_bytes bytes in
    let* () =
      (match json with
       | `Object fields ->
         let allowed =
           [ "owner"
           ; "upload_id"
           ; "begin_key"
           ; "manifest"
           ; "created_at"
           ; "stage_deadline"
           ; "parent_context"
           ]
         in
         if
           List.exists fields ~f:(fun (name, _) ->
             not (List.mem allowed name ~equal:String.equal))
         then invalid "payload" "unexpected durable upload manifest field"
         else Ok ()
       | _ -> invalid "payload" "durable upload manifest is not an object")
      |> F.store
    in
    let* owner = field json "owner" (id B.Owner.of_json) |> F.store in
    let* upload_id =
      field json "upload_id" (id P.Id.Authoring_upload.of_json) |> F.store
    in
    let* begin_key = field json "begin_key" (id P.Idempotency_key.of_json) |> F.store in
    let* manifest = field json "manifest" (id B.Manifest.of_json) |> F.store in
    let* parent_context =
      optional json "parent_context" (id P.Authoring_context.Context.Expected.of_json)
      |> F.store
    in
    let* created_at = field json "created_at" timestamp |> F.store in
    let* stage_deadline = field json "stage_deadline" timestamp |> F.store in
    if P.Timestamp.compare created_at stage_deadline >= 0
    then Error (Store_error.Corrupt "invalid upload stage deadline")
    else
      Ok
        { owner
        ; upload_id
        ; begin_key
        ; manifest
        ; parent_context
        ; created_at
        ; stage_deadline
        }
  ;;
end

module Phase = struct
  type outcome =
    | Open
    | Ready of
        { commit_key : P.Idempotency_key.t
        ; payload_digest : Chatml_source.Digest.t
        ; receipt_deadline : P.Timestamp.t
        }
    | Rejected of
        { commit_key : P.Idempotency_key.t
        ; reason : B.Rejection.t
        ; receipt_deadline : P.Timestamp.t
        }
    | Expired of { tombstone_deadline : P.Timestamp.t }

  type t =
    { upload_id : P.Id.Authoring_upload.t
    ; outcome : outcome
    }

  let kind = "store.authoring_upload.phase"

  let to_payload t =
    let fields = [ "upload_id", P.Id.Authoring_upload.to_json t.upload_id ] in
    let fields =
      match t.outcome with
      | Open -> ("kind", `String "open") :: fields
      | Ready { commit_key; payload_digest; receipt_deadline } ->
        [ "kind", `String "ready"
        ; "upload_id", P.Id.Authoring_upload.to_json t.upload_id
        ; "commit_key", P.Idempotency_key.to_json commit_key
        ; "payload_digest", `String (Chatml_source.Digest.to_hex payload_digest)
        ; "receipt_deadline", P.Timestamp.to_json receipt_deadline
        ]
      | Rejected { commit_key; reason; receipt_deadline } ->
        [ "kind", `String "rejected"
        ; "upload_id", P.Id.Authoring_upload.to_json t.upload_id
        ; "commit_key", P.Idempotency_key.to_json commit_key
        ; "reason", B.Rejection.to_json reason
        ; "receipt_deadline", P.Timestamp.to_json receipt_deadline
        ]
      | Expired { tombstone_deadline } ->
        [ "kind", `String "expired"
        ; "upload_id", P.Id.Authoring_upload.to_json t.upload_id
        ; "tombstone_deadline", P.Timestamp.to_json tombstone_deadline
        ]
    in
    `Object fields
  ;;

  let to_bytes t ~max_bytes = encode ~kind ~max_bytes (to_payload t)

  let of_bytes bytes ~max_bytes =
    let* json = decode ~kind ~max_bytes bytes in
    let* kind = field json "kind" F.string |> F.store in
    let allowed =
      match kind with
      | "open" -> [ "kind"; "upload_id" ]
      | "expired" -> [ "kind"; "upload_id"; "tombstone_deadline" ]
      | "ready" ->
        [ "kind"; "upload_id"; "commit_key"; "payload_digest"; "receipt_deadline" ]
      | "rejected" -> [ "kind"; "upload_id"; "commit_key"; "reason"; "receipt_deadline" ]
      | _ -> []
    in
    let* () =
      if List.is_empty allowed
      then Error (Store_error.Corrupt "unknown durable upload phase")
      else exact_fields json allowed |> F.store
    in
    let* upload_id =
      field json "upload_id" (id P.Id.Authoring_upload.of_json) |> F.store
    in
    let* outcome =
      match kind with
      | "open" -> Ok Open
      | "expired" ->
        let* tombstone_deadline = field json "tombstone_deadline" timestamp |> F.store in
        Ok (Expired { tombstone_deadline })
      | "ready" ->
        let* commit_key =
          field json "commit_key" (id P.Idempotency_key.of_json) |> F.store
        in
        let* payload_digest = field json "payload_digest" digest |> F.store in
        let* receipt_deadline = field json "receipt_deadline" timestamp |> F.store in
        Ok (Ready { commit_key; payload_digest; receipt_deadline })
      | "rejected" ->
        let* commit_key =
          field json "commit_key" (id P.Idempotency_key.of_json) |> F.store
        in
        let* reason = field json "reason" (id B.Rejection.of_json) |> F.store in
        let* receipt_deadline = field json "receipt_deadline" timestamp |> F.store in
        Ok (Rejected { commit_key; reason; receipt_deadline })
      | _ -> assert false
    in
    Ok { upload_id; outcome }
  ;;
end

module Progress = struct
  type t =
    { upload_id : P.Id.Authoring_upload.t
    ; member_index : int
    ; accepted_until_byte : int
    }

  let kind = "store.authoring_upload.progress"

  let to_bytes t ~max_bytes =
    encode
      ~kind
      ~max_bytes
      (`Object
          [ "upload_id", P.Id.Authoring_upload.to_json t.upload_id
          ; "member_index", int_json t.member_index
          ; "accepted_until_byte", int_json t.accepted_until_byte
          ])
  ;;

  let of_bytes bytes ~max_bytes =
    let* json = decode ~kind ~max_bytes bytes in
    let* () =
      exact_fields json [ "upload_id"; "member_index"; "accepted_until_byte" ] |> F.store
    in
    let* upload_id =
      field json "upload_id" (id P.Id.Authoring_upload.of_json) |> F.store
    in
    let* member_index = field json "member_index" int |> F.store in
    let* accepted_until_byte = field json "accepted_until_byte" int |> F.store in
    Ok { upload_id; member_index; accepted_until_byte }
  ;;
end

module Intent = struct
  module Source_ref = struct
    type location =
      | Uploaded_member of int
      | Supplemental_file of int

    type t =
      { source : P.Authoring.Source.t
      ; location : location
      }

    let create ~source ~location =
      if Option.is_some (P.Authoring.Source.editor_revision source)
      then Or_error.error_string "published bundle source has editor revision"
      else (
        match location with
        | (Uploaded_member ordinal | Supplemental_file ordinal) when ordinal < 0 ->
          Or_error.error_string "negative published source ordinal"
        | Uploaded_member _ | Supplemental_file _ -> Ok { source; location })
    ;;

    let source t = t.source
    let location t = t.location

    let to_json t =
      let kind, ordinal =
        match t.location with
        | Uploaded_member ordinal -> "uploaded", ordinal
        | Supplemental_file ordinal -> "supplemental", ordinal
      in
      `Object
        [ "source", P.Authoring.Source.to_json t.source
        ; "kind", `String kind
        ; "ordinal", int_json ordinal
        ]
    ;;

    let of_json json =
      let* () = exact_fields json [ "source"; "kind"; "ordinal" ] in
      let* source = field json "source" (id P.Authoring.Source.of_json) in
      let* kind = field json "kind" F.string in
      let* ordinal = field json "ordinal" int in
      let* location =
        match kind with
        | "uploaded" -> Ok (Uploaded_member ordinal)
        | "supplemental" -> Ok (Supplemental_file ordinal)
        | _ -> invalid "kind" "unknown published source location"
      in
      match create ~source ~location with
      | Ok source -> Ok source
      | Error _ -> invalid "source" "invalid published source reference"
    ;;
  end

  module Graph = struct
    type t =
      { selector : B.Manifest.Script.Selector.t option
      ; lock : Source_ref.t
      ; sources : Source_ref.t list
      }

    let create ~selector ~lock ~sources =
      if List.is_empty sources
      then Or_error.error_string "published graph has no selected sources"
      else Ok { selector; lock; sources }
    ;;

    let selector t = t.selector
    let lock t = t.lock
    let sources t = t.sources

    let to_json t =
      `Object
        ([ "lock", Source_ref.to_json t.lock
         ; "sources", `Array (List.map t.sources ~f:Source_ref.to_json)
         ]
         @ optional_field "selector" t.selector B.Manifest.Script.Selector.to_json)
    ;;

    let of_json json =
      let* () =
        match json with
        | `Object fields ->
          if
            List.for_all fields ~f:(fun (name, _) ->
              List.mem [ "selector"; "lock"; "sources" ] name ~equal:String.equal)
          then Ok ()
          else invalid "graph" "unexpected published graph field"
        | _ -> invalid "graph" "published graph must be object"
      in
      let* selector = optional json "selector" (id B.Manifest.Script.Selector.of_json) in
      let* lock = field json "lock" Source_ref.of_json in
      let* sources =
        let* values = field json "sources" F.array in
        Result.all (List.map values ~f:Source_ref.of_json)
      in
      match create ~selector ~lock ~sources with
      | Ok graph -> Ok graph
      | Error _ -> invalid "graph" "invalid published graph"
    ;;
  end

  type t =
    { owner : B.Owner.t
    ; upload_id : P.Id.Authoring_upload.t
    ; commit_key : P.Idempotency_key.t
    ; payload_digest : Chatml_source.Digest.t
    ; manifest_fingerprint : Chatml_source.Digest.t
    ; receipt : B.Receipt.t
    ; graphs : Graph.t list
    }

  let kind = "store.authoring_upload.intent"

  let project_graph_matches project graph =
    Chatml_source.Id.equal
      (P.Authoring.Source.id (Source_ref.source (Graph.lock graph)))
      (B.Receipt.Project.selected_lock project)
    && List.exists (Graph.sources graph) ~f:(fun source ->
      Chatml_source.Id.equal
        (P.Authoring.Source.id (Source_ref.source source))
        (B.Receipt.Project.root_source project))
    && List.exists (Graph.sources graph) ~f:(fun source ->
      Chatml_source.Id.equal
        (P.Authoring.Source.id (Source_ref.source source))
        (B.Receipt.Project.entry_source project))
  ;;

  let graphs_match_receipt receipt graphs =
    match B.Receipt.basis receipt, graphs with
    | Project project, [ graph ] ->
      Option.is_none (Graph.selector graph) && project_graph_matches project graph
    | Project _, _ -> false
    | Chatmd_document document, _ ->
      let selected =
        List.filter_map (B.Receipt.Chatmd_document.scripts document) ~f:(function
          | B.Receipt.Script.Source_only _ -> None
          | Selected_project { selector; project; _ } -> Some (selector, project))
      in
      List.length selected = List.length graphs
      && List.for_all2_exn selected graphs ~f:(fun (selector, project) graph ->
        Option.value_map
          (Graph.selector graph)
          ~default:false
          ~f:(B.Manifest.Script.Selector.equal selector)
        && project_graph_matches project graph)
  ;;

  let create
        ~owner
        ~upload_id
        ~commit_key
        ~payload_digest
        ~manifest_fingerprint
        ~receipt
        ~graphs
        ~max_graphs
        ~max_sources
        ~max_bytes
    =
    let source_count = ref 0 in
    let next_supplemental = ref 0 in
    let within =
      max_graphs >= 0
      && max_sources >= 0
      && max_graphs <= 65536
      && max_sources <= 65536
      && List.length (List.take graphs (max_graphs + 1)) <= max_graphs
      && List.for_all graphs ~f:(fun graph ->
        let sources = Graph.lock graph :: Graph.sources graph in
        let count = List.length (List.take sources (max_sources + 1)) in
        if count > max_sources - !source_count
        then false
        else (
          source_count := !source_count + count;
          List.for_all sources ~f:(fun source ->
            match Source_ref.location source with
            | Uploaded_member _ -> true
            | Supplemental_file ordinal ->
              if ordinal <> !next_supplemental
              then false
              else (
                incr next_supplemental;
                true))))
    in
    if
      max_graphs < 0
      || max_sources < 0
      || max_bytes <= 0
      || (not within)
      || (not (graphs_match_receipt receipt graphs))
      || (not (B.Owner.equal owner (B.Receipt.owner receipt)))
      || (not (P.Id.Authoring_upload.equal upload_id (B.Receipt.upload_id receipt)))
      || not
           (Chatml_source.Digest.equal
              manifest_fingerprint
              (B.Receipt.manifest_fingerprint receipt))
    then Or_error.error_string "invalid published bundle intent"
    else
      Ok
        { owner
        ; upload_id
        ; commit_key
        ; payload_digest
        ; manifest_fingerprint
        ; receipt
        ; graphs
        }
  ;;

  let owner t = t.owner
  let upload_id t = t.upload_id
  let commit_key t = t.commit_key
  let payload_digest t = t.payload_digest
  let manifest_fingerprint t = t.manifest_fingerprint
  let receipt t = t.receipt
  let graphs t = t.graphs

  let supplementary t =
    List.concat_map t.graphs ~f:(fun graph -> Graph.lock graph :: Graph.sources graph)
    |> List.filter_map ~f:(fun source ->
      match Source_ref.location source with
      | Uploaded_member _ -> None
      | Supplemental_file ordinal -> Some (ordinal, Source_ref.source source))
  ;;

  let supplementary_count t = List.length (supplementary t)

  let supplementary_bytes t =
    List.fold (supplementary t) ~init:0 ~f:(fun count (_, source) ->
      count + P.Authoring.Source.byte_length source)
  ;;

  let to_payload t =
    `Object
      [ "owner", B.Owner.to_json t.owner
      ; "upload_id", P.Id.Authoring_upload.to_json t.upload_id
      ; "commit_key", P.Idempotency_key.to_json t.commit_key
      ; "payload_digest", `String (Chatml_source.Digest.to_hex t.payload_digest)
      ; ( "manifest_fingerprint"
        , `String (Chatml_source.Digest.to_hex t.manifest_fingerprint) )
      ; "receipt", B.Receipt.to_json t.receipt
      ; "graphs", `Array (List.map t.graphs ~f:Graph.to_json)
      ]
  ;;

  let to_bytes t ~max_bytes = encode ~kind ~max_bytes (to_payload t)

  let of_bytes bytes ~max_bytes ~max_graphs ~max_sources =
    let* json = decode ~kind ~max_bytes bytes in
    let* () =
      exact_fields
        json
        [ "owner"
        ; "upload_id"
        ; "commit_key"
        ; "payload_digest"
        ; "manifest_fingerprint"
        ; "receipt"
        ; "graphs"
        ]
      |> F.store
    in
    let* owner = field json "owner" (id B.Owner.of_json) |> F.store in
    let* upload_id =
      field json "upload_id" (id P.Id.Authoring_upload.of_json) |> F.store
    in
    let* commit_key = field json "commit_key" (id P.Idempotency_key.of_json) |> F.store in
    let* payload_digest = field json "payload_digest" digest |> F.store in
    let* manifest_fingerprint = field json "manifest_fingerprint" digest |> F.store in
    let* receipt = field json "receipt" (id B.Receipt.of_json) |> F.store in
    let* graphs =
      (let* values = field json "graphs" F.array in
       if List.length (List.take values (max_graphs + 1)) > max_graphs
       then invalid "graphs" "too many published graphs"
       else Result.all (List.map values ~f:Graph.of_json))
      |> F.store
    in
    let* intent =
      create
        ~owner
        ~upload_id
        ~commit_key
        ~payload_digest
        ~manifest_fingerprint
        ~receipt
        ~graphs
        ~max_graphs
        ~max_sources
        ~max_bytes
      |> Result.map_error ~f:(fun _ ->
        Store_error.Corrupt "invalid published bundle intent")
    in
    Ok intent
  ;;
end

module Marker = struct
  type t =
    { intent_digest : Chatml_source.Digest.t
    ; inventory_digest : Chatml_source.Digest.t
    ; receipt_digest : Chatml_source.Digest.t
    }

  let kind = "store.authoring_upload.marker"

  let create ~intent_digest ~inventory_digest ~receipt_digest =
    { intent_digest; inventory_digest; receipt_digest }
  ;;

  let intent_digest t = t.intent_digest
  let inventory_digest t = t.inventory_digest
  let receipt_digest t = t.receipt_digest

  let to_bytes t ~max_bytes =
    encode
      ~kind
      ~max_bytes
      (`Object
          [ "intent_digest", `String (Chatml_source.Digest.to_hex t.intent_digest)
          ; "inventory_digest", `String (Chatml_source.Digest.to_hex t.inventory_digest)
          ; "receipt_digest", `String (Chatml_source.Digest.to_hex t.receipt_digest)
          ])
  ;;

  let of_bytes bytes ~max_bytes =
    let* json = decode ~kind ~max_bytes bytes in
    let* () =
      exact_fields json [ "intent_digest"; "inventory_digest"; "receipt_digest" ]
      |> F.store
    in
    let* intent_digest = field json "intent_digest" digest |> F.store in
    let* inventory_digest = field json "inventory_digest" digest |> F.store in
    let* receipt_digest = field json "receipt_digest" digest |> F.store in
    Ok { intent_digest; inventory_digest; receipt_digest }
  ;;
end
