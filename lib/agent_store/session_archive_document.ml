open! Core
module D = Document_schema
module F = Document_fields
module P = Agent_protocol
module R = Session_archive_record

type t = { carrier : R.t D.Extension_carrier.t }

let configuration_exn result =
  Result.map_error result ~f:(fun e -> Sexp.to_string_hum (D.Error.sexp_of_t e))
  |> Result.ok_or_failwith
;;

let limits = F.limits ~max_bytes:32768 |> configuration_exn
let kind = "store.session_archive"

let enum name values json =
  let open Result.Let_syntax in
  let%bind text = F.string json in
  List.Assoc.find values text ~equal:String.equal
  |> Result.of_option
       ~error:
         (D.Error.Invalid_field
            { path = [ name ]; reason = "unsupported lifecycle value" })
;;

let status_to_json = function
  | R.Status.Active -> `String "active"
  | Archived -> `String "archived"
  | Removed -> `String "removed"
;;

let status_of_json =
  enum "status" [ "active", R.Status.Active; "archived", Archived; "removed", Removed ]
;;

let admission_to_json = function
  | R.Admission.Automatic -> `String "automatic"
  | Explicit_resume_required -> `String "explicit_resume_required"
;;

let admission_of_json =
  enum
    "admission"
    [ "automatic", R.Admission.Automatic
    ; "explicit_resume_required", Explicit_resume_required
    ]
;;

let action_to_json = function
  | R.Outcome.Archive -> `String "archive"
  | Restore -> `String "restore"
  | Resume -> `String "resume"
  | Remove -> `String "remove"
;;

let action_of_json =
  enum
    "action"
    [ "archive", R.Outcome.Archive
    ; "restore", Restore
    ; "resume", Resume
    ; "remove", Remove
    ]
;;

let disposition_to_json = function
  | R.Outcome.Applied -> `String "applied"
  | Already_current -> `String "already_current"
;;

let disposition_of_json =
  enum "disposition" [ "applied", R.Outcome.Applied; "already_current", Already_current ]
;;

let revision_of_json json =
  F.decimal json |> Result.bind ~f:(fun value -> R.Revision.of_int64 value |> F.protocol)
;;

let timestamp json = P.Timestamp.of_json json |> F.protocol
let session_id json = P.Id.Session.of_json json |> F.protocol

let key_json (key : Idempotency_store.Key.t) =
  `Object
    [ "principal_id", P.Id.Principal.to_json key.principal_id
    ; "session_id", F.option_json key.session_id ~f:P.Id.Session.to_json
    ; "method_name", `String key.method_name
    ; "idempotency_key", P.Idempotency_key.to_json key.idempotency_key
    ]
;;

let receipt_id key =
  P.Json_codec.canonical_string (key_json key)
  |> F.protocol
  |> Result.map ~f:(fun bytes ->
    Digestif.SHA256.digest_string bytes |> Digestif.SHA256.to_hex)
;;

let key_decode json =
  let open Result.Let_syntax in
  let%bind principal_id =
    F.required json "principal_id" (fun j -> P.Id.Principal.of_json j |> F.protocol)
  in
  let%bind session_id = F.required json "session_id" session_id in
  let%bind method_name = F.required json "method_name" F.string in
  let%map idempotency_key =
    F.required json "idempotency_key" (fun j -> P.Idempotency_key.of_json j |> F.protocol)
  in
  Idempotency_store.Key.
    { principal_id; session_id = Some session_id; method_name; idempotency_key }
;;

let anchor_json (a : R.Anchor.t) =
  `Object
    [ "generation", `Number (Int.to_string a.generation)
    ; "session_revision", F.decimal_json a.session_revision
    ; "latest_event_sequence", F.decimal_json a.latest_event_sequence
    ]
;;

let anchor_decode json =
  let open Result.Let_syntax in
  let%bind generation =
    F.required json "generation" (fun j ->
      P.Json_codec.bounded_int ~min:0 ~max:Int.max_value j |> F.protocol)
  in
  let%bind session_revision = F.required json "session_revision" F.decimal in
  let%bind latest_event_sequence = F.required json "latest_event_sequence" F.decimal in
  R.Anchor.create ~generation ~session_revision ~latest_event_sequence |> F.protocol
;;

let outcome_json (o : R.Outcome.t) =
  `Object
    [ "session_id", P.Id.Session.to_json o.session_id
    ; "anchor", anchor_json o.anchor
    ; "lifecycle_revision", F.decimal_json (R.Revision.to_int64 o.lifecycle_revision)
    ; "status", status_to_json o.status
    ; "admission", admission_to_json o.admission
    ; "action", action_to_json o.action
    ; "disposition", disposition_to_json o.disposition
    ; "completed_at", P.Timestamp.to_json o.completed_at
    ]
;;

let outcome_decode json =
  let open Result.Let_syntax in
  let%bind session_id = F.required json "session_id" session_id in
  let%bind anchor = F.required json "anchor" anchor_decode in
  let%bind lifecycle_revision = F.required json "lifecycle_revision" revision_of_json in
  let%bind status = F.required json "status" status_of_json in
  let%bind admission = F.required json "admission" admission_of_json in
  let%bind action = F.required json "action" action_of_json in
  let%bind disposition = F.required json "disposition" disposition_of_json in
  let%bind completed_at = F.required json "completed_at" timestamp in
  R.Outcome.create
    ~session_id
    ~anchor
    ~lifecycle_revision
    ~status
    ~admission
    ~action
    ~disposition
    ~completed_at
  |> F.protocol
;;

let receipt_decode json =
  let open Result.Let_syntax in
  let%bind key = F.required json "key" key_decode in
  let%bind id = F.required json "id" F.digest in
  let%bind expected = receipt_id key in
  if not (String.equal id expected)
  then F.invalid "id" "lifecycle receipt identity differs"
  else (
    let%bind request_digest = F.required json "request_digest" F.digest in
    let%bind outcome = F.required json "outcome" outcome_decode in
    let%bind created_at = F.required json "created_at" timestamp in
    let%bind expires_at = F.required json "expires_at" timestamp in
    let%bind completion_acknowledged =
      F.required json "completion_acknowledged" F.boolean
    in
    R.Receipt.create
      ~key
      ~request_digest
      ~outcome
      ~created_at
      ~expires_at
      ~completion_acknowledged
    |> F.protocol)
;;

let receipt_json (r : R.Receipt.t) =
  let open Result.Let_syntax in
  let%map id = receipt_id r.key in
  `Object
    [ "id", `String id
    ; "key", key_json r.key
    ; "request_digest", `String r.request_digest
    ; "outcome", outcome_json r.outcome
    ; "created_at", P.Timestamp.to_json r.created_at
    ; "expires_at", P.Timestamp.to_json r.expires_at
    ; ("completion_acknowledged", if r.completion_acknowledged then `True else `False)
    ]
;;

let decode json =
  let open Result.Let_syntax in
  let%bind session_id = F.required json "session_id" session_id in
  let%bind status = F.required json "status" status_of_json in
  let%bind admission = F.required json "admission" admission_of_json in
  let%bind revision = F.required json "revision" revision_of_json in
  let%bind values = F.required json "receipts" F.array in
  let%bind receipts = List.map values ~f:receipt_decode |> Result.all in
  R.restore ~session_id ~status ~admission ~revision ~receipts |> F.protocol
;;

let encode state =
  let open Result.Let_syntax in
  let%map receipts = List.map (R.receipts state) ~f:receipt_json |> Result.all in
  `Object
    [ "session_id", P.Id.Session.to_json (R.session_id state)
    ; "status", status_to_json (R.status state)
    ; "admission", admission_to_json (R.admission state)
    ; "revision", F.decimal_json (R.Revision.to_int64 (R.revision state))
    ; "receipts", `Array receipts
    ]
;;

let values names = F.shape (List.map names ~f:(fun name -> name, D.Shape.value))

let outcome_shape =
  F.shape
    [ "session_id", D.Shape.value
    ; "anchor", values [ "generation"; "session_revision"; "latest_event_sequence" ]
    ; "lifecycle_revision", D.Shape.value
    ; "status", D.Shape.value
    ; "admission", D.Shape.value
    ; "action", D.Shape.value
    ; "disposition", D.Shape.value
    ; "completed_at", D.Shape.value
    ]
;;

let receipt_shape =
  F.shape
    [ "id", D.Shape.value
    ; "key", values [ "principal_id"; "session_id"; "method_name"; "idempotency_key" ]
    ; "request_digest", D.Shape.value
    ; "outcome", outcome_shape
    ; "created_at", D.Shape.value
    ; "expires_at", D.Shape.value
    ; "completion_acknowledged", D.Shape.value
    ]
;;

let shape =
  F.shape
    [ "session_id", D.Shape.value
    ; "status", D.Shape.value
    ; "admission", D.Shape.value
    ; "revision", D.Shape.value
    ; ( "receipts"
      , D.Shape.array receipt_shape ~identity_field:(Some "id") |> configuration_exn )
    ]
;;

let codec =
  D.Domain_codec.create
    ~limits
    ~kind
    ~version:2
    ~shape
    ~supported_semantics:[]
    ~decode
    ~encode
  |> configuration_exn
;;

let stored_session_id document =
  let open Result.Let_syntax in
  let%bind () = F.expect_versions document ~kind ~versions:[ 1; 2 ] in
  F.required (D.Document.payload document) "session_id" session_id
;;

let of_document document =
  let open Result.Let_syntax in
  let%bind _ = stored_session_id document in
  let%bind step =
    D.Conversion.Step.of_function ~kind ~from_version:1 ~f:(function
      | `Object fields ->
        let%map fields =
          List.fold_result
            [ "status", `String "archived"
            ; "admission", `String "explicit_resume_required"
            ; "revision", `String "1"
            ; "receipts", `Array []
            ]
            ~init:fields
            ~f:(fun fields (name, value) ->
              match List.Assoc.find fields name ~equal:String.equal with
              | None -> Ok (fields @ [ name, value ])
              | Some original when D.Json.equal original value -> Ok fields
              | Some _ ->
                F.invalid name "original archive field collides with lifecycle semantics")
        in
        `Object fields
      | _ -> F.invalid "payload" "archive payload must be an object")
  in
  let%bind conversion =
    D.Conversion.create
      ~limits
      ~targets:[ kind, 2 ]
      ~max_steps:1
      ~max_operations:1
      ~steps:[ step ]
  in
  let%bind document = D.Conversion.upgrade conversion document in
  let%map carrier = D.Domain_codec.decode codec document in
  { carrier }
;;

let value t = D.Extension_carrier.value t.carrier
let to_document t = D.Domain_codec.encode codec t.carrier

let authored state =
  let open Result.Let_syntax in
  let%bind document =
    D.Domain_codec.encode codec (D.Extension_carrier.of_authored_value state)
  in
  of_document document
;;

let with_state t next ~now =
  let open Result.Let_syntax in
  let%bind () = R.validate_successor (value t) ~next ~now |> F.protocol in
  let next_receipts =
    Map.of_alist_exn
      (module Idempotency_store.Key)
      (List.map (R.receipts next) ~f:(fun r -> r.R.Receipt.key, r))
  in
  let retiring =
    List.filter
      (R.receipts (value t))
      ~f:(fun previous ->
        match Map.find next_receipts previous.R.Receipt.key with
        | Some current ->
          not (R.Receipt.equal previous current || R.Receipt.same_proof previous current)
        | None -> true)
  in
  let%bind ids =
    List.map retiring ~f:(fun r -> receipt_id r.R.Receipt.key) |> Result.all
  in
  let retired = String.Set.of_list ids in
  let%bind carrier =
    match D.Extension_carrier.template t.carrier with
    | None -> Ok t.carrier
    | Some document ->
      let payload = D.Document.payload document in
      let%bind payload =
        match payload with
        | `Object fields ->
          Ok
            (`Object
                (List.map fields ~f:(fun (name, json) ->
                   if String.equal name "receipts"
                   then
                     ( name
                     , match json with
                       | `Array entries ->
                         `Array
                           (List.filter entries ~f:(fun entry ->
                              match D.Json.field entry ~name:"id" with
                              | Value (`String id) -> not (Set.mem retired id)
                              | Absent | Null | Value _ -> true))
                       | _ -> json )
                   else name, json)))
        | _ -> F.invalid "payload" "archive payload must be an object"
      in
      let envelope =
        match D.Document.json document with
        | `Object fields ->
          `Object
            (List.map fields ~f:(fun (name, value) ->
               name, if String.equal name "payload" then payload else value))
        | json -> json
      in
      let%bind document = D.Document.inspect ~limits envelope in
      D.Domain_codec.decode codec document
  in
  let changed = { carrier = D.Extension_carrier.with_value carrier next } in
  let%bind document = to_document changed in
  of_document document
;;

let prepare t prepared ~now =
  if not (R.equal (value t) (R.Prepared.previous prepared))
  then F.invalid "basis" "archive preparation basis differs"
  else with_state t (R.Prepared.next prepared) ~now
;;

let acknowledge t ~key ~request_digest =
  let open Result.Let_syntax in
  let%bind next = R.acknowledge (value t) ~key ~request_digest |> F.protocol in
  (* Acknowledgement retires nothing; only immutable receipt bookkeeping changes. *)
  let changed = { carrier = D.Extension_carrier.with_value t.carrier next } in
  let%bind document = to_document changed in
  of_document document
;;
