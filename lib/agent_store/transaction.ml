open! Core
module D = Document_schema
module F = Document_fields

let kind = "session.transaction"
let current_schema_version = 1

module Value = struct
  type t =
    { schema_version : int
    ; session_id : Agent_protocol.Id.Session.t
    ; generation : int
    ; transaction_sequence : int64
    ; previous_transaction_hash : string option
    ; session_revision : int64
    ; first_event_sequence : int64 option
    ; last_event_sequence : int64 option
    ; accepted_at_ns : int64
    ; command_audit : (D.Document.t[@sexp.opaque]) option
    ; delta : (D.Document.t[@sexp.opaque])
    ; durable_events : (D.Document.t[@sexp.opaque]) list
    }
  [@@deriving sexp_of]
end

module Stored = struct
  type metadata =
    { session_id : string
    ; generation : int64
    ; transaction_sequence : int64
    ; previous_transaction_hash : string option
    ; session_revision : int64
    ; first_event_sequence : int64 option
    ; last_event_sequence : int64 option
    ; accepted_at_ns : int64
    ; durable_event_count : int
    }

  type t =
    { record : Document_record.t
    ; metadata : metadata
    }

  let record t = t.record
  let metadata t = t.metadata
  let digest t = Document_record.stored_digest t.record

  let validate_event_range metadata =
    match metadata.first_event_sequence, metadata.last_event_sequence with
    | None, None when metadata.durable_event_count = 0 -> Ok ()
    | Some first, Some last
      when Int64.(last >= first)
           && metadata.durable_event_count > 0
           && Int64.equal
                Int64.(last - first)
                (Int64.of_int (metadata.durable_event_count - 1)) -> Ok ()
    | _ -> F.invalid "durable_events" "event range differs from event count"
  ;;

  let project ~limits payload =
    let open Result.Let_syntax in
    let%bind session_id = F.required payload "session_id" F.string in
    let%bind generation = F.required payload "generation" F.decimal in
    let%bind transaction_sequence = F.required payload "transaction_sequence" F.decimal in
    let%bind previous_transaction_hash =
      F.optional payload "previous_transaction_hash" F.digest
    in
    let%bind session_revision = F.required payload "session_revision" F.decimal in
    let%bind first_event_sequence = F.optional payload "first_event_sequence" F.decimal in
    let%bind last_event_sequence = F.optional payload "last_event_sequence" F.decimal in
    let%bind accepted_at_ns = F.required payload "accepted_at_ns" F.decimal in
    let%bind durable_events = F.required payload "durable_events" F.array in
    let metadata =
      { session_id
      ; generation
      ; transaction_sequence
      ; previous_transaction_hash
      ; session_revision
      ; first_event_sequence
      ; last_event_sequence
      ; accepted_at_ns
      ; durable_event_count = List.length durable_events
      }
    in
    let%bind () = validate_event_range metadata in
    let%bind delta = F.required payload "delta" (F.document ~limits) in
    let%bind () = F.expect_versions delta ~kind:"session.delta" ~versions:[ 1; 2 ] in
    let%bind audit = F.optional payload "command_audit" (F.document ~limits) in
    let%bind () =
      match audit with
      | None -> Ok ()
      | Some audit -> F.expect audit ~kind:"session.command_audit" ~version:1
    in
    let%map () =
      List.foldi durable_events ~init:(Ok ()) ~f:(fun index checked json ->
        let%bind () = checked in
        let%bind event = F.document ~limits json in
        let%bind () = F.expect event ~kind:"session.event" ~version:1 in
        let event = D.Document.payload event in
        let%bind event_session = F.required event "session_id" F.string in
        let%bind sequence = F.required event "sequence" F.decimal in
        let%bind revision = F.required event "revision" F.decimal in
        match first_event_sequence with
        | None -> F.invalid "durable_events" "event has no stored sequence range"
        | Some first ->
          if
            String.equal event_session session_id
            && Int64.equal sequence Int64.(first + of_int index)
            && Int64.equal revision session_revision
          then Ok ()
          else F.invalid "durable_events" "stored event metadata differs from transaction")
    in
    metadata
  ;;

  let of_record record =
    let document = Document_record.document record in
    let open Result.Let_syntax in
    let%bind () = F.expect document ~kind ~version:1 |> F.store in
    let%bind limits =
      F.limits ~max_bytes:(String.length (Document_record.stored_bytes record)) |> F.store
    in
    let%map metadata = project ~limits (D.Document.payload document) |> F.store in
    { record; metadata }
  ;;
end

type provenance =
  { stored : Stored.t
  ; carrier : Value.t D.Extension_carrier.t
  }

type t =
  { schema_version : int
  ; session_id : Agent_protocol.Id.Session.t
  ; generation : int
  ; transaction_sequence : int64
  ; previous_transaction_hash : string option
  ; session_revision : int64
  ; first_event_sequence : int64 option
  ; last_event_sequence : int64 option
  ; accepted_at_ns : int64
  ; command_audit : (D.Document.t[@sexp.opaque]) option
  ; delta : (D.Document.t[@sexp.opaque])
  ; durable_events : (D.Document.t[@sexp.opaque]) list
  ; provenance : (provenance[@sexp.opaque])
  }
[@@deriving sexp_of]

let value t = D.Extension_carrier.value t.provenance.carrier
let carrier t = t.provenance.carrier
let stored t = t.provenance.stored
let hash t = Stored.digest (stored t)
let encode t = Document_record.stored_bytes (Stored.record (stored t))

let of_carrier stored carrier =
  let value = D.Extension_carrier.value carrier in
  { schema_version = value.Value.schema_version
  ; session_id = value.session_id
  ; generation = value.generation
  ; transaction_sequence = value.transaction_sequence
  ; previous_transaction_hash = value.previous_transaction_hash
  ; session_revision = value.session_revision
  ; first_event_sequence = value.first_event_sequence
  ; last_event_sequence = value.last_event_sequence
  ; accepted_at_ns = value.accepted_at_ns
  ; command_audit = value.command_audit
  ; delta = value.delta
  ; durable_events = value.durable_events
  ; provenance = { stored; carrier }
  }
;;

let validate_value (value : Value.t) =
  let open Result.Let_syntax in
  let%bind () =
    if value.generation < 0
    then F.invalid "generation" "must be nonnegative"
    else if value.schema_version <> current_schema_version
    then F.invalid "schema_version" "must equal current envelope version"
    else Ok ()
  in
  let%bind () =
    List.fold_result
      [ "transaction_sequence", value.transaction_sequence
      ; "session_revision", value.session_revision
      ; "accepted_at_ns", value.accepted_at_ns
      ]
      ~init:()
      ~f:(fun () (name, counter) ->
        if Int64.(counter < zero) then F.invalid name "must be nonnegative" else Ok ())
  in
  let%map _ =
    match value.previous_transaction_hash with
    | None -> Ok None
    | Some digest -> Result.map (F.digest (`String digest)) ~f:Option.some
  in
  ()
;;

let decode_value ~limits payload =
  let open Result.Let_syntax in
  let%bind metadata = Stored.project ~limits payload in
  let%bind session_id =
    Agent_protocol.Id.Session.of_string metadata.session_id |> F.protocol
  in
  let%bind generation =
    if Int64.(metadata.generation <= of_int Int.max_value)
    then Ok (Int64.to_int_exn metadata.generation)
    else F.invalid "generation" "exceeds host integer range"
  in
  let%bind command_audit = F.optional payload "command_audit" (F.document ~limits) in
  let%bind delta = F.required payload "delta" (F.document ~limits) in
  let%bind events = F.required payload "durable_events" F.array in
  let%bind durable_events = Result.all (List.map events ~f:(F.document ~limits)) in
  let value =
    Value.
      { schema_version = current_schema_version
      ; session_id
      ; generation
      ; transaction_sequence = metadata.transaction_sequence
      ; previous_transaction_hash = metadata.previous_transaction_hash
      ; session_revision = metadata.session_revision
      ; first_event_sequence = metadata.first_event_sequence
      ; last_event_sequence = metadata.last_event_sequence
      ; accepted_at_ns = metadata.accepted_at_ns
      ; command_audit
      ; delta
      ; durable_events
      }
  in
  let%map () = validate_value value in
  value
;;

let encode_value (value : Value.t) =
  let open Result.Let_syntax in
  let%map () = validate_value value in
  `Object
    [ "session_id", `String (Agent_protocol.Id.Session.to_string value.session_id)
    ; "generation", F.decimal_json (Int64.of_int value.generation)
    ; "transaction_sequence", F.decimal_json value.transaction_sequence
    ; ( "previous_transaction_hash"
      , F.option_json value.previous_transaction_hash ~f:(fun value -> `String value) )
    ; "session_revision", F.decimal_json value.session_revision
    ; "first_event_sequence", F.option_json value.first_event_sequence ~f:F.decimal_json
    ; "last_event_sequence", F.option_json value.last_event_sequence ~f:F.decimal_json
    ; "accepted_at_ns", F.decimal_json value.accepted_at_ns
    ; "command_audit", F.option_json value.command_audit ~f:D.Document.json
    ; "delta", D.Document.json value.delta
    ; "durable_events", `Array (List.map value.durable_events ~f:D.Document.json)
    ]
;;

let codec ~limits =
  D.Domain_codec.create
    ~limits
    ~kind
    ~version:current_schema_version
    ~shape:
      (F.shape
         (List.map
            [ "session_id"
            ; "generation"
            ; "transaction_sequence"
            ; "previous_transaction_hash"
            ; "session_revision"
            ; "first_event_sequence"
            ; "last_event_sequence"
            ; "accepted_at_ns"
            ; "command_audit"
            ; "delta"
            ; "durable_events"
            ]
            ~f:(fun name -> name, D.Shape.value)))
    ~supported_semantics:[]
    ~decode:(decode_value ~limits)
    ~encode:encode_value
;;

let restore stored ~limits =
  let open Result.Let_syntax in
  let%bind codec = codec ~limits |> F.store in
  let%bind document =
    F.upgrade (Document_record.document (Stored.record stored)) ~limits ~kind |> F.store
  in
  let%map carrier = D.Domain_codec.decode codec document |> F.store in
  of_carrier stored carrier
;;

let of_carrier_value ~limits carrier =
  let open Result.Let_syntax in
  let%bind codec = codec ~limits |> F.store in
  let%bind document = D.Domain_codec.encode codec carrier |> F.store in
  let%bind record =
    Document_record.of_document document ~limits |> Result.map_error ~f:F.record_error
  in
  let%bind stored = Stored.of_record record in
  restore stored ~limits
;;

let create
      ~limits
      ~session_id
      ~generation
      ~transaction_sequence
      ~previous_transaction_hash
      ~session_revision
      ~first_event_sequence
      ~last_event_sequence
      ~accepted_at_ns
      ~command_audit
      ~delta
      ~durable_events
  =
  let value =
    Value.
      { schema_version = current_schema_version
      ; session_id
      ; generation
      ; transaction_sequence
      ; previous_transaction_hash
      ; session_revision
      ; first_event_sequence
      ; last_event_sequence
      ; accepted_at_ns
      ; command_audit
      ; delta
      ; durable_events
      }
  in
  of_carrier_value ~limits (D.Extension_carrier.of_authored_value value)
;;

let with_value t ~limits value =
  of_carrier_value ~limits (D.Extension_carrier.with_value (carrier t) value)
;;

let validate t = validate_value (value t) |> F.store

let decode_record record ~limits =
  let open Result.Let_syntax in
  let%bind stored = Stored.of_record record in
  restore stored ~limits
;;

let decode payload =
  let open Result.Let_syntax in
  let%bind limits =
    F.limits ~max_bytes:(D.Limits.max_bytes D.Limits.default) |> F.store
  in
  let%bind document = D.Document.decode ~limits payload |> F.store in
  (* This payload entry point follows framing in existing consumers. Keep exact
     input bytes, rather than normalizing through an authored record. *)
  let%bind framed =
    Frame.encode ~max_payload_length:(D.Limits.max_bytes limits) ~flags:0 payload
    |> Result.map_error ~f:(fun error -> Store_error.Framing error)
  in
  let%bind record =
    Document_record.decode_file ~limits ~expected_digest:None framed
    |> Result.map_error ~f:F.record_error
  in
  let (_ : D.Document.t) = document in
  decode_record record ~limits
;;
