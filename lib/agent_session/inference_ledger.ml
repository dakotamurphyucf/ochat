open! Core
module P = Agent_protocol
module Q = P.Inference_query
module O = Inference.Observation
module D = Document_schema
module C = D.Extension_carrier
module X = Persistence_codec

let ( let* ) result f = Result.bind result ~f

module Limit = struct
  type t =
    | Attempt_count
    | Turn_count
    | Retained_bytes
    | Protected_future_data
  [@@deriving equal, sexp_of]
end

module Error = struct
  type t =
    | Document of D.Error.t
    | Observation of O.Error.t
    | Invalid_field of
        { field : string
        ; reason : string
        }
    | Conflicting_handle
    | Ordinal_exhausted
    | Invalid_transition
  [@@deriving equal, sexp_of]
end

let document result = Result.map_error result ~f:(fun error -> Error.Document error)
let observation result = Result.map_error result ~f:(fun error -> Error.Observation error)
let invalid field reason = Error (Error.Invalid_field { field; reason })

let invariant result =
  Result.ok_or_failwith
    (Result.map_error result ~f:(fun error -> Sexp.to_string_hum (Error.sexp_of_t error)))
;;

let protocol field result =
  Result.map_error result ~f:(fun _ ->
    Error.Invalid_field { field; reason = "invalid host value" })
;;

let increment value =
  if Int64.equal value Int64.max_value
  then Error Error.Ordinal_exhausted
  else Ok Int64.(value + one)
;;

module Limits = struct
  type t =
    { max_attempts : int
    ; max_turns : int
    ; max_retained_bytes : int
    ; document_limits : D.Limits.t
    }

  let create ~max_attempts ~max_turns ~max_retained_bytes ~document_limits =
    if max_attempts <= 0 || max_turns <= 0 || max_retained_bytes <= 0
    then invalid "limits" "required positive bounds"
    else Ok { max_attempts; max_turns; max_retained_bytes; document_limits }
  ;;

  let default =
    let document_limits =
      D.Limits.create
        ~max_bytes:(4 * 1024 * 1024)
        ~max_depth:256
        ~max_fields:1_000_000
        ~max_nodes:2_000_000
      |> document
      |> invariant
    in
    create
      ~max_attempts:256
      ~max_turns:256
      ~max_retained_bytes:(4 * 1024 * 1024)
      ~document_limits
    |> invariant
  ;;
end

type tracking =
  | Tracked
  | Untracked of Limit.t
[@@deriving equal, sexp_of]

type observation_disposition =
  | Added
  | Replaced
  | Duplicate
  | Stale
  | Ignored_retired
  | Ignored_untracked
  | Diagnostic_omitted
[@@deriving equal, sexp_of]

module Handle = struct
  type t =
    { session_id : P.Id.Session.t
    ; generation : int
    ; ordinal : int64
    ; scope : Transcript.Scope.t
    ; accounting_id : O.Observation_id.t
    ; context_id : O.Observation_id.t
    ; operation_id : P.Id.Operation.t option
    ; invocation_id : P.Id.Invocation.t option
    ; tracking : tracking
    }

  let session_id t = t.session_id
  let generation t = t.generation
  let ordinal t = t.ordinal
  let scope t = t.scope
  let accounting_id t = t.accounting_id
  let context_id t = t.context_id
  let operation_id t = t.operation_id
  let invocation_id t = t.invocation_id

  let equal a b =
    P.Id.Session.equal a.session_id b.session_id
    && Int.equal a.generation b.generation
    && Int64.equal a.ordinal b.ordinal
    && Transcript.Scope.equal a.scope b.scope
    && O.Observation_id.equal a.accounting_id b.accounting_id
    && O.Observation_id.equal a.context_id b.context_id
    && Option.equal P.Id.Operation.equal a.operation_id b.operation_id
    && Option.equal P.Id.Invocation.equal a.invocation_id b.invocation_id
    && equal_tracking a.tracking b.tracking
  ;;
end

module Row = struct
  type t =
    { handle : Handle.t
    ; record : O.Attempt_record.t
    }

  let handle t = t.handle
  let record t = t.record
end

module Turn_handle = struct
  type t =
    { session_id : P.Id.Session.t
    ; operation_id : P.Id.Operation.t
    ; generation : int
    ; ordinal : int64
    ; tracking : tracking
    }

  let operation_id t = t.operation_id
  let generation t = t.generation
end

type turn =
  { handle : Turn_handle.t
  ; operation : P.Operation.t
  }

type value =
  { session_id : P.Id.Session.t
  ; generation : int
  ; last_ordinal : int64
  ; last_turn_ordinal : int64
  ; revision : int64
  ; coverage : Q.Coverage.t
  ; rows : Row.t Int64.Map.t
  ; turns : turn Int64.Map.t
  }

type t =
  { carrier : value C.t
  ; limits : Limits.t
  }

let value t = C.value t.carrier
let revision t = (value t).revision
let rows t = Map.data (value t).rows
let find t ~ordinal = Map.find (value t).rows ordinal
let str s = `String s
let dec n = str (Int64.to_string n)
let integer n = `Number (Int.to_string n)
let obj f = `Object f

let opt f = function
  | None -> `Null
  | Some value -> f value
;;

(* These shapes declare ownership of typed, mutable components. Future fields
   stay in the child carrier; the public safe codecs receive only owned fields. *)
module Shape = struct
  let v = D.Shape.value
  let o = X.shape_exn
  let a ?identity_field shape = X.array_shape_exn ?identity_field shape
  let fields = X.fields_shape
  let tag name cases = X.tagged_shape_exn ~discriminator:name cases
  let key = fields [ "source"; "attempt" ]

  let scope =
    o
      [ "key", key
      ; ( "parent"
        , D.Shape.nullable (o [ "scope", key; "call_entry_id", v; "call_alias", v ]) )
      ]
  ;;

  let count =
    tag
      "kind"
      [ "unknown", fields [ "kind"; "reason" ]
      ; "actual", fields [ "kind"; "tokens" ]
      ; ( "estimated"
        , o [ "kind", v; "tokens", v; "estimator", fields [ "method"; "version" ] ] )
      ]
  ;;

  let usage =
    o
      [ ( "counts"
        , o
            (List.map
               [ "input"
               ; "output"
               ; "reported_total"
               ; "cached_input"
               ; "cache_write_input"
               ; "reasoning_output"
               ]
               ~f:(fun name -> name, count)) )
      ; "inclusions", a (fields [ "subset"; "included_in" ])
      ]
  ;;

  let context = o [ "preparation_id", v; "count", count; "capacity", v ]

  let selection =
    tag
      "kind"
      [ "omitted", fields [ "kind" ]
      ; "null", fields [ "kind" ]
      ; "withheld", fields [ "kind" ]
      ; "value", fields [ "kind"; "value" ]
      ]
  ;;

  let feature =
    tag
      "kind"
      (List.map
         [ "text_input"
         ; "image_input"
         ; "document_input"
         ; "function_tools"
         ; "custom_tools"
         ; "opaque_replay"
         ]
         ~f:(fun name -> name, fields [ "kind" ])
       @ [ "setting", fields [ "kind"; "name" ] ])
  ;;

  let configuration =
    o
      [ "adapter", v
      ; "profile", v
      ; "profile_revision", v
      ; "account", v
      ; "model", v
      ; "preparation_id", v
      ; "transport", v
      ; ( "settings"
        , a
            ~identity_field:"name"
            (o [ "name", v; "selection", selection; "provenance", v ]) )
      ; "withheld_settings", v
      ; "capabilities", a (o [ "feature", feature; "support", v ])
      ]
  ;;

  let failure =
    tag
      "reason"
      (List.map
         [ "missing"
         ; "denied"
         ; "invalid_credential"
         ; "timed_out"
         ; "invalid_request"
         ; "rate_limited"
         ; "unavailable"
         ; "unknown"
         ; "connection"
         ; "timeout"
         ; "invalid_http"
         ; "invalid_content_type"
         ; "body_limit"
         ; "framing_limit"
         ; "protocol"
         ]
         ~f:(fun reason -> reason, fields [ "type"; "reason" ])
       @ [ "http_status", fields [ "type"; "reason"; "status" ] ])
  ;;

  let outcome =
    tag
      "type"
      [ "completed", fields [ "type" ]
      ; "refused", fields [ "type" ]
      ; "incomplete", fields [ "type"; "reason" ]
      ; "failed", o [ "type", v; "failure", failure ]
      ]
  ;;

  let terminal = o [ "scope", scope; "delivery", v; "outcome", outcome ]

  let state =
    tag
      "kind"
      [ "prepared", fields [ "kind" ]
      ; "running", fields [ "kind" ]
      ; "terminal", o [ "kind", v; "terminal", terminal ]
      ; "interrupted", fields [ "kind"; "reason"; "delivery" ]
      ]
  ;;

  let diagnostic_reason =
    tag
      "kind"
      (List.map
         [ "connection"
         ; "timeout"
         ; "malformed_protocol"
         ; "unsupported_input"
         ; "provider_failure"
         ; "local_result_invalid"
         ; "conflicting_observation"
         ]
         ~f:(fun name -> name, fields [ "kind" ])
       @ [ "authentication", fields [ "kind"; "reason" ]
         ; "http_status", fields [ "kind"; "status" ]
         ; "limit", fields [ "kind"; "limit" ]
         ])
  ;;

  let diagnostic =
    o [ "phase", v; "reason", diagnostic_reason; "delivery", v; "elapsed_ms", v ]
  ;;

  let observation =
    tag
      "kind"
      (List.map
         [ "usage", usage
         ; "context_estimate", context
         ; "configuration", configuration
         ; "diagnostic", diagnostic
         ]
         ~f:(fun (kind, payload) ->
           ( kind
           , o
               [ "schema_version", v
               ; "scope", scope
               ; "id", v
               ; "revision", v
               ; "kind", v
               ; "payload", payload
               ] )))
  ;;

  let record =
    o
      [ "schema_version", v
      ; "scope", scope
      ; "accounting_id", v
      ; "configuration", configuration
      ; "state", state
      ; "observations", a ~identity_field:"id" observation
      ; "omitted_diagnostics", v
      ]
  ;;

  let handle =
    o
      [ "session_id", v
      ; "generation", v
      ; "ordinal", v
      ; "scope", scope
      ; "accounting_id", v
      ; "context_id", v
      ; "operation_id", v
      ; "invocation_id", v
      ]
  ;;

  let row = o [ "ordinal", v; "handle", handle; "record", record ]
  let turn = o [ "ordinal", v; "operation", Session_record_shapes.operation ]

  let coverage =
    fields
      [ "before_tracking_unknown"
      ; "retired_attempts"
      ; "untracked_attempts"
      ; "retired_turns"
      ; "untracked_turns"
      ; "tracking_limit"
      ]
  ;;

  let ledger =
    o
      [ "session_id", v
      ; "generation", v
      ; "last_ordinal", v
      ; "last_turn_ordinal", v
      ; "revision", v
      ; "coverage", coverage
      ; "rows", a ~identity_field:"ordinal" row
      ; "turns", a ~identity_field:"ordinal" turn
      ]
  ;;
end

let handle_to_json (h : Handle.t) =
  obj
    [ "session_id", P.Id.Session.to_json h.session_id
    ; "generation", integer h.generation
    ; "ordinal", dec h.ordinal
    ; "scope", Transcript.Scope.to_json h.scope
    ; "accounting_id", str (O.Observation_id.to_string h.accounting_id)
    ; "context_id", str (O.Observation_id.to_string h.context_id)
    ; "operation_id", opt P.Id.Operation.to_json h.operation_id
    ; "invocation_id", opt P.Id.Invocation.to_json h.invocation_id
    ]
;;

let row_to_json (r : Row.t) =
  obj
    [ "ordinal", dec r.handle.ordinal
    ; "handle", handle_to_json r.handle
    ; "record", O.Attempt_record.to_json r.record
    ]
;;

let value_to_json v =
  obj
    [ "session_id", P.Id.Session.to_json v.session_id
    ; "generation", integer v.generation
    ; "last_ordinal", dec v.last_ordinal
    ; "last_turn_ordinal", dec v.last_turn_ordinal
    ; "revision", dec v.revision
    ; "coverage", Q.Coverage.to_json v.coverage
    ; "rows", `Array (List.map (Map.data v.rows) ~f:row_to_json)
    ; ( "turns"
      , `Array
          (List.map (Map.data v.turns) ~f:(fun turn ->
             obj
               [ "ordinal", dec turn.handle.ordinal
               ; "operation", P.Operation.to_json turn.operation
               ])) )
    ]
;;

let decode_error field result =
  Result.map_error result ~f:(fun _ ->
    D.Error.Invalid_field { path = [ field ]; reason = "invalid inference ledger field" })
;;

let get fields name f = decode_error name (P.Json_codec.required_as fields name f)
let decimal json = X.nonnegative_int64 json

let decode_id json =
  let* value = P.Json_codec.string json in
  Result.map_error (O.Observation_id.of_string value) ~f:(fun _ ->
    P.Error.create
      Invalid_request
      ~message:"invalid observation identity"
      ~retryable:false
      ())
;;

let record_consistent (h : Handle.t) record =
  if
    (not (Transcript.Scope.equal h.scope (O.Attempt_record.scope record)))
    || not
         (O.Observation_id.equal h.accounting_id (O.Attempt_record.accounting_id record))
  then invalid "record" "conflicting admitted identity"
  else
    Result.all_unit
      (List.map (O.Attempt_record.observations record) ~f:(fun o ->
         match O.payload o with
         | O.Context_estimate _ when not (O.Observation_id.equal h.context_id (O.id o)) ->
           invalid "context_id" "context is not designated identity"
         | O.Usage _ | Context_estimate _ | Configuration _ | Diagnostic _ -> Ok ()))
;;

let validate_value ~limits v =
  if
    v.generation < 0
    || Int64.(v.last_ordinal < zero || v.last_turn_ordinal < zero || v.revision < zero)
  then invalid "metadata" "negative counter"
  else if
    Map.length v.rows > limits.Limits.max_attempts
    || Map.length v.turns > limits.max_turns
  then invalid "rows" "retained count exceeds configured bound"
  else (
    let check_total count retired untracked last =
      let count = Int64.of_int count in
      if
        Int64.(retired > max_value - count || untracked > max_value - count - retired)
        || not (Int64.equal Int64.(count + retired + untracked) last)
      then
        invalid "coverage" "admission counter differs from retained and omitted coverage"
      else Ok ()
    in
    let* () =
      check_total
        (Map.length v.rows)
        v.coverage.retired_attempts
        v.coverage.untracked_attempts
        v.last_ordinal
    in
    let* () =
      check_total
        (Map.length v.turns)
        v.coverage.retired_turns
        v.coverage.untracked_turns
        v.last_turn_ordinal
    in
    let* () =
      let compare_turn_identity left right =
        let generation = Int.compare left.handle.generation right.handle.generation in
        if generation <> 0
        then generation
        else P.Id.Operation.compare left.handle.operation_id right.handle.operation_id
      in
      if List.contains_dup (Map.data v.turns) ~compare:compare_turn_identity
      then invalid "turns" "duplicate retained operation identity"
      else Ok ()
    in
    let* () =
      Result.all_unit
        (List.map (Map.to_alist v.rows) ~f:(fun (ordinal, row) ->
           let h = row.Row.handle in
           let key = Transcript.Scope.key h.scope in
           if
             (not (P.Id.Session.equal h.session_id v.session_id))
             || h.generation < 0
             || h.generation > v.generation
             || (h.generation < v.generation
                 &&
                 match O.Attempt_record.state row.record with
                 | Prepared | Running -> true
                 | Terminal _ | Interrupted _ -> false)
             || Int64.(ordinal <= zero || ordinal > v.last_ordinal)
             || (not (Int64.equal ordinal h.ordinal))
             || (not
                   (String.equal
                      (Transcript.Attempt_id.to_string key.attempt)
                      ("ordinal:" ^ Int64.to_string ordinal)))
             || (not
                   (String.is_prefix
                      (Transcript.Source_id.to_string key.source)
                      ~prefix:(P.Id.Session.to_string v.session_id ^ ":")))
             || (not (String.equal (O.Observation_id.to_string h.accounting_id) "usage"))
             || (not (String.equal (O.Observation_id.to_string h.context_id) "context"))
             || not (equal_tracking h.tracking Tracked)
           then Error Error.Conflicting_handle
           else
             let* () = record_consistent h row.record in
             observation
               (Result.map
                  (O.Attempt_record.of_json
                     (O.Attempt_record.to_json row.record)
                     ~limits:O.Admission.attempt)
                  ~f:(fun _ -> ()))))
    in
    Result.all_unit
      (List.map (Map.to_alist v.turns) ~f:(fun (ordinal, turn) ->
         let h = turn.handle in
         if
           (not (P.Id.Session.equal h.session_id v.session_id))
           || h.generation < 0
           || h.generation > v.generation
           || (h.generation < v.generation
               &&
               match turn.operation.state with
               | Starting | Running | Cancelling -> true
               | Completed | Failed _ | Cancelled | Interrupted _ -> false)
           || (not (Int64.equal ordinal h.ordinal))
           || Int64.(ordinal <= zero || ordinal > v.last_turn_ordinal)
           || (not (P.Id.Operation.equal h.operation_id turn.operation.id))
           || not (Int.equal h.generation turn.operation.generation)
         then Error Error.Conflicting_handle
         else (
           match turn.operation.kind with
           | P.Operation.Turn _ ->
             protocol
               "operation"
               (Result.map
                  (P.Operation.of_json (P.Operation.to_json turn.operation))
                  ~f:(fun _ -> ()))
           | Compaction -> invalid "turn" "compaction is not a host turn"))))
;;

let value_of_json ~limits json =
  let* fields = decode_error "payload" (P.Json_codec.fields json) in
  let* session_id = get fields "session_id" P.Id.Session.of_json in
  let* generation = get fields "generation" X.integer in
  let* last_ordinal = get fields "last_ordinal" decimal in
  let* last_turn_ordinal = get fields "last_turn_ordinal" decimal in
  let* revision = get fields "revision" decimal in
  let* coverage = get fields "coverage" Q.Coverage.of_json in
  let* raw_rows =
    get fields "rows" (function
      | `Array values -> Ok values
      | _ ->
        Error
          (P.Error.create Invalid_request ~message:"expected array" ~retryable:false ()))
  in
  let* raw_turns =
    get fields "turns" (function
      | `Array values -> Ok values
      | _ ->
        Error
          (P.Error.create Invalid_request ~message:"expected array" ~retryable:false ()))
  in
  if
    List.length raw_rows > limits.Limits.max_attempts
    || List.length raw_turns > limits.max_turns
  then
    Error
      (D.Error.Invalid_field
         { path = [ "rows" ]; reason = "configured cardinality exceeded" })
  else
    let* rows =
      Result.all
        (List.map raw_rows ~f:(fun json ->
           let* f = decode_error "row" (P.Json_codec.fields json) in
           let* ordinal = get f "ordinal" decimal in
           let* raw_handle = get f "handle" (fun x -> Ok x) in
           let* hf = decode_error "handle" (P.Json_codec.fields raw_handle) in
           let* sid = get hf "session_id" P.Id.Session.of_json in
           let* gen = get hf "generation" X.integer in
           let* ho = get hf "ordinal" decimal in
           let* scope_json = get hf "scope" (fun x -> Ok x) in
           let* scope =
             decode_error
               "scope"
               (Transcript.Scope.of_json scope_json ~limits:O.Admission.attempt)
           in
           let* accounting_id = get hf "accounting_id" decode_id in
           let* context_id = get hf "context_id" decode_id in
           let* operation_id =
             get hf "operation_id" (X.nullable P.Id.Operation.of_json)
           in
           let* invocation_id =
             get hf "invocation_id" (X.nullable P.Id.Invocation.of_json)
           in
           let* raw_record = get f "record" (fun x -> Ok x) in
           let* record =
             decode_error
               "record"
               (O.Attempt_record.of_json raw_record ~limits:O.Admission.attempt)
           in
           Ok
             ( ordinal
             , { Row.handle =
                   { Handle.session_id = sid
                   ; generation = gen
                   ; ordinal = ho
                   ; scope
                   ; accounting_id
                   ; context_id
                   ; operation_id
                   ; invocation_id
                   ; tracking = Tracked
                   }
               ; record
               } )))
    in
    let* turns =
      Result.all
        (List.map raw_turns ~f:(fun json ->
           let* f = decode_error "turn" (P.Json_codec.fields json) in
           let* ordinal = get f "ordinal" decimal in
           let* operation = get f "operation" P.Operation.of_json in
           Ok
             ( ordinal
             , { handle =
                   { Turn_handle.session_id
                   ; operation_id = operation.id
                   ; generation = operation.generation
                   ; ordinal
                   ; tracking = Tracked
                   }
               ; operation
               } )))
    in
    let* rows =
      decode_error
        "rows"
        (Map.of_alist (module Int64) rows
         |> function
         | `Ok value -> Ok value
         | `Duplicate_key _ -> Error ())
    in
    let* turns =
      decode_error
        "turns"
        (Map.of_alist (module Int64) turns
         |> function
         | `Ok value -> Ok value
         | `Duplicate_key _ -> Error ())
    in
    let v =
      { session_id
      ; generation
      ; last_ordinal
      ; last_turn_ordinal
      ; revision
      ; coverage
      ; rows
      ; turns
      }
    in
    let* () = decode_error "payload" (validate_value ~limits v) in
    Ok v
;;

let codec limits =
  match
    D.Domain_codec.create
      ~limits:limits.Limits.document_limits
      ~kind:"session.inference_ledger"
      ~version:1
      ~shape:Shape.ledger
      ~supported_semantics:[]
      ~decode:(value_of_json ~limits)
      ~encode:(fun v -> Ok (value_to_json v))
  with
  | Ok codec -> codec
  | Error error -> raise_s [%sexp "invalid inference ledger codec", (error : D.Error.t)]
;;

let encode t = document (D.Domain_codec.encode (codec t.limits) t.carrier)
let to_document = encode

let record_with (r : Row.t) ~state ~observations ~omitted_diagnostics =
  observation
    (O.Attempt_record.create
       ~scope:r.handle.scope
       ~accounting_id:r.handle.accounting_id
       ~configuration:(O.Attempt_record.configuration r.record)
       ~state
       ~observations
       ~omitted_diagnostics
       ~limits:O.Admission.attempt)
;;

let ended = function
  | O.Attempt_record.Prepared | Running -> false
  | Terminal _ | Interrupted _ -> true
;;

let turn_ended = function
  | P.Operation.Starting | Running | Cancelling -> false
  | Completed | Failed _ | Cancelled | Interrupted _ -> true
;;

(* The reserve is real bounded admission of the complete captured tree with every
   growable metadata scalar at its longest spelling. No unknown field is removed.
   A later untracked admission/diagnostic omission cannot consume row capacity. *)
let replace_fields json replacements =
  match json with
  | `Object fields ->
    `Object
      (List.map fields ~f:(fun (name, value) ->
         ( name
         , Option.value
             (List.Assoc.find replacements name ~equal:String.equal)
             ~default:value )))
  | _ -> json
;;

let check_capacity t =
  let* doc = encode t in
  let payload = D.Document.payload doc in
  let* reserved_rows =
    match D.Json.field payload ~name:"rows" with
    | Value (`Array rows) ->
      Result.all
        (List.map rows ~f:(fun row ->
           match D.Json.field row ~name:"record" with
           | Value record ->
             let record =
               replace_fields record [ "omitted_diagnostics", dec Int64.max_value ]
             in
             let row = replace_fields row [ "record", record ] in
             let* _ =
               document (D.Json.validate_and_measure ~limits:O.Admission.attempt row)
             in
             Ok row
           | Absent | Null -> invalid "record" "missing admitted record"))
    | Absent | Null | Value _ -> invalid "rows" "missing admitted rows"
  in
  let coverage =
    match D.Json.field payload ~name:"coverage" with
    | Value value ->
      replace_fields
        value
        [ "before_tracking_unknown", `False
        ; "retired_attempts", dec Int64.max_value
        ; "untracked_attempts", dec Int64.max_value
        ; "retired_turns", dec Int64.max_value
        ; "untracked_turns", dec Int64.max_value
        ; "tracking_limit", str "protected_future_data"
        ]
    | Absent | Null -> `Null
  in
  let reserved_payload =
    replace_fields
      payload
      [ "generation", integer Int.max_value
      ; "last_ordinal", dec Int64.max_value
      ; "last_turn_ordinal", dec Int64.max_value
      ; "revision", dec Int64.max_value
      ; "coverage", coverage
      ; "rows", `Array reserved_rows
      ]
  in
  let reserved = replace_fields (D.Document.json doc) [ "payload", reserved_payload ] in
  let* bytes =
    document (D.Json.validate_and_measure ~limits:t.limits.document_limits reserved)
  in
  if bytes > t.limits.max_retained_bytes
  then Error (Error.Document (D.Error.Limit_exceeded "inference retained bytes"))
  else Ok ()
;;

let publish t v =
  let* revision = increment (value t).revision in
  let candidate = { t with carrier = C.with_value t.carrier { v with revision } } in
  let* () = check_capacity candidate in
  let* doc = encode candidate in
  let* carrier = document (D.Domain_codec.decode (codec t.limits) doc) in
  Ok { candidate with carrier }
;;

let create ~session_id ~generation ~before_tracking_unknown ~limits =
  if generation < 0
  then invalid "generation" "negative generation"
  else
    let* coverage =
      protocol
        "coverage"
        (Q.Coverage.create
           ~before_tracking_unknown
           ~retired_attempts:0L
           ~untracked_attempts:0L
           ~retired_turns:0L
           ~untracked_turns:0L
           ~tracking_status:Available)
    in
    let t =
      { limits
      ; carrier =
          C.of_authored_value
            { session_id
            ; generation
            ; last_ordinal = 0L
            ; last_turn_ordinal = 0L
            ; revision = 0L
            ; coverage
            ; rows = Int64.Map.empty
            ; turns = Int64.Map.empty
            }
      }
    in
    let* () = check_capacity t in
    let* doc = encode t in
    let* carrier = document (D.Domain_codec.decode (codec limits) doc) in
    Ok { t with carrier }
;;

let of_document doc ~limits =
  let* conversion =
    document
      (D.Conversion.create
         ~limits:limits.Limits.document_limits
         ~targets:[ "session.inference_ledger", 1 ]
         ~max_steps:1
         ~max_operations:1
         ~steps:[])
  in
  let* doc = document (D.Conversion.upgrade conversion doc) in
  let* carrier = document (D.Domain_codec.decode (codec limits) doc) in
  let t = { carrier; limits } in
  let* () = check_capacity t in
  Ok t
;;

let session_id t = (value t).session_id
let generation t = (value t).generation

let qualify_source t source =
  Result.map_error
    (Transcript.Source_id.of_string
       (P.Id.Session.to_string (session_id t)
        ^ ":"
        ^ Transcript.Source_id.to_string source))
    ~f:(fun _ ->
      Error.Invalid_field
        { field = "source"; reason = "session-qualified source is invalid" })
;;

let validate t ~limits ~session_id:expected_session_id ~generation:expected_generation =
  let* doc = encode t in
  let* admitted = of_document doc ~limits in
  if
    P.Id.Session.equal (session_id admitted) expected_session_id
    && Int.equal (generation admitted) expected_generation
  then Ok ()
  else invalid "identity" "ledger differs from its session or generation"
;;

let with_generation t ~generation =
  let v = value t in
  if generation < v.generation
  then invalid "generation" "generation cannot decrease"
  else if generation = v.generation
  then Ok t
  else if
    List.exists (Map.data v.rows) ~f:(fun row ->
      not (ended (O.Attempt_record.state row.Row.record)))
    || List.exists (Map.data v.turns) ~f:(fun turn ->
      not (turn_ended turn.operation.state))
  then Error Error.Invalid_transition
  else publish t { v with generation }
;;

let coverage_update
      coverage
      ?retired_attempts
      ?untracked_attempts
      ?retired_turns
      ?untracked_turns
      ~status
      ()
  =
  protocol
    "coverage"
    (Q.Coverage.create
       ~before_tracking_unknown:coverage.Q.Coverage.before_tracking_unknown
       ~retired_attempts:
         (Option.value retired_attempts ~default:coverage.retired_attempts)
       ~untracked_attempts:
         (Option.value untracked_attempts ~default:coverage.untracked_attempts)
       ~retired_turns:(Option.value retired_turns ~default:coverage.retired_turns)
       ~untracked_turns:(Option.value untracked_turns ~default:coverage.untracked_turns)
       ~tracking_status:status)
;;

let public_limit = function
  | Limit.Attempt_count -> Q.Coverage.Attempt_count
  | Turn_count -> Turn_count
  | Retained_bytes -> Retained_bytes
  | Protected_future_data -> Protected_future_data
;;

(* Planning recognizes exactly container-retirement extension conflicts. Other
   document/domain errors propagate. Retrying another eligible row never resets
   a carrier or discards an uninterpreted field. *)
let retire_attempt t =
  let v = value t in
  let rec choose protected = function
    | [] -> Ok (None, protected)
    | (ordinal, row) :: rest ->
      if not (ended (O.Attempt_record.state row.Row.record))
      then choose protected rest
      else
        let* retired_attempts = increment v.coverage.retired_attempts in
        let* coverage =
          coverage_update
            v.coverage
            ~retired_attempts
            ~status:v.coverage.tracking_status
            ()
        in
        let next = { v with rows = Map.remove v.rows ordinal; coverage } in
        let trial = { t with carrier = C.with_value t.carrier next } in
        (match encode trial with
         | Error (Error.Document (D.Error.Extension_conflict _)) -> choose true rest
         | Error error -> Error error
         | Ok _ -> Ok (Some trial, protected))
  in
  choose false (Map.to_alist v.rows)
;;

let retire_turn t =
  let v = value t in
  let rec choose protected = function
    | [] -> Ok (None, protected)
    | (ordinal, turn) :: rest ->
      if not (turn_ended turn.operation.state)
      then choose protected rest
      else
        let* retired_turns = increment v.coverage.retired_turns in
        let* coverage =
          coverage_update v.coverage ~retired_turns ~status:v.coverage.tracking_status ()
        in
        let trial =
          { t with
            carrier =
              C.with_value
                t.carrier
                { v with turns = Map.remove v.turns ordinal; coverage }
          }
        in
        (match encode trial with
         | Error (Error.Document (D.Error.Extension_conflict _)) -> choose true rest
         | Error error -> Error error
         | Ok _ -> Ok (Some trial, protected))
  in
  choose false (Map.to_alist v.turns)
;;

let capacity_failure = function
  | Error.Document (D.Error.Limit_exceeded name)
    when String.equal name "bytes" || String.equal name "inference retained bytes" -> true
  | Error.Observation (O.Error.Json (D.Error.Limit_exceeded name))
    when String.equal name "bytes" -> true
  | Document _
  | Observation _
  | Invalid_field _
  | Conflicting_handle
  | Ordinal_exhausted
  | Invalid_transition -> false
;;

let admit t ~source ~relation ~operation_id ~invocation_id ~configuration =
  let v = value t in
  let* () =
    observation
      (Result.map
         (O.Configuration.of_json
            (O.Configuration.to_json configuration)
            ~limits:O.Admission.observation)
         ~f:(fun _ -> ()))
  in
  let* ordinal = increment v.last_ordinal in
  let* source = qualify_source t source in
  let* attempt =
    Result.map_error
      (Transcript.Attempt_id.of_string ("ordinal:" ^ Int64.to_string ordinal))
      ~f:(fun _ -> Error.Conflicting_handle)
  in
  let* scope =
    Result.map_error (Transcript.Scope.create ~source ~attempt ~relation) ~f:(fun _ ->
      Error.Conflicting_handle)
  in
  let* accounting_id = observation (O.Observation_id.of_string "usage") in
  let* context_id = observation (O.Observation_id.of_string "context") in
  let handle =
    { Handle.session_id = v.session_id
    ; generation = v.generation
    ; ordinal
    ; scope
    ; accounting_id
    ; context_id
    ; operation_id
    ; invocation_id
    ; tracking = Tracked
    }
  in
  let* record =
    observation
      (O.Attempt_record.create
         ~scope
         ~accounting_id
         ~configuration
         ~state:Prepared
         ~observations:[]
         ~omitted_diagnostics:0L
         ~limits:O.Admission.attempt)
  in
  let row = { Row.handle; record } in
  let rec plan current protected =
    let current_value = value current in
    let at_count = Map.length current_value.rows >= current.limits.max_attempts in
    let trial =
      if at_count
      then None
      else
        Some
          { current_value with
            last_ordinal = ordinal
          ; rows = Map.set current_value.rows ~key:ordinal ~data:row
          }
    in
    let* admitted =
      match trial with
      | None -> Ok None
      | Some trial ->
        (match publish current trial with
         | Ok next -> Ok (Some next)
         | Error error when capacity_failure error -> Ok None
         | Error error -> Error error)
    in
    match admitted with
    | Some next -> Ok (next, handle, Tracked)
    | None ->
      let* retired, saw_protected = retire_attempt current in
      (match retired with
       | Some next -> plan next (protected || saw_protected)
       | None ->
         (* Failed planning does not publish provisional retirements. *)
         let reason =
           if protected || saw_protected
           then Limit.Protected_future_data
           else if at_count
           then Attempt_count
           else Retained_bytes
         in
         let* untracked_attempts = increment v.coverage.untracked_attempts in
         let* coverage =
           coverage_update
             v.coverage
             ~untracked_attempts
             ~status:(Limited (public_limit reason))
             ()
         in
         let* next = publish t { v with last_ordinal = ordinal; coverage } in
         let tracking = Untracked reason in
         Ok (next, { handle with tracking }, tracking))
  in
  plan t false
;;

let check_handle t (h : Handle.t) =
  let v = value t in
  let key = Transcript.Scope.key h.scope in
  if
    (not (P.Id.Session.equal v.session_id h.session_id))
    || h.generation < 0
    || h.generation > v.generation
    || Int64.(h.ordinal <= zero || h.ordinal > v.last_ordinal)
    || (not
          (String.equal
             (Transcript.Attempt_id.to_string key.attempt)
             ("ordinal:" ^ Int64.to_string h.ordinal)))
    || (not
          (String.is_prefix
             (Transcript.Source_id.to_string key.source)
             ~prefix:(P.Id.Session.to_string v.session_id ^ ":")))
    || (not (String.equal (O.Observation_id.to_string h.accounting_id) "usage"))
    || not (String.equal (O.Observation_id.to_string h.context_id) "context")
  then Error Error.Conflicting_handle
  else (
    match Map.find v.rows h.ordinal with
    | Some row when Handle.equal h row.Row.handle -> Ok (Some row)
    | Some _ -> Error Error.Conflicting_handle
    | None -> Ok None)
;;

let same_state (a : O.Attempt_record.state) (b : O.Attempt_record.state) =
  match a, b with
  | O.Attempt_record.Prepared, Prepared | Running, Running -> true
  | Terminal a, Terminal b -> Inference.Event.Terminal.equal a b
  | Interrupted a, Interrupted b ->
    O.Attempt_record.equal_interruption a.reason b.reason
    && Inference.Event.Terminal.equal_delivery a.delivery b.delivery
  | (Prepared | Running | Terminal _ | Interrupted _), _ -> false
;;

let validate_state_transition previous state =
  if same_state previous state
  then Ok ()
  else if ended previous
  then Error Error.Invalid_transition
  else (
    match previous, state with
    | O.Attempt_record.Running, Prepared -> Error Error.Invalid_transition
    | Prepared, (Prepared | Running | Terminal _ | Interrupted _)
    | Running, (Running | Terminal _ | Interrupted _) -> Ok ()
    | (Terminal _ | Interrupted _), _ -> Error Error.Invalid_transition)
;;

let set_state t handle state =
  let* row = check_handle t handle in
  match row with
  | None -> Ok t
  | Some row ->
    let previous = O.Attempt_record.state row.record in
    let* () = validate_state_transition previous state in
    if same_state previous state
    then Ok t
    else
      let* record =
        record_with
          row
          ~state
          ~observations:(O.Attempt_record.observations row.record)
          ~omitted_diagnostics:(O.Attempt_record.omitted_diagnostics row.record)
      in
      let v = value t in
      publish
        t
        { v with
          rows = Map.set v.rows ~key:handle.Handle.ordinal ~data:{ row with record }
        }
;;

let observe t handle incoming =
  let* row = check_handle t handle in
  match row with
  | None ->
    Ok
      ( t
      , match handle.Handle.tracking with
        | Tracked -> Ignored_retired
        | Untracked _ -> Ignored_untracked )
  | Some row ->
    if not (Transcript.Scope.equal handle.scope (O.scope incoming))
    then Error Error.Conflicting_handle
    else (
      let diagnostics =
        match O.payload incoming with
        | O.Diagnostic _ -> true
        | Usage _ | Context_estimate _ | Configuration _ -> false
      in
      let* () = observation (O.validate incoming ~limits:O.Admission.observation) in
      let* () =
        match O.payload incoming with
        | Usage _ when not (O.Observation_id.equal handle.accounting_id (O.id incoming))
          -> invalid "accounting_id" "non-designated usage identity"
        | Context_estimate _
          when not (O.Observation_id.equal handle.context_id (O.id incoming)) ->
          invalid "context_id" "non-designated context identity"
        | Configuration configuration
          when not
                 (O.Configuration.equal
                    configuration
                    (O.Attempt_record.configuration row.record)) ->
          Error (Error.Observation O.Error.Conflicting_revision)
        | Usage _ | Context_estimate _ | Configuration _ | Diagnostic _ -> Ok ()
      in
      let observations = O.Attempt_record.observations row.record in
      let* latest =
        observation (O.Latest.create ~max_observations:64 ~max_retained_bytes:(64 * 1024))
      in
      let* latest =
        List.fold observations ~init:(Ok latest) ~f:(fun latest observation_value ->
          let* latest = latest in
          let* latest, _ = observation (O.Latest.observe latest observation_value) in
          Ok latest)
      in
      let omitted () =
        let* omitted_diagnostics =
          increment (O.Attempt_record.omitted_diagnostics row.record)
        in
        let* record =
          record_with
            row
            ~state:(O.Attempt_record.state row.record)
            ~observations
            ~omitted_diagnostics
        in
        let v = value t in
        let* next =
          publish
            t
            { v with rows = Map.set v.rows ~key:handle.ordinal ~data:{ row with record } }
        in
        Ok (next, Diagnostic_omitted)
      in
      let reconciled = O.Latest.observe latest incoming in
      match reconciled with
      | Error O.Error.Retention_limit when diagnostics -> omitted ()
      | Error error -> Error (Error.Observation error)
      | Ok (latest, disposition) ->
        (match disposition with
         | O.Latest.Duplicate -> Ok (t, Duplicate)
         | Stale -> Ok (t, Stale)
         | Added | Replaced ->
           let next_observations = O.Latest.observations latest in
           let diagnostic_bytes =
             List.fold next_observations ~init:0 ~f:(fun acc observation ->
               match O.payload observation with
               | Diagnostic _ -> acc + O.encoded_bytes observation
               | Usage _ | Context_estimate _ | Configuration _ -> acc)
           in
           let diagnostic_too_large = diagnostics && O.encoded_bytes incoming > 1024 in
           if
             diagnostic_too_large
             || (diagnostics
                 && (diagnostic_bytes > 16 * 1024
                     || List.count next_observations ~f:(fun value ->
                          match O.payload value with
                          | Diagnostic _ -> true
                          | Usage _ | Context_estimate _ | Configuration _ -> false)
                        > 16))
           then omitted ()
           else
             let* record =
               record_with
                 row
                 ~state:(O.Attempt_record.state row.record)
                 ~observations:next_observations
                 ~omitted_diagnostics:(O.Attempt_record.omitted_diagnostics row.record)
             in
             let v = value t in
             (match
                publish
                  t
                  { v with
                    rows = Map.set v.rows ~key:handle.ordinal ~data:{ row with record }
                  }
              with
              | Ok next ->
                Ok
                  ( next
                  , match disposition with
                    | Added -> Added
                    | Replaced -> Replaced
                    | Duplicate -> Duplicate
                    | Stale -> Stale )
              | Error error when diagnostics && capacity_failure error -> omitted ()
              | Error error -> Error error)))
;;

let admit_turn t (operation : P.Operation.t) =
  let v = value t in
  let* () =
    protocol
      "operation"
      (Result.map (P.Operation.of_json (P.Operation.to_json operation)) ~f:(fun _ -> ()))
  in
  if operation.generation <> v.generation || turn_ended operation.state
  then Error Error.Invalid_transition
  else (
    match operation.kind with
    | P.Operation.Compaction -> invalid "turn" "compaction is not a host turn"
    | Turn _ ->
      (match
         List.find (Map.data v.turns) ~f:(fun turn ->
           P.Id.Operation.equal turn.operation.id operation.id
           && Int.equal turn.operation.generation operation.generation)
       with
       | Some turn when not (turn_ended turn.operation.state) ->
         Ok (t, turn.handle, Tracked)
       | Some _ -> Error Error.Invalid_transition
       | None ->
         let* ordinal = increment v.last_turn_ordinal in
         let handle =
           { Turn_handle.session_id = v.session_id
           ; operation_id = operation.id
           ; generation = operation.generation
           ; ordinal
           ; tracking = Tracked
           }
         in
         let rec plan current protected =
           let cv = value current in
           let at_count = Map.length cv.turns >= t.limits.max_turns in
           let* admitted =
             if at_count
             then Ok None
             else (
               match
                 publish
                   current
                   { cv with
                     last_turn_ordinal = ordinal
                   ; turns = Map.set cv.turns ~key:ordinal ~data:{ handle; operation }
                   }
               with
               | Ok next -> Ok (Some next)
               | Error error when capacity_failure error -> Ok None
               | Error error -> Error error)
           in
           match admitted with
           | Some next -> Ok (next, handle, Tracked)
           | None ->
             let* retired, saw_protected = retire_turn current in
             (match retired with
              | Some next -> plan next (protected || saw_protected)
              | None ->
                let reason =
                  if protected || saw_protected
                  then Limit.Protected_future_data
                  else if at_count
                  then Turn_count
                  else Retained_bytes
                in
                let* untracked_turns = increment v.coverage.untracked_turns in
                let* coverage =
                  coverage_update
                    v.coverage
                    ~untracked_turns
                    ~status:(Limited (public_limit reason))
                    ()
                in
                let* next = publish t { v with last_turn_ordinal = ordinal; coverage } in
                let tracking = Untracked reason in
                Ok (next, { handle with tracking }, tracking))
         in
         plan t false))
;;

let find_turn_handle t ~operation_id ~generation =
  List.find_map (Map.data (value t).turns) ~f:(fun turn ->
    Option.some_if
      (P.Id.Operation.equal turn.operation.id operation_id
       && Int.equal turn.operation.generation generation)
      turn.handle)
;;

let finish_turn t (handle : Turn_handle.t) (operation : P.Operation.t) =
  let v = value t in
  if
    (not (P.Id.Session.equal handle.session_id v.session_id))
    || handle.generation < 0
    || handle.generation > v.generation
    || Int64.(handle.ordinal <= zero || handle.ordinal > v.last_turn_ordinal)
    || (not (P.Id.Operation.equal handle.operation_id operation.id))
    || (not (Int.equal handle.generation operation.generation))
    || (match operation.kind with
        | P.Operation.Compaction -> true
        | Turn _ -> false)
    || not (turn_ended operation.state)
  then Error Error.Conflicting_handle
  else (
    match Map.find v.turns handle.ordinal with
    | None -> Ok t
    | Some turn ->
      if
        (not (P.Id.Operation.equal turn.operation.id handle.operation_id))
        || (not (P.Operation.equal_kind turn.operation.kind operation.kind))
        || (not (P.Timestamp.equal turn.operation.started_at operation.started_at))
        || not (equal_tracking handle.tracking Tracked)
      then Error Error.Conflicting_handle
      else if turn_ended turn.operation.state
      then
        if
          D.Json.equal
            (P.Operation.to_json turn.operation)
            (P.Operation.to_json operation)
        then Ok t
        else Error Error.Invalid_transition
      else
        let* () =
          protocol
            "operation"
            (Result.map
               (P.Operation.of_json (P.Operation.to_json operation))
               ~f:(fun _ -> ()))
        in
        (match operation.kind with
         | Compaction -> Error Error.Invalid_transition
         | Turn _ ->
           publish
             t
             { v with
               turns = Map.set v.turns ~key:handle.ordinal ~data:{ turn with operation }
             }))
;;

let row_view (row : Row.t) ~include_configuration ~include_diagnostics =
  let record = row.record in
  let observations = O.Attempt_record.observations record in
  let usage =
    List.find observations ~f:(fun observation ->
      match O.payload observation with
      | Usage _ -> true
      | Context_estimate _ | Configuration _ | Diagnostic _ -> false)
  in
  let context =
    List.find observations ~f:(fun observation ->
      match O.payload observation with
      | Context_estimate _ -> true
      | Usage _ | Configuration _ | Diagnostic _ -> false)
  in
  let diagnostics =
    if include_diagnostics
    then
      Some
        (List.filter observations ~f:(fun observation ->
           match O.payload observation with
           | Diagnostic _ -> true
           | Usage _ | Context_estimate _ | Configuration _ -> false))
    else None
  in
  protocol
    "read_projection"
    (Q.Attempt.create
       ~ordinal:row.handle.ordinal
       ~generation:row.handle.generation
       ~scope:row.handle.scope
       ~operation_id:row.handle.operation_id
       ~invocation_id:row.handle.invocation_id
       ~accounting_id:row.handle.accounting_id
       ~state:(O.Attempt_record.state record)
       ~usage
       ~context
       ~configuration:
         (if include_configuration
          then Some (O.Attempt_record.configuration record)
          else None)
       ~diagnostics
       ~omitted_diagnostics:
         (if include_diagnostics
          then Some (O.Attempt_record.omitted_diagnostics record)
          else None))
  |> invariant
;;

let absent_usage row =
  match O.Attempt_record.state row.Row.record with
  | Prepared -> O.Count.Not_submitted
  | Running -> Not_reported
  | Interrupted _ -> Interrupted
  | Terminal terminal ->
    (match Inference.Event.Terminal.delivery terminal with
     | Definitely_not_submitted -> Not_submitted
     | Possibly_submitted | Response_started -> Not_reported)
;;

let add_sum sum tokens =
  match sum with
  | Q.Metric.Overflow -> sum
  | Tokens current ->
    if Int64.(tokens > max_value - current)
    then Overflow
    else Tokens Int64.(current + tokens)
;;

let metric rows component =
  let zero_unknown : Q.Metric.unknown_counts =
    { not_reported = 0L
    ; explicit_null = 0L
    ; interrupted = 0L
    ; not_submitted = 0L
    ; before_tracking = 0L
    }
  in
  let ( actual
      , actual_attempts
      , estimated
      , estimated_attempts
      , estimator
      , mixed_estimators
      , unknown )
    =
    List.fold
      rows
      ~init:(Q.Metric.Tokens 0L, 0L, Q.Metric.Tokens 0L, 0L, None, false, zero_unknown)
      ~f:(fun (actual, na, estimated, ne, estimator, mixed, unknown) row ->
        let count =
          List.find_map
            (O.Attempt_record.observations row.Row.record)
            ~f:(fun observation ->
              match O.payload observation with
              | Usage usage -> Some (O.Count.view (O.Usage.count usage component))
              | Context_estimate _ | Configuration _ | Diagnostic _ -> None)
          |> Option.value ~default:(O.Count.Unknown (absent_usage row))
        in
        match count with
        | O.Count.Actual tokens ->
          ( add_sum actual tokens
          , Int64.(na + one)
          , estimated
          , ne
          , estimator
          , mixed
          , unknown )
        | Estimated { tokens; estimator = current } ->
          ( actual
          , na
          , add_sum estimated tokens
          , Int64.(ne + one)
          , Some (Option.value estimator ~default:current)
          , mixed
            || Option.exists estimator ~f:(fun first ->
              not (O.Estimator.equal first current))
          , unknown )
        | Unknown reason ->
          let unknown =
            match reason with
            | Not_reported ->
              { unknown with not_reported = Int64.(unknown.not_reported + one) }
            | Explicit_null ->
              { unknown with explicit_null = Int64.(unknown.explicit_null + one) }
            | Interrupted ->
              { unknown with interrupted = Int64.(unknown.interrupted + one) }
            | Not_submitted ->
              { unknown with not_submitted = Int64.(unknown.not_submitted + one) }
            | Before_tracking ->
              { unknown with before_tracking = Int64.(unknown.before_tracking + one) }
          in
          actual, na, estimated, ne, estimator, mixed, unknown)
  in
  ignore estimator;
  protocol
    "metric"
    (Q.Metric.create
       ~actual
       ~actual_attempts
       ~estimated
       ~estimated_attempts
       ~mixed_estimators
       ~unknown)
  |> invariant
;;

let summary t =
  let v = value t in
  let rows = Map.data v.rows in
  let zero : Q.Summary.turns =
    { pending = 0L; completed = 0L; failed = 0L; cancelled = 0L; interrupted = 0L }
  in
  let turns =
    Map.fold v.turns ~init:zero ~f:(fun ~key:_ ~data:turn counts ->
      match turn.operation.state with
      | Starting | Running | Cancelling ->
        { counts with pending = Int64.(counts.pending + one) }
      | Completed -> { counts with completed = Int64.(counts.completed + one) }
      | Failed _ -> { counts with failed = Int64.(counts.failed + one) }
      | Cancelled -> { counts with cancelled = Int64.(counts.cancelled + one) }
      | Interrupted _ -> { counts with interrupted = Int64.(counts.interrupted + one) })
  in
  let components : Q.Summary.components =
    { input = metric rows Input
    ; output = metric rows Output
    ; reported_total = metric rows Reported_total
    ; cached_input = metric rows Cached_input
    ; cache_write_input = metric rows Cache_write_input
    ; reasoning_output = metric rows Reasoning_output
    }
  in
  protocol
    "summary"
    (Q.Summary.create
       ~retained_attempts:(Int64.of_int (List.length rows))
       ~turns
       ~components
       ~coverage:v.coverage
       ~accounting_revision:v.revision)
  |> invariant
;;

let validate_update previous ~incoming =
  let before = value previous in
  let after = value incoming in
  let* () =
    validate
      previous
      ~limits:Limits.default
      ~session_id:before.session_id
      ~generation:before.generation
  in
  let* () =
    validate
      incoming
      ~limits:Limits.default
      ~session_id:before.session_id
      ~generation:after.generation
  in
  let nondecreasing a b = Int64.(b >= a) in
  let* () =
    if
      after.generation >= before.generation
      && nondecreasing before.last_ordinal after.last_ordinal
      && nondecreasing before.last_turn_ordinal after.last_turn_ordinal
      && nondecreasing before.revision after.revision
      && ((not before.coverage.before_tracking_unknown)
          || after.coverage.before_tracking_unknown)
      && nondecreasing before.coverage.retired_attempts after.coverage.retired_attempts
      && nondecreasing
           before.coverage.untracked_attempts
           after.coverage.untracked_attempts
      && nondecreasing before.coverage.retired_turns after.coverage.retired_turns
      && nondecreasing before.coverage.untracked_turns after.coverage.untracked_turns
    then Ok ()
    else Error Error.Invalid_transition
  in
  let* () =
    if
      Int64.equal before.revision after.revision
      && not (D.Json.equal (value_to_json before) (value_to_json after))
    then Error Error.Invalid_transition
    else Ok ()
  in
  let* () =
    let missing_rows =
      Map.count before.rows ~f:(fun row ->
        not (Map.mem after.rows row.Row.handle.ordinal))
    in
    let missing_turns =
      Map.count before.turns ~f:(fun turn ->
        not (Map.mem after.turns turn.handle.ordinal))
    in
    if
      Int64.(
        after.coverage.retired_attempts - before.coverage.retired_attempts
        >= of_int missing_rows)
      && Int64.(
           after.coverage.retired_turns - before.coverage.retired_turns
           >= of_int missing_turns)
    then Ok ()
    else invalid "coverage" "retired tracked identities require retired coverage"
  in
  let* () =
    List.fold_result (Map.to_alist before.rows) ~init:() ~f:(fun () (ordinal, old) ->
      match Map.find after.rows ordinal with
      | None ->
        if ended (O.Attempt_record.state old.record)
        then Ok ()
        else Error Error.Invalid_transition
      | Some next ->
        if
          (not (Handle.equal old.handle next.handle))
          || (not
                (O.Configuration.equal
                   (O.Attempt_record.configuration old.record)
                   (O.Attempt_record.configuration next.record)))
          || not
               (nondecreasing
                  (O.Attempt_record.omitted_diagnostics old.record)
                  (O.Attempt_record.omitted_diagnostics next.record))
        then Error Error.Conflicting_handle
        else
          let* () =
            validate_state_transition
              (O.Attempt_record.state old.record)
              (O.Attempt_record.state next.record)
          in
          let* latest =
            observation
              (O.Latest.create ~max_observations:64 ~max_retained_bytes:(64 * 1024))
          in
          let* latest =
            List.fold_result
              (O.Attempt_record.observations old.record)
              ~init:latest
              ~f:(fun latest value ->
                observation (O.Latest.observe latest value) |> Result.map ~f:fst)
          in
          let* latest =
            List.fold_result
              (O.Attempt_record.observations next.record)
              ~init:latest
              ~f:(fun latest value ->
                observation (O.Latest.observe latest value) |> Result.map ~f:fst)
          in
          let supplied = O.Attempt_record.observations next.record in
          if
            List.length supplied = List.length (O.Latest.observations latest)
            && List.for_all supplied ~f:(fun value ->
              Option.exists (O.Latest.find latest (O.key value)) ~f:(O.equal value))
          then Ok ()
          else Error Error.Invalid_transition)
  in
  let* () =
    List.fold_result (Map.to_alist before.turns) ~init:() ~f:(fun () (ordinal, old) ->
      match Map.find after.turns ordinal with
      | None ->
        if turn_ended old.operation.state then Ok () else Error Error.Invalid_transition
      | Some next ->
        if
          (not (P.Id.Session.equal old.handle.session_id next.handle.session_id))
          || (not (P.Id.Operation.equal old.handle.operation_id next.handle.operation_id))
          || (not (Int.equal old.handle.generation next.handle.generation))
          || (not (Int64.equal old.handle.ordinal next.handle.ordinal))
          || (not (equal_tracking old.handle.tracking next.handle.tracking))
          || (not (P.Operation.equal_kind old.operation.kind next.operation.kind))
          || not (P.Timestamp.equal old.operation.started_at next.operation.started_at)
        then Error Error.Conflicting_handle
        else if
          turn_ended old.operation.state
          && not
               (D.Json.equal
                  (P.Operation.to_json old.operation)
                  (P.Operation.to_json next.operation))
        then Error Error.Invalid_transition
        else Ok ())
  in
  let* () =
    if
      List.exists (Map.keys after.rows) ~f:(fun ordinal ->
        Int64.(ordinal <= before.last_ordinal) && not (Map.mem before.rows ordinal))
      || List.exists (Map.keys after.turns) ~f:(fun ordinal ->
        Int64.(ordinal <= before.last_turn_ordinal) && not (Map.mem before.turns ordinal))
    then Error Error.Invalid_transition
    else Ok ()
  in
  let* adopted =
    document
      (D.Domain_codec.adopt
         (codec Limits.default)
         ~previous:previous.carrier
         ~incoming:incoming.carrier)
  in
  let* adopted = document (D.Domain_codec.encode (codec Limits.default) adopted) in
  let* supplied = encode incoming in
  if
    String.equal
      (Jsonaf.to_string (D.Document.json adopted))
      (Jsonaf.to_string (D.Document.json supplied))
  then Ok ()
  else invalid "carrier" "update omits protected previous fields"
;;
