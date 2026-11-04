open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec
module Intent = Recurrence_execution_intent
module Attempt = Recurrence_execution_attempt
module I = Recurrence_identity

type projection =
  { occurrence : I.Occurrence.t
  ; intent_sha256 : string
  ; attempt_sha256 : string
  ; session : P.Session_ref.t
  ; mode : P.Run.Mode.t
  ; admission : P.Run_receipt.t
  }

type t =
  { projection : projection
  ; json : Jsonaf.t
  ; sha256 : string
  }

let occurrence t = t.projection.occurrence
let intent_sha256 t = t.projection.intent_sha256
let attempt_sha256 t = t.projection.attempt_sha256
let session t = t.projection.session
let mode t = t.projection.mode
let admission t = t.projection.admission
let run_id t = (admission t).run_id
let digest t = t.sha256
let equal a b = D.Json.equal a.json b.json
let to_json t = t.json
let invalid message = Error (P.Error.invalid_request message)

let protocol result =
  Result.map_error result ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
;;

let limits =
  D.Limits.create ~max_bytes:8192 ~max_depth:24 ~max_fields:128 ~max_nodes:256
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let digest_of_json json =
  let%bind.Result text = P.Json_codec.string json in
  if
    String.length text = 64
    && String.for_all text ~f:(function
      | '0' .. '9' | 'a' .. 'f' -> true
      | _ -> false)
  then Ok text
  else invalid "invalid recurrence Run origin digest"
;;

let decode json =
  let open Result.Let_syntax in
  let%bind fields = X.object_ json in
  let%bind occurrence = X.required fields "occurrence" I.Occurrence.of_json in
  let%bind intent_sha256 = X.required fields "intent_sha256" digest_of_json in
  let%bind attempt_sha256 = X.required fields "attempt_sha256" digest_of_json in
  let%bind session = X.required fields "session" P.Session_ref.of_json in
  let%bind mode = X.required fields "mode" P.Run.Mode.of_json in
  let%bind admission = X.required fields "admission" P.Run_receipt.of_json in
  if
    P.Run_receipt.Kind.equal admission.kind Admission
    && Int64.equal admission.run_revision 0L
  then Ok { occurrence; intent_sha256; attempt_sha256; session; mode; admission }
  else invalid "recurrence Run origin requires an Admission receipt"
;;

let encode p =
  `Object
    [ "occurrence", I.Occurrence.to_json p.occurrence
    ; "intent_sha256", `String p.intent_sha256
    ; "attempt_sha256", `String p.attempt_sha256
    ; "session", P.Session_ref.to_json p.session
    ; "mode", P.Run.Mode.to_json p.mode
    ; "admission", P.Run_receipt.to_json p.admission
    ]
;;

let codec =
  X.codec_exn
    ~version:1
    ~limits
    ~kind:"ochat.recurrence_run_origin"
    ~shape:
      (X.fields_shape
         [ "occurrence"
         ; "intent_sha256"
         ; "attempt_sha256"
         ; "session"
         ; "mode"
         ; "admission"
         ])
    ~decode
    ~encode:(fun value -> Ok (encode value))
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind document = D.Document.inspect ~limits json |> protocol in
  let%bind carrier = D.Domain_codec.decode codec document |> protocol in
  let%map canonical = P.Json_codec.canonical_string json in
  { projection = D.Extension_carrier.value carrier
  ; json
  ; sha256 = Digestif.SHA256.(digest_string canonical |> to_hex)
  }
;;

let validate t ~intent ~attempt =
  let open Result.Let_syntax in
  let%bind () = Attempt.validate attempt ~intent in
  let admitted = admission t in
  if
    I.Occurrence.equal (occurrence t) (Intent.occurrence intent)
    && String.equal (intent_sha256 t) (Intent.digest intent)
    && String.equal (attempt_sha256 t) (Attempt.digest attempt)
    && P.Session_ref.equal (session t) (Intent.target intent)
    && P.Run.Mode.equal (mode t) (Intent.mode intent)
    && P.Id.Principal.equal admitted.principal_id (Attempt.principal_id attempt)
    && P.Run_source.equal admitted.source (Intent.source intent)
    && P.Idempotency_key.equal admitted.key (Attempt.key attempt)
    && String.equal admitted.request_sha256 (Attempt.request_sha256 attempt)
    && Int64.(admitted.session_revision > Attempt.expected_revision attempt)
  then Ok ()
  else invalid "Run origin differs from the exact immutable occurrence attempt"
;;

let validate_run t ~(run : P.Run.t) ~admission:actual_admission =
  let open Result.Let_syntax in
  let%bind () = P.Run.validate run in
  let admitted = admission t in
  if
    P.Run_receipt.equal admitted actual_admission
    && P.Run_receipt.Kind.equal admitted.kind Admission
    && P.Id.Run.equal admitted.run_id run.id
    && P.Id.Principal.equal admitted.principal_id run.principal_id
    && P.Run_source.equal admitted.source run.source
    && P.Session_ref.equal (session t) run.session
    && P.Run.Mode.equal (mode t) run.mode
    && Int64.(admitted.run_revision <= run.revision)
  then Ok ()
  else invalid "Run origin differs from the actual retained Run admission"
;;

let create ~intent ~attempt ~(run : P.Run.t) ~admission =
  let open Result.Let_syntax in
  let projection =
    { occurrence = Intent.occurrence intent
    ; intent_sha256 = Intent.digest intent
    ; attempt_sha256 = Attempt.digest attempt
    ; session = run.session
    ; mode = run.mode
    ; admission
    }
  in
  let%bind document =
    D.Domain_codec.encode codec (D.Extension_carrier.of_authored_value projection)
    |> protocol
  in
  let%bind t = of_json (D.Document.json document) in
  let%bind () = validate t ~intent ~attempt in
  let%map () = validate_run t ~run ~admission in
  t
;;
