open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec

type projection =
  { number : int
  ; principal_id : P.Id.Principal.t
  ; expected_revision : int64
  ; key : P.Idempotency_key.t
  ; intent_sha256 : string
  ; request_sha256 : string
  }

type t =
  { projection : projection
  ; json : Jsonaf.t
  ; sha256 : string
  }

let maximum_count = 8
let number t = t.projection.number
let principal_id t = t.projection.principal_id
let expected_revision t = t.projection.expected_revision
let key t = t.projection.key
let intent_sha256 t = t.projection.intent_sha256
let request_sha256 t = t.projection.request_sha256
let digest t = t.sha256
let equal a b = D.Json.equal a.json b.json
let to_json t = t.json
let invalid message = Error (P.Error.invalid_request message)

let protocol result =
  Result.map_error result ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
;;

let limits =
  D.Limits.create ~max_bytes:16384 ~max_depth:16 ~max_fields:128 ~max_nodes:256
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
  else invalid "invalid recurrence attempt digest"
;;

let decode json =
  let open Result.Let_syntax in
  let%bind fields = X.object_ json in
  let%bind number =
    X.required fields "number" (P.Json_codec.bounded_int ~min:0 ~max:(maximum_count - 1))
  in
  let%bind principal_id = X.required fields "principal_id" P.Id.Principal.of_json in
  let%bind expected_revision =
    X.required fields "expected_revision" X.nonnegative_int64
  in
  let%bind key = X.required fields "key" P.Idempotency_key.of_json in
  let%bind intent_sha256 = X.required fields "intent_sha256" digest_of_json in
  let%map request_sha256 = X.required fields "request_sha256" digest_of_json in
  { number; principal_id; expected_revision; key; intent_sha256; request_sha256 }
;;

let encode p =
  `Object
    [ "number", `Number (Int.to_string p.number)
    ; "principal_id", P.Id.Principal.to_json p.principal_id
    ; "expected_revision", X.int64_json p.expected_revision
    ; "key", P.Idempotency_key.to_json p.key
    ; "intent_sha256", `String p.intent_sha256
    ; "request_sha256", `String p.request_sha256
    ]
;;

let codec =
  X.codec_exn
    ~version:1
    ~limits
    ~kind:"ochat.recurrence_execution_attempt"
    ~shape:
      (X.fields_shape
         [ "number"
         ; "principal_id"
         ; "expected_revision"
         ; "key"
         ; "intent_sha256"
         ; "request_sha256"
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

let request t ~intent =
  let open Result.Let_syntax in
  if not (String.equal (intent_sha256 t) (Recurrence_execution_intent.digest intent))
  then invalid "recurrence attempt belongs to another immutable intent"
  else (
    let%bind request =
      Recurrence_execution_intent.request
        intent
        ~expected_revision:(expected_revision t)
        ~key:(key t)
    in
    let%bind expected =
      Run_admission_request.request_sha256 (Run_admission_request.of_headless request)
    in
    if String.equal expected (request_sha256 t)
    then Ok request
    else invalid "recurrence attempt normalized request digest differs")
;;

let validate t ~intent = Result.map (request t ~intent) ~f:(fun _ -> ())

let create ~intent ~number ~principal_id ~expected_revision ~key =
  let open Result.Let_syntax in
  if number < 0 || number >= maximum_count
  then invalid "recurrence attempt number exceeds finite profile"
  else (
    let%bind _ = P.Id.Principal.of_json (P.Id.Principal.to_json principal_id) in
    let%bind request =
      Recurrence_execution_intent.request intent ~expected_revision ~key
    in
    let%bind request_sha256 =
      Run_admission_request.request_sha256 (Run_admission_request.of_headless request)
    in
    let projection =
      { number
      ; principal_id
      ; expected_revision
      ; key
      ; intent_sha256 = Recurrence_execution_intent.digest intent
      ; request_sha256
      }
    in
    let%bind document =
      D.Domain_codec.encode codec (D.Extension_carrier.of_authored_value projection)
      |> protocol
    in
    of_json (D.Document.json document))
;;
