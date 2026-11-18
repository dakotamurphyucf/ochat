open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec
module Intent = Recurrence_execution_intent
module Attempt = Recurrence_execution_attempt

type projection =
  { intent_sha256 : string
  ; attempt_sha256 : string
  ; target : P.Session_ref.t
  ; source : P.Run_source.t
  ; target_revision : int64
  ; resolved_at : P.Timestamp.t
  }

type t =
  { projection : projection
  ; json : Jsonaf.t
  }

let attempt_sha256 t = t.projection.attempt_sha256
let target_revision t = t.projection.target_revision
let resolved_at t = t.projection.resolved_at
let equal a b = D.Json.equal a.json b.json
let to_json t = t.json
let invalid message = Error (P.Error.invalid_request message)

let protocol result =
  Result.map_error result ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
;;

let limits =
  D.Limits.create ~max_bytes:4096 ~max_depth:24 ~max_fields:64 ~max_nodes:128
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
  else invalid "invalid retry-resolution digest"
;;

let decode json =
  let open Result.Let_syntax in
  let%bind fields = X.object_ json in
  let%bind intent_sha256 = X.required fields "intent_sha256" digest_of_json in
  let%bind attempt_sha256 = X.required fields "attempt_sha256" digest_of_json in
  let%bind target = X.required fields "target" P.Session_ref.of_json in
  let%bind source = X.required fields "source" P.Run_source.of_json in
  let%bind target_revision = X.required fields "target_revision" X.nonnegative_int64 in
  let%map resolved_at = X.required fields "resolved_at" P.Timestamp.of_json in
  { intent_sha256; attempt_sha256; target; source; target_revision; resolved_at }
;;

let encode p =
  `Object
    [ "intent_sha256", `String p.intent_sha256
    ; "attempt_sha256", `String p.attempt_sha256
    ; "target", P.Session_ref.to_json p.target
    ; "source", P.Run_source.to_json p.source
    ; "target_revision", X.int64_json p.target_revision
    ; "resolved_at", P.Timestamp.to_json p.resolved_at
    ]
;;

let codec =
  X.codec_exn
    ~version:1
    ~limits
    ~kind:"ochat.recurrence_retry_resolution"
    ~shape:
      (X.fields_shape
         [ "intent_sha256"
         ; "attempt_sha256"
         ; "target"
         ; "source"
         ; "target_revision"
         ; "resolved_at"
         ])
    ~decode
    ~encode:(fun p -> Ok (encode p))
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind document = D.Document.inspect ~limits json |> protocol in
  let%map carrier = D.Domain_codec.decode codec document |> protocol in
  { projection = D.Extension_carrier.value carrier; json }
;;

let validate t ~intent ~attempt =
  let open Result.Let_syntax in
  let%bind () = Attempt.validate attempt ~intent in
  if
    String.equal t.projection.intent_sha256 (Intent.digest intent)
    && String.equal (attempt_sha256 t) (Attempt.digest attempt)
    && P.Session_ref.equal t.projection.target (Intent.target intent)
    && P.Run_source.equal t.projection.source (Intent.source intent)
  then Ok ()
  else invalid "retry-resolution reference differs from its exact attempt"
;;

let create ~intent ~attempt ~target_revision ~resolved_at =
  let open Result.Let_syntax in
  let projection =
    { intent_sha256 = Intent.digest intent
    ; attempt_sha256 = Attempt.digest attempt
    ; target = Intent.target intent
    ; source = Intent.source intent
    ; target_revision
    ; resolved_at
    }
  in
  let%bind document =
    D.Domain_codec.encode codec (D.Extension_carrier.of_authored_value projection)
    |> protocol
  in
  let%bind t = of_json (D.Document.json document) in
  let%map () = validate t ~intent ~attempt in
  t
;;
