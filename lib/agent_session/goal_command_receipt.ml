open! Core
module P = Agent_protocol
module X = Persistence_codec

type t =
  { principal_id : P.Id.Principal.t
  ; key : P.Idempotency_key.t
  ; request_sha256 : string
  ; goal_id : P.Id.Goal.t
  ; goal_revision : int64
  ; catalog_revision : int64
  ; source : P.Run_source.t option
  ; accepted_at : P.Timestamp.t
  }
[@@deriving equal, sexp_of]

let create
      ~principal_id
      ~key
      ~request_sha256
      ~goal_id
      ~goal_revision
      ~catalog_revision
      ~source
      ~accepted_at
  =
  let open Result.Let_syntax in
  let%bind () =
    match source with
    | None -> Ok ()
    | Some source -> P.Run_source.validate source
  in
  let%bind _ = P.Id.Principal.of_json (P.Id.Principal.to_json principal_id) in
  let%bind _ = P.Id.Goal.of_json (P.Id.Goal.to_json goal_id) in
  let%bind _ = P.Idempotency_key.of_json (P.Idempotency_key.to_json key) in
  let%bind _ = P.Timestamp.of_json (P.Timestamp.to_json accepted_at) in
  if
    Int64.(goal_revision < 0L || catalog_revision <= 0L)
    || String.length request_sha256 <> 64
    || not
         (String.for_all request_sha256 ~f:(function
            | '0' .. '9' | 'a' .. 'f' -> true
            | _ -> false))
  then Error (P.Error.invalid_request "invalid goal command receipt")
  else
    Ok
      { principal_id
      ; key
      ; request_sha256
      ; goal_id
      ; goal_revision
      ; catalog_revision
      ; source
      ; accepted_at
      }
;;

let to_json t =
  `Object
    [ "principal_id", P.Id.Principal.to_json t.principal_id
    ; "key", P.Idempotency_key.to_json t.key
    ; "request_sha256", `String t.request_sha256
    ; "goal_id", P.Id.Goal.to_json t.goal_id
    ; "goal_revision", X.int64_json t.goal_revision
    ; "catalog_revision", X.int64_json t.catalog_revision
    ; "source", X.option_json P.Run_source.to_json t.source
    ; "accepted_at", P.Timestamp.to_json t.accepted_at
    ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind f = X.object_ json in
  let%bind principal_id = X.required f "principal_id" P.Id.Principal.of_json in
  let%bind key = X.required f "key" P.Idempotency_key.of_json in
  let%bind request_sha256 = X.required f "request_sha256" P.Json_codec.string in
  let%bind goal_id = X.required f "goal_id" P.Id.Goal.of_json in
  let%bind goal_revision = X.required f "goal_revision" X.nonnegative_int64 in
  let%bind catalog_revision = X.required f "catalog_revision" X.nonnegative_int64 in
  let%bind source = X.required f "source" (X.nullable P.Run_source.of_json) in
  let%bind accepted_at = X.required f "accepted_at" P.Timestamp.of_json in
  create
    ~principal_id
    ~key
    ~request_sha256
    ~goal_id
    ~goal_revision
    ~catalog_revision
    ~source
    ~accepted_at
;;

let shape =
  X.shape_exn
    [ "principal_id", Document_schema.Shape.value
    ; "key", Document_schema.Shape.value
    ; "request_sha256", Document_schema.Shape.value
    ; "goal_id", Document_schema.Shape.value
    ; "goal_revision", Document_schema.Shape.value
    ; "catalog_revision", Document_schema.Shape.value
    ; "source", X.nullable_shape Run_record_shapes.source
    ; "accepted_at", Document_schema.Shape.value
    ]
;;
