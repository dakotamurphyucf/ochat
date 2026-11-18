open! Core
module X = Goal_wire
module J = Json_codec
module Error = Protocol_error

type t =
  { principal_id : Id.Principal.t
  ; key : Idempotency_key.t
  ; request_sha256 : string
  ; goal_id : Id.Goal.t
  ; goal_revision : int64
  ; catalog_revision : int64
  ; source : Run_source.t option
  ; accepted_at : Timestamp.t
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
    | Some source -> Run_source.validate source
  in
  let%bind _ = Id.Principal.of_json (Id.Principal.to_json principal_id) in
  let%bind _ = Id.Goal.of_json (Id.Goal.to_json goal_id) in
  let%bind _ = Idempotency_key.of_json (Idempotency_key.to_json key) in
  let%bind _ = Timestamp.of_json (Timestamp.to_json accepted_at) in
  if
    Int64.(goal_revision < 0L || catalog_revision <= 0L)
    || String.length request_sha256 <> 64
    || not
         (String.for_all request_sha256 ~f:(function
            | '0' .. '9' | 'a' .. 'f' -> true
            | _ -> false))
  then Error (Error.invalid_request "invalid goal command receipt")
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
    [ "principal_id", Id.Principal.to_json t.principal_id
    ; "key", Idempotency_key.to_json t.key
    ; "request_sha256", `String t.request_sha256
    ; "goal_id", Id.Goal.to_json t.goal_id
    ; "goal_revision", X.counter_json t.goal_revision
    ; "catalog_revision", X.counter_json t.catalog_revision
    ; "source", X.option_json Run_source.to_json t.source
    ; "accepted_at", Timestamp.to_json t.accepted_at
    ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind f = J.fields json in
  let%bind principal_id = J.required_as f "principal_id" Id.Principal.of_json in
  let%bind key = J.required_as f "key" Idempotency_key.of_json in
  let%bind request_sha256 = J.required_as f "request_sha256" Json_codec.string in
  let%bind goal_id = J.required_as f "goal_id" Id.Goal.of_json in
  let%bind goal_revision = J.required_as f "goal_revision" X.counter in
  let%bind catalog_revision = J.required_as f "catalog_revision" X.counter in
  let%bind source = J.required_as f "source" (X.nullable Run_source.of_json) in
  let%bind accepted_at = J.required_as f "accepted_at" Timestamp.of_json in
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
