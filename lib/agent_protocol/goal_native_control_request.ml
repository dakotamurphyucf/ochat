open! Core
module Error = Protocol_error
module J = Json_codec
module Wire = Goal_native_request_codec

module Mutation = struct
  type t =
    | Hold of Goal.Reason.t
    | Abandon of Goal.Reason.t
end

type t =
  { generation : int
  ; goal_id : Id.Goal.t
  ; expected_catalog_revision : int64
  ; key : Idempotency_key.t
  ; expected_goal_revision : int64
  ; mutation : Mutation.t
  }

let create
      ~generation
      ~goal_id
      ~expected_catalog_revision
      ~expected_goal_revision
      ~key
      ~mutation
  =
  let open Result.Let_syntax in
  let%bind _ = Id.Goal.of_json (Id.Goal.to_json goal_id) in
  let%bind _ = Idempotency_key.of_json (Idempotency_key.to_json key) in
  if
    generation < 0
    || Int64.(expected_catalog_revision < 0L)
    || Int64.(expected_goal_revision < 0L)
  then Error (Error.invalid_request "invalid native Goal control fences")
  else
    Ok
      { generation
      ; goal_id
      ; expected_catalog_revision
      ; key
      ; expected_goal_revision
      ; mutation
      }
;;

let generation t = t.generation
let goal_id t = t.goal_id
let expected_catalog_revision t = t.expected_catalog_revision
let key t = t.key
let expected_goal_revision t = t.expected_goal_revision
let mutation t = t.mutation

let to_json t =
  let kind, reason =
    match t.mutation with
    | Mutation.Hold reason -> "hold", reason
    | Abandon reason -> "abandon", reason
  in
  `Object
    [ "generation", `String (Int.to_string t.generation)
    ; "goal_id", Id.Goal.to_json t.goal_id
    ; "expected_catalog_revision", `String (Int64.to_string t.expected_catalog_revision)
    ; "key", Idempotency_key.to_json t.key
    ; "expected_goal_revision", `String (Int64.to_string t.expected_goal_revision)
    ; "kind", `String kind
    ; "reason", Goal.Reason.to_json reason
    ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind f =
    Wire.fields
      json
      ~names:
        [ "generation"
        ; "goal_id"
        ; "expected_catalog_revision"
        ; "key"
        ; "expected_goal_revision"
        ; "kind"
        ; "reason"
        ]
  in
  let%bind generation = J.required_as f "generation" Wire.counter in
  let%bind generation =
    Result.of_option
      (Int64.to_int generation)
      ~error:(Error.invalid_request "native Goal generation overflow")
  in
  let%bind goal_id = J.required_as f "goal_id" Id.Goal.of_json in
  let%bind expected_catalog_revision =
    J.required_as f "expected_catalog_revision" Wire.counter
  in
  let%bind key = J.required_as f "key" Idempotency_key.of_json in
  let%bind expected_goal_revision =
    J.required_as f "expected_goal_revision" Wire.counter
  in
  let%bind kind = J.required_as f "kind" J.string in
  let%bind reason = J.required_as f "reason" Goal.Reason.of_json in
  let%bind mutation =
    match kind with
    | "hold" -> Ok (Mutation.Hold reason)
    | "abandon" -> Ok (Mutation.Abandon reason)
    | _ -> Error (Error.invalid_request "unknown native Goal control")
  in
  create
    ~generation
    ~goal_id
    ~expected_catalog_revision
    ~key
    ~expected_goal_revision
    ~mutation
;;
