open! Core
module Error = Protocol_error
module J = Json_codec
module Wire = Goal_native_request_codec

type t =
  { generation : int
  ; goal_id : Id.Goal.t
  ; expected_catalog_revision : int64
  ; key : Idempotency_key.t
  ; source : Run_source.t
  ; objective : Goal.Objective.t
  ; budget : Goal.Budget.t
  }

let create ~generation ~source ~goal_id ~expected_catalog_revision ~objective ~budget ~key
  =
  let open Result.Let_syntax in
  let%bind _ = Id.Goal.of_json (Id.Goal.to_json goal_id) in
  let%bind _ = Idempotency_key.of_json (Idempotency_key.to_json key) in
  let%bind () = Run_source.validate source in
  if
    generation < 0
    || Int64.(expected_catalog_revision < 0L)
    || not (Int.equal generation source.generation)
  then Error (Error.invalid_request "invalid native Goal create fences")
  else
    Ok { generation; goal_id; expected_catalog_revision; key; source; objective; budget }
;;

let generation t = t.generation
let goal_id t = t.goal_id
let expected_catalog_revision t = t.expected_catalog_revision
let key t = t.key
let source t = t.source
let objective t = t.objective
let budget t = t.budget

let to_json t =
  `Object
    [ "generation", `String (Int.to_string t.generation)
    ; "goal_id", Id.Goal.to_json t.goal_id
    ; "expected_catalog_revision", `String (Int64.to_string t.expected_catalog_revision)
    ; "key", Idempotency_key.to_json t.key
    ; "source", Run_source.to_json t.source
    ; "objective", Goal.Objective.to_json t.objective
    ; "budget", Goal.Budget.to_json t.budget
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
        ; "source"
        ; "objective"
        ; "budget"
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
  let%bind source = J.required_as f "source" Run_source.of_json in
  let%bind objective = J.required_as f "objective" Goal.Objective.of_json in
  let%bind budget = J.required_as f "budget" Goal.Budget.of_json in
  create ~generation ~goal_id ~expected_catalog_revision ~key ~source ~objective ~budget
;;
