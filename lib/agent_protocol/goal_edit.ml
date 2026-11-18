open! Core
module Error = Protocol_error
module J = Json_codec

module Mutation = struct
  type t =
    | Revise_objective of Goal.Objective.t
    | Record_progress of Goal.Progress.t
  [@@deriving sexp_of]
end

type t =
  { generation : int
  ; source : Run_source.t
  ; goal_id : Id.Goal.t
  ; expected_catalog_revision : int64
  ; expected_goal_revision : int64
  ; key : Idempotency_key.t
  ; mutation : Mutation.t
  }

let create
      ~generation
      ~source
      ~goal_id
      ~expected_catalog_revision
      ~expected_goal_revision
      ~key
      ~mutation
  =
  let open Result.Let_syntax in
  let%bind _ = Id.Goal.of_json (Id.Goal.to_json goal_id) in
  let%bind _ = Idempotency_key.of_json (Idempotency_key.to_json key) in
  let%bind () = Run_source.validate source in
  if
    generation < 0
    || (not (Int.equal generation source.generation))
    || Int64.(expected_catalog_revision < 0L || expected_goal_revision < 0L)
  then Error (Error.invalid_request "invalid Goal edit fences")
  else
    Ok
      { generation
      ; source
      ; goal_id
      ; expected_catalog_revision
      ; expected_goal_revision
      ; key
      ; mutation
      }
;;

let generation t = t.generation
let source t = t.source
let goal_id t = t.goal_id
let expected_catalog_revision t = t.expected_catalog_revision
let expected_goal_revision t = t.expected_goal_revision
let key t = t.key
let mutation t = t.mutation

let to_json t =
  let kind, value =
    match t.mutation with
    | Mutation.Revise_objective value -> "objective", Goal.Objective.to_json value
    | Record_progress value -> "progress", Goal.Progress.to_json value
  in
  `Object
    [ "generation", `String (Int.to_string t.generation)
    ; "source", Run_source.to_json t.source
    ; "goal_id", Id.Goal.to_json t.goal_id
    ; "expected_catalog_revision", `String (Int64.to_string t.expected_catalog_revision)
    ; "expected_goal_revision", `String (Int64.to_string t.expected_goal_revision)
    ; "key", Idempotency_key.to_json t.key
    ; "kind", `String kind
    ; "value", value
    ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () = J.validate_limits ~max_bytes:32768 ~max_depth:8 json in
  let%bind f = J.fields json in
  let names =
    [ "generation"
    ; "source"
    ; "goal_id"
    ; "expected_catalog_revision"
    ; "expected_goal_revision"
    ; "key"
    ; "kind"
    ; "value"
    ]
  in
  let%bind () =
    if
      List.for_all (J.to_alist f) ~f:(fun (name, _) ->
        List.mem names name ~equal:String.equal)
    then Ok ()
    else Error (Error.invalid_request "unknown Goal edit field")
  in
  let counter = function
    | `String s ->
      (match Int64.of_string_opt s with
       | Some n when Int64.(n >= 0L) && String.equal (Int64.to_string n) s -> Ok n
       | Some _ | None -> Error (Error.invalid_request "invalid Goal edit counter"))
    | _ -> Error (Error.invalid_request "Goal edit counters must be decimal strings")
  in
  let%bind generation = J.required_as f "generation" counter in
  let%bind generation =
    Result.of_option
      (Int64.to_int generation)
      ~error:(Error.invalid_request "Goal edit generation overflow")
  in
  let%bind source = J.required_as f "source" Run_source.of_json in
  let%bind goal_id = J.required_as f "goal_id" Id.Goal.of_json in
  let%bind expected_catalog_revision =
    J.required_as f "expected_catalog_revision" counter
  in
  let%bind expected_goal_revision = J.required_as f "expected_goal_revision" counter in
  let%bind key = J.required_as f "key" Idempotency_key.of_json in
  let%bind kind = J.required_as f "kind" J.string in
  let%bind mutation =
    match kind with
    | "objective" ->
      J.required_as f "value" Goal.Objective.of_json
      |> Result.map ~f:(fun value -> Mutation.Revise_objective value)
    | "progress" ->
      J.required_as f "value" Goal.Progress.of_json
      |> Result.map ~f:(fun value -> Mutation.Record_progress value)
    | _ -> Error (Error.invalid_request "unknown Goal edit kind")
  in
  create
    ~generation
    ~source
    ~goal_id
    ~expected_catalog_revision
    ~expected_goal_revision
    ~key
    ~mutation
;;
