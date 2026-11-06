open! Core
module J = Json_codec

module Input = struct
  type t = Run_input.t

  (* Input has no public equality API. Its typed encoding is deterministic and
     complete; this local equality does not expand the shared Input contract. *)
  let equal left right =
    Jsonaf.exactly_equal (Run_input.to_json left) (Run_input.to_json right)
  ;;

  let sexp_of_t = Run_input.sexp_of_t
end

type t =
  { generation : int
  ; source : Run_source.t
  ; goal_id : Id.Goal.t
  ; expected_goal_revision : int64
  ; expected_catalog_revision : int64
  ; input : Input.t
  ; key : Idempotency_key.t
  }
[@@deriving equal, sexp_of]

let max_bytes = Run_limits.max_document_bytes + 4096
let max_depth = Run_limits.max_depth + 1

let to_json t =
  `Object
    [ "kind", `String "goal_start_episode_v1"
    ; "generation", `String (Int.to_string t.generation)
    ; "source", Run_source.to_json t.source
    ; "goal_id", Id.Goal.to_json t.goal_id
    ; "expected_goal_revision", `String (Int64.to_string t.expected_goal_revision)
    ; "expected_catalog_revision", `String (Int64.to_string t.expected_catalog_revision)
    ; "input", Run_input.to_json t.input
    ; "key", Idempotency_key.to_json t.key
    ]
;;

let create
      ~generation
      ~source
      ~goal_id
      ~expected_goal_revision
      ~expected_catalog_revision
      ~input
      ~key
  =
  let open Result.Let_syntax in
  let%bind _ = Id.Goal.of_json (Id.Goal.to_json goal_id) in
  let%bind _ = Idempotency_key.of_json (Idempotency_key.to_json key) in
  let%bind () = Run_source.validate source in
  let%bind _ = Run_input.of_json (Run_input.to_json input) in
  if
    generation < 0
    || (not (Int.equal generation source.generation))
    || Int64.(expected_goal_revision < 0L || expected_catalog_revision < 0L)
  then Error (Protocol_error.invalid_request "invalid native Goal start fences")
  else (
    let t =
      { generation
      ; source
      ; goal_id
      ; expected_goal_revision
      ; expected_catalog_revision
      ; input
      ; key
      }
    in
    let%map () = Extension_codec.validate_json ~max_bytes ~max_depth (to_json t) in
    t)
;;

let generation t = t.generation
let source t = t.source
let goal_id t = t.goal_id
let expected_goal_revision t = t.expected_goal_revision
let expected_catalog_revision t = t.expected_catalog_revision
let input t = t.input
let key t = t.key

let of_json json =
  let open Result.Let_syntax in
  let%bind () = Extension_codec.validate_json ~max_bytes ~max_depth json in
  let%bind fields = J.fields json in
  let%bind () =
    Extension_codec.closed
      fields
      [ "kind"
      ; "generation"
      ; "source"
      ; "goal_id"
      ; "expected_goal_revision"
      ; "expected_catalog_revision"
      ; "input"
      ; "key"
      ]
  in
  let%bind kind = J.required_as fields "kind" J.string in
  let%bind () =
    if String.equal kind "goal_start_episode_v1"
    then Ok ()
    else Error (Protocol_error.invalid_request "unsupported native Goal start kind")
  in
  let%bind generation64 =
    J.required_as fields "generation" Goal_native_request_codec.counter
  in
  let%bind generation =
    Result.of_option
      (Int64.to_int generation64)
      ~error:
        (Protocol_error.invalid_request "native Goal start generation exceeds host range")
  in
  let%bind source = J.required_as fields "source" Run_source.of_json in
  let%bind goal_id = J.required_as fields "goal_id" Id.Goal.of_json in
  let%bind expected_goal_revision =
    J.required_as fields "expected_goal_revision" Goal_native_request_codec.counter
  in
  let%bind expected_catalog_revision =
    J.required_as fields "expected_catalog_revision" Goal_native_request_codec.counter
  in
  let%bind input = J.required_as fields "input" Run_input.of_json in
  let%bind key = J.required_as fields "key" Idempotency_key.of_json in
  create
    ~generation
    ~source
    ~goal_id
    ~expected_goal_revision
    ~expected_catalog_revision
    ~input
    ~key
;;

let digest t =
  let%map.Result canonical = J.canonical_string (to_json t) in
  Digestif.SHA256.(digest_string canonical |> to_hex)
;;
