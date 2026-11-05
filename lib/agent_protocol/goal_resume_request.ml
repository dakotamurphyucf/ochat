open! Core

module P = struct
  module Id = Id
  module Idempotency_key = Idempotency_key
  module Run_source = Run_source
  module Error = Protocol_error
  module Json_codec = Json_codec
end

module X = struct
  let int64_json n = `String (Int64.to_string n)
  let host_counter_to_json n = int64_json (Int64.of_int n)
end

type t =
  { session_id : P.Id.Session.t
  ; generation : int
  ; goal_id : P.Id.Goal.t
  ; run_id : P.Id.Run.t
  ; source : P.Run_source.t
  ; expected_goal_revision : int64
  ; expected_catalog_revision : int64
  ; expected_run_revision : int64
  ; key : P.Idempotency_key.t
  }
[@@deriving equal, sexp_of]

let create
      ~session_id
      ~generation
      ~goal_id
      ~run_id
      ~source
      ~expected_goal_revision
      ~expected_catalog_revision
      ~expected_run_revision
      ~key
  =
  let open Result.Let_syntax in
  let%bind _ = P.Id.Session.of_json (P.Id.Session.to_json session_id) in
  let%bind _ = P.Id.Goal.of_json (P.Id.Goal.to_json goal_id) in
  let%bind _ = P.Id.Run.of_json (P.Id.Run.to_json run_id) in
  let%bind _ = P.Idempotency_key.of_json (P.Idempotency_key.to_json key) in
  let%bind () = P.Run_source.validate source in
  if
    generation < 0
    || (not (Int.equal source.generation generation))
    || Int64.(expected_goal_revision < 0L)
    || Int64.(expected_catalog_revision < 0L)
    || Int64.(expected_run_revision < 0L)
  then Error (P.Error.invalid_request "invalid Goal resume request fences")
  else
    Ok
      { session_id
      ; generation
      ; goal_id
      ; run_id
      ; source
      ; expected_goal_revision
      ; expected_catalog_revision
      ; expected_run_revision
      ; key
      }
;;

let session_id t = t.session_id
let generation t = t.generation
let goal_id t = t.goal_id
let run_id t = t.run_id
let source t = t.source
let expected_goal_revision t = t.expected_goal_revision
let expected_catalog_revision t = t.expected_catalog_revision
let expected_run_revision t = t.expected_run_revision
let key t = t.key

let to_json t =
  `Object
    [ "kind", `String "goal_resume_episode_v1"
    ; "session_id", P.Id.Session.to_json t.session_id
    ; "generation", X.host_counter_to_json t.generation
    ; "goal_id", P.Id.Goal.to_json t.goal_id
    ; "run_id", P.Id.Run.to_json t.run_id
    ; "source", P.Run_source.to_json t.source
    ; "expected_goal_revision", X.int64_json t.expected_goal_revision
    ; "expected_catalog_revision", X.int64_json t.expected_catalog_revision
    ; "expected_run_revision", X.int64_json t.expected_run_revision
    ; "key", P.Idempotency_key.to_json t.key
    ]
;;

let of_json json =
  let open Result.Let_syntax in
  let module J = Json_codec in
  let%bind () = J.validate_limits ~max_bytes:4096 ~max_depth:8 json in
  let%bind f = J.fields json in
  let names =
    [ "kind"
    ; "session_id"
    ; "generation"
    ; "goal_id"
    ; "run_id"
    ; "source"
    ; "expected_goal_revision"
    ; "expected_catalog_revision"
    ; "expected_run_revision"
    ; "key"
    ]
  in
  let%bind () =
    if
      List.for_all (J.to_alist f) ~f:(fun (name, _) ->
        List.mem names name ~equal:String.equal)
    then Ok ()
    else Error (P.Error.invalid_request "unknown Goal resume request field")
  in
  let%bind kind = J.required_as f "kind" J.string in
  let%bind () =
    if String.equal kind "goal_resume_episode_v1"
    then Ok ()
    else Error (P.Error.invalid_request "unsupported Goal resume request kind")
  in
  let counter = function
    | `String encoded ->
      (match Int64.of_string_opt encoded with
       | Some n when Int64.(n >= 0L) && String.equal encoded (Int64.to_string n) -> Ok n
       | Some _ | None -> Error (P.Error.invalid_request "invalid Goal resume counter"))
    | _ -> Error (P.Error.invalid_request "Goal resume counters must be decimal strings")
  in
  let%bind session_id = J.required_as f "session_id" Id.Session.of_json in
  let%bind generation64 = J.required_as f "generation" counter in
  let%bind generation =
    Result.of_option
      (Int64.to_int generation64)
      ~error:(P.Error.invalid_request "Goal resume generation exceeds host range")
  in
  let%bind goal_id = J.required_as f "goal_id" Id.Goal.of_json in
  let%bind run_id = J.required_as f "run_id" Id.Run.of_json in
  let%bind source = J.required_as f "source" Run_source.of_json in
  let%bind expected_goal_revision = J.required_as f "expected_goal_revision" counter in
  let%bind expected_catalog_revision =
    J.required_as f "expected_catalog_revision" counter
  in
  let%bind expected_run_revision = J.required_as f "expected_run_revision" counter in
  let%bind key = J.required_as f "key" Idempotency_key.of_json in
  create
    ~session_id
    ~generation
    ~goal_id
    ~run_id
    ~source
    ~expected_goal_revision
    ~expected_catalog_revision
    ~expected_run_revision
    ~key
;;

let digest t =
  let%map.Result canonical = Json_codec.canonical_string (to_json t) in
  Digestif.SHA256.(digest_string canonical |> to_hex)
;;
