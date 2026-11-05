open! Core
module P = Agent_protocol
module X = Persistence_codec

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

let digest t =
  let json =
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
  in
  let%map.Result canonical = P.Json_codec.canonical_string json in
  Digestif.SHA256.(digest_string canonical |> to_hex)
;;
