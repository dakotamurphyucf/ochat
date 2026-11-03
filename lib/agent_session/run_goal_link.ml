open! Core
module P = Agent_protocol
module X = Persistence_codec

type t =
  { goal_id : P.Id.Goal.t
  ; episode : P.Goal_episode.t
  ; intent_sha256 : string
  }
[@@deriving equal, sexp_of]

let run_id t = (P.Goal_episode.admission t.episode).run_id
let goal_id t = t.goal_id
let episode t = t.episode
let intent_sha256 t = t.intent_sha256
let invalid message = Error (P.Error.invalid_request message)

let create ~goal_id ~episode ~intent_sha256 =
  let open Result.Let_syntax in
  let%bind _ = P.Id.Goal.of_json (P.Id.Goal.to_json goal_id) in
  let%bind _ = P.Goal_episode.of_json (P.Goal_episode.to_json episode) in
  if
    String.length intent_sha256 = 64
    && String.for_all intent_sha256 ~f:(function
      | '0' .. '9' | 'a' .. 'f' -> true
      | _ -> false)
  then Ok { goal_id; episode; intent_sha256 }
  else invalid "goal association requires a canonical SHA256 intent digest"
;;

let to_jsonaf t =
  `Object
    [ "run_id", P.Id.Run.to_json (run_id t)
    ; "goal_id", P.Id.Goal.to_json t.goal_id
    ; "episode", P.Goal_episode.to_json t.episode
    ; "intent_sha256", `String t.intent_sha256
    ]
;;

let of_jsonaf json =
  let open Result.Let_syntax in
  let%bind () =
    P.Extension_codec.validate_json
      ~max_bytes:P.Run_limits.max_document_bytes
      ~max_depth:P.Run_limits.max_depth
      json
  in
  let%bind fields = X.object_ json in
  let%bind id = X.required fields "run_id" P.Id.Run.of_json in
  let%bind goal_id = X.required fields "goal_id" P.Id.Goal.of_json in
  let%bind episode = X.required fields "episode" P.Goal_episode.of_json in
  let%bind intent_sha256 = X.required fields "intent_sha256" P.Json_codec.string in
  let%bind t = create ~goal_id ~episode ~intent_sha256 in
  if P.Id.Run.equal id (run_id t)
  then Ok t
  else invalid "goal association Run identity differs from its admission"
;;

let shape =
  X.shape_exn
    [ "run_id", Document_schema.Shape.value
    ; "goal_id", Document_schema.Shape.value
    ; ( "episode"
      , X.shape_exn
          [ "session", X.fields_shape [ "server_id"; "session_id" ]
          ; "admission", Run_record_shapes.receipt
          ] )
    ; "intent_sha256", Document_schema.Shape.value
    ]
;;
