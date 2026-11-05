open! Core
module P = Agent_protocol

type t =
  { request : Run_admission_request.Headless.t
  ; intent : Goal_episode_admission.intent
  }

let create ~goal_id ~expected_goal_revision ~expected_catalog_revision ~request =
  let open Result.Let_syntax in
  let%bind () =
    if P.Run.Mode.equal request.Run_admission_request.Headless.mode Workflow
    then Ok ()
    else Error (P.Error.invalid_request "Goal episodes require Workflow mode")
  in
  let%map intent =
    Goal_episode_admission.intent
      ~goal_id
      ~expected_goal_revision
      ~expected_catalog_revision
  in
  { request; intent }
;;

let request t = t.request
let intent t = t.intent
