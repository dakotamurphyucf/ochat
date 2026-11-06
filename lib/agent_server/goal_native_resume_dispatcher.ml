open! Core
module A = Agent_session.Session_actor
module P = Agent_protocol

type t =
  { factory : Session_factory.t
  ; closed : bool Atomic.t
  }

let create ~factory = { factory; closed = Atomic.make false }
let close t = Atomic.set t.closed true

let start_failure = function
  | Error error -> Some error
  | Ok (Goal_run_service.Outcome.Retained _) -> None
  | Ok (Attempted attempt) ->
    (match Goal_run_service.Attempt.result attempt with
     | Error error -> Some error
     | Ok
         ( Goal_run_service.Activation_result.Admission_uncertain error
         | Activation_failed { error; _ } ) -> Some error
     | Ok (Activated _ | Controlled _ | Settled _ | Retained _) -> None)
;;

let process_entry t (entry : Session_registry.entry) =
  let open Result.Let_syntax in
  if Atomic.get t.closed
  then Ok ()
  else (
    let%bind selection = A.select_native_goal_action entry.actor in
    match selection with
    | None -> Ok ()
    | Some selection ->
      let session =
        match selection with
        | A.Goal_native_action.Selection.Start selected ->
          A.Goal_native_start.Selection.session selected
        | Resume selected -> A.Goal_native_resume.Selection.session selected
      in
      let%bind () = Session_factory.check_registered_entry t.factory entry ~session in
      if Atomic.get t.closed
      then Ok ()
      else (
        (* Observe only after the actual source bracket has joined. Missing host
           remains pending; unknown admission never receives replay permission. *)
        match selection with
        | A.Goal_native_action.Selection.Start selection ->
          let failure =
            Goal_run_service.start_native_episode t.factory ~entry ~selection
            |> start_failure
          in
          A.observe_native_goal_start_dispatch entry.actor ~selection ~failure
        | Resume selection ->
          let result =
            Goal_run_service.resume_native_episode t.factory ~entry ~selection
          in
          let failure =
            match result with
            | Ok (A.Goal_resume_result.Resumed _ | Retained _) -> None
            | Ok (Resume_failed { error; _ } | Rejected error | Uncertain error)
            | Error error -> Some error
          in
          A.observe_native_goal_dispatch entry.actor ~selection ~failure))
;;
