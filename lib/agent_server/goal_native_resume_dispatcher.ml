open! Core
module A = Agent_session.Session_actor
module P = Agent_protocol

type t =
  { factory : Session_factory.t
  ; closed : bool Atomic.t
  }

let create ~factory = { factory; closed = Atomic.make false }
let close t = Atomic.set t.closed true

let process_entry t (entry : Session_registry.entry) =
  let open Result.Let_syntax in
  if Atomic.get t.closed
  then Ok ()
  else (
    let%bind selection = A.select_native_goal_resume entry.actor in
    match selection with
    | None -> Ok ()
    | Some selection ->
      let%bind () =
        Session_factory.check_registered_entry
          t.factory
          entry
          ~session:(A.Goal_native_resume.Selection.session selection)
      in
      if Atomic.get t.closed
      then Ok ()
      else (
        let result = Goal_run_service.resume_native_episode t.factory ~entry ~selection in
        let failure =
          match result with
          | Ok (A.Goal_resume_result.Resumed _ | Retained _) -> None
          | Ok (Resume_failed { error; _ } | Rejected error | Uncertain error)
          | Error error -> Some error
        in
        (* Factual projection cannot issue new permission. Actor recognizes an
           unconsumed busy reservation and leaves its same accepted loan pending;
           attempted/unknown writers never become eligible again. *)
        A.observe_native_goal_dispatch entry.actor ~selection ~failure))
;;
