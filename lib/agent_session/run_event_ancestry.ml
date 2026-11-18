open! Core
module P = Agent_protocol

let find runs ~(executing : P.Moderator_execution.t) =
  if not (P.Moderator_execution.equal_phase executing.context.phase Internal_event)
  then Ok None
  else (
    let matches (work : P.Run_work.t) =
      match work.key with
      | Retained (Moderator_execution id) ->
        P.Id.Moderator_execution.equal id executing.context.id
      | Operation _
      | Retained (Job _ | Schedule _ | Invocation _ | Subscription _ | Delivery _) ->
        false
    in
    match
      List.filter runs ~f:(fun (run : P.Run.t) ->
        List.exists run.owned_work ~f:matches
        || List.exists run.relinquished_work ~f:matches
        || List.exists run.terminal_work ~f:(fun proof -> matches proof.work))
    with
    | [] -> Ok None
    | [ run ] -> Ok (Some run)
    | _ :: _ :: _ ->
      Error
        (P.Error.create
           Conflict
           ~message:"queued event has conflicting retained Run owners"
           ~retryable:false
           ()))
;;
