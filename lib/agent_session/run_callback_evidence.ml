open! Core
module P = Agent_protocol

let capture
      (run : P.Run.t)
      ~(executing : P.Moderator_execution.t)
      ~(completed : P.Moderator_execution.t)
  =
  let open Result.Let_syntax in
  let work =
    List.find run.owned_work ~f:(fun work ->
      match work.P.Run_work.key with
      | Retained (Moderator_execution id) ->
        P.Id.Moderator_execution.equal id executing.context.id
      | Operation _
      | Retained (Job _ | Schedule _ | Invocation _ | Subscription _ | Delivery _) ->
        false)
  in
  match work with
  | None -> Ok run.terminal_work
  | Some work ->
    let%bind outcome =
      if
        P.Moderator_execution.equal_context executing.context completed.context
        && Int.equal work.generation run.source.generation
        && Int.equal completed.context.generation run.source.generation
        && P.Invocation.equal_observer completed.context.source run.source.observer
        &&
        match executing.status with
        | Running -> true
        | Completed _ | Failed _ | Interrupted _ -> false
      then (
        match completed.status with
        | Completed _ -> Ok P.Run_work.Terminal.Succeeded
        | Failed _ -> Ok Failed
        | Interrupted _ -> Ok Interrupted
        | Running -> Error (P.Error.invalid_request "owned callback is still running"))
      else
        Error (P.Error.invalid_request "terminal callback differs from its owned borrow")
    in
    (match
       List.find run.terminal_work ~f:(fun proof -> P.Run_work.equal proof.work work)
     with
     | Some proof ->
       if P.Run_work.Terminal.equal_outcome proof.outcome outcome
       then Ok run.terminal_work
       else Error (P.Error.invalid_request "owned callback terminal evidence changed")
     | None ->
       if Int64.equal run.revision Int64.max_value
       then Error (P.Error.invalid_request "callback run revision exhausted")
       else (
         let%bind () = P.Run_limits.check_count (List.length run.terminal_work + 1) in
         let%map proof =
           P.Run_work.Terminal.create ~work ~outcome ~revision:(Int64.succ run.revision)
         in
         proof :: run.terminal_work))
;;
