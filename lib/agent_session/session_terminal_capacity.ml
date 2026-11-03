open! Core
module P = Agent_protocol

let validate
      ~index
      ~jobs
      ~permissions
      ~schedules
      ~revision
      ~transaction_sequence
      ~event_sequence
  =
  let open Result.Let_syntax in
  match index with
  | None -> Ok ()
  | Some index ->
    let%bind capacity = Run_state.terminal_capacity index in
    let%bind retries =
      List.fold_result (Run_state.runs index) ~init:0 ~f:(fun total run ->
        match run.P.Run.lifecycle with
        | Terminal _ -> Ok total
        | Admitted | Active | Waiting _ ->
          let proven =
            Set.of_list
              (module P.Run_work)
              (List.map run.terminal_work ~f:(fun proof -> proof.P.Run_work.Terminal.work))
          in
          let%bind retries =
            List.fold_result run.owned_work ~init:0 ~f:(fun retries work ->
              let additional =
                if Set.mem proven work
                then 0
                else (
                  match work.P.Run_work.key with
                  | Retained (Job { id; attempt }) ->
                    let maximum =
                      List.find jobs ~f:(fun job -> P.Id.Job.equal job.P.Job.id id)
                      |> Option.value_map ~default:attempt ~f:(fun job ->
                        match job.retry_policy with
                        | Never -> attempt
                        | Safe_retry { max_attempts; _ } | Idempotent { max_attempts; _ }
                          -> max_attempts)
                    in
                    Int.max 0 (maximum - attempt)
                  | Operation _
                  | Retained
                      ( Schedule _
                      | Invocation _
                      | Subscription _
                      | Delivery _
                      | Moderator_execution _ ) -> 0)
              in
              if additional > P.Run_limits.max_occurrences - retries
              then Error (P.Error.invalid_request "Run retry capacity bound exhausted")
              else Ok (retries + additional))
          in
          let missing =
            List.count run.owned_work ~f:(fun work -> not (Set.mem proven work))
          in
          let waits =
            List.count (Run_state.intents index) ~f:(fun intent ->
              P.Id.Run.equal intent.Run_intent.receipt.run_id run.id
              &&
              match intent.disposition, intent.action with
              | Pending, Wait _ -> true
              | _ -> false)
          in
          let remaining = 1 + missing + (2 * waits) + (2 * retries) in
          if Int64.(run.revision > max_value - of_int remaining)
          then Error (P.Error.invalid_request "Run retry terminal headroom exhausted")
          else Ok (total + (2 * retries)))
    in
    let steps = Run_terminal_capacity.remaining_steps capacity + retries in
    if steps = 0
    then Ok ()
    else (
      let pending =
        List.count permissions ~f:(fun permission ->
          P.Permission.equal_state permission.P.Permission.state Pending)
      in
      let reviews =
        List.count jobs ~f:(fun job ->
          (match job.P.Job.status with
           | Queued | Running -> true
           | _ -> false)
          &&
          match job.payload with
          | `Object fields ->
            List.Assoc.find fields "type" ~equal:String.equal
            |> Option.value_map ~default:false ~f:(function
              | `String name -> String.equal name "permission_review"
              | _ -> false)
          | _ -> false)
      in
      let timers =
        List.count schedules ~f:(fun schedule ->
          match schedule.P.Schedule.status with
          | Scheduled | Delivering -> true
          | Delivered | Cancelled | Failed _ -> false)
      in
      (* completed_delta has <=4 base payloads, terminal_delta <=3; apply adds
         <=1 moderator overlay and <=1 session.updated. Actual permission/review
         cleanup and timer cancellation each add one payload per retained row.
         This bounds known owner steps, not arbitrary future callback output. *)
      let events = (6 * steps) + pending + reviews + timers in
      let enough value reserve = Int64.(value <= max_value - of_int reserve) in
      if
        enough revision steps
        && enough transaction_sequence steps
        && enough event_sequence events
      then Ok ()
      else Error (P.Error.invalid_request "session lacks reserved Run terminal counters"))
;;
