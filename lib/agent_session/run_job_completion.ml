open! Core
module P = Agent_protocol

let exact_wait (run : P.Run.t) (job : P.Job.t) =
  match run.lifecycle with
  | Waiting wake ->
    (match wake.occurrence with
     | Job_completion { job_id; attempt } ->
       P.Id.Job.equal job_id job.id
       && Int.equal attempt job.attempt
       && Int.equal wake.source.generation job.generation
     | Delivered_timer _ | Subscription_delivery _ -> false)
  | Admitted | Active | Terminal _ -> false
;;

let owns (state : Session_state.t) job =
  Option.exists state.run_state ~f:(fun index ->
    List.exists (Run_state.runs index) ~f:(fun run ->
      match run.P.Run.lifecycle with
      | Terminal _ -> false
      | Admitted | Active | Waiting _ ->
        List.exists run.owned_work ~f:(fun work -> Run_job_occurrence.matches job ~work)))
;;

let awaits (state : Session_state.t) job =
  Option.exists state.run_state ~f:(fun index ->
    List.exists (Run_state.runs index) ~f:(fun run -> exact_wait run job))
;;

let prepare
      (state : Session_state.t)
      ~(running : P.Job.t)
      ~(terminal : P.Job.t)
      ~(successor : P.Job.t)
      ~(outcome : P.Run_work.Terminal.outcome)
      ~now
  =
  let open Result.Let_syntax in
  let same_attempt job =
    P.Id.Job.equal running.id job.P.Job.id
    && P.Id.Session.equal running.session_id job.session_id
    && Int.equal running.generation job.generation
    && Int.equal running.attempt job.attempt
    && Option.equal P.Job.equal_launch running.launch job.launch
    && Jsonaf.exactly_equal running.payload job.payload
  in
  let%bind () =
    if
      same_attempt terminal
      && same_attempt successor
      && List.exists state.jobs ~f:(fun job -> same_attempt job)
      &&
      match running.status with
      | Running | Waiting_permission _ | Waiting_completion _ -> true
      | Queued | Succeeded | Failed _ | Cancelled | Interrupted _ -> false
    then Ok ()
    else
      Error
        (P.Error.invalid_request
           "run terminal job differs from its actual execution attempt")
  in
  match state.run_state with
  | None -> Ok []
  | Some original ->
    let%bind result = P.Job.terminal_result terminal in
    let%bind () =
      match result with
      | None -> Error (P.Error.invalid_request "owned job has no actual terminal result")
      | Some stored ->
        let matches =
          match P.Stored_completion.outcome stored, outcome with
          | Succeeded, Succeeded
          | Failed, (Failed | Limited | Interrupted)
          | Cancelled, Cancelled
          | Expired, Limited -> true
          | Succeeded, (Failed | Cancelled | Limited | Interrupted | Unconfirmed)
          | Failed, (Succeeded | Cancelled | Unconfirmed)
          | Cancelled, (Succeeded | Failed | Limited | Interrupted | Unconfirmed)
          | Expired, (Succeeded | Failed | Cancelled | Interrupted | Unconfirmed) -> false
        in
        if matches
        then Ok ()
        else
          Error
            (P.Error.invalid_request
               "owned job proof disagrees with actual terminal outcome")
    in
    let%bind work =
      P.Run_work.create
        ~key:(Retained (Job { id = running.id; attempt = running.attempt }))
        ~generation:running.generation
    in
    let changed = ref false in
    let%map index =
      List.fold_result (Run_state.runs original) ~init:original ~f:(fun index previous ->
        match previous.P.Run.lifecycle with
        | Terminal _ -> Ok index
        | Admitted | Active | Waiting _ ->
          if not (List.exists previous.owned_work ~f:(P.Run_work.equal work))
          then Ok index
          else (
            changed := true;
            let%bind () =
              if Int64.equal previous.revision Int64.max_value
              then
                Error (P.Error.invalid_request "run revision exhausted at job completion")
              else Ok ()
            in
            let revision = Int64.succ previous.revision in
            let%bind proof = P.Run_work.Terminal.create ~work ~outcome ~revision in
            let%bind () =
              if
                List.exists previous.terminal_work ~f:(fun old ->
                  P.Run_work.equal old.work work)
              then
                Error
                  (P.Error.invalid_request
                     "run job attempt already has immutable terminal evidence")
              else Ok ()
            in
            let%bind owned_work =
              match successor.status with
              | Queued ->
                let%map next = Run_job_occurrence.work successor in
                Set.add (Set.of_list (module P.Run_work) previous.owned_work) next
                |> Set.to_list
              | Running | Waiting_permission _ | Waiting_completion _ ->
                Error
                  (P.Error.invalid_request "terminal job successor is still executing")
              | Succeeded | Failed _ | Cancelled | Interrupted _ -> Ok previous.owned_work
            in
            let%bind next =
              P.Run.create
                ~id:previous.id
                ~session:previous.session
                ~principal_id:previous.principal_id
                ~source:previous.source
                ~mode:previous.mode
                ~lifecycle:previous.lifecycle
                ~revision
                ~owned_work
                ~relinquished_work:previous.relinquished_work
                ~terminal_work:(proof :: previous.terminal_work)
                ~created_at:previous.created_at
                ~updated_at:now
            in
            let%bind index = Run_state.advance index ~run:next ~intents:[] in
            if not (exact_wait previous running)
            then Ok index
            else (
              let%bind frame =
                Chat_response.Background_delivery.create
                  ~source:previous.source.observer
                  terminal
                |> Result.map_error ~f:P.Error.invalid_request
              in
              let%bind delivery = Run_job_delivery.capture next ~frame in
              Run_state.add_job_delivery index delivery)))
    in
    if !changed then [ Session_delta.Run_state_changed index ] else []
;;

let cancel_queued (state : Session_state.t) ~job ~now =
  let open Result.Let_syntax in
  let%bind () =
    if
      P.Job.equal_kind job.P.Job.kind Async_tool
      && (match job.status with
          | Queued -> true
          | Running
          | Waiting_permission _
          | Waiting_completion _
          | Succeeded
          | Failed _
          | Cancelled
          | Interrupted _ -> false)
      && List.exists state.jobs ~f:(fun current ->
        Jsonaf.exactly_equal (P.Job.to_json current) (P.Job.to_json job))
    then Ok ()
    else Error (P.Error.invalid_request "queued cancellation has no actual current job")
  in
  match state.run_state with
  | None -> Ok []
  | Some original ->
    let%bind work = Run_job_occurrence.work job in
    let changed = ref false in
    let%map index =
      List.fold_result (Run_state.runs original) ~init:original ~f:(fun index run ->
        match run.P.Run.lifecycle with
        | Terminal _ -> Ok index
        | Admitted | Active | Waiting _ ->
          if not (List.exists run.owned_work ~f:(P.Run_work.equal work))
          then Ok index
          else (
            let%bind () =
              if Int64.equal run.revision Int64.max_value
              then
                Error
                  (P.Error.invalid_request
                     "run revision exhausted at queued cancellation")
              else Ok ()
            in
            let revision = Int64.succ run.revision in
            let%bind proof =
              P.Run_work.Terminal.create ~work ~outcome:Cancelled ~revision
            in
            changed := true;
            let waiting =
              match run.lifecycle with
              | Waiting wake ->
                (match wake.occurrence with
                 | Job_completion { job_id; attempt } ->
                   P.Id.Job.equal job_id job.id
                   &&
                     (match work.key with
                     | Retained (Job { attempt = planned; _ }) ->
                       Int.equal attempt planned
                     | Operation _
                     | Retained
                         ( Schedule _
                         | Invocation _
                         | Subscription _
                         | Delivery _
                         | Moderator_execution _ ) -> false)
                 | Delivered_timer _ | Subscription_delivery _ -> false)
              | Admitted | Active | Terminal _ -> false
            in
            if waiting
            then
              Run_retirement.interrupt_with_evidence
                index
                ~run_id:run.id
                ~evidence:[ proof ]
                ~session_revision:(Int64.succ state.counters.revision)
                ~now
            else (
              let%bind run =
                P.Run.create
                  ~id:run.id
                  ~session:run.session
                  ~principal_id:run.principal_id
                  ~source:run.source
                  ~mode:run.mode
                  ~lifecycle:run.lifecycle
                  ~revision
                  ~owned_work:run.owned_work
                  ~relinquished_work:run.relinquished_work
                  ~terminal_work:(proof :: run.terminal_work)
                  ~created_at:run.created_at
                  ~updated_at:now
              in
              Run_state.advance index ~run ~intents:[])))
    in
    if !changed then [ Session_delta.Run_state_changed index ] else []
;;
