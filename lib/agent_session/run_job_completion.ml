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

let cancelled_dependency_work
      (state : Session_state.t)
      ~(parent : P.Job.t)
      ~result
      cancelled
  =
  let open Result.Let_syntax in
  let invalid message = Error (P.Error.invalid_request message) in
  let%bind () =
    if
      List.exists state.jobs ~f:(fun actual ->
        Jsonaf.exactly_equal (P.Job.to_json actual) (P.Job.to_json parent))
    then Ok ()
    else invalid "dependency cancellation requires its exact retained parent"
  in
  let rec descendants seen (job : P.Job.t) =
    let key = P.Id.Job.to_string job.id in
    if Set.mem seen key
    then invalid "cancelled dependency chain contains a cycle"
    else (
      let seen = Set.add seen key in
      match job.status with
      | Waiting_completion dependency ->
        let%bind () =
          Job_dependency.validate
            ~invocations:state.invocations
            ~events:state.moderator_executions
            ~jobs:state.jobs
            ~subscriptions:state.subscriptions
            job
        in
        (match dependency.work with
         | Subscription _ -> Ok []
         | Job id ->
           let%bind child =
             List.find state.jobs ~f:(fun child -> P.Id.Job.equal child.P.Job.id id)
             |> Result.of_option
                  ~error:
                    (P.Error.invalid_request "cancelled dependency target disappeared")
           in
           let%bind terminal = P.Job.terminal_result child in
           (match terminal with
            | Some _ -> Ok []
            | None ->
              let%map rest = descendants seen child in
              child :: rest))
      | Queued
      | Running
      | Waiting_permission _
      | Succeeded
      | Failed _
      | Cancelled
      | Interrupted _ -> Ok [])
  in
  let%bind expected =
    match Option.map result ~f:P.Stored_completion.outcome with
    | Some (Cancelled | Expired) -> descendants String.Set.empty parent
    | Some (Succeeded | Failed) | None ->
      if List.is_empty cancelled
      then Ok []
      else invalid "non-cancelling parent outcome cannot cancel dependencies"
  in
  let%bind expected = List.map expected ~f:Run_job_occurrence.work |> Result.all in
  let expected = Set.of_list (module P.Run_work) expected in
  let%bind actual =
    List.fold_result
      cancelled
      ~init:(Set.empty (module P.Run_work))
      ~f:(fun work terminal ->
        let%bind before =
          List.find state.jobs ~f:(fun job ->
            P.Id.Job.equal job.P.Job.id terminal.P.Job.id)
          |> Result.of_option
               ~error:(P.Error.invalid_request "cancelled dependency is not retained")
        in
        let%bind _ = P.Job.of_json (P.Job.to_json terminal) in
        let projection =
          { before with
            status = terminal.status
          ; result = terminal.result
          ; completed_at = terminal.completed_at
          ; delivery = terminal.delivery
          }
        in
        let%bind () =
          if
            (not (P.Id.Job.equal before.id parent.id))
            && Jsonaf.exactly_equal (P.Job.to_json projection) (P.Job.to_json terminal)
            &&
            match before.status, terminal.status with
            | (Queued | Running | Waiting_permission _ | Waiting_completion _), Cancelled
              -> true
            | _ -> false
          then Ok ()
          else invalid "cancelled dependency differs from its actual active occurrence"
        in
        let%bind () =
          P.Job.validate_delivery_transition ~previous:(Some before) terminal
        in
        let%bind result = P.Job.terminal_result terminal in
        let%bind () =
          if
            Option.exists result ~f:(fun stored ->
              P.Stored_completion.equal_outcome
                (P.Stored_completion.outcome stored)
                Cancelled)
          then Ok ()
          else invalid "cancelled dependency has no actual cancellation result"
        in
        let%bind occurrence = Run_job_occurrence.work before in
        if Set.mem work occurrence
        then invalid "cancelled dependency occurrence is repeated"
        else Ok (Set.add work occurrence))
  in
  if Set.equal expected actual
  then Ok actual
  else invalid "cancelled dependencies differ from the actual parent dependency chain"
;;

let prepare
      ?startup_run
      ?(work_delta = Session_delta.Batch [])
      ?(cancelled_dependencies = [])
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
    let%bind cancelled =
      cancelled_dependency_work state ~parent:running ~result cancelled_dependencies
    in
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
            let%bind dependency_proofs =
              List.filter previous.owned_work ~f:(fun work -> Set.mem cancelled work)
              |> List.map ~f:(fun work ->
                if
                  List.exists previous.terminal_work ~f:(fun old ->
                    P.Run_work.equal old.work work)
                then
                  Error
                    (P.Error.invalid_request
                       "cancelled dependency already has immutable terminal evidence")
                else P.Run_work.Terminal.create ~work ~outcome:Cancelled ~revision)
              |> Result.all
            in
            let%bind producer_state, producer_proofs =
              Run_producer_settlement.capture
                state
                ~delta:
                  (Session_delta.Batch
                     (work_delta
                      :: Session_delta.Job_changed successor
                      :: List.map cancelled_dependencies ~f:(fun job ->
                        Session_delta.Job_changed job)))
                ~run:previous
                ~owned_work
            in
            let%bind () =
              if Option.equal Run_state.equal state.run_state producer_state.run_state
              then Ok ()
              else
                Error
                  (P.Error.invalid_request "Job companion work cannot advance its Run")
            in
            let%bind index =
              Run_producer_settlement.settle
                ?startup_run
                index
                ~state
                ~projected:producer_state
                ~previous
                ~owned_work
                ~terminal_work:
                  (proof :: (previous.terminal_work @ dependency_proofs @ producer_proofs))
                ~now
            in
            if not (exact_wait previous running)
            then Ok index
            else (
              let%bind frame =
                Chat_response.Background_delivery.create
                  ~source:previous.source.observer
                  terminal
                |> Result.map_error ~f:P.Error.invalid_request
              in
              let%bind next =
                Result.of_option
                  (Run_state.find index previous.id)
                  ~error:(P.Error.invalid_request "job settlement lost its Run")
              in
              let%bind delivery = Run_job_delivery.capture next ~frame in
              Run_state.add_job_delivery index delivery)))
    in
    if !changed then [ Session_delta.Run_state_changed index ] else []
;;

let cancel_queued ?startup_run (state : Session_state.t) ~job ~now =
  let open Result.Let_syntax in
  let%bind () =
    if
      (match job.P.Job.kind with
       | Async_tool | Model_call -> true
       | Nested_agent | Scheduled_event | Shell_process | Compaction -> false)
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
            else if P.Job.equal_kind job.kind Model_call
            then
              Run_work_settlement.prepare
                ?startup_run
                index
                ~state
                ~previous:run
                ~owned_work:run.owned_work
                ~terminal_work:(proof :: run.terminal_work)
                ~operation_id:None
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
