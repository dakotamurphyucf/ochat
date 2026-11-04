open! Core
module P = Agent_protocol

let invalid message = Error (P.Error.invalid_request message)

let append_children index ~(run : P.Run.t) ~work_state ~children ~now =
  let open Result.Let_syntax in
  let%bind () =
    let unique = Set.of_list (module P.Run_work) children in
    if
      (not (List.is_empty children))
      && Int.equal (Set.length unique) (List.length children)
      && List.for_all children ~f:(fun child ->
        Int.equal child.P.Run_work.generation run.source.generation
        && (not (List.mem run.owned_work child ~equal:P.Run_work.equal))
        && (not (List.mem run.relinquished_work child ~equal:P.Run_work.equal))
        && not
             (List.exists run.terminal_work ~f:(fun proof ->
                P.Run_work.equal proof.work child)))
    then Ok ()
    else invalid "child work must introduce distinct new occurrences"
  in
  let%bind revision =
    if Int64.equal run.revision Int64.max_value
    then invalid "Run revision exhausted at child admission"
    else Ok (Int64.succ run.revision)
  in
  let%bind next =
    P.Run.create
      ~id:run.id
      ~session:run.session
      ~principal_id:run.principal_id
      ~source:run.source
      ~mode:run.mode
      ~lifecycle:run.lifecycle
      ~revision
      ~owned_work:(run.owned_work @ children)
      ~relinquished_work:run.relinquished_work
      ~terminal_work:run.terminal_work
      ~created_at:run.created_at
      ~updated_at:now
  in
  let%bind index = Run_state.advance index ~run:next ~intents:[] in
  let%map () = Session_state.validate { work_state with run_state = Some index } in
  Session_delta.Run_state_changed index
;;

let prepare state ~(run : P.Run.t) ~parent ~work_state ~children ~now =
  let open Result.Let_syntax in
  let%bind index =
    Result.of_option
      state.Session_state.run_state
      ~error:(P.Error.invalid_request "child Run index is absent")
  in
  let%bind installed =
    Run_source_installation.captured
      (Run_state.installation index)
      ~generation:state.identity.generation
  in
  let%bind observer = Moderator_checkpoint.observer state.moderator in
  let%bind prospective_observer =
    Moderator_checkpoint.observer work_state.Session_state.moderator
  in
  let%bind () =
    if
      Option.equal Run_state.equal state.run_state work_state.run_state
      && P.Run_source.equal installed run.source
      && Option.equal P.Invocation.equal_observer observer (Some installed.observer)
      && Option.equal P.Invocation.equal_observer prospective_observer observer
      && Option.exists (Run_state.find index run.id) ~f:(P.Run.equal run)
      && P.Id.Session.equal
           state.identity.session_id
           work_state.Session_state.identity.session_id
      && Int.equal state.identity.generation work_state.identity.generation
      && Int.equal state.identity.generation run.source.generation
      && (match run.lifecycle with
          | Admitted | Active | Waiting _ -> true
          | Terminal _ -> false)
      && List.mem run.owned_work parent ~equal:P.Run_work.equal
      && (not (List.mem run.relinquished_work parent ~equal:P.Run_work.equal))
      && not
           (List.exists run.terminal_work ~f:(fun proof ->
              P.Run_work.equal proof.work parent))
    then Ok ()
    else invalid "child work requires its exact live retained Run parent"
  in
  append_children index ~run ~work_state ~children ~now
;;

let prepare_attributed_callback
      state
      ~(run : P.Run.t)
      ~retained_owner
      ~work_state
      ~children
      ~now
  =
  let open Result.Let_syntax in
  let%bind index =
    Result.of_option
      state.Session_state.run_state
      ~error:(P.Error.invalid_request "callback child Run index is absent")
  in
  let%bind installed =
    Run_source_installation.captured
      (Run_state.installation index)
      ~generation:state.identity.generation
  in
  let%bind observer = Moderator_checkpoint.observer state.moderator in
  let%bind prospective_observer =
    Moderator_checkpoint.observer work_state.Session_state.moderator
  in
  let%bind () =
    if
      Option.equal Run_state.equal state.run_state work_state.run_state
      && P.Run_source.equal installed run.source
      && Option.equal P.Invocation.equal_observer observer (Some installed.observer)
      && Option.equal P.Invocation.equal_observer prospective_observer observer
      && Option.exists (Run_state.find index run.id) ~f:(P.Run.equal run)
      && P.Id.Session.equal state.identity.session_id work_state.identity.session_id
      && P.Id.Session.equal
           state.identity.session_id
           (P.Session_ref.session_id run.session)
      && Int.equal state.identity.generation work_state.identity.generation
      && Int.equal state.identity.generation run.source.generation
      && retained_owner installed
      &&
      match run.lifecycle with
      | Admitted | Active | Waiting _ -> true
      | Terminal _ -> false
    then Ok ()
    else invalid "child work requires its exact live original callback"
  in
  let%bind () =
    if
      List.for_all children ~f:(fun (child : P.Job.t) ->
        P.Id.Session.equal child.session_id state.identity.session_id
        && Int.equal child.generation state.identity.generation
        && Option.equal
             P.Inference_origin.equal
             child.inference_origin
             (Some (Run run.id))
        && (not (List.exists state.jobs ~f:(fun job -> P.Id.Job.equal job.id child.id)))
        && List.exists work_state.jobs ~f:(fun job ->
          P.Id.Job.equal job.id child.id
          && Jsonaf.exactly_equal (P.Job.to_json job) (P.Job.to_json child)))
    then Ok ()
    else invalid "callback children require exact new Job rows with original Run origin"
  in
  let%bind children = List.map children ~f:Run_job_occurrence.work |> Result.all in
  append_children index ~run ~work_state ~children ~now
;;

let prepare_observation state ~(run : P.Run.t) ~observing ~work_state ~children ~now =
  let retained_owner (installed : P.Run_source.t) =
    let retained invocations =
      List.exists invocations ~f:(P.Invocation.equal observing)
    in
    retained state.Session_state.invocations
    && retained work_state.Session_state.invocations
    && P.Id.Session.equal observing.context.session_id state.identity.session_id
    && Int.equal observing.context.generation state.identity.generation
    && Option.equal
         P.Inference_origin.equal
         observing.inference_origin
         (Some (Run run.id))
    &&
    match observing.observation with
    | Some { status = Observing; observer; _ } ->
      P.Invocation.equal_observer observer installed.observer
    | None | Some { status = Awaiting | Observed | Observation_failed _; _ } -> false
  in
  prepare_attributed_callback state ~run ~retained_owner ~work_state ~children ~now
;;

let prepare_callback state ~(run : P.Run.t) ~executing ~work_state ~children ~now =
  let open Result.Let_syntax in
  let%bind origin =
    Invocation_inference_provenance.of_event
      ~runs:state.Session_state.run_state
      ~jobs:state.jobs
      executing
  in
  let retained_owner (installed : P.Run_source.t) =
    let retained events = List.exists events ~f:(P.Moderator_execution.equal executing) in
    retained state.moderator_executions
    && retained work_state.Session_state.moderator_executions
    && P.Id.Session.equal executing.context.session_id state.identity.session_id
    && Int.equal executing.context.generation state.identity.generation
    && P.Invocation.equal_observer executing.context.source installed.observer
    && Option.equal P.Inference_origin.equal origin (Some (Run run.id))
    &&
    match executing.status with
    | Running -> true
    | Completed _ | Failed _ | Interrupted _ -> false
  in
  prepare_attributed_callback state ~run ~retained_owner ~work_state ~children ~now
;;
