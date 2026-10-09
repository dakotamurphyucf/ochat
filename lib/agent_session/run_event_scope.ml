open! Core
module P = Agent_protocol

module Run_id = struct
  include P.Id.Run
  include Comparator.Make (P.Id.Run)
end

let pending_finish_ids index =
  Run_state.intents index
  |> List.filter_map ~f:(fun (intent : Run_intent.t) ->
    match intent.disposition, intent.action with
    | Pending, Finish _ -> Some intent.receipt.run_id
    | Pending, (Continue | Wait _) | (Consumed _ | Retired), _ -> None)
  |> Set.of_list (module Run_id)
;;

let owns_operation (run : P.Run.t) operation_id =
  List.exists run.owned_work ~f:(fun work ->
    match work.P.Run_work.key with
    | Operation id -> P.Id.Operation.equal id operation_id
    | Retained _ -> false)
;;

let owns_job (run : P.Run.t) (job : P.Moderator_execution.job_attempt) =
  List.exists run.owned_work ~f:(fun work ->
    match work.P.Run_work.key with
    | Retained (Job { id; attempt }) ->
      P.Id.Job.equal id job.job_id && Int.equal attempt job.attempt
    | Operation _
    | Retained
        (Schedule _ | Invocation _ | Subscription _ | Delivery _ | Moderator_execution _)
      -> false)
;;

let selected_by_context (run : P.Run.t) (context : P.Moderator_execution.context) =
  match context.operation_id, context.job, run.lifecycle with
  | Some id, None, (Admitted | Active) -> owns_operation run id
  | None, Some job, (Admitted | Active) -> owns_job run job
  | None, None, Active ->
    P.Moderator_execution.equal_phase context.phase Internal_event
    && List.exists run.owned_work ~f:(fun work ->
      match work.P.Run_work.key with
      | Retained (Moderator_execution id) -> P.Id.Moderator_execution.equal id context.id
      | Operation _
      | Retained (Job _ | Schedule _ | Invocation _ | Subscription _ | Delivery _) ->
        false)
  | None, None, Admitted ->
    P.Moderator_execution.equal_phase context.phase Session_start
    && P.Run.Mode.equal run.mode Workflow
    && List.is_empty run.owned_work
  | Some _, Some _, _
  | None, None, (Waiting _ | Terminal _)
  | Some _, None, (Waiting _ | Terminal _)
  | None, Some _, (Waiting _ | Terminal _) -> false
;;

let select (state : Session_state.t) ~(executing : P.Moderator_execution.t) =
  match state.run_state with
  | None -> Ok None
  | Some index ->
    let installed =
      Run_source_installation.captured
        (Run_state.installation index)
        ~generation:state.identity.generation
    in
    let pending_finishes = pending_finish_ids index in
    let candidates =
      match installed with
      | Error _ -> []
      | Ok installed ->
        List.filter (Run_state.runs index) ~f:(fun (run : P.Run.t) ->
          P.Run_source.equal run.source installed
          && Int.equal executing.context.generation run.source.generation
          && P.Invocation.equal_observer executing.context.source run.source.observer
          && (not (Set.mem pending_finishes run.id))
          && selected_by_context run executing.context)
    in
    (match candidates with
     | [] -> Ok None
     | [ run ] -> Ok (Some run)
     | _ :: _ :: _ ->
       Error
         (P.Error.create
            Conflict
            ~message:"actual callback belongs to more than one run"
            ~retryable:false
            ()))
;;
