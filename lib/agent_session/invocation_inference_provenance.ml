open! Core
module P = Agent_protocol

let invalid message = Error (P.Error.invalid_request message)
let all_runs runs = Option.value_map runs ~default:[] ~f:Run_state.runs

let work_exists (run : P.Run.t) ~f =
  List.exists run.owned_work ~f
  || List.exists run.relinquished_work ~f
  || List.exists run.terminal_work ~f:(fun terminal -> f terminal.work)
;;

let origin_of_owners = function
  | [] -> Ok P.Inference_origin.Unbound
  | [ (run : P.Run.t) ] -> Ok (P.Inference_origin.Run run.id)
  | _ :: _ :: _ -> invalid "inference ancestry has conflicting retained Runs"
;;

let operation_owners runs id =
  List.filter (all_runs runs) ~f:(fun run ->
    work_exists run ~f:(fun work ->
      match work.P.Run_work.key with
      | Operation candidate -> P.Id.Operation.equal id candidate
      | Retained _ -> false))
;;

let event_owners runs id =
  List.filter (all_runs runs) ~f:(fun run ->
    work_exists run ~f:(fun work ->
      match work.P.Run_work.key with
      | Retained (Moderator_execution candidate) ->
        P.Id.Moderator_execution.equal id candidate
      | Operation _ | Retained _ -> false))
;;

let of_operation ~runs (operation : P.Operation.t) =
  origin_of_owners (operation_owners runs operation.id)
;;

let of_invocation (invocation : P.Invocation.t) =
  match invocation.inference_origin with
  | Some origin -> Ok origin
  | None -> invalid "invocation inference origin is unavailable"
;;

let of_job (job : P.Job.t) =
  match job.inference_origin with
  | Some origin -> Ok origin
  | None -> invalid "Job inference origin is unavailable"
;;

let of_event ~runs ~jobs (event : P.Moderator_execution.t) =
  let open Result.Let_syntax in
  let%bind causal = Run_event_ancestry.find (all_runs runs) ~executing:event in
  match causal with
  | Some run -> Ok (Some (P.Inference_origin.Run run.id))
  | None ->
    (match event.context.job, event.context.operation_id with
     | Some parent, _ ->
       (match
          List.find jobs ~f:(fun (job : P.Job.t) -> P.Id.Job.equal job.id parent.job_id)
        with
        | Some job -> Ok job.inference_origin
        | None -> invalid "event inference parent Job is unavailable")
     | None, Some id ->
       origin_of_owners (operation_owners runs id) |> Result.map ~f:Option.some
     | None, None ->
       origin_of_owners (event_owners runs event.context.id) |> Result.map ~f:Option.some)
;;

let validate ~invocations ~jobs ~events ~runs =
  let open Result.Let_syntax in
  let runs_list = all_runs runs in
  List.fold_result invocations ~init:() ~f:(fun () (invocation : P.Invocation.t) ->
    match invocation.inference_origin with
    | None -> Ok ()
    | Some origin ->
      let%bind () = P.Inference_origin.validate origin in
      let%bind named =
        match origin with
        | Unbound -> Ok None
        | Run id ->
          (match
             List.find runs_list ~f:(fun (run : P.Run.t) -> P.Id.Run.equal id run.id)
           with
           | None -> invalid "invocation inference origin names an unavailable Run"
           | Some run ->
             if
               P.Id.Session.equal
                 invocation.context.session_id
                 (P.Session_ref.session_id run.session)
               && Int.equal invocation.context.generation run.source.generation
             then Ok (Some run)
             else
               invalid
                 "invocation inference origin differs from its Run session or generation")
      in
      let%bind () =
        match named, invocation.observation with
        | Some run, Some observation ->
          if P.Invocation.equal_observer observation.observer run.source.observer
          then Ok ()
          else invalid "invocation inference origin differs from its observation source"
        | None, _ | Some _, None -> Ok ()
      in
      let compare_origin candidate =
        match candidate with
        | None -> Ok ()
        | Some candidate ->
          if P.Inference_origin.equal origin candidate
          then Ok ()
          else invalid "invocation inference origin contradicts its original parent"
      in
      let%bind () =
        match invocation.context.parent_invocation with
        | None -> Ok ()
        | Some id ->
          List.find invocations ~f:(fun (parent : P.Invocation.t) ->
            P.Id.Invocation.equal id parent.context.id)
          |> Option.value_map ~default:(Ok ()) ~f:(fun parent ->
            compare_origin parent.inference_origin)
      in
      let%bind () =
        match invocation.context.parent_job with
        | None -> Ok ()
        | Some id ->
          List.find jobs ~f:(fun (job : P.Job.t) -> P.Id.Job.equal id job.id)
          |> Option.value_map ~default:(Ok ()) ~f:(fun job ->
            compare_origin job.inference_origin)
      in
      let%bind () =
        match invocation.parent_event with
        | None -> Ok ()
        | Some id ->
          (match
             List.find events ~f:(fun (event : P.Moderator_execution.t) ->
               P.Id.Moderator_execution.equal id event.context.id)
           with
           | None -> Ok ()
           | Some event ->
             let%bind () =
               match named with
               | None -> Ok ()
               | Some run ->
                 if P.Invocation.equal_observer event.context.source run.source.observer
                 then Ok ()
                 else invalid "invocation inference origin differs from its event source"
             in
             let%bind causal = Run_event_ancestry.find runs_list ~executing:event in
             (match causal with
              | Some run -> compare_origin (Some (P.Inference_origin.Run run.id))
              | None ->
                (* Missing historical Job classification cannot contradict the
                  child. A positively retained queued event takes precedence
                  over its physical drainer, including after terminalization. *)
                (match event.context.job with
                 | Some parent ->
                   List.find jobs ~f:(fun (job : P.Job.t) ->
                     P.Id.Job.equal job.id parent.job_id)
                   |> Option.value_map ~default:(Ok ()) ~f:(fun job ->
                     compare_origin job.inference_origin)
                 | None ->
                   let owners =
                     match event.context.operation_id with
                     | Some id -> operation_owners runs id
                     | None -> event_owners runs event.context.id
                   in
                   if List.is_empty owners
                   then Ok ()
                   else (
                     let%bind expected = origin_of_owners owners in
                     compare_origin (Some expected)))))
      in
      let owners =
        List.filter runs_list ~f:(fun run ->
          work_exists run ~f:(fun work ->
            match work.P.Run_work.key with
            | Retained (Invocation id) -> P.Id.Invocation.equal id invocation.context.id
            | Operation _ | Retained _ -> false))
      in
      if List.is_empty owners
      then Ok ()
      else (
        let%bind expected = origin_of_owners owners in
        compare_origin (Some expected)))
;;

let validate_transition ~previous ~incoming =
  List.fold_result incoming ~init:() ~f:(fun () (invocation : P.Invocation.t) ->
    match
      List.find previous ~f:(fun (old : P.Invocation.t) ->
        P.Id.Invocation.equal old.context.id invocation.context.id)
    with
    | None -> Ok ()
    | Some old ->
      if
        Option.equal
          P.Inference_origin.equal
          old.inference_origin
          invocation.inference_origin
      then Ok ()
      else invalid "invocation inference origin is immutable")
;;
