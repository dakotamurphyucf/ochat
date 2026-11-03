open! Core
module P = Agent_protocol

let invalid message = Error (P.Error.invalid_request message)

let validate ~jobs ~runs =
  let open Result.Let_syntax in
  let runs = Option.value_map runs ~default:[] ~f:Run_state.runs in
  List.fold_result jobs ~init:() ~f:(fun () (job : P.Job.t) ->
    let%bind () = P.Job.validate_inference_origin_transition ~previous:None job in
    match job.inference_origin with
    | None -> Ok ()
    | Some origin ->
      let owners =
        List.filter runs ~f:(fun (run : P.Run.t) ->
          let matches (work : P.Run_work.t) =
            Int.equal work.generation job.generation
            &&
            match work.key with
            | Retained (Job { id; attempt = _ }) -> P.Id.Job.equal id job.id
            | Operation _
            | Retained
                ( Schedule _
                | Invocation _
                | Subscription _
                | Delivery _
                | Moderator_execution _ ) -> false
          in
          List.exists run.owned_work ~f:matches
          || List.exists run.relinquished_work ~f:matches
          || List.exists run.terminal_work ~f:(fun terminal -> matches terminal.work))
      in
      (match origin, owners with
       | Unbound, [] -> Ok ()
       | Unbound, _ :: _ ->
         invalid "unbound Job inference origin contradicts retained Run custody"
       | Run id, [ owner ] ->
         if
           P.Id.Run.equal id owner.id
           && P.Id.Session.equal job.session_id (P.Session_ref.session_id owner.session)
           && Int.equal job.generation owner.source.generation
         then Ok ()
         else invalid "Job inference origin differs from its retained Run occurrence"
       | Run _, [] -> invalid "Job inference origin has no retained Run occurrence"
       | Run _, _ :: _ :: _ ->
         invalid "Job inference origin has conflicting retained Run custody"))
;;

let validate_transition ~previous ~incoming =
  List.fold_result incoming ~init:() ~f:(fun () (job : P.Job.t) ->
    let previous =
      List.find previous ~f:(fun (old : P.Job.t) -> P.Id.Job.equal old.id job.id)
    in
    P.Job.validate_inference_origin_transition ~previous job)
;;
