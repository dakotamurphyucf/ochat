open! Core
module P = Agent_protocol

type t =
  { goal_id : P.Id.Goal.t
  ; episode : P.Goal_episode.t
  }

let goal_id t = t.goal_id
let episode t = t.episode
let invalid message = Error (P.Error.invalid_request message)

let of_run (state : Session_state.t) (run : P.Run.t) =
  let open Result.Let_syntax in
  match
    Option.bind state.run_state ~f:(fun index -> Run_state.goal_link index run.id)
  with
  | None -> Ok None
  | Some link ->
    let goal_id = Run_goal_link.goal_id link in
    let episode = Run_goal_link.episode link in
    let%bind goal =
      Option.bind state.goal_catalog ~f:(fun catalog -> Goal_catalog.find catalog goal_id)
      |> Result.of_option ~error:(P.Error.invalid_request "associated goal is absent")
    in
    if
      Option.equal P.Goal_episode.equal (P.Goal.episode goal) (Some episode)
      && P.Run_source.equal (P.Goal.source goal) run.source
      && P.Session_ref.equal (P.Goal.session goal) run.session
    then Ok (Some { goal_id; episode })
    else invalid "work belongs to an obsolete goal episode or source"
;;

let run_of_work (state : Session_state.t) work =
  let owners =
    Option.value_map state.run_state ~default:[] ~f:Run_state.runs
    |> List.filter ~f:(fun run ->
      List.mem (run.P.Run.owned_work @ run.relinquished_work) work ~equal:P.Run_work.equal
      || List.exists run.terminal_work ~f:(fun terminal ->
        P.Run_work.equal terminal.work work))
  in
  match owners with
  | [] -> Ok None
  | [ run ] -> Ok (Some run)
  | _ :: _ :: _ -> invalid "work has conflicting retained Run origins"
;;

let of_work state work =
  let%bind.Result run = run_of_work state work in
  match run with
  | None -> Ok None
  | Some run -> of_run state run
;;

let run_of_job (state : Session_state.t) (job : P.Job.t) =
  let open Result.Let_syntax in
  let%bind () = Job_inference_provenance.validate ~jobs:[ job ] ~runs:state.run_state in
  match job.inference_origin with
  | None -> invalid "Job inference origin is unavailable"
  | Some Unbound -> Ok None
  | Some (Run id) ->
    Option.bind state.run_state ~f:(fun index -> Run_state.find index id)
    |> Result.of_option ~error:(P.Error.invalid_request "Job inference Run is absent")
    |> Result.map ~f:Option.some
;;

let of_job state job =
  let%bind.Result run = run_of_job state job in
  match run with
  | None -> Ok None
  | Some run -> of_run state run
;;

let run_of_invocation (state : Session_state.t) (invocation : P.Invocation.t) =
  let open Result.Let_syntax in
  let%bind origin = Invocation_inference_provenance.of_invocation invocation in
  match origin with
  | Unbound -> Ok None
  | Run id ->
    Option.bind state.run_state ~f:(fun index -> Run_state.find index id)
    |> Result.of_option
         ~error:(P.Error.invalid_request "invocation inference Run is absent")
    |> Result.map ~f:Option.some
;;

module Retained_parent = struct
  type t =
    { run : P.Run.t
    ; work : P.Run_work.t
    }

  let error code message = P.Error.create code ~message ~retryable:false ()

  let of_work state work =
    let%map.Result owner = run_of_work state work in
    Option.map owner ~f:(fun run -> { run; work })
  ;;

  let of_job (state : Session_state.t) (job : P.Job.t) =
    let open Result.Let_syntax in
    let matches (work : P.Run_work.t) =
      Int.equal work.generation job.generation
      &&
      match work.key with
      | Retained (Job { id; attempt = _ }) -> P.Id.Job.equal id job.id
      | Operation _ | Retained _ -> false
    in
    let owners =
      Option.value_map state.run_state ~default:[] ~f:Run_state.runs
      |> List.filter ~f:(fun run ->
        List.exists run.P.Run.owned_work ~f:matches
        || List.exists run.relinquished_work ~f:matches
        || List.exists run.terminal_work ~f:(fun terminal -> matches terminal.work))
    in
    match owners with
    | [] -> Ok None
    | [ _ ] ->
      let%bind work = Run_job_occurrence.work job in
      let%bind parent = of_work state work in
      (match parent with
       | Some _ -> Ok parent
       | None ->
         Error (error Permission_denied "native Job has no current Run occurrence"))
    | _ :: _ :: _ -> Error (error Invalid_state "native Job has conflicting Run parents")
  ;;

  let of_event (state : Session_state.t) (event : P.Moderator_execution.t) =
    let open Result.Let_syntax in
    let%bind work =
      P.Run_work.create
        ~key:(Retained (Moderator_execution event.context.id))
        ~generation:event.context.generation
    in
    let%bind parent = of_work state work in
    match parent, event.context.job with
    | Some _, _ | None, None -> Ok parent
    | None, Some attempt ->
      (match
         List.find state.jobs ~f:(fun job -> P.Id.Job.equal job.id attempt.job_id)
       with
       | Some job
         when Int.equal job.generation event.context.generation
              && Int.equal job.attempt attempt.attempt -> of_job state job
       | None | Some _ ->
         Error (error Conflict "native event parent attempt is unavailable"))
  ;;

  let of_invocation (state : Session_state.t) (invocation : P.Invocation.t) =
    let open Result.Let_syntax in
    let job id =
      match List.find state.jobs ~f:(fun job -> P.Id.Job.equal job.id id) with
      | Some job -> of_job state job
      | None -> Error (error Conflict "native child parent Job is unavailable")
    in
    let rec lookup remaining (invocation : P.Invocation.t) =
      if remaining = 0
      then Error (error Invalid_state "native child ancestry is cyclic")
      else (
        let%bind work =
          P.Run_work.create
            ~key:(Retained (Invocation invocation.context.id))
            ~generation:invocation.context.generation
        in
        let%bind parent = of_work state work in
        match parent with
        | Some _ -> Ok parent
        | None ->
          (match invocation.context.parent_invocation, invocation.context.parent_job with
           | Some id, _ ->
             (match
                List.find state.invocations ~f:(fun parent ->
                  P.Id.Invocation.equal parent.context.id id)
              with
              | Some parent -> lookup (remaining - 1) parent
              | None ->
                Error (error Conflict "native child parent invocation is unavailable"))
           | None, Some id -> job id
           | None, None ->
             (match invocation.parent_event with
              | None -> Ok None
              | Some id ->
                (match
                   List.find state.moderator_executions ~f:(fun event ->
                     P.Id.Moderator_execution.equal event.context.id id)
                 with
                 | Some event -> of_event state event
                 | None -> Error (error Conflict "native parent event is unavailable")))))
    in
    lookup (List.length state.invocations + 1) invocation
  ;;
end
