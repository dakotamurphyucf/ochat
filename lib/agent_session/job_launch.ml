open Core
module P = Agent_protocol
module J = P.Job

let invalid message = Error (P.Error.invalid_request message)

let same_owner ~session_id ~generation actual_id actual_generation =
  match
    P.Id.Session.equal session_id actual_id && Int.equal generation actual_generation
  with
  | true -> Ok ()
  | false -> invalid "job launch ancestry crosses session or generation"
;;

let ancestor ~session_id ~generation ~invocations ~events owner =
  let open Result.Let_syntax in
  let event id =
    let%bind event =
      List.find events ~f:(fun event ->
        P.Id.Moderator_execution.equal event.P.Moderator_execution.context.id id)
      |> Result.of_option
           ~error:
             (P.Error.invalid_request "job launch references an unknown moderator event")
    in
    let%map () =
      same_owner ~session_id ~generation event.context.session_id event.context.generation
    in
    Option.map event.context.job ~f:(fun parent -> parent.job_id, Some parent.attempt)
  in
  let rec invocation seen id =
    let key = P.Id.Invocation.to_string id in
    match Set.mem seen key with
    | true -> invalid "job launch invocation ancestry contains a cycle"
    | false ->
      let seen = Set.add seen key in
      let%bind parent =
        List.find invocations ~f:(fun invocation ->
          P.Id.Invocation.equal invocation.P.Invocation.context.id id)
        |> Result.of_option
             ~error:
               (P.Error.invalid_request "job launch references an unknown invocation")
      in
      let c = parent.context in
      let%bind () = same_owner ~session_id ~generation c.session_id c.generation in
      (match c.parent_job, c.parent_invocation, parent.parent_event with
       | Some id, None, None -> Ok (Some (id, None))
       | None, Some id, None -> invocation seen id
       | None, None, Some id -> event id
       | None, None, None -> Ok None
       | _ -> invalid "job launch invocation has conflicting parents")
  in
  match owner with
  | J.Invocation id -> invocation String.Set.empty id
  | Moderator_event id -> event id
;;

let parent ~session_id ~generation ~jobs = function
  | None -> Ok None
  | Some (id, expected_attempt) ->
    let open Result.Let_syntax in
    let%bind job =
      List.find jobs ~f:(fun job -> P.Id.Job.equal job.J.id id)
      |> Result.of_option
           ~error:(P.Error.invalid_request "job launch references an unknown parent job")
    in
    let%bind () = same_owner ~session_id ~generation job.session_id job.generation in
    let%map () =
      match expected_attempt with
      | Some attempt when attempt <= 0 || attempt > job.attempt ->
        invalid "job launch references an unavailable parent attempt"
      | _ when job.attempt <= 0 -> invalid "job launch parent has never executed"
      | _ -> Ok ()
    in
    Some (job, expected_attempt)
;;

let depth = function
  | None -> Ok 0
  | Some (job, _) ->
    let inherited =
      Option.value_map job.J.launch ~default:0 ~f:(fun launch -> launch.nested_depth)
    in
    (match inherited >= 0 && inherited < Int.max_value with
     | true -> Ok (inherited + 1)
     | false -> invalid "job launch nesting depth is invalid or exhausted")
;;

let derive ~session_id ~generation ~invocations ~events ~jobs ~owner =
  let open Result.Let_syntax in
  let%bind ancestor = ancestor ~session_id ~generation ~invocations ~events owner in
  let%bind parent = parent ~session_id ~generation ~jobs ancestor in
  let%bind nested_depth = depth parent in
  let%map parent_job =
    match parent with
    | None -> Ok None
    | Some (job, Some attempt) when not (Int.equal job.attempt attempt) ->
      invalid "job launch cannot borrow an obsolete event attempt"
    | Some (job, _) -> Ok (Some (job.id, job.attempt))
  in
  J.{ owner; parent_job; nested_depth }
;;

let validate ~invocations ~events ~jobs (job : J.t) =
  let result =
    let open Result.Let_syntax in
    match job.launch with
    | None -> Ok ()
    | Some launch ->
      let%bind _ = J.of_json (J.to_json job) in
      let%bind ancestor =
        ancestor
          ~session_id:job.session_id
          ~generation:job.generation
          ~invocations
          ~events
          launch.owner
      in
      let%bind parent =
        parent ~session_id:job.session_id ~generation:job.generation ~jobs ancestor
      in
      let%bind expected_depth = depth parent in
      let%bind () =
        match Int.equal expected_depth launch.nested_depth with
        | true -> Ok ()
        | false -> invalid "job launch nesting depth differs from its ancestry"
      in
      (match parent, launch.parent_job with
       | None, None -> Ok ()
       | Some (parent, event_attempt), Some (id, attempt)
         when P.Id.Job.equal id parent.id
              && (not (P.Id.Job.equal job.id id))
              && attempt > 0
              && attempt <= parent.attempt
              && Option.value_map event_attempt ~default:true ~f:(Int.equal attempt) ->
         Ok ()
       | _ -> invalid "job launch parent attempt differs from its ancestry")
  in
  Result.map_error result ~f:(fun error ->
    P.Error.create Journal_corrupt ~message:error.message ~retryable:false ())
;;
