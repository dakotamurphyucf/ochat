open! Core
module P = Agent_protocol
module Doc = Developer_run_document

module Error = struct
  type t =
    | Conflict
    | Limit_exceeded
    | Store of Store_error.t
end

type t =
  { owner : P.Execution_owner.developer
  ; capture_fingerprint : Chatml_source.Digest.t
  ; max_jobs : int
  ; max_invocations : int
  ; max_selected : int
  ; max_job_payload_bytes : int
  ; limits : Document_schema.Limits.t
  ; poll : unit -> unit
  }

let create
      ~owner
      ~capture_fingerprint
      ~max_jobs
      ~max_invocations
      ~max_selected
      ~max_job_payload_bytes
      ~limits
      ~poll
  =
  { owner
  ; capture_fingerprint
  ; max_jobs
  ; max_invocations
  ; max_selected
  ; max_job_payload_bytes
  ; limits
  ; poll
  }
;;

let ( let* ) value f = Result.bind value ~f
let store value = Result.map_error value ~f:(fun error -> Error.Store error)

let current_invocation t invocation =
  match (P.Host_invocation.context invocation).owner with
  | P.Execution_owner.Conversation _ -> false
  | Developer owner -> P.Execution_owner.equal_developer owner t.owner
;;

let dispatch t document ~observed ~dispatched =
  let context = P.Host_invocation.context dispatched in
  if
    (not (current_invocation t dispatched))
    || not (P.Invocation.equal_status (P.Host_invocation.status dispatched) Dispatching)
  then Error Error.Conflict
  else (
    let prior = Doc.find_invocation document context.id in
    let* () =
      if Option.is_none prior && Doc.invocation_count document >= t.max_invocations
      then Error Error.Limit_exceeded
      else Ok ()
    in
    let* expected_revision =
      match prior, observed with
      | None, None -> Ok None
      | Some prior, Some (revision, value)
        when Int.equal (Doc.Invocation_entry.revision prior) revision
             && P.Host_invocation.equal_context
                  (P.Host_invocation.context (Doc.Invocation_entry.value prior))
                  context
             && P.Host_invocation.equal_context (P.Host_invocation.context value) context
             && P.Invocation.equal_status
                  (P.Host_invocation.status (Doc.Invocation_entry.value prior))
                  Admitted -> Ok (Some revision)
      | None, Some _ | Some _, None | Some _, Some _ -> Error Error.Conflict
    in
    Doc.set_invocation
      document
      ~poll:t.poll
      ~id:context.id
      ~expected_revision
      ~after:dispatched
      ~max_invocations:t.max_invocations
    |> store)
;;

let new_job t job =
  let state = P.Developer_job.state job in
  P.Execution_owner.equal_developer (P.Developer_job.owner job) t.owner
  && Chatml_source.Digest.equal
       (P.Developer_job.capture_fingerprint job)
       t.capture_fingerprint
  && (match state.status with
      | P.Job.Queued -> true
      | Running
      | Waiting_permission _
      | Waiting_completion _
      | Succeeded
      | Failed _
      | Cancelled
      | Interrupted _ -> false)
  && Int.equal state.attempt 0
;;

let within_job_limits t document selected =
  let count = List.length selected in
  if count > t.max_selected || count > t.max_jobs - Doc.job_count document
  then Error Error.Limit_exceeded
  else Ok ()
;;

let append_selected t document selected =
  Doc.append_selected
    document
    ~poll:t.poll
    ~limits:t.limits
    ~selected
    ~max_jobs:t.max_jobs
    ~max_selected:t.max_selected
    ~max_job_payload_bytes:t.max_job_payload_bytes
  |> store
;;

let root_selected t document ~phase ~selected =
  if
    not
      (List.for_all selected ~f:(fun job ->
         new_job t job
         && (not (P.Job.equal_kind (P.Developer_job.kind job) P.Job.Model_call))
         &&
         match P.Developer_job.Launch.parent (P.Developer_job.launch job) with
         | Run_root ->
           Option.is_none (P.Developer_job.Launch.parent_job (P.Developer_job.launch job))
         | Invocation _ -> false))
  then Error Error.Conflict
  else
    let* () = within_job_limits t document selected in
    let* next = Doc.set_phase document ~epoch:t.owner.epoch ~phase |> store in
    if List.is_empty selected then Ok next else append_selected t next selected
;;

let terminal_from_dispatch prior terminal =
  let* outcome =
    match P.Host_invocation.status terminal with
    | P.Invocation.Resolved outcome -> Ok outcome
    | Admitted | Dispatching | Published _ -> Error Error.Conflict
  in
  let* expected =
    P.Host_invocation.resolve prior outcome
    |> Result.map_error ~f:(fun _ -> Error.Conflict)
  in
  let* canonical =
    P.Host_invocation.developer_to_json expected
    |> Result.map_error ~f:(fun _ -> Error.Conflict)
  in
  let* actual =
    P.Host_invocation.developer_to_json terminal
    |> Result.map_error ~f:(fun _ -> Error.Conflict)
  in
  Ok (Jsonaf.exactly_equal canonical actual)
;;

let terminal_selected t document ~observed ~terminal ~selected =
  let context = P.Host_invocation.context terminal in
  if
    not
      (current_invocation t terminal
       && List.for_all selected ~f:(fun job ->
         new_job t job
         && not (P.Job.equal_kind (P.Developer_job.kind job) P.Job.Model_call)))
  then Error Error.Conflict
  else
    let* () = within_job_limits t document selected in
    let* prior =
      match Doc.find_invocation document context.id, observed with
      | Some prior, Some (revision, value)
        when Int.equal (Doc.Invocation_entry.revision prior) revision
             && P.Host_invocation.equal_context
                  (P.Host_invocation.context (Doc.Invocation_entry.value prior))
                  (P.Host_invocation.context value)
             && P.Invocation.equal_status
                  (P.Host_invocation.status (Doc.Invocation_entry.value prior))
                  Dispatching -> Ok prior
      | None, _ | Some _, None | Some _, Some _ -> Error Error.Conflict
    in
    let* same_terminal =
      terminal_from_dispatch (Doc.Invocation_entry.value prior) terminal
    in
    let* () = if same_terminal then Ok () else Error Error.Conflict in
    let* () =
      if
        List.for_all selected ~f:(fun job ->
          let launch = P.Developer_job.launch job in
          match P.Developer_job.Launch.parent launch with
          | Run_root -> false
          | Invocation parent ->
            P.Id.Invocation.equal parent context.id
            &&
              (match P.Developer_job.Launch.parent_job launch, context.parent_job with
              | None, None -> true
              | Some (job_id, attempt), Some parent_job ->
                P.Id.Job.equal job_id parent_job
                && Option.value_map
                     (Doc.find_job document job_id)
                     ~default:false
                     ~f:(fun claimed ->
                       let state = P.Developer_job.state claimed in
                       Int.equal state.attempt attempt
                       &&
                       match state.status with
                       | P.Job.Running -> true
                       | Queued
                       | Waiting_permission _
                       | Waiting_completion _
                       | Succeeded
                       | Failed _
                       | Cancelled
                       | Interrupted _ -> false)
              | None, Some _ | Some _, None -> false))
      then Ok ()
      else Error Error.Conflict
    in
    let* next =
      Doc.set_invocation
        document
        ~poll:t.poll
        ~id:context.id
        ~expected_revision:(Some (Doc.Invocation_entry.revision prior))
        ~after:terminal
        ~max_invocations:t.max_invocations
      |> store
    in
    if List.is_empty selected then Ok next else append_selected t next selected
;;

module External_model_parent = struct
  type t =
    | Run_root
    | Dispatching of int * P.Host_invocation.t
    | Claimed_model of P.Developer_job.t
end

let external_model_job_parent_matches
      document
      launch
      (context : P.Host_invocation.context)
      ~(owner : P.Execution_owner.developer)
  =
  let same_owner =
    match context.owner with
    | P.Execution_owner.Conversation _ -> false
    | Developer parent -> P.Execution_owner.equal_developer parent owner
  in
  same_owner
  &&
  match P.Developer_job.Launch.parent_job launch, context.parent_job with
  | None, None -> true
  | Some (id, attempt), Some parent_id when P.Id.Job.equal id parent_id ->
    Option.value_map (Doc.find_job document id) ~default:false ~f:(fun prior ->
      P.Execution_owner.equal_developer (P.Developer_job.owner prior) owner
      && attempt > 0
      && attempt <= (P.Developer_job.state prior).attempt)
  | None, Some _ | Some _, None | Some _, Some _ -> false
;;

let external_model t document ~parent ~job =
  let launch = P.Developer_job.launch job in
  if
    (not (P.Job.equal_kind (P.Developer_job.kind job) P.Job.Model_call))
    || (not (new_job t job))
    || (not (Doc.valid_external_model_launch_shape launch))
    || Option.is_some (Doc.find_job document (P.Developer_job.id job))
  then Error Error.Conflict
  else
    let open Result.Let_syntax in
    let%bind () =
      match parent, P.Developer_job.Launch.parent launch with
      | External_model_parent.Run_root, Run_root ->
        if
          Option.is_none (P.Developer_job.Launch.parent_job launch)
          && Int.equal (P.Developer_job.Launch.nested_depth launch) 0
        then Ok ()
        else Error Error.Conflict
      | External_model_parent.Claimed_model observed, Run_root ->
        let parent_launch = P.Developer_job.launch observed in
        let parent_id = P.Developer_job.id observed in
        let parent_depth = P.Developer_job.Launch.nested_depth parent_launch in
        (match Doc.find_job document parent_id with
         | Some current ->
           if
             P.Developer_job.same_immutable current observed
             && Int.equal
                  (P.Developer_job.state current).attempt
                  (P.Developer_job.state observed).attempt
             && (match (P.Developer_job.state current).status with
                 | Running -> true
                 | Queued
                 | Waiting_permission _
                 | Waiting_completion _
                 | Succeeded
                 | Failed _
                 | Cancelled
                 | Interrupted _ -> false)
             && P.Job.equal_kind (P.Developer_job.kind current) Model_call
             && P.Execution_owner.equal_developer (P.Developer_job.owner current) t.owner
             && Chatml_source.Digest.equal
                  (P.Developer_job.capture_fingerprint current)
                  (P.Developer_job.capture_fingerprint job)
             && parent_depth < Int.max_value
             && Int.equal (P.Developer_job.Launch.nested_depth launch) (parent_depth + 1)
             &&
             match P.Developer_job.Launch.parent_job launch with
             | Some (id, attempt) ->
               P.Id.Job.equal id parent_id
               && Int.equal attempt (P.Developer_job.state current).attempt
             | None -> false
           then Ok ()
           else Error Error.Conflict
         | None -> Error Error.Conflict)
      | External_model_parent.Dispatching (revision, observed), Invocation id ->
        (match Doc.find_invocation document id with
         | Some current
           when Int.equal (Doc.Invocation_entry.revision current) revision
                && P.Host_invocation.equal_context
                     (P.Host_invocation.context (Doc.Invocation_entry.value current))
                     (P.Host_invocation.context observed)
                && P.Id.Invocation.equal (P.Host_invocation.context observed).id id
                && P.Invocation.equal_status
                     (P.Host_invocation.status (Doc.Invocation_entry.value current))
                     Dispatching
                && P.Invocation.equal_status
                     (P.Host_invocation.status observed)
                     Dispatching
                && external_model_job_parent_matches
                     document
                     launch
                     (P.Host_invocation.context observed)
                     ~owner:t.owner -> Ok ()
         | None | Some _ -> Error Error.Conflict)
      | External_model_parent.Run_root, Invocation _
      | External_model_parent.Dispatching _, Run_root
      | External_model_parent.Claimed_model _, Invocation _ -> Error Error.Conflict
    in
    let%bind () =
      match
        P.Developer_model_call.of_json (P.Developer_job.payload job) ~limits:t.limits
      with
      | Ok _ -> Ok ()
      | Error { code = P.Protocol_error.Resource_limit; _ } -> Error Error.Limit_exceeded
      | Error _ -> Error Error.Conflict
    in
    let%bind () =
      if Doc.job_count document >= t.max_jobs then Error Error.Limit_exceeded else Ok ()
    in
    let%bind payload_bytes =
      List.fold_result (Doc.jobs document) ~init:0 ~f:(fun accumulated prior ->
        t.poll ();
        let%bind length =
          Document_schema.Json.validate_and_measure
            ~limits:t.limits
            (P.Developer_job.payload prior)
          |> Document_fields.store
          |> store
        in
        if length > t.max_job_payload_bytes - accumulated
        then Error (Error.Store (Store_error.Corrupt "admitted payload exceeds limit"))
        else Ok (accumulated + length))
    in
    let%bind incoming_bytes =
      Document_schema.Json.validate_and_measure
        ~limits:t.limits
        (P.Developer_job.payload job)
      |> Result.map_error ~f:(function
        | Document_schema.Error.Limit_exceeded _ -> Error.Limit_exceeded
        | _ -> Error.Conflict)
    in
    let%bind () =
      if incoming_bytes > t.max_job_payload_bytes - payload_bytes
      then Error Error.Limit_exceeded
      else Ok ()
    in
    Doc.append_selected
      document
      ~poll:t.poll
      ~limits:t.limits
      ~selected:[ job ]
      ~max_jobs:t.max_jobs
      ~max_selected:1
      ~max_job_payload_bytes:t.max_job_payload_bytes
    |> store
;;

let claim t document ~observed ~now =
  let id = P.Developer_job.id observed in
  match Doc.find_job document id with
  | None -> Error Error.Conflict
  | Some prior when not (P.Developer_job.same_immutable prior observed) ->
    Error Error.Conflict
  | Some prior ->
    let state = P.Developer_job.state prior in
    if
      (not (Int.equal state.attempt (P.Developer_job.state observed).attempt))
      || state.attempt = Int.max_value
      || (not (P.Execution_owner.equal_developer (P.Developer_job.owner prior) t.owner))
      ||
      match state.status with
      | P.Job.Queued -> false
      | Running
      | Waiting_permission _
      | Waiting_completion _
      | Succeeded
      | Failed _
      | Cancelled
      | Interrupted _ -> true
    then Error Error.Conflict
    else if
      Option.value_map state.next_run_at ~default:false ~f:(fun due ->
        P.Timestamp.compare now due < 0)
    then Error Error.Conflict
    else (
      let after : P.Developer_job.State.t =
        { status = P.Job.Running
        ; attempt = state.attempt + 1
        ; started_at = Some now
        ; next_run_at = None
        ; completed_at = None
        ; result = None
        ; retry_failure = None
        }
      in
      let* _ =
        P.Developer_job.with_state prior after
        |> Result.map_error ~f:(fun _ -> Error.Conflict)
      in
      Doc.set_job_state document ~poll:t.poll ~id ~expected_attempt:state.attempt ~after
      |> store)
;;

let cancel_inactive t document ~observed ~now =
  let id = P.Developer_job.id observed in
  match Doc.find_job document id with
  | None -> Error Error.Conflict
  | Some prior when not (P.Developer_job.same_immutable prior observed) ->
    Error Error.Conflict
  | Some prior ->
    let before = P.Developer_job.state prior in
    let issued = P.Developer_job.state observed in
    if
      (not (P.Execution_owner.equal_developer (P.Developer_job.owner prior) t.owner))
      || (not (Int.equal before.attempt issued.attempt))
      ||
      match before.status with
      | P.Job.Queued | Waiting_completion _ -> false
      | Running | Waiting_permission _ | Succeeded | Failed _ | Cancelled | Interrupted _
        -> true
    then Error Error.Conflict
    else (
      let after : P.Developer_job.State.t =
        { before with
          status = P.Job.Cancelled
        ; next_run_at = None
        ; completed_at = Some now
        ; result = None
        ; retry_failure = None
        }
      in
      let* _ =
        P.Developer_job.with_state prior after
        |> Result.map_error ~f:(fun _ -> Error.Conflict)
      in
      Doc.set_job_state document ~poll:t.poll ~id ~expected_attempt:before.attempt ~after
      |> store)
;;

let terminal_status = function
  | P.Job.Succeeded | Failed _ | Cancelled | Interrupted _ -> true
  | Queued | Running | Waiting_permission _ | Waiting_completion _ -> false
;;

let legal_claimed_transition ~current_epoch before after =
  let old_status = before.P.Developer_job.State.status in
  let new_status = after.P.Developer_job.State.status in
  if not current_epoch
  then (
    match old_status with
    | P.Job.Running | Waiting_permission _ -> terminal_status new_status
    | Queued | Waiting_completion _ | Succeeded | Failed _ | Cancelled | Interrupted _ ->
      false)
  else (
    match old_status, new_status with
    | Running, Waiting_permission _ | Waiting_permission _, Running -> true
    | (Running | Waiting_permission _), (Queued | Waiting_completion _) -> true
    | (Running | Waiting_permission _), (Succeeded | Failed _ | Cancelled | Interrupted _)
      -> true
    | ( (Queued | Waiting_completion _ | Succeeded | Failed _ | Cancelled | Interrupted _)
      , _ ) -> false
    | Running, Running | Waiting_permission _, Waiting_permission _ -> false)
;;

let claimed_transition
      t
      document
      ~claimed
      ~(after : P.Developer_job.State.t)
      ~current_epoch
  =
  let id = P.Developer_job.id claimed in
  match Doc.find_job document id with
  | None -> Error Error.Conflict
  | Some prior when not (P.Developer_job.same_immutable prior claimed) ->
    Error Error.Conflict
  | Some prior ->
    let before = P.Developer_job.state prior in
    let issued = P.Developer_job.state claimed in
    if
      (not (P.Execution_owner.equal_developer (P.Developer_job.owner prior) t.owner))
      || (not (Int.equal before.attempt issued.attempt))
      || (not (Int.equal after.attempt issued.attempt))
      || (not (legal_claimed_transition ~current_epoch before after))
      || (not
            (Option.equal
               (fun left right -> Int.equal (P.Timestamp.compare left right) 0)
               before.started_at
               issued.started_at))
      ||
      match after.status with
      | P.Job.Queued -> Option.is_some after.started_at
      | Running
      | Waiting_permission _
      | Waiting_completion _
      | Succeeded
      | Failed _
      | Cancelled
      | Interrupted _ ->
        not
          (Option.equal
             (fun left right -> Int.equal (P.Timestamp.compare left right) 0)
             after.started_at
             issued.started_at)
    then Error Error.Conflict
    else
      let* _ =
        P.Developer_job.with_state prior after
        |> Result.map_error ~f:(fun _ -> Error.Conflict)
      in
      Doc.set_job_state document ~poll:t.poll ~id ~expected_attempt:before.attempt ~after
      |> store
;;

let claimed_selected t document ~claimed ~after ~selected ~current_epoch =
  let* () =
    if List.is_empty selected
    then Ok ()
    else (
      match after.P.Developer_job.State.status with
      | P.Job.Waiting_completion _ | Succeeded | Failed _ | Cancelled | Interrupted _ ->
        Ok ()
      | Queued | Running | Waiting_permission _ -> Error Error.Conflict)
  in
  let* next = claimed_transition t document ~claimed ~after ~current_epoch in
  if (not current_epoch) || List.is_empty selected
  then Ok next
  else (
    let parent_launch = P.Developer_job.launch claimed in
    let parent_depth = P.Developer_job.Launch.nested_depth parent_launch in
    let parent_id = P.Developer_job.id claimed in
    let parent_attempt = (P.Developer_job.state claimed).attempt in
    if
      Int.equal parent_depth Int.max_value
      || not
           (List.for_all selected ~f:(fun job ->
              let launch = P.Developer_job.launch job in
              new_job t job
              && P.Job.equal_kind (P.Developer_job.kind job) P.Job.Async_tool
              && Int.equal (P.Developer_job.Launch.nested_depth launch) (parent_depth + 1)
              &&
              match P.Developer_job.Launch.parent launch with
              | P.Developer_job.Launch.Invocation _ -> false
              | Run_root ->
                (match P.Developer_job.Launch.parent_job launch with
                 | Some (id, attempt) ->
                   P.Id.Job.equal id parent_id && Int.equal attempt parent_attempt
                 | None -> false)))
    then Error Error.Conflict
    else
      let* () = within_job_limits t next selected in
      append_selected t next selected)
;;
