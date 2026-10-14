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
  if not (current_invocation t terminal && List.for_all selected ~f:(new_job t))
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
