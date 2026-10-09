open! Core
module P = Agent_protocol
module W = P.Session_work

type t =
  { server_id : P.Id.Server.t
  ; principal : P.Principal.t
  }

let create ~server_id ~principal = { server_id; principal }

let job_status = function
  | P.Job.Queued -> W.Status.Accepted
  | Running -> Running
  | Waiting_permission _ -> Waiting_approval
  | Waiting_completion _ -> Waiting_work
  | Succeeded -> Succeeded
  | Failed _ -> Failed
  | Cancelled -> Cancelled
  | Interrupted _ -> Interrupted
;;

let job_delivery = function
  | P.Job.Not_required -> W.Delivery_state.Not_applicable
  | Pending -> Pending
  | Delivered _ -> Acknowledged
  | Discarded _ -> Discarded
;;

let schedule_status = function
  | P.Schedule.Scheduled -> W.Status.Accepted
  | Delivering -> Running
  | Delivered -> Succeeded
  | Cancelled -> Cancelled
  | Failed _ -> Failed
;;

let extension_state (value : P.Extension_status.t) =
  let open W in
  match value.kind, value.state with
  | Invocation, "admitted" -> Status.Accepted, Delivery_state.Not_applicable
  | Invocation, "dispatching" -> Running, Not_applicable
  | Invocation, "resolved.pending" -> Waiting_work, Pending
  | Invocation, "published.pending" -> Waiting_work, Acknowledged
  | Invocation, "resolved.complete" -> Succeeded, Pending
  | Invocation, "published.complete" -> Succeeded, Acknowledged
  | Invocation, "resolved.failed" -> Failed, Pending
  | Invocation, "published.failed" -> Failed, Acknowledged
  | Invocation, "resolved.cancelled" -> Cancelled, Pending
  | Invocation, "published.cancelled" -> Cancelled, Acknowledged
  | Subscription, "active" -> Running, Not_applicable
  | Subscription, "succeeded" -> Succeeded, Not_applicable
  | Subscription, "failed" -> Failed, Not_applicable
  | Subscription, "cancelled" -> Cancelled, Not_applicable
  | Subscription, "expired" -> Interrupted, Not_applicable
  | Delivery, "pending" -> Accepted, Pending
  | Delivery, "committed" -> Succeeded, Acknowledged
  | Delivery, "failed" -> Failed, Not_applicable
  | Moderator_execution, "running" -> Running, Not_applicable
  | Moderator_execution, ("failed" | "failed.retired") -> Failed, Not_applicable
  | Moderator_execution, ("interrupted" | "interrupted.retired") ->
    Interrupted, Not_applicable
  | Moderator_execution, "completed.pending" -> Succeeded, Pending
  | Moderator_execution, "completed.waiting_compaction" -> Waiting_work, Pending
  | Moderator_execution, "completed.discarded" -> Succeeded, Discarded
  | Moderator_execution, "completed.applied" -> Succeeded, Acknowledged
  | Moderator_execution, "completed" -> Succeeded, Not_applicable
  | (Invocation | Subscription | Delivery | Moderator_execution), _ ->
    Unsupported, Not_applicable
;;

let extension_key (value : P.Extension_status.t) =
  match value.kind with
  | Invocation ->
    P.Id.Invocation.of_string value.id |> Result.map ~f:(fun id -> W.Key.Invocation id)
  | Subscription ->
    P.Id.Subscription.of_string value.id
    |> Result.map ~f:(fun id -> W.Key.Subscription id)
  | Delivery ->
    P.Id.Delivery.of_string value.id |> Result.map ~f:(fun id -> W.Key.Delivery id)
  | Moderator_execution ->
    P.Id.Moderator_execution.of_string value.id
    |> Result.map ~f:(fun id -> W.Key.Moderator_execution id)
;;

let work t (snapshot : P.Snapshot.t) =
  let open Result.Let_syntax in
  let%bind () =
    if P.Principal.has_scope t.principal P.Scope.View_session_transcript
    then Ok ()
    else
      Error
        (P.Error.create
           Permission_denied
           ~message:"activity requires transcript visibility"
           ~retryable:false
           ())
  in
  let%bind () =
    if P.Principal.has_scope t.principal P.Scope.View_security_state
    then Ok ()
    else
      Error
        (P.Error.create
           Permission_denied
           ~message:"activity requires security visibility"
           ~retryable:false
           ())
  in
  let%bind () =
    if
      List.length snapshot.jobs
      + List.length snapshot.schedules
      + List.length snapshot.extension_status
      > 4096
    then Error (P.Error.invalid_request "work observation exceeds 4096 scanned records")
    else Ok ()
  in
  let session_id = snapshot.session.id in
  let generation = snapshot.session.generation in
  let session = P.Session_ref.create ~server_id:t.server_id ~session_id in
  let create key status delivery =
    W.create ~session ~generation ~key ~status ~delivery ~revision:snapshot.revision
  in
  let%bind jobs =
    List.filter_map snapshot.jobs ~f:(fun (job : P.Job.t) ->
      if Int.equal job.generation generation then Some job else None)
    |> List.map ~f:(fun (job : P.Job.t) ->
      if not (P.Id.Session.equal job.session_id session_id)
      then Error (P.Error.invalid_request "work job belongs to another session")
      else
        create
          (Job { id = job.id; attempt = job.attempt })
          (job_status job.status)
          (job_delivery job.delivery))
    |> Result.all
  in
  let%bind schedules =
    List.filter snapshot.schedules ~f:(fun (schedule : P.Schedule.t) ->
      Int.equal schedule.generation generation)
    |> List.map ~f:(fun (schedule : P.Schedule.t) ->
      if not (P.Id.Session.equal schedule.session_id session_id)
      then Error (P.Error.invalid_request "work schedule belongs to another session")
      else
        create
          (Schedule schedule.id)
          (schedule_status schedule.status)
          W.Delivery_state.Not_applicable)
    |> Result.all
  in
  let%bind extensions =
    List.filter snapshot.extension_status ~f:(fun (value : P.Extension_status.t) ->
      Int.equal value.generation generation)
    |> List.map ~f:(fun value ->
      let%bind key = extension_key value in
      let status, delivery = extension_state value in
      create key status delivery)
    |> Result.all
  in
  let rows = List.sort (jobs @ schedules @ extensions) ~compare:W.compare_key in
  if List.length rows > 4096
  then Error (P.Error.invalid_request "work observation exceeds 4096 retained records")
  else if List.contains_dup rows ~compare:W.compare_key
  then Error (P.Error.invalid_request "duplicate retained work identity")
  else Ok rows
;;
