open Core
module P = Agent_protocol

type row =
  { key : string
  ; label : string
  ; status : string
  ; active : bool
  }
[@@deriving equal]

type t =
  { session_id : string
  ; generation : int
  ; active_jobs : int
  ; pending_completions : int
  ; rows : row array
  }
[@@deriving equal]

let job_active (job : P.Job.t) =
  match job.status with
  | Queued | Running | Waiting_permission _ | Waiting_completion _ -> true
  | Succeeded | Failed _ | Cancelled | Interrupted _ -> false
;;

let job_row (job : P.Job.t) =
  let kind =
    match job.kind with
    | Model_call -> "Model job"
    | Nested_agent -> "Agent job"
    | Scheduled_event -> "Scheduled job"
    | Async_tool -> "Tool job"
    | Shell_process -> "Shell job"
    | Compaction -> "Compaction job"
  in
  let state =
    match job.status with
    | Queued -> "Queued"
    | Running -> "Running"
    | Waiting_permission _ -> "Waiting for approval"
    | Waiting_completion _ -> "Waiting for dependent work"
    | Succeeded -> "Succeeded"
    | Failed _ -> "Failed"
    | Cancelled -> "Cancelled"
    | Interrupted _ -> "Interrupted; outcome may be uncertain"
  in
  let delivery =
    match job_active job, job.delivery with
    | true, _ | _, Not_required -> ""
    | false, Pending -> " · completion pending"
    | false, Delivered _ -> " · completion handed off"
    | false, Discarded { reason = Authority_changed; _ } ->
      " · completion discarded: authority changed"
  in
  let progress =
    match job.progress with
    | Some progress when job_active job ->
      Printf.sprintf " · progress update %d" progress.sequence
    | _ -> ""
  in
  { key = P.Id.Job.to_string job.id
  ; label = kind
  ; status = state ^ delivery ^ progress
  ; active = job_active job
  }
;;

let extension_row (value : P.Extension_status.t) =
  let label, status, active =
    match value.kind, value.state with
    | Invocation, "admitted" -> "Invocation", "Accepted", true
    | Invocation, "dispatching" -> "Invocation", "Executing", true
    | Invocation, "resolved.pending" ->
      "Invocation", "Background work accepted; acknowledgement pending", true
    | Invocation, "published.pending" ->
      "Invocation", "Background work acknowledged", false
    | Invocation, "resolved.complete" ->
      "Invocation", "Result ready; acknowledgement pending", true
    | Invocation, "published.complete" -> "Invocation", "Result acknowledged", false
    | Invocation, "resolved.failed" ->
      "Invocation", "Failed; acknowledgement pending", true
    | Invocation, "published.failed" -> "Invocation", "Failure acknowledged", false
    | Invocation, "resolved.cancelled" ->
      "Invocation", "Cancelled; acknowledgement pending", true
    | Invocation, "published.cancelled" ->
      "Invocation", "Cancellation acknowledged", false
    | Subscription, "active" -> "Subscription", "Waiting for completion", true
    | Subscription, "succeeded" -> "Subscription", "Succeeded", false
    | Subscription, "failed" -> "Subscription", "Failed", false
    | Subscription, "expired" -> "Subscription", "Expired", false
    | Subscription, "cancelled" -> "Subscription", "Cancelled", false
    | Delivery, "pending" -> "Notification", "Pending publication", true
    | Delivery, "committed" -> "Notification", "Recorded in conversation", false
    | Delivery, "failed" -> "Notification", "Publication failed", false
    | Moderator_execution, "running" -> "Moderator event", "Running", true
    | Moderator_execution, "completed.pending" ->
      "Moderator event", "Follow-up pending", true
    | Moderator_execution, "completed.waiting_compaction" ->
      "Moderator event", "Waiting for compaction", true
    | Moderator_execution, "completed" -> "Moderator event", "Completed", false
    | Moderator_execution, "completed.applied" ->
      "Moderator event", "Follow-up applied", false
    | Moderator_execution, "completed.discarded" ->
      "Moderator event", "Follow-up discarded", false
    | Moderator_execution, "failed" -> "Moderator event", "Failed", false
    | Moderator_execution, "failed.retired" -> "Moderator event", "Failed; retired", false
    | Moderator_execution, "interrupted" -> "Moderator event", "Interrupted", false
    | Moderator_execution, "interrupted.retired" ->
      "Moderator event", "Interrupted; retired", false
    | _ -> "Extension", "Unsupported status", false
  in
  { key = value.id; label; status; active }
;;

let schedule_row (schedule : P.Schedule.t) =
  let status, active =
    match schedule.status with
    | Scheduled -> "Scheduled", true
    | Delivering -> "Delivering", true
    | Delivered -> "Delivered", false
    | Cancelled -> "Cancelled", false
    | Failed _ -> "Failed", false
  in
  { key = P.Id.Schedule.to_string schedule.id; label = "Timer"; status; active }
;;

let of_snapshot (snapshot : P.Snapshot.t) =
  let generation = snapshot.session.generation in
  let jobs =
    List.filter snapshot.jobs ~f:(fun job ->
      Int.equal job.generation generation
      && P.Id.Session.equal job.session_id snapshot.session.id)
    |> List.sort ~compare:(fun a b ->
      match P.Timestamp.compare b.created_at a.created_at with
      | 0 -> P.Id.Job.compare a.id b.id
      | order -> order)
  in
  let extensions =
    List.filter snapshot.extension_status ~f:(fun status ->
      Int.equal status.generation generation)
  in
  let schedules =
    List.filter snapshot.schedules ~f:(fun schedule ->
      Int.equal schedule.generation generation
      && P.Id.Session.equal schedule.session_id snapshot.session.id)
  in
  let rows =
    List.map jobs ~f:job_row
    @ List.map extensions ~f:extension_row
    @ List.map schedules ~f:schedule_row
  in
  let rows =
    List.stable_sort rows ~compare:(fun a b -> Bool.compare b.active a.active)
    |> Array.of_list
  in
  { session_id = P.Id.Session.to_string snapshot.session.id
  ; generation
  ; active_jobs = List.count jobs ~f:job_active
  ; pending_completions =
      List.count jobs ~f:(fun job ->
        match job_active job, job.delivery with
        | false, Pending -> true
        | _ -> false)
  ; rows
  }
;;
