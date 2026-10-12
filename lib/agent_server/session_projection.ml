open! Core

let metadata state =
  Agent_store.Session_store.Metadata.
    { schema_version = Agent_store.Session_store.current_metadata_schema_version
    ; session = Agent_session.Session_state.summary state
    ; prompt_artifact =
        Agent_protocol.Id.Prompt_revision.to_string state.spec.prompt_revision_id
    ; workspace_identity = state.spec.workspace_instance.conflict_domain
    ; data_schema_version = Agent_session.Session_state.current_schema_version
    }
;;

let runnable_job job =
  match job.Agent_protocol.Job.status with
  | Queued -> true
  | Running
  | Waiting_permission _
  | Waiting_completion _
  | Succeeded
  | Failed _
  | Cancelled
  | Interrupted _ -> false
;;

let deliverable_job job =
  match job.Agent_protocol.Job.delivery with
  | Pending -> true
  | Not_required | Delivered _ | Discarded _ -> false
;;

let active_schedule schedule =
  match schedule.Agent_protocol.Schedule.status with
  | Scheduled | Delivering -> true
  | Delivered | Cancelled | Failed _ -> false
;;

let owner_grace_deadline state =
  List.find_map state.Agent_session.Session_state.attachments ~f:(fun attachment ->
    match attachment.Agent_protocol.Session.Attachment.mode, attachment.owner_lease with
    | Owner_read_write, Some lease -> lease.disconnect_grace_until
    | (Owner_read_write | Read_write | Read_only), _ -> None)
;;

let index_entry state =
  let earliest_schedule_due =
    state.Agent_session.Session_state.schedules
    |> List.filter ~f:active_schedule
    |> List.map ~f:(fun schedule -> schedule.Agent_protocol.Schedule.next_due_at)
    |> List.min_elt ~compare:Agent_protocol.Timestamp.compare
  in
  Agent_store.Session_index.Entry.
    { session = Agent_session.Session_state.summary state
    ; runnable_job_count = List.count state.jobs ~f:runnable_job
    ; deliverable_job_count = List.count state.jobs ~f:deliverable_job
    ; earliest_schedule_due
    ; owner_grace_deadline = owner_grace_deadline state
    ; pending_initial_start = state.pending_initial_start
    ; archived = false
    ; lifecycle_revision = Agent_store.Session_archive_record.Revision.zero
    ; admission = Automatic
    }
;;
