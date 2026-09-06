open! Core

type t =
  | Batch of t list
  | Created of Session_state.t
  | Lifecycle_changed of Session_state.Lifecycle.t
  | Workspace_changed of Workspace_instance.t
  | Canonical_entries_appended of Agent_protocol.History.entry list
  | Canonical_history_replaced of Agent_protocol.History.entry list
  | Initial_prompt_count_changed of int
  | Deferred_entries_enqueued of Agent_protocol.History.entry list
  | Deferred_entries_adopted
  | Active_operation_changed of Agent_protocol.Operation.t option
  | Attachment_added of Agent_protocol.Session.Attachment.t
  | Attachment_removed of Agent_protocol.Id.Attachment.t
  | Permission_changed of Agent_protocol.Permission.t
  | Grant_changed of Agent_protocol.Grant.t
  | Job_changed of Agent_protocol.Job.t
  | Schedule_changed of Agent_protocol.Schedule.t
  | Moderator_changed of Jsonaf.t option
  | Shell_changed of Session.Shell_state.t
  | History_block_reserved of int64
  | Compaction_generation_changed of int
  | Compaction_archived of Session_state.Compaction_archive.t
  | Owner_lease_generation_changed of int64
  | Failure_changed of Agent_protocol.Error.t option
  | Halt_changed of string option
  | Reset_generation of int
[@@deriving sexp]

let replace_by compare_id id value values ~id_of =
  value :: List.filter values ~f:(fun candidate -> compare_id (id_of candidate) id <> 0)
;;

let rec apply state = function
  | Batch deltas -> List.fold_result deltas ~init:state ~f:apply
  | Created created -> Ok created
  | Lifecycle_changed lifecycle -> Ok { state with lifecycle }
  | Workspace_changed workspace_instance ->
    let quota_key =
      Option.map state.spec.quota_key ~f:(fun quota_key ->
        { quota_key with Quota_key.conflict_domain = workspace_instance.conflict_domain })
    in
    Ok { state with spec = { state.spec with workspace_instance; quota_key } }
  | Canonical_entries_appended entries ->
    Ok
      { state with
        conversation =
          { state.conversation with
            canonical_history = state.conversation.canonical_history @ entries
          }
      }
  | Canonical_history_replaced canonical_history ->
    Ok { state with conversation = { state.conversation with canonical_history } }
  | Initial_prompt_count_changed initial_prompt_entry_count ->
    Ok
      { state with conversation = { state.conversation with initial_prompt_entry_count } }
  | Compaction_archived archive ->
    Ok
      { state with
        conversation =
          { state.conversation with
            compaction_archives = archive :: state.conversation.compaction_archives
          }
      }
  | Deferred_entries_enqueued entries ->
    Ok
      { state with
        conversation =
          { state.conversation with
            deferred_user_entries = state.conversation.deferred_user_entries @ entries
          }
      }
  | Deferred_entries_adopted ->
    Ok
      { state with
        conversation =
          { state.conversation with
            canonical_history =
              state.conversation.canonical_history
              @ state.conversation.deferred_user_entries
          ; deferred_user_entries = []
          }
      }
  | Active_operation_changed active_operation -> Ok { state with active_operation }
  | Attachment_added attachment ->
    Ok
      { state with
        attachments =
          replace_by
            Agent_protocol.Id.Attachment.compare
            attachment.id
            attachment
            state.attachments
            ~id_of:(fun value -> value.Agent_protocol.Session.Attachment.id)
      }
  | Attachment_removed attachment_id ->
    Ok
      { state with
        attachments =
          List.filter state.attachments ~f:(fun value ->
            Agent_protocol.Id.Attachment.compare value.id attachment_id <> 0)
      }
  | Permission_changed permission ->
    Ok
      { state with
        permissions =
          replace_by
            Agent_protocol.Id.Permission.compare
            permission.id
            permission
            state.permissions
            ~id_of:(fun value -> value.Agent_protocol.Permission.id)
      }
  | Grant_changed grant ->
    Ok
      { state with
        grants =
          replace_by
            Agent_protocol.Id.Grant.compare
            grant.id
            grant
            state.grants
            ~id_of:(fun value -> value.Agent_protocol.Grant.id)
      }
  | Job_changed job ->
    Ok
      { state with
        jobs =
          replace_by
            Agent_protocol.Id.Job.compare
            job.id
            job
            state.jobs
            ~id_of:(fun value -> value.Agent_protocol.Job.id)
      }
  | Schedule_changed schedule ->
    Ok
      { state with
        schedules =
          replace_by
            Agent_protocol.Id.Schedule.compare
            schedule.id
            schedule
            state.schedules
            ~id_of:(fun value -> value.Agent_protocol.Schedule.id)
      }
  | Moderator_changed moderator -> Ok { state with moderator }
  | Shell_changed shell -> Ok { state with shell }
  | History_block_reserved reserved_history_through ->
    if Int64.(reserved_history_through < state.conversation.reserved_history_through)
    then
      Error
        (Agent_protocol.Error.create
           Journal_corrupt
           ~message:"history reservation regressed"
           ~retryable:false
           ())
    else
      Ok
        { state with
          conversation =
            { state.conversation with
              next_history_sequence = reserved_history_through
            ; reserved_history_through
            }
        }
  | Compaction_generation_changed compaction_generation ->
    if compaction_generation <= state.conversation.compaction_generation
    then
      Error
        (Agent_protocol.Error.create
           Conflict
           ~message:"compaction generation did not advance"
           ~retryable:false
           ())
    else
      Ok { state with conversation = { state.conversation with compaction_generation } }
  | Owner_lease_generation_changed owner_lease_generation ->
    if Int64.(owner_lease_generation <= state.counters.owner_lease_generation)
    then
      Error
        (Agent_protocol.Error.create
           Lease_stale
           ~message:"owner lease generation did not advance"
           ~retryable:false
           ())
    else Ok { state with counters = { state.counters with owner_lease_generation } }
  | Failure_changed failure -> Ok { state with failure }
  | Halt_changed halt_reason ->
    Ok { state with halted = Option.is_some halt_reason; halt_reason }
  | Reset_generation generation ->
    if generation <= state.identity.generation
    then
      Error
        (Agent_protocol.Error.create
           Conflict
           ~message:"session generation did not advance"
           ~retryable:false
           ())
    else Ok { state with identity = { state.identity with generation } }
;;
