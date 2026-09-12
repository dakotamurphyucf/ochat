open! Core

type persistence =
  { commit :
      command_audit:string option
      -> previous:Session_state.t
      -> Session_transition.t
      -> (unit, Agent_protocol.Error.t) result
  }

type services =
  { now : unit -> Agent_protocol.Timestamp.t
  ; monotonic_now : unit -> Mtime.t
  ; create_attachment_id : unit -> Agent_protocol.Id.Attachment.t
  ; create_reclaim_token : unit -> string
  ; state_committed : Session_state.t -> Agent_protocol.Event.Durable.t list -> unit
  ; job_results : Agent_store.Job_result_store.Publisher.t option
  ; subscription_limits : Staged_subscriptions.limits
  ; schedule_limits : Staged_schedules.limits
  ; notification_limits : Staged_notifications.limits
  ; ingress_limits : Staged_ingress.limits
  }

type submission =
  { session : Agent_protocol.Session.t
  ; history_id : Agent_protocol.History.Id.t
  ; disposition : Agent_protocol.Method_result.Send_message.disposition
  ; operation_id : Agent_protocol.Id.Operation.t option
  }

type reset_options = Administration.reset_options =
  { keep_history : bool
  ; keep_tasks : bool
  ; keep_grants : bool
  ; keep_labels : bool
  ; workspace_instance : Workspace_instance.t option
  }

module Extension_change = struct
  type t =
    | Invocation of Agent_protocol.Invocation.t
    | Subscription of Agent_protocol.Subscription.t
    | Delivery of Agent_protocol.Delivery.t
    | Publish of Agent_protocol.Delivery.t * Agent_protocol.History.entry
    | Start_job of Agent_protocol.Job.t
    | Schedule of Agent_protocol.Schedule.t
    | Moderator_state of Jsonaf.t option
end

type job_scope =
  { job : Agent_protocol.Job.t
  ; progress : Job_progress_buffer.t
  ; deadline : Agent_protocol.Timestamp.t option
  ; cancelled : unit Eio.Promise.t
  ; cancel_signal : unit Eio.Promise.u
  ; mutable active : bool
  ; mutable cancel_requested : bool
  ; mutable unfinished_on_return : bool
  ; mutable cancel : (unit -> unit) option
  }

type moderator_borrow_kind =
  | Invocation
  | Observation

type moderator_borrow =
  { operation_id : Agent_protocol.Id.Operation.t option
  ; job_scope : job_scope option
  ; kind : moderator_borrow_kind
  ; invocation : Agent_protocol.Invocation.t
  ; mutable committed : bool
  ; mutable accepts_children : bool
  ; mutable callback_finished : bool
  ; mutable cancel : (unit -> unit) option
  ; mutable cancel_requested : bool
  }

type event_borrow_kind =
  | Queued
  | Ordinary

type queued_event_borrow =
  { kind : event_borrow_kind
  ; job_scope : job_scope option
  ; receipt : Agent_protocol.Moderator_execution.t
  ; before : Session.Moderator_state.Identity_snapshot.t
  ; event : Session.Snapshot.t
  ; retirement_reason : string option
  ; mutable callback_active : bool
  ; mutable committed : bool
  ; mutable cancel : (unit -> unit) option
  ; mutable cancel_requested : bool
  }

type invocation_owner =
  | Foreground of Agent_protocol.Id.Operation.t
  | Invocation_moderator of moderator_borrow
  | Event_moderator of queued_event_borrow
  | Background_job of job_scope

type delegated_event_claim =
  | Delegated_unavailable
  | Delegated_borrow of queued_event_borrow
  | Delegated_replay of Agent_protocol.Moderator_execution.t

type invocation_execution =
  { owner : invocation_owner
  ; dispatched : Agent_protocol.Invocation.t
  ; mutable accepts_children : bool
  }

type _ request =
  | Claim_delegated_event :
      Agent_protocol.Id.Moderator_execution.t
      * Agent_protocol.Moderator_execution.delegation
      * Session.Moderator_state.Identity_snapshot.t
      * Chat_response.Moderation.Event.t
      -> delegated_event_claim request
  | Claim_job_moderator :
      job_scope * Agent_protocol.Invocation.t
      -> moderator_borrow request
  | Claim_job_event :
      job_scope
      * Agent_protocol.Id.Moderator_execution.t
      * Session.Moderator_state.Identity_snapshot.t
      * Chat_response.Moderation.Event.t
      -> queued_event_borrow option request
  | Manage_moderator_follow_up :
      Agent_protocol.Id.Operation.t * Agent_protocol.Invocation.observer
      -> unit request
  | Admit_moderator_turn : Agent_protocol.Id.Operation.t -> unit request
  | Admit_notification_turn : Agent_protocol.Id.Operation.t -> unit request
  | Claim_ordinary_event :
      Agent_protocol.Id.Moderator_execution.t
      * Agent_protocol.Id.Operation.t option
      * Session.Moderator_state.Identity_snapshot.t
      * Chat_response.Moderation.Event.t
      -> queued_event_borrow option request
  | Claim_queued_event :
      Agent_protocol.Id.Moderator_execution.t
      * Agent_protocol.Id.Operation.t option
      * Session.Moderator_state.Identity_snapshot.t
      -> queued_event_borrow option request
  | Claim_queued_retirement :
      Agent_protocol.Id.Moderator_execution.t
      * Session.Moderator_state.Identity_snapshot.t
      * string
      -> queued_event_borrow option request
  | Commit_queued_event :
      queued_event_borrow
      * Session.Moderator_state.Identity_snapshot.t
      * Agent_protocol.Invocation.follow_up
      * Agent_protocol.Moderator_execution.Decision.t option
      * string list
      -> unit request
  | Finish_queued_event : queued_event_borrow * bool -> unit request
  | Set_queued_event_cancel : queued_event_borrow * (unit -> unit) -> unit request
  | Commit_invocation_call :
      Agent_protocol.Id.Operation.t * Agent_protocol.Invocation.t * History_entry.t
      -> unit request
  | Claim_invocation :
      Agent_protocol.Id.Operation.t * Agent_protocol.Invocation.t
      -> invocation_execution request
  | Claim_idle_invocation :
      moderator_borrow * Agent_protocol.Invocation.t
      -> invocation_execution request
  | Claim_event_invocation :
      queued_event_borrow * Agent_protocol.Invocation.t
      -> invocation_execution request
  | Claim_job_scope :
      Agent_protocol.Id.Job.t * int * int * Agent_protocol.Timestamp.t option
      -> job_scope request
  | Set_job_scope_cancel : job_scope * (unit -> unit) -> unit request
  | Seal_job_scope : job_scope -> unit request
  | Finish_job_scope : job_scope -> unit request
  | Prepare_background_job :
      Agent_protocol.Job.launch_owner * Chat_response.Background_request.t
      -> Agent_protocol.Job.t request
  | Stage_background_job : Agent_protocol.Job.t * Staged_jobs.capacity -> unit request
  | Stage_subscription_mutation :
      Agent_protocol.Job.launch_owner
      * Agent_protocol.Invocation.observer
      * Agent_protocol.Subscription.t option
      * Agent_protocol.Subscription.t
      -> int request
  | Create_script_subscription :
      Agent_protocol.Job.launch_owner
      * Agent_protocol.Invocation.observer
      * string
      * int
      * Agent_protocol.Completion.wake
      * Jsonaf.t option
      -> (int * Agent_protocol.Subscription.t) request
  | Finish_script_subscription :
      Agent_protocol.Job.launch_owner
      * Agent_protocol.Invocation.observer
      * Agent_protocol.Id.Subscription.t
      * int
      * Agent_protocol.Completion.t
      -> (int * Agent_protocol.Subscription.t) request
  | Select_subscription_mutations :
      Agent_protocol.Job.launch_owner * Agent_protocol.Invocation.observer * int list
      -> unit request
  | Abort_subscription_mutation : Agent_protocol.Job.launch_owner * int -> unit request
  | Read_script_subscription :
      Agent_protocol.Job.launch_owner
      * Agent_protocol.Invocation.observer
      * Agent_protocol.Id.Subscription.t
      -> Agent_protocol.Subscription.t request
  | Expire_subscriptions : int request
  | Create_script_ingress :
      Agent_protocol.Job.launch_owner
      * Agent_protocol.Invocation.observer
      * Agent_protocol.Id.Subscription.t
      * int
      * string
      * Jsonaf.t
      -> (int * External_ingress.t) request
  | Revoke_script_ingress :
      Agent_protocol.Job.launch_owner
      * Agent_protocol.Invocation.observer
      * Agent_protocol.Id.Capability.t
      * string
      -> (int * External_ingress.t) request
  | Read_script_ingress :
      Agent_protocol.Job.launch_owner
      * Agent_protocol.Invocation.observer
      * Agent_protocol.Id.Capability.t
      -> External_ingress.t request
  | Select_ingress_mutations :
      Agent_protocol.Job.launch_owner * Agent_protocol.Invocation.observer * int list
      -> unit request
  | Abort_ingress_mutation : Agent_protocol.Job.launch_owner * int -> unit request
  | Stage_schedule_mutation :
      Agent_protocol.Job.launch_owner
      * Agent_protocol.Invocation.observer
      * Agent_protocol.Schedule.t option
      * Agent_protocol.Schedule.t
      -> int request
  | Create_script_schedule :
      Agent_protocol.Job.launch_owner
      * Agent_protocol.Invocation.observer
      * int
      * Jsonaf.t
      * Agent_protocol.Schedule.misfire
      -> (int * Agent_protocol.Schedule.t) request
  | Select_schedule_mutations :
      Agent_protocol.Job.launch_owner * Agent_protocol.Invocation.observer * int list
      -> unit request
  | Abort_schedule_mutation : Agent_protocol.Job.launch_owner * int -> unit request
  | Read_script_schedule :
      Agent_protocol.Job.launch_owner
      * Agent_protocol.Invocation.observer
      * Agent_protocol.Id.Schedule.t
      -> Agent_protocol.Schedule.t request
  | Select_background_jobs :
      Agent_protocol.Job.launch_owner * Agent_protocol.Id.Job.t list
      -> unit request
  | Create_script_notification :
      Agent_protocol.Job.launch_owner
      * Agent_protocol.Invocation.observer
      * Chat_response.Notification_operations.correlation
      * Agent_protocol.Completion.t
      * Agent_protocol.Completion.wake
      * (string * string) list option
      -> (int * Agent_protocol.Delivery.t) request
  | Read_script_notification :
      Agent_protocol.Job.launch_owner
      * Agent_protocol.Invocation.observer
      * Agent_protocol.Id.Delivery.t
      -> Agent_protocol.Delivery.t request
  | Select_notification_mutations :
      Agent_protocol.Job.launch_owner * Agent_protocol.Invocation.observer * int list
      -> unit request
  | Abort_notification_mutation : Agent_protocol.Job.launch_owner * int -> unit request
  | Abort_background_job :
      Agent_protocol.Job.launch_owner * Agent_protocol.Id.Job.t
      -> unit request
  | Has_staged_background_job :
      Agent_protocol.Job.launch_owner * Agent_protocol.Id.Job.t
      -> bool request
  | Read_script_job :
      Agent_protocol.Job.launch_owner * Agent_protocol.Id.Job.t
      -> Agent_protocol.Job.t request
  | Read_script_job_result :
      Agent_protocol.Job.launch_owner * Agent_protocol.Job.t
      -> Agent_protocol.Completion.t request
  | Cancel_script_job :
      Agent_protocol.Job.launch_owner * Agent_protocol.Id.Job.t
      -> unit request
  | Claim_job_invocation :
      job_scope * Agent_protocol.Invocation.t
      -> invocation_execution request
  | Finish_invocation :
      invocation_execution
      * Agent_protocol.Invocation.outcome
      * Agent_protocol.Invocation.follow_up option
      * bool
      -> Agent_protocol.Invocation.t request
  | Claim_moderator_invocation :
      Agent_protocol.Id.Operation.t * Agent_protocol.Invocation.t
      -> moderator_borrow request
  | Claim_moderator_observation :
      Agent_protocol.Id.Operation.t * Agent_protocol.Id.Invocation.t
      -> moderator_borrow request
  | Claim_next_moderator_observation :
      Agent_protocol.Id.Operation.t * Agent_protocol.Invocation.observer
      -> moderator_borrow option request
  | Claim_idle_moderator_observation :
      Agent_protocol.Invocation.observer * bool
      -> moderator_borrow option request
  | Commit_moderator_invocation :
      moderator_borrow
      * Agent_protocol.Invocation.t
      * Session.Moderator_state.Identity_snapshot.t
      -> unit request
  | Finish_moderator_invocation :
      moderator_borrow * Agent_protocol.Invocation.outcome option
      -> unit request
  | Set_idle_moderator_cancel : moderator_borrow * (unit -> unit) -> unit request
  | Commit_extensions :
      int * int64 * Extension_change.t list
      -> Agent_protocol.Session.t request
  | State : Session_state.t request
  | Snapshot : Agent_protocol.Snapshot.t request
  | Authorize_writer : Agent_protocol.Id.Attachment.t -> unit request
  | Set_operation_worker : Operation_worker.t option -> unit request
  | Enable_automatic_turn_budget : Chat_response.Runtime_semantics.policy -> unit request
  | Set_automatic_turn_pauses :
      Chat_response.Runtime_semantics.pause_condition list
      -> unit request
  | Change_moderator : Jsonaf.t option -> Agent_protocol.Session.t request
  | Change_workspace : Workspace_instance.t -> Agent_protocol.Session.t request
  | Shell_approval_grants : Session.Shell_state.Approval_grant.persisted list request
  | Replace_shell_approval_grants :
      Session.Shell_state.Approval_grant.persisted list
      -> unit request
  | Shell_manifest_grants : Session.Shell_state.Manifest_grant.persisted list request
  | Add_shell_manifest_grant :
      Session.Shell_state.Manifest_grant.persisted
      -> unit request
  | Reset :
      Agent_protocol.Id.Attachment.t * int64 * reset_options
      -> Agent_protocol.Session.t request
  | Upgrade_prompt :
      Agent_protocol.Id.Attachment.t * int64 * Agent_protocol.Id.Prompt_revision.t
      -> Agent_protocol.Session.t request
  | Commit_administration :
      Agent_protocol.Id.Attachment.t
      * int64
      * Session_state.Compaction_archive.kind
      * Session_state.t
      -> Agent_protocol.Session.t request
  | Start :
      Agent_protocol.Id.Attachment.t * int64 option
      -> Agent_protocol.Session.t request
  | Start_initial_delegated :
      Agent_store.Delegation_store.Reference.t * int64 option
      -> Agent_protocol.Session.t request
  | Fail_initial_delegated :
      Agent_store.Delegation_store.Reference.t * Agent_protocol.Error.t
      -> Agent_protocol.Session.t request
  | Queue_start : Agent_protocol.Id.Attachment.t -> Agent_protocol.Session.t request
  | Activate_queued_start : Agent_protocol.Session.t request
  | Stop :
      Agent_protocol.Id.Attachment.t * Agent_protocol.Session.stop_mode
      -> Agent_protocol.Session.t request
  | Stop_delegated :
      Agent_store.Delegation_store.Reference.t * Agent_protocol.Session.stop_mode
      -> Agent_protocol.Session.t request
  | Stop_managed :
      Agent_store.Delegation_store.Reference.t
      * Agent_protocol.Idempotency_key.t
      * Agent_protocol.Session.stop_mode
      * int
      * int option
      -> Managed_stop.t request
  | Stop_delegated_at_epoch :
      Agent_store.Delegation_store.Reference.t * int64 * bool
      -> Agent_protocol.Session.t request
  | Append_history :
      Agent_protocol.Id.Attachment.t * Agent_protocol.History.entry list
      -> Agent_protocol.Session.t request
  | Defer_history :
      Agent_protocol.Id.Attachment.t * Agent_protocol.History.entry list
      -> Agent_protocol.Session.t request
  | Submit_message :
      Agent_protocol.Id.Attachment.t * Agent_protocol.History.entry
      -> submission request
  | Submit_managed_message :
      Agent_store.Delegation_store.Reference.t
      * Agent_protocol.Idempotency_key.t
      * string
      * int
      * int option
      * Agent_protocol.History.entry
      -> Managed_submission.t request
  | Compact :
      Agent_protocol.Id.Attachment.t * int64 option
      -> Agent_protocol.Session.t request
  | Delete_history :
      Agent_protocol.Id.Attachment.t * int64 * Agent_protocol.History.Id.t
      -> Agent_protocol.Session.t request
  | Adopt_deferred : Agent_protocol.Session.t request
  | Reserve_history_block : int -> History_id_source.reservation request
  | Commit_worker_entry : Agent_protocol.Id.Operation.t * History_entry.t -> unit request
  | Prepare_authoring_input :
      Agent_protocol.Id.Operation.t
      * Chat_response.Authoring_materialization.t
      * History_entry.t list
      * Chat_response.Moderation.Effective_entry.t list
      -> History_entry.t list request
  | Publish_invocation_output :
      Agent_protocol.Id.Operation.t * Agent_protocol.Id.Invocation.t * History_entry.t
      -> unit request
  | Commit_worker_moderator :
      Agent_protocol.Id.Operation.t * Jsonaf.t option
      -> unit request
  | Consume_deferred : Agent_protocol.Id.Operation.t -> History_entry.t list request
  | Consume_notifications :
      Agent_protocol.Id.Operation.t * Notification_delivery.t
      -> Chat_response.In_memory_stream.Safe_point_input.batch request
  | Deliver_idle_notifications : Notification_delivery.idle -> bool request
  | Admit_standalone_delivery : Standalone_delivery.t -> unit request
  | Retire_obsolete_moderator_delivery : int64 * Agent_protocol.Job.t -> bool request
  | Deliver_standalone_completion :
      int64
      * Agent_protocol.Job.t
      * Chat_response.Tool_capability.t
      * Chat_response.One_off_request.policy
      -> unit request
  | Consume_initial_notifications :
      Agent_protocol.Id.Operation.t * Notification_delivery.idle
      -> Chat_response.In_memory_stream.Safe_point_input.batch request
  | Has_writer_attachment : bool request
  | Invocation_granted : string * string -> bool request
  | Worker_ready : Agent_protocol.Id.Operation.t * (unit -> unit) -> unit request
  | Worker_terminal :
      Agent_protocol.Id.Operation.t * Operation_worker.outcome
      -> unit request
  | Compaction_terminal :
      Agent_protocol.Id.Operation.t * compaction_outcome
      -> unit request
  | Cancel_operation :
      Agent_protocol.Id.Attachment.t * Agent_protocol.Id.Operation.t
      -> Agent_protocol.Session.t request
  | Open_permission :
      Agent_protocol.Permission.t * float option * Agent_protocol.Permission.choice
      -> Agent_protocol.Permission.resolution Eio.Promise.t request
  | Respond_permission :
      Agent_protocol.Id.Attachment.t
      * Agent_protocol.Id.Principal.t option
      * Agent_protocol.Id.Permission.t
      * int
      * Agent_protocol.Permission.choice
      * string option
      -> Agent_protocol.Permission.t request
  | Resolve_permission_system :
      Agent_protocol.Id.Permission.t
      * int
      * Agent_protocol.Permission.choice
      * string option
      -> Agent_protocol.Permission.t request
  | Revoke_grant :
      Agent_protocol.Id.Attachment.t * Agent_protocol.Id.Grant.t * string
      -> (Agent_protocol.Grant.t * Agent_protocol.Session.t) request
  | Change_job :
      Agent_protocol.Id.Attachment.t * Agent_protocol.Job.t
      -> Agent_protocol.Session.t request
  | Add_job : Agent_protocol.Job.t -> Agent_protocol.Job.t request
  | Read_job : Agent_protocol.Id.Job.t -> Agent_protocol.Job.t request
  | Publish_job_progress :
      Agent_protocol.Id.Invocation.t * Ochat_function.Progress.t
      -> unit request
  | Claim_job : Agent_protocol.Id.Job.t * int -> Agent_protocol.Job.t option request
  | Complete_job :
      Agent_protocol.Id.Job.t * int * int * Runtime_builder.model_job_outcome
      -> Agent_protocol.Job.t request
  | Complete_background_job :
      Agent_protocol.Id.Job.t * int * int * Agent_protocol.Completion.t
      -> Agent_protocol.Job.t request
  | Defer_background_job :
      Agent_protocol.Id.Job.t * int * int * Agent_protocol.Job.dependency
      -> Agent_protocol.Job.t request
  | Refresh_background_job :
      Agent_protocol.Id.Job.t * int * int
      -> Agent_protocol.Job.t request
  | Recover_background_results : int * int -> unit request
  | Deliver_job :
      Agent_protocol.Id.Job.t
      * int
      * Session.Moderator_state.Identity_snapshot.t option
      * Agent_protocol.Job.t option
      * Jsonaf.t option
      -> Agent_protocol.Job.t request
  | Cancel_job_internal : Agent_protocol.Id.Job.t -> Agent_protocol.Job.t request
  | Cancel_job :
      Agent_protocol.Id.Attachment.t * Agent_protocol.Id.Job.t
      -> Agent_protocol.Job.t request
  | Interrupt_job :
      Agent_protocol.Id.Job.t * int * int * string
      -> Agent_protocol.Job.t request
  | Change_schedule :
      Agent_protocol.Id.Attachment.t
      * [ `Created | `Cancelled ]
      * Agent_protocol.Schedule.t
      -> Agent_protocol.Session.t request
  | Add_schedule : Agent_protocol.Schedule.t -> Agent_protocol.Schedule.t request
  | Cancel_schedule_internal :
      Agent_protocol.Id.Schedule.t
      -> Agent_protocol.Schedule.t request
  | Claim_schedule :
      Agent_protocol.Id.Schedule.t * int
      -> Agent_protocol.Schedule.t option request
  | Due_schedules :
      (Agent_protocol.Session.observed_state * Agent_protocol.Schedule.t list) request
  | Retry_schedule :
      Agent_protocol.Id.Schedule.t * int
      -> Agent_protocol.Schedule.t request
  | Complete_schedule :
      Agent_protocol.Id.Schedule.t
      * int
      * Session.Moderator_state.Identity_snapshot.t option
      * Agent_protocol.Schedule.t option
      * Jsonaf.t option
      -> Agent_protocol.Schedule.t request
  | Prepare_ingress_submission :
      Agent_protocol.Invocation.observer
      * Agent_protocol.Id.Principal.t
      * Agent_protocol.Id.Capability.t
      * string
      * Agent_protocol.Idempotency_key.t
      * Jsonaf.t
      -> Ingress_submission.decision request
  | Commit_ingress_submission :
      Ingress_submission.t
      * Session.Moderator_state.Identity_snapshot.t
      * Session.Moderator_state.Identity_snapshot.t
      -> External_ingress.receipt request
  | Fail_schedule :
      Agent_protocol.Id.Schedule.t * int * Agent_protocol.Error.t
      -> Agent_protocol.Schedule.t request
  | Skip_schedule :
      Agent_protocol.Id.Schedule.t * int
      -> Agent_protocol.Schedule.t request
  | Claim_idle_moderator : History_entry.t list option request
  | Apply_observation_follow_up : bool request
  | Complete_idle_moderator : Runtime_builder.moderator_drain -> unit request
  | Fail_idle_moderator : Agent_protocol.Error.t -> unit request
  | Expire_permission :
      Agent_protocol.Id.Permission.t * int * Agent_protocol.Permission.choice
      -> unit request
  | Attach :
      (Agent_protocol.Session.attachment_mode
      * bool
      * Agent_protocol.Id.Principal.t option
      * string option)
      -> (Agent_protocol.Session.Attachment.t
         * Subscriber.t option
         * Agent_protocol.Snapshot.t
         * string option)
           request
  | Detach : Agent_protocol.Id.Attachment.t -> unit request
  | Renew_owner :
      Agent_protocol.Id.Attachment.t * int64
      -> (Agent_protocol.Session.Owner_lease.t * Agent_protocol.Session.t) request
  | Owner_expired : int64 -> unit request
  | Checkpoint :
      (Session_state.t -> (unit, Agent_protocol.Error.t) result)
      -> unit request
  | Quiescent_checkpoint :
      (Session_state.t -> ('a, Agent_protocol.Error.t) result)
      -> 'a option request
  | Shutdown : unit request

and compaction_outcome =
  | Compacted of History_entry.t list
  | Compaction_cancelled of string
  | Compaction_failed of Agent_protocol.Error.t

type packed =
  | Pack :
      string option * 'a request * ('a, Agent_protocol.Error.t) result Eio.Promise.u
      -> packed

type permission_waiter =
  { resolver : Agent_protocol.Permission.resolution Eio.Promise.u
  ; resume_observed : Agent_protocol.Session.observed_state
  }

type t =
  { sw : Eio.Switch.t
  ; sleep : float -> unit
  ; mailbox : packed Mailbox.t
  ; persistence : persistence
  ; services : services
  ; subscriber_mutex : Eio.Mutex.t
  ; active_calls : Active_calls.t
  ; subscribers : (Agent_protocol.Id.Attachment.t, Subscriber.t) Map.Poly.t ref
  ; permission_waiters :
      (Agent_protocol.Id.Permission.t, permission_waiter) Map.Poly.t ref
  ; mutable operation_worker : Operation_worker.t option
  ; compaction_env : Eio_unix.Stdenv.base option
  ; owner_lease_duration_ms : int
  ; max_attachments : int
  ; subscriber_capacity : int
  ; schedule_permission_timeouts : bool
  ; mutable owner_timer_cancel : unit Eio.Promise.u option
  ; mutable active_cancel : (unit -> unit) option
  ; mutable idle_moderator_borrowed : bool
  ; mutable moderator_borrow : moderator_borrow option
  ; mutable queued_event_borrow : queued_event_borrow option
  ; mutable foreground_moderator :
      (Agent_protocol.Id.Operation.t * Agent_protocol.Invocation.observer) option
  ; mutable invocation_executions : invocation_execution list
  ; mutable job_scopes : job_scope list
  ; staged_jobs : Staged_jobs.t
  ; staged_subscriptions : Staged_subscriptions.t
  ; staged_ingress : Staged_ingress.t
  ; staged_schedules : Staged_schedules.t
  ; staged_notifications : Staged_notifications.t
  ; mutable notification_inputs :
      (Agent_protocol.Id.Operation.t * Agent_protocol.Id.Delivery.t list) option
  ; extension_clock : Extension_clock.t
  ; invocation_gate : Chat_response.Execution_gate.t
  ; event_sequence : int64 Atomic.t
  ; mutable state : Session_state.t
  ; mutable stopped : bool
  ; mutable command_audit : string option
  }

let error code message = Agent_protocol.Error.create code ~message ~retryable:false ()

let job_scope_current t scope =
  match
    ( List.mem t.job_scopes scope ~equal:phys_equal
    , List.find t.state.jobs ~f:(fun job ->
        Agent_protocol.Id.Job.equal job.id scope.job.id) )
  with
  | true, Some job
    when Int.equal job.generation scope.job.generation
         && Int.equal job.generation t.state.identity.generation
         && Int.equal job.attempt scope.job.attempt
         && Agent_protocol.Job.equal_kind job.kind scope.job.kind
         && Jsonaf.exactly_equal job.payload scope.job.payload -> Ok job
  | _ -> Error (error Conflict "background callback no longer owns this job attempt")
;;

let job_scope_can_execute t scope =
  let open Result.Let_syntax in
  let%bind job = job_scope_current t scope in
  match job.status, t.state.lifecycle.desired, t.state.halted, t.state.failure with
  | (Running | Waiting_permission _), Running, false, None
    when scope.active
         && (not scope.cancel_requested)
         && Option.for_all scope.deadline ~f:(fun deadline ->
           Agent_protocol.Timestamp.compare (t.services.now ()) deadline < 0) -> Ok ()
  | _ -> Error (error Conflict "background job cannot admit another invocation")
;;

let job_execution_owned_by scope execution =
  match execution.owner with
  | Background_job owner -> phys_equal owner scope
  | Event_moderator borrow -> Option.exists borrow.job_scope ~f:(phys_equal scope)
  | Invocation_moderator borrow -> Option.exists borrow.job_scope ~f:(phys_equal scope)
  | _ -> false
;;

let signal_job_scope scope =
  match Eio.Promise.peek scope.cancelled with
  | Some () -> ()
  | None -> Eio.Promise.resolve scope.cancel_signal ()
;;

let cancel_job_scope (scope : job_scope) =
  scope.cancel_requested <- true;
  signal_job_scope scope;
  Option.iter scope.cancel ~f:(fun cancel -> cancel ())
;;

let cancel_job_moderators t scope =
  Option.iter t.moderator_borrow ~f:(fun borrow ->
    match Option.exists borrow.job_scope ~f:(phys_equal scope) with
    | false -> ()
    | true ->
      borrow.cancel_requested <- true;
      borrow.accepts_children <- false;
      Option.iter borrow.cancel ~f:(fun cancel -> cancel ()));
  Option.iter t.queued_event_borrow ~f:(fun borrow ->
    match Option.exists borrow.job_scope ~f:(phys_equal scope) with
    | false -> ()
    | true ->
      borrow.cancel_requested <- true;
      Option.iter borrow.cancel ~f:(fun cancel -> cancel ()))
;;

let job_has_moderator t scope =
  Option.exists t.moderator_borrow ~f:(fun borrow ->
    Option.exists borrow.job_scope ~f:(phys_equal scope))
  || Option.exists t.queued_event_borrow ~f:(fun borrow ->
    Option.exists borrow.job_scope ~f:(phys_equal scope))
;;

let cancel_job_scopes t jobs =
  List.iter t.job_scopes ~f:(fun scope ->
    match
      List.exists jobs ~f:(fun (job : Agent_protocol.Job.t) ->
        Agent_protocol.Id.Job.equal job.id scope.job.id
        && Int.equal job.generation scope.job.generation
        && Int.equal job.attempt scope.job.attempt)
    with
    | true ->
      cancel_job_scope scope;
      cancel_job_moderators t scope
    | false -> ())
;;

let moderator_is_borrowed t =
  Option.is_some t.moderator_borrow || Option.is_some t.queued_event_borrow
;;

let call t ?(priority = Mailbox.Normal) ?command_audit request =
  let promise, resolver = Eio.Promise.create () in
  let open Result.Let_syntax in
  let%bind () =
    Mailbox.push t.mailbox ~priority (Pack (command_audit, request, resolver))
  in
  Eio.Promise.await promise
;;

let publish_durable t events =
  Eio.Mutex.use_rw ~protect:true t.subscriber_mutex (fun () ->
    List.iter events ~f:(Active_calls.finish t.active_calls);
    Map.iter !(t.subscribers) ~f:(fun subscriber ->
      List.iter events ~f:(Subscriber.publish_durable subscriber)))
;;

let broadcast_recoverable t event =
  Eio.Mutex.use_rw ~protect:true t.subscriber_mutex (fun () ->
    Active_calls.observe t.active_calls event;
    Map.iter !(t.subscribers) ~f:(fun subscriber ->
      Subscriber.publish_recoverable subscriber event))
;;

let sync_extension_clock t =
  let timers =
    t.state.schedules @ Staged_schedules.values t.staged_schedules
    |> List.filter_map ~f:(fun timer ->
      match timer.Agent_protocol.Schedule.ownership, timer.status with
      | Some _, (Scheduled | Delivering) ->
        Some (Extension_clock.Key.Schedule timer.id, timer.next_due_at)
      | _ -> None)
  in
  let subscriptions =
    t.state.subscriptions @ Staged_subscriptions.values t.staged_subscriptions
    |> List.filter_map ~f:(fun subscription ->
      match
        subscription.Agent_protocol.Subscription.context.source, subscription.result
      with
      | Some _, None ->
        Some
          ( Extension_clock.Key.Subscription subscription.context.id
          , subscription.context.deadline )
      | _ -> None)
  in
  Extension_clock.reconcile
    t.extension_clock
    ~retained:(timers @ subscriptions)
    ~wall_now:(t.services.now ())
    ~monotonic_now:(t.services.monotonic_now ())
;;

let install t transition =
  let previous = t.state in
  let command_audit = t.command_audit in
  match t.persistence.commit ~command_audit ~previous transition with
  | Error error -> Error error
  | Ok () ->
    t.command_audit <- None;
    t.state <- transition.state;
    sync_extension_clock t;
    Atomic.set t.event_sequence t.state.counters.event_sequence;
    publish_durable t transition.events;
    t.services.state_committed t.state transition.events;
    Ok ()
;;

let current_snapshot t =
  Eio.Mutex.use_ro t.subscriber_mutex (fun () ->
    let active_tool_calls, active_agent_calls = Active_calls.snapshot t.active_calls in
    { (Session_state.snapshot ~now:(t.services.now ()) t.state) with
      active_tool_calls
    ; active_agent_calls
    })
;;

let transition t ~delta ~payloads =
  let open Result.Let_syntax in
  let%bind transition =
    Session_transition.apply ~now:(t.services.now ()) t.state ~delta ~payloads
  in
  let%map () = install t transition in
  Agent_protocol.Session.(Session_state.summary t.state)
;;

let abort_staged_work t ~owner =
  Staged_jobs.abort_owner t.staged_jobs ~owner;
  Staged_subscriptions.release_owner t.staged_subscriptions ~owner;
  Staged_ingress.release_owner t.staged_ingress ~owner;
  Staged_schedules.release_owner t.staged_schedules ~owner;
  Staged_notifications.release_owner t.staged_notifications ~owner;
  sync_extension_clock t
;;

let abort_all_staged_work t =
  Staged_jobs.abort_all t.staged_jobs;
  Staged_subscriptions.abort_all t.staged_subscriptions;
  Staged_ingress.abort_all t.staged_ingress;
  Staged_schedules.abort_all t.staged_schedules;
  Staged_notifications.abort_all t.staged_notifications;
  sync_extension_clock t
;;

let with_staged_transaction t owner f =
  match f () with
  | Ok _ as result ->
    Staged_jobs.commit t.staged_jobs ~owner;
    Staged_subscriptions.release_owner t.staged_subscriptions ~owner;
    Staged_ingress.release_owner t.staged_ingress ~owner;
    Staged_schedules.release_owner t.staged_schedules ~owner;
    Staged_notifications.release_owner t.staged_notifications ~owner;
    sync_extension_clock t;
    result
  | Error _ as result ->
    abort_staged_work t ~owner;
    result
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    abort_staged_work t ~owner;
    Stdlib.Printexc.raise_with_backtrace exn backtrace
;;

let lookup_subscription t id =
  List.find t.state.subscriptions ~f:(fun subscription ->
    Agent_protocol.Id.Subscription.equal subscription.context.id id)
;;

let lookup_notification t id =
  List.find t.state.deliveries ~f:(fun value ->
    Agent_protocol.Id.Delivery.equal value.context.id id)
;;

let lookup_ingress t id =
  List.find t.state.ingress_registrations ~f:(fun value ->
    Agent_protocol.Id.Capability.equal value.External_ingress.context.id id)
;;

let ingress_subscription_lookup t subscriptions id epoch =
  let matches value =
    Agent_protocol.Id.Subscription.equal value.Agent_protocol.Subscription.context.id id
  in
  match
    List.find (subscriptions @ t.state.subscriptions) ~f:(fun value ->
      matches value && value.epoch = epoch)
  with
  | Some _ as result -> result
  | None -> List.find (List.rev subscriptions @ t.state.subscriptions) ~f:matches
;;

let selected_ingress t owner subscriptions =
  Staged_ingress.selected
    t.staged_ingress
    ~owner
    ~lookup:(lookup_ingress t)
    ~subscription:(ingress_subscription_lookup t subscriptions)
;;

let staged_work_changes t owner =
  let open Result.Let_syntax in
  let%bind subscriptions =
    Staged_subscriptions.selected
      t.staged_subscriptions
      ~owner
      ~lookup:(lookup_subscription t)
  in
  let%bind registrations = selected_ingress t owner subscriptions in
  let%bind () =
    List.fold_result registrations ~init:() ~f:(fun () value ->
      match lookup_ingress t value.External_ingress.context.id with
      | Some _ -> Ok ()
      | None ->
        let%bind due =
          Extension_clock.is_due
            t.extension_clock
            (Subscription value.context.subscription_id)
            ~now:(t.services.monotonic_now ())
        in
        (match
           (not due)
           && Agent_protocol.Timestamp.compare
                (t.services.now ())
                value.context.expires_at
              < 0
         with
         | true -> Ok ()
         | false ->
           Error
             (error Conflict "ingress subscription expired before registration commit")))
  in
  let%bind () =
    match
      List.exists registrations ~f:(fun value ->
        Option.is_none (lookup_ingress t value.External_ingress.context.id))
    with
    | false -> Ok ()
    | true ->
      Staged_ingress.check_capacity
        ~limits:t.services.ingress_limits
        ~generation:t.state.identity.generation
        ~now:(t.services.now ())
        ~subscriptions:(t.state.subscriptions @ subscriptions)
        ~values:(t.state.ingress_registrations @ registrations)
  in
  let%bind schedules =
    Staged_schedules.selected t.staged_schedules ~owner ~lookup:(fun id ->
      List.find t.state.schedules ~f:(fun value ->
        Agent_protocol.Id.Schedule.equal value.id id))
  in
  let%map notifications =
    Staged_notifications.selected
      t.staged_notifications
      ~owner
      ~lookup:(lookup_notification t)
  in
  let jobs = Staged_jobs.selected t.staged_jobs ~owner in
  ( List.map jobs ~f:(fun job -> Session_delta.Job_changed job)
    @ Staged_ingress.ordered_changes ~subscriptions ~registrations
    @ List.map schedules ~f:(fun schedule -> Session_delta.Schedule_changed schedule)
    @ List.map notifications ~f:(fun delivery -> Session_delta.Delivery_changed delivery)
  , List.map jobs ~f:(fun job ->
      Agent_protocol.Event.Durable.Payload.Job_state_changed job)
    @ List.map schedules ~f:(fun schedule ->
      Agent_protocol.Event.Durable.Payload.Schedule_state_changed schedule) )
;;

let prepare_background_job_launch t ~owner request =
  call t (Prepare_background_job (owner, request))
;;

let create_script_notification
      ?disclosure_pins
      t
      ~owner
      ~source
      ~correlation
      ~completion
      ~wake
  =
  call
    t
    (Create_script_notification
       (owner, source, correlation, completion, wake, disclosure_pins))
;;

let read_script_notification t ~owner ~source ~id =
  call t (Read_script_notification (owner, source, id))
;;

let select_notification_mutations t ~owner ~source ~receipts =
  call t (Select_notification_mutations (owner, source, receipts))
;;

let abort_notification_mutation t ~owner ~receipt =
  call t (Abort_notification_mutation (owner, receipt))
;;

let stage_background_job t ~job ~capacity =
  Eio.Cancel.protect (fun () ->
    match call t (Stage_background_job (job, capacity)) with
    | Ok () -> Ok ()
    | Error _ as result ->
      capacity.Staged_jobs.abort ();
      result)
;;

let select_background_jobs t ~owner ~ids = call t (Select_background_jobs (owner, ids))

let stage_subscription_mutation t ~owner ~source ~previous ~next =
  call t (Stage_subscription_mutation (owner, source, previous, next))
;;

let create_script_subscription
      t
      ~owner
      ~source
      ~kind
      ~lifetime_ms
      ~wake
      ~completion_schema
  =
  call
    t
    (Create_script_subscription (owner, source, kind, lifetime_ms, wake, completion_schema))
;;

let select_subscription_mutations t ~owner ~source ~receipts =
  call t (Select_subscription_mutations (owner, source, receipts))
;;

let finish_script_subscription t ~owner ~source ~id ~expected_epoch completion =
  call t (Finish_script_subscription (owner, source, id, expected_epoch, completion))
;;

let abort_subscription_mutation t ~owner ~receipt =
  Eio.Cancel.protect (fun () -> call t (Abort_subscription_mutation (owner, receipt)))
;;

let read_script_subscription t ~owner ~source ~id =
  call t (Read_script_subscription (owner, source, id))
;;

let expire_subscriptions t = call t Expire_subscriptions

let create_script_ingress
      t
      ~owner
      ~source
      ~subscription_id
      ~expected_epoch
      ~namespace
      ~schema
  =
  call
    t
    (Create_script_ingress
       (owner, source, subscription_id, expected_epoch, namespace, schema))
;;

let revoke_script_ingress t ~owner ~source ~id ~reason =
  call t (Revoke_script_ingress (owner, source, id, reason))
;;

let read_script_ingress t ~owner ~source ~id =
  call t (Read_script_ingress (owner, source, id))
;;

let select_ingress_mutations t ~owner ~source ~receipts =
  call t (Select_ingress_mutations (owner, source, receipts))
;;

let abort_ingress_mutation t ~owner ~receipt =
  Eio.Cancel.protect (fun () -> call t (Abort_ingress_mutation (owner, receipt)))
;;

let stage_schedule_mutation t ~owner ~source ~previous ~next =
  call t (Stage_schedule_mutation (owner, source, previous, next))
;;

let create_script_schedule t ~owner ~source ~delay_ms ~payload ~misfire =
  call t (Create_script_schedule (owner, source, delay_ms, payload, misfire))
;;

let select_schedule_mutations t ~owner ~source ~receipts =
  call t (Select_schedule_mutations (owner, source, receipts))
;;

let abort_schedule_mutation t ~owner ~receipt =
  Eio.Cancel.protect (fun () -> call t (Abort_schedule_mutation (owner, receipt)))
;;

let read_script_schedule t ~owner ~source ~id =
  call t (Read_script_schedule (owner, source, id))
;;

let subscription_is_due_at
      t
      (subscription : Agent_protocol.Subscription.t)
      ~now
      ~monotonic_now
  =
  match subscription.context.source with
  | None -> Ok (Agent_protocol.Timestamp.compare subscription.context.deadline now <= 0)
  | Some _ ->
    Extension_clock.is_due
      t.extension_clock
      (Subscription subscription.context.id)
      ~now:monotonic_now
;;

let subscription_is_due t subscription =
  subscription_is_due_at
    t
    subscription
    ~now:(t.services.now ())
    ~monotonic_now:(t.services.monotonic_now ())
;;

let subscription_terminal_time_at
      (subscription : Agent_protocol.Subscription.t)
      completion
      ~now
  =
  let lower_bound =
    match completion with
    | Agent_protocol.Completion.Expired -> subscription.context.deadline
    | _ -> subscription.context.created_at
  in
  match Agent_protocol.Timestamp.compare now lower_bound < 0 with
  | true -> lower_bound
  | false -> now
;;

let subscription_terminal_time t subscription completion =
  subscription_terminal_time_at subscription completion ~now:(t.services.now ())
;;

let expire_subscriptions_internal t =
  let module P = Agent_protocol in
  let open Result.Let_syntax in
  let%bind due =
    List.filter t.state.subscriptions ~f:(fun subscription ->
      Option.is_none subscription.result)
    |> List.map ~f:(fun subscription ->
      subscription_is_due t subscription
      |> Result.map ~f:(fun due -> Option.some_if due subscription))
    |> Result.all
    |> Result.map ~f:List.filter_opt
  in
  match due with
  | [] -> Ok 0
  | _ ->
    let open Result.Let_syntax in
    let%bind terminal =
      List.map due ~f:(fun subscription ->
        P.Subscription.finish
          subscription
          ~expected_epoch:subscription.epoch
          ~now:(subscription_terminal_time t subscription Expired)
          Expired
        |> Result.map ~f:fst)
      |> Result.all
    in
    let timer_ids =
      List.filter_map due ~f:(fun subscription -> subscription.timer_id)
      |> Hash_set.of_list (module P.Id.Schedule)
    in
    let cancelled =
      List.filter_map t.state.schedules ~f:(fun schedule ->
        match Hash_set.mem timer_ids schedule.id, schedule.status with
        | true, (P.Schedule.Scheduled | Delivering) ->
          Some { schedule with status = P.Schedule.Cancelled }
        | _ -> None)
    in
    let%map _ =
      transition
        t
        ~delta:
          (Session_delta.Batch
             (List.map terminal ~f:(fun subscription ->
                Session_delta.Subscription_expired subscription)
              @ List.map cancelled ~f:(fun schedule ->
                Session_delta.Schedule_changed schedule)))
        ~payloads:
          (List.map cancelled ~f:(fun schedule ->
             P.Event.Durable.Payload.Schedule_cancelled schedule))
    in
    List.length terminal
;;

let abort_background_job t ~owner ~id =
  Eio.Cancel.protect (fun () -> call t (Abort_background_job (owner, id)))
;;

let has_staged_background_job t ~owner ~id =
  call t (Has_staged_background_job (owner, id))
;;

let read_script_job t ~owner ~id = call t (Read_script_job (owner, id))

let read_script_job_result t ~owner ~expected =
  call t (Read_script_job_result (owner, expected))
;;

let cancel_script_job t ~owner ~id = call t (Cancel_script_job (owner, id))

let commit_extensions_internal t generation expected_revision changes =
  let open Result.Let_syntax in
  if
    generation <> t.state.identity.generation
    || not (Int64.equal expected_revision t.state.counters.revision)
  then
    Error
      (error Conflict "extension transaction uses a stale session revision or generation")
  else if
    (moderator_is_borrowed t || not (List.is_empty t.invocation_executions))
    && List.exists changes ~f:(function
      | Extension_change.Moderator_state _ -> moderator_is_borrowed t
      | Invocation value ->
        List.exists t.invocation_executions ~f:(fun execution ->
          Agent_protocol.Id.Invocation.compare
            value.context.id
            execution.dispatched.context.id
          = 0)
        || Option.exists t.moderator_borrow ~f:(fun borrow ->
          Agent_protocol.Id.Invocation.compare
            value.context.id
            borrow.invocation.context.id
          = 0)
      | _ -> false)
  then Error (error Conflict "invocation callback owns this state transaction")
  else if List.is_empty changes || List.length changes > 256
  then
    Error
      (error
         Resource_limit
         "extension transaction must contain between 1 and 256 changes")
  else if
    List.exists changes ~f:(function
      | Extension_change.Publish _ -> true
      | _ -> false)
    && (Option.is_some t.state.active_operation
        || t.idle_moderator_borrowed
        || t.state.halted
        || (not
              (Agent_protocol.Session.equal_desired_state
                 t.state.lifecycle.desired
                 Running))
        ||
        match t.state.lifecycle.observed with
        | Agent_protocol.Session.Idle -> false
        | _ -> true)
  then
    Error
      (error
         Invalid_state
         "notification publication requires an idle running session safe point")
  else (
    let new_jobs = Hash_set.create (module Agent_protocol.Id.Job) in
    let new_schedules = Hash_set.create (module Agent_protocol.Id.Schedule) in
    let%bind deltas =
      List.map changes ~f:(function
        | Extension_change.Invocation value -> Ok (Session_delta.Invocation_changed value)
        | Subscription value -> Ok (Session_delta.Subscription_changed value)
        | Delivery value ->
          (match value.completion_projection with
           | Some _
             when not
                    (List.exists t.state.deliveries ~f:(fun old ->
                       Agent_protocol.Id.Delivery.equal old.context.id value.context.id))
             ->
             Error
               (error
                  Permission_denied
                  "standalone delivery requires checked adapter admission")
           | _ -> Ok (Session_delta.Delivery_changed value))
        | Publish (value, entry) ->
          let%bind () =
            match value.completion_projection with
            | Some _
              when not
                     (List.exists t.state.deliveries ~f:(fun old ->
                        Agent_protocol.Id.Delivery.equal old.context.id value.context.id))
              ->
              Error
                (error
                   Permission_denied
                   "standalone delivery requires checked adapter admission")
            | _ -> Ok ()
          in
          let%map () = Notification_history.validate ~delivery:value entry in
          Session_delta.Delivery_committed (value, entry)
        | Moderator_state value -> Ok (Session_delta.Moderator_changed value)
        | Start_job job ->
          let%bind () =
            Extension_invariants.owner
              ~session_id:t.state.identity.session_id
              ~generation
              job.session_id
              job.generation
          in
          if
            Hash_set.mem new_jobs job.id
            || List.exists t.state.jobs ~f:(fun old ->
              Agent_protocol.Id.Job.compare old.id job.id = 0)
          then Error (error Conflict "extension job identity is already admitted")
          else (
            match job.status with
            | Queued ->
              Hash_set.add new_jobs job.id;
              Ok (Session_delta.Job_changed job)
            | _ -> Error (error Invalid_state "extension job must start queued"))
        | Schedule schedule ->
          let%bind () =
            Extension_invariants.owner
              ~session_id:t.state.identity.session_id
              ~generation
              schedule.session_id
              schedule.generation
          in
          if
            Hash_set.mem new_schedules schedule.id
            || List.exists t.state.schedules ~f:(fun old ->
              Agent_protocol.Id.Schedule.compare old.id schedule.id = 0)
          then Error (error Conflict "extension schedule identity is already admitted")
          else (
            match schedule.status with
            | Scheduled ->
              Hash_set.add new_schedules schedule.id;
              Ok (Session_delta.Schedule_changed schedule)
            | _ -> Error (error Invalid_state "extension schedule must start scheduled")))
      |> Result.all
    in
    let delta = Session_delta.Batch deltas in
    let%bind candidate = Session_delta.apply t.state delta in
    let old_ids = Hash_set.create (module History_entry.Id) in
    List.iter t.state.conversation.canonical_history ~f:(fun entry ->
      Hash_set.add old_ids entry.id);
    let appended =
      List.filter candidate.conversation.canonical_history ~f:(fun entry ->
        not (Hash_set.mem old_ids entry.id))
    in
    let history_payloads =
      if List.is_empty appended
      then []
      else [ Agent_protocol.Event.Durable.Payload.History_appended appended ]
    in
    let work_payloads =
      List.filter_map changes ~f:(function
        | Extension_change.Start_job job ->
          Some (Agent_protocol.Event.Durable.Payload.Job_state_changed job)
        | Schedule schedule ->
          Some (Agent_protocol.Event.Durable.Payload.Schedule_created schedule)
        | _ -> None)
    in
    transition t ~delta ~payloads:(history_payloads @ work_payloads))
;;

let set_operation_worker t worker =
  if moderator_is_borrowed t
  then Error (error Conflict "cannot replace a borrowed moderator runtime")
  else (
    match worker, t.state.active_operation, t.idle_moderator_borrowed with
    | None, Some _, _ -> Error (error Conflict "cannot unload an active runtime")
    | None, None, true -> Error (error Conflict "cannot unload a borrowed moderator")
    | None, None, false | Some _, _, _ ->
      t.operation_worker <- worker;
      Ok ())
;;

let change_moderator t moderator =
  if moderator_is_borrowed t
  then Error (error Conflict "moderator invocation owns the moderator checkpoint")
  else transition t ~delta:(Session_delta.Moderator_changed moderator) ~payloads:[]
;;

let change_workspace t workspace =
  match t.state.lifecycle.observed, t.state.active_operation with
  | Agent_protocol.Session.Stopped, None when List.is_empty t.job_scopes ->
    transition t ~delta:(Session_delta.Workspace_changed workspace) ~payloads:[]
  | ( ( Queued_for_slot
      | Starting
      | Recovering
      | Idle
      | Running_turn _
      | Compacting _
      | Waiting_for_permission _
      | Stopping
      | Failed _
      | Stopped )
    , _ ) ->
    Error (error Invalid_state "workspace replacement requires a stopped session")
;;

let lifecycle t ~desired ~observed =
  let open Result.Let_syntax in
  let%bind transition =
    let delta = Session_delta.Lifecycle_changed { desired; observed } in
    Session_transition.apply
      ~now:(t.services.now ())
      t.state
      ~delta:
        (if t.state.pending_initial_start
         then Batch [ Initial_start_consumed; delta ]
         else delta)
      ~payloads:
        [ Agent_protocol.Event.Durable.Payload.Session_state_changed
            { desired_state = desired; observed_state = observed }
        ]
  in
  let%map () = install t transition in
  Session_state.summary t.state
;;

let start_internal ?expected_parent_stop_epoch t =
  if
    Option.exists expected_parent_stop_epoch ~f:(fun epoch ->
      not (Option.equal Int64.equal t.state.parent_stop_epoch (Some epoch)))
  then Error (error Conflict "parent stopped while child start was being prepared")
  else if t.idle_moderator_borrowed
  then Error (error Conflict "cannot restart while the idle moderator is borrowed")
  else (
    match t.state.lifecycle.desired, t.state.lifecycle.observed with
    | Running, (Idle | Running_turn _ | Starting | Queued_for_slot) ->
      Ok (Session_state.summary t.state)
    | _, _ ->
      let open Result.Let_syntax in
      let%bind _ = lifecycle t ~desired:Running ~observed:Starting in
      lifecycle t ~desired:Running ~observed:Idle)
;;

let queue_start_internal t =
  if t.idle_moderator_borrowed
  then Error (error Conflict "cannot restart while the idle moderator is borrowed")
  else (
    match t.state.lifecycle.desired, t.state.lifecycle.observed with
    | Running, (Queued_for_slot | Starting | Idle | Running_turn _) ->
      Ok (Session_state.summary t.state)
    | _, _ -> lifecycle t ~desired:Running ~observed:Queued_for_slot)
;;

let activate_queued_start t =
  match t.state.lifecycle.desired, t.state.lifecycle.observed with
  | Running, Queued_for_slot ->
    let open Result.Let_syntax in
    let%bind _ = lifecycle t ~desired:Running ~observed:Starting in
    lifecycle t ~desired:Running ~observed:Idle
  | Running, (Starting | Idle | Running_turn _) -> Ok (Session_state.summary t.state)
  | _, _ -> Error (error Invalid_state "session has no queued start intent")
;;

let background_terminal_result (job : Agent_protocol.Job.t) completion =
  match job.kind with
  | Async_tool -> Some (Agent_protocol.Completion.to_json completion)
  | _ -> job.result
;;

let stopped_jobs t mode =
  if Agent_protocol.Session.equal_stop_mode mode Graceful
  then []
  else
    List.filter_map t.state.jobs ~f:(fun (job : Agent_protocol.Job.t) ->
      match job.status with
      | Queued | Running | Waiting_permission _ | Waiting_completion _ ->
        Some
          { job with
            status = Cancelled
          ; result = background_terminal_result job (Cancelled "session stopped")
          ; completed_at = Some (t.services.now ())
          ; delivery =
              (match job.delivery with
               | Not_required -> Not_required
               | _ -> Pending)
          }
      | Succeeded | Failed _ | Cancelled | Interrupted _ -> None)
;;

let cancel_permission t reason (permission : Agent_protocol.Permission.t) =
  { permission with
    state = Cancelled
  ; resolution =
      Some
        { choice = Deny
        ; principal_id = None
        ; resolved_at = t.services.now ()
        ; reason = Some reason
        }
  }
;;

let resolve_cleaned_permission_waiters t permissions =
  List.iter permissions ~f:(fun permission ->
    Option.iter permission.Agent_protocol.Permission.resolution ~f:(fun resolution ->
      Option.iter (Map.find !(t.permission_waiters) permission.id) ~f:(fun waiter ->
        Eio.Promise.resolve waiter.resolver resolution));
    t.permission_waiters := Map.remove !(t.permission_waiters) permission.id)
;;

let pending_invocation_permissions t ~matches =
  List.filter t.state.permissions ~f:(fun permission ->
    Agent_protocol.Permission.equal_state permission.state Pending
    &&
    match permission.owner with
    | Invocation id -> matches id
    | Operation _ -> false)
;;

let permission_resume_observed
      t
      ~resolved
      ~(fallback : Agent_protocol.Session.observed_state)
  =
  match t.state.lifecycle.desired, t.state.active_operation with
  | Stopped, None -> Agent_protocol.Session.Stopped
  | _ ->
    (match
       List.find t.state.permissions ~f:(fun permission ->
         Agent_protocol.Permission.equal_state permission.state Pending
         && not
              (List.exists resolved ~f:(Agent_protocol.Id.Permission.equal permission.id)))
     with
     | Some permission -> Waiting_for_permission permission.id
     | None ->
       (match fallback with
        | (Running_turn id | Compacting id)
          when not
                 (Option.exists t.state.active_operation ~f:(fun operation ->
                    Agent_protocol.Id.Operation.equal operation.id id)) ->
          (match t.state.lifecycle.desired with
           | Running -> Idle
           | Stopped -> Stopped)
        | _ -> fallback))
;;

let cleanup_invocation_permissions t ids =
  let permissions =
    pending_invocation_permissions t ~matches:(fun id ->
      List.mem ids id ~equal:Agent_protocol.Id.Invocation.equal)
    |> List.map
         ~f:(cancel_permission t "invocation finished before permission resolution")
  in
  let lifecycle =
    match t.state.lifecycle.observed with
    | Waiting_for_permission id
      when List.exists permissions ~f:(fun p ->
             Agent_protocol.Id.Permission.equal p.id id) ->
      Option.map (Map.find !(t.permission_waiters) id) ~f:(fun waiter ->
        { t.state.lifecycle with
          observed =
            permission_resume_observed
              t
              ~resolved:
                (List.map permissions ~f:(fun p -> p.Agent_protocol.Permission.id))
              ~fallback:waiter.resume_observed
        })
    | _ -> None
  in
  ( permissions
  , List.map permissions ~f:(fun permission ->
      Session_delta.Permission_changed permission)
    @ Option.to_list
        (Option.map lifecycle ~f:(fun value -> Session_delta.Lifecycle_changed value))
  , List.map permissions ~f:(fun permission ->
      Agent_protocol.Event.Durable.Payload.Permission_resolved permission)
    @ Option.to_list
        (Option.map lifecycle ~f:(fun value ->
           Agent_protocol.Event.Durable.Payload.Session_state_changed
             { desired_state = value.desired; observed_state = value.observed })) )
;;

let stop_transition
      ?parent_stop_epoch
      ?managed_receipt
      t
      mode
      ~extension_work
      lifecycle
      deltas
      payloads
  =
  let open Result.Let_syntax in
  let%bind discarded =
    Observation_follow_up.discard t.state.invocations ~reason:"session stopped"
  in
  let%bind events =
    Observation_follow_up.discard_events
      t.state.moderator_executions
      ~reason:"session stopped"
  in
  let jobs = stopped_jobs t mode in
  let permissions =
    pending_invocation_permissions t ~matches:(fun _ -> true)
    |> List.map ~f:(cancel_permission t "session stopped")
  in
  let%map result =
    transition
      t
      ~delta:
        (Session_delta.Batch
           ((if t.state.pending_initial_start
             then [ Session_delta.Initial_start_consumed ]
             else [])
            @ (Session_delta.Lifecycle_changed lifecycle :: deltas)
            @ Option.to_list
                (Option.map managed_receipt ~f:(fun receipt ->
                   Session_delta.Managed_stop_admitted receipt))
            @ Option.to_list
                (Option.map parent_stop_epoch ~f:(fun epoch ->
                   Session_delta.Parent_stop_epoch_changed epoch))
            @ List.map discarded ~f:Observation_follow_up.delta
            @ List.map events ~f:Observation_follow_up.event_delta
            @ List.map permissions ~f:(fun permission ->
              Session_delta.Permission_changed permission)
            @ List.map jobs ~f:(fun job -> Session_delta.Job_changed job)
            @ Extension_stop.deltas extension_work))
      ~payloads:
        ((Agent_protocol.Event.Durable.Payload.Session_state_changed
            { desired_state = lifecycle.desired; observed_state = lifecycle.observed }
          :: payloads)
         @ List.map jobs ~f:(fun job ->
           Agent_protocol.Event.Durable.Payload.Job_state_changed job)
         @ List.map permissions ~f:(fun permission ->
           Agent_protocol.Event.Durable.Payload.Permission_resolved permission)
         @ Extension_stop.payloads extension_work)
  in
  cancel_job_scopes t jobs;
  resolve_cleaned_permission_waiters t permissions;
  result
;;

let cancel_event_for_operation t operation_id =
  Option.iter t.queued_event_borrow ~f:(fun borrow ->
    if
      Option.exists
        borrow.receipt.context.operation_id
        ~f:(Agent_protocol.Id.Operation.equal operation_id)
    then (
      borrow.cancel_requested <- true;
      Option.iter borrow.cancel ~f:(fun cancel -> cancel ())))
;;

let cancel_independent_moderator t =
  Option.iter t.moderator_borrow ~f:(fun borrow ->
    match borrow.operation_id with
    | Some _ -> ()
    | None ->
      borrow.cancel_requested <- true;
      Option.iter borrow.cancel ~f:(fun cancel -> cancel ()));
  Option.iter t.queued_event_borrow ~f:(fun borrow ->
    match borrow.receipt.context.operation_id with
    | Some _ -> ()
    | None ->
      borrow.cancel_requested <- true;
      Option.iter borrow.cancel ~f:(fun cancel -> cancel ()))
;;

let stop_internal ?parent_stop_epoch ?managed_receipt t mode =
  let open Result.Let_syntax in
  let%bind extension_work =
    Extension_stop.prepare ~state:t.state ~mode ~now:(t.services.now ())
  in
  match t.state.active_operation with
  | None ->
    let open Result.Let_syntax in
    let%map session =
      match t.state.lifecycle.desired, t.state.lifecycle.observed with
      | Stopped, Stopped
        when (not t.state.pending_initial_start)
             && Option.is_none parent_stop_epoch
             && Option.is_none managed_receipt
             && List.is_empty (stopped_jobs t mode)
             && Extension_stop.is_empty extension_work
             && not
                  (List.exists t.state.invocations ~f:Observation_follow_up.pending
                   || List.exists
                        t.state.moderator_executions
                        ~f:Observation_follow_up.pending_event) ->
        Ok (Session_state.summary t.state)
      | _, _ ->
        stop_transition
          ?parent_stop_epoch
          ?managed_receipt
          t
          mode
          ~extension_work
          { desired = Stopped; observed = Stopped }
          []
          []
    in
    (match mode with
     | Cancel ->
       cancel_independent_moderator t;
       abort_all_staged_work t
     | Graceful -> ());
    session
  | Some operation ->
    let lifecycle =
      Session_state.Lifecycle.{ desired = Stopped; observed = t.state.lifecycle.observed }
    in
    let operation =
      match mode with
      | Agent_protocol.Session.Graceful -> operation
      | Cancel ->
        { operation with
          state = Agent_protocol.Operation.Cancelling
        ; updated_at = t.services.now ()
        }
    in
    let open Result.Let_syntax in
    let%bind session =
      stop_transition
        ?parent_stop_epoch
        ?managed_receipt
        t
        mode
        ~extension_work
        lifecycle
        [ Session_delta.Active_operation_changed (Some operation) ]
        []
    in
    if Agent_protocol.Session.equal_stop_mode mode Cancel
    then (
      abort_all_staged_work t;
      cancel_event_for_operation t operation.id;
      cancel_independent_moderator t;
      Option.iter t.active_cancel ~f:(fun cancel -> cancel ()));
    Ok session
;;

let stop_managed_internal t reference key mode generation max_receipts =
  let module M = Managed_stop in
  let module P = Agent_protocol in
  let module D = Agent_store.Delegation_store in
  let open Result.Let_syntax in
  let%bind () =
    match t.state.spec.delegation with
    | Some current
      when D.Reference.equal current reference
           && P.Id.Session.equal reference.child_session_id t.state.identity.session_id ->
      Ok ()
    | _ ->
      Error (error Permission_denied "delegation.stop: child relationship does not match")
  in
  match
    List.find t.state.managed_stops ~f:(fun receipt ->
      D.Reference.equal receipt.M.reference reference
      && P.Idempotency_key.equal receipt.key key)
  with
  | Some receipt ->
    (match P.Session.equal_stop_mode receipt.mode mode with
     | true -> Ok receipt
     | false -> Error (error Conflict "managed stop key was used for a different mode"))
  | None ->
    let%bind () =
      match Int.equal generation t.state.identity.generation with
      | true -> Ok ()
      | false -> Error (error Conflict "child generation changed before managed stop")
    in
    let%bind () =
      match max_receipts with
      | Some maximum when maximum < 1 || List.length t.state.managed_stops >= maximum ->
        Error (error Invalid_state "managed stop receipt capacity reached")
      | None | Some _ -> Ok ()
    in
    let%bind stop_epoch =
      match t.state.lifecycle.desired with
      | Stopped -> Ok t.state.stop_epoch
      | Running when Int64.equal t.state.stop_epoch Int64.max_value ->
        Error (error Invalid_state "stop epoch overflow")
      | Running -> Ok Int64.(t.state.stop_epoch + 1L)
    in
    let%bind receipt =
      M.create ~reference ~key ~mode ~generation ~stop_epoch ~now:(t.services.now ())
    in
    let%map _ = stop_internal ~managed_receipt:receipt t mode in
    receipt
;;

let append_history t entries =
  transition
    t
    ~delta:(Session_delta.Canonical_entries_appended entries)
    ~payloads:[ Agent_protocol.Event.Durable.Payload.History_appended entries ]
;;

let defer_history ?(extra_deltas = []) t entries =
  let payloads =
    List.map entries ~f:(fun entry ->
      Agent_protocol.Event.Durable.Payload.History_message_deferred entry)
  in
  transition
    t
    ~delta:
      (Session_delta.Batch
         (extra_deltas @ [ Session_delta.Deferred_entries_enqueued entries ]))
    ~payloads
;;

let write_attachment t attachment_id =
  match
    List.find t.state.attachments ~f:(fun attachment ->
      Agent_protocol.Id.Attachment.compare attachment.id attachment_id = 0)
  with
  | None -> Error (error Invalid_request "attachment is not active")
  | Some { mode = Read_only; _ } ->
    Error (error Permission_denied "read-only attachment cannot mutate the session")
  | Some ({ mode = Read_write; _ } as attachment) -> Ok attachment
  | Some ({ mode = Owner_read_write; owner_lease = None; _ } as attachment) ->
    Ok attachment
  | Some ({ mode = Owner_read_write; owner_lease = Some lease; _ } as attachment) ->
    if Option.is_some lease.disconnect_grace_until
    then Error (error Lease_stale "owner attachment is disconnected")
    else if Agent_protocol.Timestamp.compare lease.expires_at (t.services.now ()) <= 0
    then Error (error Lease_stale "owner lease has expired")
    else Ok attachment
;;

let has_writer_attachment t =
  List.exists t.state.attachments ~f:(fun attachment ->
    match attachment.Agent_protocol.Session.Attachment.mode with
    | Read_write -> true
    | Owner_read_write ->
      Option.value_map attachment.owner_lease ~default:true ~f:(fun lease ->
        Option.is_none lease.disconnect_grace_until
        && Agent_protocol.Timestamp.compare lease.expires_at (t.services.now ()) > 0)
    | Read_only -> false)
;;

let grant_is_unexpired t grant =
  match grant.Agent_protocol.Grant.expires_at with
  | None -> true
  | Some expires_at -> Agent_protocol.Timestamp.compare expires_at (t.services.now ()) > 0
;;

let grant_identity_matches grant ~tool_name ~identity_digest =
  match grant.Agent_protocol.Grant.scope with
  | Exact_session | Durable_exact -> String.equal grant.identity_digest identity_digest
  | Prefix_session ->
    String.equal
      grant.identity_digest
      (Permission_policy.prefix_identity_digest ~tool_name)
;;

let invocation_granted t ~tool_name ~identity_digest =
  List.exists t.state.grants ~f:(fun grant ->
    Agent_protocol.Grant.equal_state grant.state Active
    && String.equal grant.tool_name tool_name
    && grant_is_unexpired t grant
    && grant_identity_matches grant ~tool_name ~identity_digest)
;;

let with_writer t attachment_id f =
  Result.bind (write_attachment t attachment_id) ~f:(fun _ -> f ())
;;

let validate_administrative_state t attachment_id expected_revision =
  let open Result.Let_syntax in
  let%bind _ = write_attachment t attachment_id in
  if not (Int64.equal expected_revision t.state.counters.revision)
  then Error (error Conflict "session revision does not match")
  else if Option.is_some t.state.active_operation
  then Error (error Conflict "session has an active foreground operation")
  else if t.idle_moderator_borrowed
  then Error (error Conflict "session moderator is processing background work")
  else if not (List.is_empty t.job_scopes)
  then Error (error Conflict "session has an active background execution scope")
  else (
    match t.state.lifecycle.observed with
    | Agent_protocol.Session.Stopped -> Ok ()
    | Queued_for_slot
    | Starting
    | Recovering
    | Idle
    | Running_turn _
    | Compacting _
    | Waiting_for_permission _
    | Stopping
    | Failed _ ->
      Error (error Invalid_state "administrative mutation requires a stopped session"))
;;

let commit_administration t attachment_id expected_revision kind candidate =
  let open Result.Let_syntax in
  let%bind () = validate_administrative_state t attachment_id expected_revision in
  let ingress_matches =
    match kind with
    | Session_state.Compaction_archive.Reset | Rebuild ->
      t.state.identity.generation < Int.max_value
      && Int.equal
           candidate.Session_state.identity.generation
           (t.state.identity.generation + 1)
      && List.is_empty candidate.ingress_registrations
    | Upgrade | Compaction ->
      List.equal
        External_ingress.equal
        candidate.ingress_registrations
        t.state.ingress_registrations
  in
  let%bind () =
    if
      Agent_protocol.Id.Session.compare
        candidate.Session_state.identity.session_id
        t.state.identity.session_id
      <> 0
      || (not (Int64.equal candidate.counters.revision expected_revision))
      || (not
            (Option.equal
               Automatic_turn_budget.equal
               candidate.automatic_turn_budget
               t.state.automatic_turn_budget))
      || (not ingress_matches)
      || Int64.(
           candidate.conversation.next_history_sequence
           < t.state.conversation.next_history_sequence)
    then
      Error (error Conflict "administrative candidate does not match the captured state")
    else Session_state.validate candidate
  in
  let%bind state = Administration.archive ~previous:t.state candidate kind in
  transition
    t
    ~delta:(Session_delta.Created state)
    ~payloads:(Administration.payloads ~previous:t.state state)
;;

let reset_internal t attachment_id expected_revision options =
  let open Result.Let_syntax in
  let%bind () = validate_administrative_state t attachment_id expected_revision in
  let%bind state = Administration.reset t.state options in
  commit_administration t attachment_id expected_revision Reset state
;;

let upgrade_prompt_internal t attachment_id expected_revision target_revision =
  let open Result.Let_syntax in
  let%bind () = validate_administrative_state t attachment_id expected_revision in
  let previous_revision = t.state.spec.prompt_revision_id in
  if Agent_protocol.Id.Prompt_revision.compare previous_revision target_revision = 0
  then Ok (Session_state.summary t.state)
  else (
    let state =
      { t.state with
        spec = { t.state.spec with prompt_revision_id = target_revision }
      ; moderator = None
      ; shell = Session.Shell_state.empty
      ; failure = None
      }
    in
    transition
      t
      ~delta:(Session_delta.Created state)
      ~payloads:
        [ Agent_protocol.Event.Durable.Payload.Prompt_upgraded
            { prompt_id = Option.value_exn state.spec.prompt_definition_id
            ; previous_revision
            ; current_revision = target_revision
            }
        ; Session_updated (Session_state.summary state)
        ])
;;

let adopt_deferred t =
  let entries = t.state.conversation.deferred_user_entries in
  if List.is_empty entries
  then Ok (Session_state.summary t.state)
  else
    transition
      t
      ~delta:Session_delta.Deferred_entries_adopted
      ~payloads:[ Agent_protocol.Event.Durable.Payload.History_appended entries ]
;;

let reserve_history_block t count =
  if count <= 0
  then Error (error Invalid_request "history reservation count must be positive")
  else if Int64.(t.state.conversation.next_history_sequence > max_value - of_int count)
  then Error (error Invalid_state "history sequence overflow")
  else (
    let first_sequence = t.state.conversation.next_history_sequence in
    let reserved_through = Int64.(first_sequence + of_int count) in
    let open Result.Let_syntax in
    let%map _ =
      transition
        t
        ~delta:(Session_delta.History_block_reserved reserved_through)
        ~payloads:[]
    in
    History_id_source.{ first_sequence; reserved_through })
;;

let current_operation t operation_id =
  match t.state.active_operation with
  | Some operation when Agent_protocol.Id.Operation.compare operation.id operation_id = 0
    -> Ok operation
  | Some _ -> Error (error Conflict "a different foreground operation is active")
  | None -> Error (error Operation_not_found "foreground operation is not active")
;;

let running_operation ?(allow_stopping = false) t operation_id =
  let open Result.Let_syntax in
  let%bind operation = current_operation t operation_id in
  match operation.state, t.state.lifecycle.desired, t.state.lifecycle.observed with
  | Agent_protocol.Operation.Running, desired, Running_turn id
    when (allow_stopping || Agent_protocol.Session.equal_desired_state desired Running)
         && Agent_protocol.Id.Operation.compare id operation_id = 0
         && not t.state.halted -> Ok operation
  | _ ->
    Error (error Invalid_state "foreground operation is not running at a tool safe point")
;;

let invocation_admission_deltas t (invocation : Agent_protocol.Invocation.t) =
  match
    List.find t.state.invocations ~f:(fun current ->
      Agent_protocol.Id.Invocation.compare current.context.id invocation.context.id = 0)
  with
  | None -> Ok [ Session_delta.Invocation_changed invocation ]
  | Some current
    when Agent_protocol.Invocation.equal_origin invocation.context.origin Model
         && Option.is_some invocation.context.call_entry_id
         && Agent_protocol.Invocation.equal current invocation -> Ok []
  | Some _ -> Error (error Conflict "invocation identity is already admitted")
;;

let commit_invocation_call t operation_id (invocation : Agent_protocol.Invocation.t) entry
  =
  let open Result.Let_syntax in
  let%bind _ = current_operation t operation_id in
  let%bind () =
    Extension_invariants.owner
      ~session_id:t.state.identity.session_id
      ~generation:t.state.identity.generation
      invocation.context.session_id
      invocation.context.generation
  in
  let%bind () = Agent_protocol.Invocation.validate invocation in
  let%bind () =
    match
      ( invocation.status
      , invocation.context.origin
      , invocation.context.parent_job
      , invocation.context.parent_invocation
      , invocation.context.call_entry_id )
    with
    | Admitted, Model, None, None, Some id
      when History_entry.Id.equal id (History_entry.id entry) -> Ok ()
    | _ ->
      Error
        (error Invalid_request "call intent requires an admitted root model invocation")
  in
  let encoded = History_codec.to_protocol entry in
  let%bind entries =
    match
      List.find t.state.conversation.canonical_history ~f:(fun item ->
        History_entry.Id.equal item.id encoded.id)
    with
    | None -> Ok [ encoded ]
    | Some existing when Agent_protocol.History.equal_entry existing encoded -> Ok []
    | Some _ -> Error (error Conflict "history call already has different content")
  in
  let%bind admission =
    match
      List.find t.state.invocations ~f:(fun current ->
        Agent_protocol.Id.Invocation.compare current.context.id invocation.context.id = 0)
    with
    | None -> Ok [ Session_delta.Invocation_changed invocation ]
    | Some current
      when List.is_empty entries
           && Agent_protocol.Invocation.equal_context current.context invocation.context
           && Option.equal
                Agent_protocol.Invocation.equal_routing
                current.routing
                invocation.routing -> Ok []
    | Some _ -> Error (error Conflict "call intent identity has different content")
  in
  if List.is_empty entries && List.is_empty admission
  then Ok ()
  else
    transition
      t
      ~delta:
        (Session_delta.Batch
           ((if List.is_empty entries
             then []
             else [ Session_delta.Canonical_entries_appended entries ])
            @ admission))
      ~payloads:
        (if List.is_empty entries
         then []
         else [ Agent_protocol.Event.Durable.Payload.History_appended entries ])
    |> Result.map ~f:(fun _ -> ())
;;

let claim_invocation t operation_id (invocation : Agent_protocol.Invocation.t) =
  let open Result.Let_syntax in
  let%bind _ = running_operation t operation_id in
  let%bind () =
    Extension_invariants.owner
      ~session_id:t.state.identity.session_id
      ~generation:t.state.identity.generation
      invocation.context.session_id
      invocation.context.generation
  in
  let%bind () =
    if
      Option.is_some invocation.context.parent_job
      || Option.is_some invocation.parent_event
    then
      Error
        (error
           Invalid_state
           "foreground invocation cannot borrow background-job ownership")
    else Ok ()
  in
  let%bind admission = invocation_admission_deltas t invocation in
  let%bind owner =
    match invocation.context.parent_invocation with
    | None -> Ok (Foreground operation_id)
    | Some parent ->
      let owned id op =
        Agent_protocol.Id.Invocation.compare id parent = 0
        && Agent_protocol.Id.Operation.compare op operation_id = 0
      in
      (match
         List.find t.invocation_executions ~f:(fun execution ->
           execution.accepts_children
           &&
           match execution.owner with
           | Foreground op -> owned execution.dispatched.context.id op
           | Invocation_moderator borrow ->
             Option.exists borrow.operation_id ~f:(owned execution.dispatched.context.id)
             && (not (borrow.committed || borrow.cancel_requested))
             && Option.exists t.moderator_borrow ~f:(phys_equal borrow)
           | Event_moderator _ | Background_job _ -> false)
       with
       | Some execution -> Ok execution.owner
       | None ->
         (match
            Option.find t.moderator_borrow ~f:(fun borrow ->
              borrow.accepts_children
              && (not borrow.committed)
              && Option.exists borrow.operation_id ~f:(owned borrow.invocation.context.id))
          with
          | Some borrow -> Ok (Invocation_moderator borrow)
          | None ->
            Error (error Conflict "parent invocation is not executing in this operation")))
  in
  let%bind dispatched = Agent_protocol.Invocation.dispatch invocation in
  let%bind _ =
    transition
      t
      ~delta:(Session_delta.Batch (admission @ [ Invocation_changed dispatched ]))
      ~payloads:[]
  in
  let execution = { owner; dispatched; accepts_children = true } in
  t.invocation_executions <- execution :: t.invocation_executions;
  Ok execution
;;

let finish_invocation t execution outcome requests commit_starts =
  let open Result.Let_syntax in
  let owner = Agent_protocol.Job.Invocation execution.dispatched.context.id in
  let%bind cancelled =
    match execution.owner with
    | Foreground operation_id ->
      let%map operation = current_operation t operation_id in
      (match operation.state with
       | Cancelling -> true
       | _ -> false)
    | Invocation_moderator borrow ->
      (match t.moderator_borrow with
       | Some current when phys_equal current borrow ->
         (match borrow.operation_id with
          | None when t.idle_moderator_borrowed ->
            Ok
              (borrow.cancel_requested
               || Option.exists borrow.job_scope ~f:(fun scope -> scope.cancel_requested)
              )
          | Some id ->
            let%map operation = current_operation t id in
            borrow.cancel_requested
            ||
              (match operation.state with
              | Cancelling -> true
              | _ -> false)
          | None -> Error (error Conflict "moderator invocation scope has ended"))
       | _ -> Error (error Conflict "moderator invocation scope has ended"))
    | Event_moderator borrow ->
      (match t.queued_event_borrow with
       | Some current when phys_equal current borrow && t.idle_moderator_borrowed ->
         Ok
           (borrow.cancel_requested
            || Option.exists borrow.job_scope ~f:(fun scope -> scope.cancel_requested)
            || Option.exists t.state.active_operation ~f:(fun operation ->
              Option.exists
                borrow.receipt.context.operation_id
                ~f:(Agent_protocol.Id.Operation.equal operation.id)
              &&
              match operation.state with
              | Cancelling -> true
              | _ -> false))
       | _ -> Error (error Conflict "event invocation scope has ended"))
    | Background_job scope ->
      let%map job = job_scope_current t scope in
      scope.cancel_requested
      ||
        (match job.status with
        | Running | Waiting_permission _ -> false
        | Waiting_completion _ | Queued | Succeeded | Failed _ | Cancelled | Interrupted _
          -> true)
  in
  let%bind () =
    if List.exists t.invocation_executions ~f:(phys_equal execution)
    then Ok ()
    else Error (error Conflict "invocation callback no longer owns its result")
  in
  let outcome =
    match cancelled with
    | true ->
      Agent_protocol.Invocation.Cancelled
        (match execution.owner with
         | Foreground _ -> "operation cancelled"
         | Background_job _ -> "background job cancelled"
         | Invocation_moderator { operation_id = Some _; _ } ->
           "moderator scope cancelled"
         | Invocation_moderator { operation_id = None; _ } | Event_moderator _ ->
           "idle moderator cancelled")
    | false -> outcome
  in
  (* The callback has returned. Even when outcome persistence fails and the
     execution stays registered for cleanup, it cannot authorize new children. *)
  execution.accepts_children <- false;
  (match commit_starts && not cancelled, outcome with
   | true, (Complete _ | Fail _ | Pending _) -> ()
   | false, _ | true, Cancelled _ -> abort_staged_work t ~owner);
  let%bind resolved =
    Agent_protocol.Invocation.resolve
      execution.dispatched
      ~session_id:t.state.identity.session_id
      ~generation:t.state.identity.generation
      outcome
  in
  let%bind resolved =
    match requests, outcome with
    | Some requests, (Complete _ | Fail _ | Pending _) ->
      Agent_protocol.Invocation.record_handler_intent resolved ~requests
    | None, _ | Some _, Cancelled _ -> Ok resolved
  in
  let permissions, permission_deltas, permission_payloads =
    cleanup_invocation_permissions t [ execution.dispatched.context.id ]
  in
  let%bind job_deltas, job_payloads = staged_work_changes t owner in
  let%bind _ =
    transition
      t
      ~delta:
        (Session_delta.Batch
           ((Session_delta.Invocation_changed resolved :: permission_deltas) @ job_deltas))
      ~payloads:(permission_payloads @ job_payloads)
  in
  resolve_cleaned_permission_waiters t permissions;
  t.invocation_executions
  <- List.filter t.invocation_executions ~f:(fun other ->
       not (phys_equal other execution));
  Ok resolved
;;

let claim_moderator_invocation t operation_id (invocation : Agent_protocol.Invocation.t) =
  let open Result.Let_syntax in
  let%bind () =
    match invocation.parent_event with
    | None -> Ok ()
    | Some _ -> Error (error Conflict "event-owned tools require their event scope")
  in
  let%bind _ = running_operation t operation_id in
  if t.idle_moderator_borrowed || Option.is_some t.moderator_borrow
  then Error (error Conflict "moderator is already borrowed")
  else (
    let%bind () =
      Extension_invariants.owner
        ~session_id:t.state.identity.session_id
        ~generation:t.state.identity.generation
        invocation.context.session_id
        invocation.context.generation
    in
    let%bind () =
      match invocation.context.parent_invocation with
      | None when Option.is_none invocation.context.parent_job -> Ok ()
      | Some parent ->
        let parent =
          List.find t.invocation_executions ~f:(fun execution ->
            execution.accepts_children
            && Agent_protocol.Id.Invocation.equal execution.dispatched.context.id parent
            &&
            match execution.owner with
            | Foreground id -> Agent_protocol.Id.Operation.equal id operation_id
            | Invocation_moderator _ | Event_moderator _ | Background_job _ -> false)
        in
        (match invocation.context.origin, parent with
         | Script, Some parent
           when Option.is_none invocation.context.parent_job
                && Option.is_none invocation.context.provider_call_id
                && Option.is_none invocation.context.call_entry_id ->
           (match parent.dispatched.context.deadline, invocation.context.deadline with
            | None, _ -> Ok ()
            | Some parent, Some child
              when Agent_protocol.Timestamp.compare child parent <= 0 -> Ok ()
            | _ ->
              Error (error Conflict "nested moderator cannot extend its parent deadline"))
         | _ -> Error (error Conflict "nested moderator requires an active script caller"))
      | None ->
        Error (error Conflict "foreground moderator cannot borrow background ownership")
    in
    let%bind admission = invocation_admission_deltas t invocation in
    let%bind dispatched = Agent_protocol.Invocation.dispatch invocation in
    let%bind _ =
      transition
        t
        ~delta:(Session_delta.Batch (admission @ [ Invocation_changed dispatched ]))
        ~payloads:[]
    in
    let borrow =
      { operation_id = Some operation_id
      ; job_scope = None
      ; kind = Invocation
      ; invocation = dispatched
      ; committed = false
      ; accepts_children = true
      ; callback_finished = false
      ; cancel = None
      ; cancel_requested = false
      }
    in
    t.moderator_borrow <- Some borrow;
    Ok borrow)
;;

let has_pending_permission t =
  List.exists t.state.permissions ~f:(fun permission ->
    Agent_protocol.Permission.equal_state permission.state Pending)
;;

let idle_actor_available t =
  (not t.idle_moderator_borrowed)
  && Option.is_none t.moderator_borrow
  && Option.is_none t.state.active_operation
  && Agent_protocol.Session.equal_desired_state t.state.lifecycle.desired Running
  && (match t.state.lifecycle.observed with
      | Agent_protocol.Session.Idle -> true
      | _ -> false)
  && (not t.state.halted)
  && Option.is_none t.state.failure
;;

let idle_moderator_eligible t = idle_actor_available t && not (has_pending_permission t)

let claim_queued_event t id operation_id snapshot =
  let open Result.Let_syntax in
  let%bind available =
    match operation_id with
    | None -> Ok (idle_moderator_eligible t)
    | Some id ->
      let%map _ = running_operation t id in
      not (t.idle_moderator_borrowed || moderator_is_borrowed t)
  in
  match available with
  | false -> Ok None
  | true ->
    let%bind receipt, event =
      (match operation_id with
       | None -> Queued_moderator_event.claim
       | Some operation_id -> Queued_moderator_event.claim_foreground ~operation_id)
        ~state:t.state
        ~id
        ~snapshot
        ~now:(t.services.now ())
    in
    let%bind retirement_reason =
      Queued_moderator_event.delivery_retirement_reason
        ~state:t.state
        ~observer:receipt.context.source
        ~event
        ~subscription_expired:(subscription_is_due t)
    in
    let%bind () =
      match retirement_reason with
      | Some _ -> Ok ()
      | None ->
        transition
          t
          ~delta:(Session_delta.Moderator_execution_changed receipt)
          ~payloads:[]
        |> Result.map ~f:ignore
    in
    let borrow =
      { kind = Queued
      ; job_scope = None
      ; receipt
      ; before = snapshot
      ; event
      ; retirement_reason
      ; callback_active = true
      ; committed = false
      ; cancel = None
      ; cancel_requested = false
      }
    in
    t.queued_event_borrow <- Some borrow;
    t.idle_moderator_borrowed <- true;
    Ok (Some borrow)
;;

let claim_ordinary_event t id operation_id snapshot event =
  let open Result.Let_syntax in
  let%bind available =
    match operation_id with
    | None ->
      (match event with
       | Chat_response.Moderation.Event.Session_start | Session_resume ->
         Ok (idle_moderator_eligible t)
       | _ -> Error (error Invalid_state "ordinary event requires an active operation"))
    | Some id ->
      let%map _ = running_operation t id in
      not (t.idle_moderator_borrowed || moderator_is_borrowed t)
  in
  match available with
  | false -> Ok None
  | true ->
    let%bind receipt, event =
      Queued_moderator_event.claim_ordinary
        ~state:t.state
        ~id
        ~snapshot
        ~operation_id
        ~event
        ~now:(t.services.now ())
    in
    let%bind _ =
      transition t ~delta:(Session_delta.Moderator_execution_changed receipt) ~payloads:[]
    in
    let borrow =
      { kind = Ordinary
      ; job_scope = None
      ; receipt
      ; before = snapshot
      ; event
      ; retirement_reason = None
      ; callback_active = true
      ; committed = false
      ; cancel = None
      ; cancel_requested = false
      }
    in
    t.queued_event_borrow <- Some borrow;
    t.idle_moderator_borrowed <- true;
    Ok (Some borrow)
;;

let claim_delegated_event t id delegation snapshot event =
  let open Result.Let_syntax in
  match t.state.lifecycle.desired, t.state.halted, t.state.failure with
  | Stopped, _, _ | _, true, _ | _, _, Some _ -> Ok Delegated_unavailable
  | Running, false, None ->
    if t.idle_moderator_borrowed || moderator_is_borrowed t
    then Ok Delegated_unavailable
    else (
      let%bind claimed =
        Queued_moderator_event.claim_delegated
          ~delegation
          ~state:t.state
          ~id
          ~snapshot
          ~event
          ~now:(t.services.now ())
      in
      match claimed with
      | Replayed receipt -> Ok (Delegated_replay receipt)
      | Claimed (receipt, event) ->
        let%bind _ =
          transition
            t
            ~delta:(Session_delta.Moderator_execution_changed receipt)
            ~payloads:[]
        in
        let borrow =
          { kind = Ordinary
          ; job_scope = None
          ; receipt
          ; before = snapshot
          ; event
          ; retirement_reason = None
          ; callback_active = true
          ; committed = false
          ; cancel = None
          ; cancel_requested = false
          }
        in
        t.queued_event_borrow <- Some borrow;
        t.idle_moderator_borrowed <- true;
        Ok (Delegated_borrow borrow))
;;

let claim_job_event t scope id snapshot event =
  let open Result.Let_syntax in
  let%bind () = job_scope_can_execute t scope in
  let%bind () =
    match t.idle_moderator_borrowed, moderator_is_borrowed t with
    | false, false -> Ok ()
    | _ -> Error (error Conflict "moderator is already borrowed")
  in
  let%bind receipt, event =
    Queued_moderator_event.claim_job
      ~job:
        { job_id = scope.job.id; attempt = scope.job.attempt; deadline = scope.deadline }
      ~state:t.state
      ~id
      ~snapshot
      ~event
      ~now:(t.services.now ())
  in
  let%bind _ =
    transition t ~delta:(Session_delta.Moderator_execution_changed receipt) ~payloads:[]
  in
  let borrow =
    { kind = Ordinary
    ; job_scope = Some scope
    ; receipt
    ; before = snapshot
    ; event
    ; retirement_reason = None
    ; callback_active = true
    ; committed = false
    ; cancel = None
    ; cancel_requested = false
    }
  in
  t.queued_event_borrow <- Some borrow;
  t.idle_moderator_borrowed <- true;
  Ok (Some borrow)
;;

let validate_queued_event_borrow t borrow =
  match t.queued_event_borrow with
  | Some current when phys_equal current borrow && t.idle_moderator_borrowed ->
    let open Result.Let_syntax in
    let%bind () =
      match borrow.job_scope, borrow.receipt.context.operation_id with
      | Some scope, None -> Result.map (job_scope_current t scope) ~f:ignore
      | None, Some id -> Result.map (current_operation t id) ~f:ignore
      | None, None when Option.is_some borrow.receipt.delegation -> Ok ()
      | None, None when Option.is_none t.state.active_operation -> Ok ()
      | _ -> Error (error Conflict "event no longer owns the moderator")
    in
    Extension_invariants.owner
      ~session_id:t.state.identity.session_id
      ~generation:t.state.identity.generation
      borrow.receipt.context.session_id
      borrow.receipt.context.generation
  | _ -> Error (error Conflict "queued event is no longer owned by this callback")
;;

let queued_retirement_available t =
  (not t.idle_moderator_borrowed)
  && (not (moderator_is_borrowed t))
  && Option.is_none t.state.active_operation
  && (not (has_pending_permission t))
  &&
  match t.state.lifecycle.desired, t.state.lifecycle.observed with
  | Running, Idle | Stopped, Stopped -> true
  | _ -> false
;;

let claim_queued_retirement t id snapshot reason =
  let open Result.Let_syntax in
  match queued_retirement_available t with
  | false -> Ok None
  | true ->
    let%bind receipt, event =
      Queued_moderator_event.claim_retirement ~state:t.state ~id ~snapshot
    in
    let%bind _ =
      Agent_protocol.Moderator_execution.retire
        receipt
        ~checkpoint_sha256:receipt.context.checkpoint_sha256
        ~reason
    in
    let borrow =
      { kind = Queued
      ; job_scope = None
      ; receipt
      ; before = snapshot
      ; event
      ; retirement_reason = Some reason
      ; callback_active = true
      ; committed = false
      ; cancel = None
      ; cancel_requested = false
      }
    in
    t.queued_event_borrow <- Some borrow;
    t.idle_moderator_borrowed <- true;
    Ok (Some borrow)
;;

let queued_event_can_commit t borrow =
  let open Result.Let_syntax in
  let%bind () = validate_queued_event_borrow t borrow in
  match borrow.job_scope, borrow.receipt.context.operation_id with
  | Some scope, None ->
    let%bind () = job_scope_can_execute t scope in
    (match
       borrow.callback_active && not (borrow.committed || borrow.cancel_requested)
     with
     | true -> Ok ()
     | false -> Error (error Conflict "job event cannot commit after completion or stop"))
  | Some _, Some _ -> Error (error Conflict "job event cannot borrow a model operation")
  | None, None when Option.is_some borrow.receipt.delegation ->
    (match t.state.lifecycle.desired, t.state.failure with
     | Running, None
       when borrow.callback_active
            && not (borrow.committed || borrow.cancel_requested || t.state.halted) ->
       Ok ()
     | _ ->
       Error (error Conflict "delegated policy cannot commit after completion or stop"))
  | None, Some id ->
    let%bind _ = running_operation t id in
    if
      borrow.callback_active
      && not (borrow.committed || borrow.cancel_requested || t.state.halted)
    then Ok ()
    else Error (error Conflict "foreground event cannot commit after completion or stop")
  | None, None ->
    (match
       ( borrow.retirement_reason
       , t.state.lifecycle.desired
       , t.state.lifecycle.observed
       , t.state.failure )
     with
     | (Some _, Running, Idle, _ | Some _, Stopped, Stopped, _)
       when borrow.callback_active
            && (not (borrow.committed || borrow.cancel_requested))
            && Option.is_none t.state.active_operation -> Ok ()
     | None, Running, Idle, None
       when borrow.callback_active
            && (not (borrow.committed || borrow.cancel_requested || t.state.halted))
            && Option.is_none t.state.active_operation -> Ok ()
     | _ -> Error (error Conflict "queued event cannot commit after completion or stop"))
;;

let event_execution_owned_by borrow execution =
  match execution.owner with
  | Event_moderator owner -> phys_equal owner borrow
  | _ -> false
;;

let active_script_parent t ~owns (invocation : Agent_protocol.Invocation.t) =
  let open Result.Let_syntax in
  let%bind parent =
    match invocation.context.origin, invocation.context.parent_invocation with
    | Script, Some parent
      when Option.is_none invocation.parent_event
           && Option.is_none invocation.context.parent_job
           && Option.is_none invocation.context.provider_call_id
           && Option.is_none invocation.context.call_entry_id ->
      List.find t.invocation_executions ~f:(fun execution ->
        execution.accepts_children
        && owns execution
        && Agent_protocol.Id.Invocation.equal execution.dispatched.context.id parent)
      |> Result.of_option
           ~error:(error Conflict "script parent is not active in this owner")
    | _ -> Error (error Conflict "owned descendants require a script parent link")
  in
  let%bind () =
    Extension_invariants.owner
      ~session_id:parent.dispatched.context.session_id
      ~generation:parent.dispatched.context.generation
      invocation.context.session_id
      invocation.context.generation
  in
  match parent.dispatched.context.deadline, invocation.context.deadline with
  | None, _ -> Ok ()
  | Some parent, Some child when Agent_protocol.Timestamp.compare child parent <= 0 ->
    Ok ()
  | Some _, _ ->
    Error (error Conflict "script descendant cannot extend its parent's deadline")
;;

let compatible_script_observer observer (invocation : Agent_protocol.Invocation.t) =
  match invocation.observation with
  | None -> Ok ()
  | Some observation
    when Agent_protocol.Invocation.equal_observer observer observation.observer -> Ok ()
  | Some _ -> Error (error Conflict "script descendant has a different moderator source")
;;

let claim_event_invocation t borrow (invocation : Agent_protocol.Invocation.t) =
  let open Result.Let_syntax in
  let%bind () = queued_event_can_commit t borrow in
  let%bind () =
    match
      ( Option.bind borrow.job_scope ~f:(fun scope -> scope.deadline)
      , invocation.context.deadline )
    with
    | None, _ -> Ok ()
    | Some parent, Some child when Agent_protocol.Timestamp.compare child parent <= 0 ->
      Ok ()
    | _ -> Error (error Conflict "job event invocation cannot extend its deadline")
  in
  let%bind () =
    match borrow.retirement_reason, invocation.parent_event with
    | None, Some parent
      when Agent_protocol.Id.Moderator_execution.equal parent borrow.receipt.context.id ->
      Ok ()
    | None, None ->
      let%bind () =
        active_script_parent t ~owns:(event_execution_owned_by borrow) invocation
      in
      compatible_script_observer borrow.receipt.context.source invocation
    | _ -> Error (error Conflict "invocation does not belong to this executing event")
  in
  let%bind () =
    Extension_invariants.owner
      ~session_id:t.state.identity.session_id
      ~generation:t.state.identity.generation
      invocation.context.session_id
      invocation.context.generation
  in
  let%bind () =
    Extension_invariants.invocation_event_owner
      ~events:t.state.moderator_executions
      invocation
  in
  let%bind admission = invocation_admission_deltas t invocation in
  let%bind dispatched = Agent_protocol.Invocation.dispatch invocation in
  let%bind _ =
    transition
      t
      ~delta:(Session_delta.Batch (admission @ [ Invocation_changed dispatched ]))
      ~payloads:[]
  in
  let execution =
    { owner = Event_moderator borrow; dispatched; accepts_children = true }
  in
  t.invocation_executions <- execution :: t.invocation_executions;
  Ok execution
;;

let commit_queued_event t borrow snapshot requests decision notifications =
  let open Result.Let_syntax in
  let%bind () = queued_event_can_commit t borrow in
  let%bind () =
    match List.exists t.invocation_executions ~f:(event_execution_owned_by borrow) with
    | true -> Error (error Conflict "event native invocations still require completion")
    | false -> Ok ()
  in
  let%bind receipt, admission_deltas =
    match borrow.retirement_reason, borrow.receipt.status with
    | Some reason, Running ->
      let%bind current_reason =
        Queued_moderator_event.delivery_retirement_reason
          ~state:t.state
          ~observer:borrow.receipt.context.source
          ~event:borrow.event
          ~subscription_expired:(subscription_is_due t)
      in
      let%bind () =
        match current_reason with
        | Some current when String.equal current reason -> Ok ()
        | _ ->
          Error
            (error Conflict "queued delivery retirement no longer matches its admission")
      in
      let%map interrupted =
        Agent_protocol.Moderator_execution.interrupt borrow.receipt ~reason
      in
      ( interrupted
      , [ Session_delta.Moderator_execution_changed borrow.receipt
        ; Moderator_execution_changed interrupted
        ] )
    | _ -> Ok (borrow.receipt, [])
  in
  let%bind completed =
    match borrow.receipt.delegation, decision, borrow.retirement_reason with
    | Some _, Some decision, None ->
      Queued_moderator_event.complete_delegated
        ~decision
        ~claimed:borrow.receipt
        ~before:borrow.before
        ~snapshot
        ~requests
    | Some _, _, _ | None, Some _, _ ->
      Error (error Invalid_state "delegated decision does not match event ownership")
    | None, None, None ->
      (match borrow.kind with
       | Queued -> Queued_moderator_event.complete
       | Ordinary -> Queued_moderator_event.complete_ordinary)
        ~claimed:borrow.receipt
        ~before:borrow.before
        ~snapshot
        ~requests
    | None, None, Some reason ->
      (match
         ( requests.Agent_protocol.Invocation.request_turn
         , requests.request_compaction
         , requests.end_session )
       with
       | false, false, None ->
         Queued_moderator_event.retire
           ~claimed:receipt
           ~before:borrow.before
           ~snapshot
           ~reason
       | _ ->
         Error (error Invalid_state "failed-head retirement cannot schedule new work"))
  in
  let%bind job_deltas, job_payloads =
    staged_work_changes t (Agent_protocol.Job.Moderator_event borrow.receipt.context.id)
  in
  let%bind _ =
    transition
      t
      ~delta:
        (Session_delta.Batch
           (admission_deltas
            @ [ Session_delta.Moderator_execution_changed completed
              ; Moderator_changed
                  (Some (Runtime_builder.encode_moderator_snapshot snapshot))
              ]
            @ job_deltas))
      ~payloads:
        (job_payloads
         @ List.map notifications ~f:(fun message ->
           Agent_protocol.Event.Durable.Payload.Moderator_notification
             (`Object
                 [ "message", `String message
                 ; ( "moderator_execution_id"
                   , Agent_protocol.Id.Moderator_execution.to_json
                       borrow.receipt.context.id )
                 ])))
  in
  borrow.committed <- true;
  Ok ()
;;

let finish_queued_event t borrow interrupted =
  let open Result.Let_syntax in
  let%bind () = validate_queued_event_borrow t borrow in
  borrow.callback_active <- false;
  abort_staged_work t ~owner:(Moderator_event borrow.receipt.context.id);
  borrow.cancel <- None;
  let children =
    List.filter t.invocation_executions ~f:(event_execution_owned_by borrow)
  in
  let permissions, permission_deltas, permission_payloads =
    cleanup_invocation_permissions
      t
      (List.map children ~f:(fun child -> child.dispatched.context.id))
  in
  let%bind children_deltas =
    List.map children ~f:(fun execution ->
      execution.accepts_children <- false;
      abort_staged_work t ~owner:(Invocation execution.dispatched.context.id);
      Agent_protocol.Invocation.cancel
        execution.dispatched
        ~reason:"event exited before recording its native outcome"
      |> Result.map ~f:(fun invocation -> Session_delta.Invocation_changed invocation))
    |> Result.all
  in
  let%bind () =
    match borrow.committed, borrow.retirement_reason with
    | true, _ | false, Some _ -> Ok ()
    | false, None ->
      let%bind terminal =
        match interrupted || borrow.cancel_requested with
        | true ->
          Agent_protocol.Moderator_execution.interrupt
            borrow.receipt
            ~reason:"queued moderator handler interrupted before checkpoint commit"
        | false ->
          Agent_protocol.Moderator_execution.fail
            borrow.receipt
            { code = "event.handler_failed"
            ; message =
                "queued moderator handler exited without committing its checkpoint"
            ; retryable = false
            ; details = `Null
            }
      in
      transition
        t
        ~delta:
          (Session_delta.Batch
             (children_deltas
              @ permission_deltas
              @ [ Moderator_execution_changed terminal ]))
        ~payloads:permission_payloads
      |> Result.map ~f:ignore
  in
  resolve_cleaned_permission_waiters t permissions;
  t.queued_event_borrow <- None;
  t.invocation_executions
  <- List.filter t.invocation_executions ~f:(fun execution ->
       not (event_execution_owned_by borrow execution));
  t.idle_moderator_borrowed <- false;
  Ok ()
;;

let observation_owner_available t operation_id =
  match operation_id with
  | Some operation_id -> Result.map (running_operation t operation_id) ~f:ignore
  | None ->
    if idle_moderator_eligible t
    then Ok ()
    else Error (error Conflict "session is not available for idle observation")
;;

let validate_installed_observer t observer =
  let open Result.Let_syntax in
  let%bind installed = Runtime_builder.moderator_snapshot_observer t.state.moderator in
  match installed, observer with
  | Some installed, Some observer
    when Agent_protocol.Invocation.equal_observer installed observer -> Ok ()
  | _ -> Error (error Conflict "observation source is not the installed moderator")
;;

let claim_moderator_observation t operation_id invocation_id =
  let open Result.Let_syntax in
  let%bind () = observation_owner_available t operation_id in
  let%bind moderator_halted =
    Runtime_builder.moderator_snapshot_is_halted t.state.moderator
  in
  let%bind () =
    match
      t.idle_moderator_borrowed, t.moderator_borrow, t.state.halted || moderator_halted
    with
    | false, None, false -> Ok ()
    | _ -> Error (error Conflict "moderator is borrowed or halted")
  in
  let%bind invocation =
    match
      List.find t.state.invocations ~f:(fun invocation ->
        Agent_protocol.Id.Invocation.equal invocation.context.id invocation_id)
    with
    | Some invocation -> Ok invocation
    | None -> Error (error Invalid_state "observation invocation is not retained")
  in
  let%bind () =
    validate_installed_observer
      t
      (Option.map invocation.observation ~f:(fun observation -> observation.observer))
  in
  let%bind () =
    Extension_invariants.owner
      ~session_id:t.state.identity.session_id
      ~generation:t.state.identity.generation
      invocation.context.session_id
      invocation.context.generation
  in
  let%bind () =
    match invocation.context.parent_invocation with
    | Some parent
      when List.exists t.invocation_executions ~f:(fun execution ->
             Agent_protocol.Id.Invocation.equal execution.dispatched.context.id parent) ->
      Error (error Conflict "observation parent is still executing")
    | _ -> Ok ()
  in
  let%bind observing = Agent_protocol.Invocation.claim_observation invocation in
  let%bind _ =
    transition t ~delta:(Session_delta.Invocation_changed observing) ~payloads:[]
  in
  let borrow =
    { operation_id
    ; job_scope = None
    ; kind = Observation
    ; invocation = observing
    ; committed = false
    ; accepts_children = Option.is_some operation_id
    ; callback_finished = false
    ; cancel = None
    ; cancel_requested = false
    }
  in
  t.moderator_borrow <- Some borrow;
  t.idle_moderator_borrowed <- Option.is_none operation_id;
  Ok borrow
;;

let claim_next_moderator_observation t operation_id observer =
  let open Result.Let_syntax in
  let%bind () = observation_owner_available t operation_id in
  let%bind () = validate_installed_observer t (Some observer) in
  let%bind moderator_halted =
    Runtime_builder.moderator_snapshot_is_halted t.state.moderator
  in
  let%bind () =
    match t.idle_moderator_borrowed, t.moderator_borrow with
    | false, None -> Ok ()
    | _ -> Error (error Conflict "moderator is already borrowed")
  in
  let eligible (invocation : Agent_protocol.Invocation.t) =
    (not (t.state.halted || moderator_halted))
    && invocation.context.generation = t.state.identity.generation
    && Agent_protocol.Id.Session.equal
         invocation.context.session_id
         t.state.identity.session_id
    && (match invocation.observation, invocation.status with
        | Some { status = Awaiting; observer = owner }, (Resolved _ | Published _) ->
          Agent_protocol.Invocation.equal_observer owner observer
        | _ -> false)
    && not
         (Option.exists invocation.context.parent_invocation ~f:(fun parent ->
            List.exists t.invocation_executions ~f:(fun execution ->
              Agent_protocol.Id.Invocation.equal execution.dispatched.context.id parent)))
  in
  let compare (a : Agent_protocol.Invocation.t) (b : Agent_protocol.Invocation.t) =
    match Agent_protocol.Timestamp.compare a.context.created_at b.context.created_at with
    | 0 -> Agent_protocol.Id.Invocation.compare a.context.id b.context.id
    | order -> order
  in
  let selected =
    List.fold t.state.invocations ~init:None ~f:(fun selected candidate ->
      match eligible candidate, selected with
      | false, _ -> selected
      | true, None -> Some candidate
      | true, Some previous when compare candidate previous < 0 -> Some candidate
      | true, Some _ -> selected)
  in
  match selected with
  | None -> Ok None
  | Some invocation ->
    claim_moderator_observation t operation_id invocation.context.id
    |> Result.map ~f:Option.some
;;

let claim_job_moderator t scope invocation =
  let open Result.Let_syntax in
  let%bind () = job_scope_can_execute t scope in
  let%bind () =
    match t.idle_moderator_borrowed, t.moderator_borrow with
    | false, None -> Ok ()
    | _ -> Error (error Conflict "moderator is already borrowed")
  in
  let%bind () = active_script_parent t ~owns:(job_execution_owned_by scope) invocation in
  let%bind admission = invocation_admission_deltas t invocation in
  let%bind dispatched = Agent_protocol.Invocation.dispatch invocation in
  let%bind _ =
    transition
      t
      ~delta:(Session_delta.Batch (admission @ [ Invocation_changed dispatched ]))
      ~payloads:[]
  in
  let borrow =
    { operation_id = None
    ; job_scope = Some scope
    ; kind = Invocation
    ; invocation = dispatched
    ; committed = false
    ; accepts_children = true
    ; callback_finished = false
    ; cancel = None
    ; cancel_requested = false
    }
  in
  t.moderator_borrow <- Some borrow;
  t.idle_moderator_borrowed <- true;
  Ok borrow
;;

let validate_moderator_borrow t borrow =
  match t.moderator_borrow with
  | Some current when phys_equal current borrow ->
    let open Result.Let_syntax in
    let%bind () =
      match borrow.job_scope, borrow.operation_id with
      | Some scope, None -> Result.map (job_scope_current t scope) ~f:ignore
      | None, Some operation_id -> Result.map (current_operation t operation_id) ~f:ignore
      | None, None
        when t.idle_moderator_borrowed && Option.is_none t.state.active_operation -> Ok ()
      | _ -> Error (error Conflict "moderator no longer owns its execution scope")
    in
    Extension_invariants.owner
      ~session_id:t.state.identity.session_id
      ~generation:t.state.identity.generation
      borrow.invocation.context.session_id
      borrow.invocation.context.generation
  | _ -> Error (error Conflict "moderator borrow is no longer owned by this callback")
;;

let invocation_execution_owned_by borrow execution =
  match execution.owner with
  | Invocation_moderator owner -> phys_equal owner borrow
  | Foreground _ | Event_moderator _ | Background_job _ -> false
;;

let background_owner_active t owner =
  let open Result.Let_syntax in
  let moderator (borrow : moderator_borrow) =
    let%bind () = validate_moderator_borrow t borrow in
    match borrow.callback_finished || borrow.committed || borrow.cancel_requested with
    | true -> Error (error Conflict "background launch owner has finished")
    | false ->
      (match borrow.job_scope, borrow.operation_id with
       | Some scope, None -> job_scope_can_execute t scope
       | None, Some id -> Result.map (running_operation t id) ~f:ignore
       | None, None ->
         (match
            t.state.lifecycle.desired, t.state.lifecycle.observed, t.state.failure
          with
          | Running, Idle, None when not t.state.halted -> Ok ()
          | _ -> Error (error Conflict "background launch owner is not running"))
       | Some _, Some _ -> Error (error Conflict "invalid background launch scope"))
  in
  let event borrow =
    let%bind () = queued_event_can_commit t borrow in
    match borrow.retirement_reason with
    | None -> Ok ()
    | Some _ -> Error (error Conflict "event retirement cannot launch background work")
  in
  match owner with
  | Agent_protocol.Job.Moderator_event id ->
    (match t.queued_event_borrow with
     | Some borrow
       when Agent_protocol.Id.Moderator_execution.equal borrow.receipt.context.id id ->
       event borrow
     | _ -> Error (error Conflict "background launch event is not executing"))
  | Invocation id ->
    (match
       List.find t.invocation_executions ~f:(fun execution ->
         execution.accepts_children
         && Agent_protocol.Id.Invocation.equal execution.dispatched.context.id id)
     with
     | Some execution ->
       (match execution.owner with
        | Foreground id -> Result.map (running_operation t id) ~f:ignore
        | Background_job scope -> job_scope_can_execute t scope
        | Invocation_moderator borrow -> moderator borrow
        | Event_moderator borrow -> event borrow)
     | None ->
       (match t.moderator_borrow with
        | Some borrow
          when Agent_protocol.Id.Invocation.equal borrow.invocation.context.id id ->
          moderator borrow
        | _ -> Error (error Conflict "background launch invocation is not executing")))
;;

let subscription_owner_active t owner source =
  let open Result.Let_syntax in
  let%bind () = background_owner_active t owner in
  let%bind () = validate_installed_observer t (Some source) in
  match owner with
  | Agent_protocol.Job.Invocation id ->
    (match t.moderator_borrow with
     | Some borrow when Agent_protocol.Id.Invocation.equal borrow.invocation.context.id id
       ->
       Ok
         (match borrow.kind with
          | Invocation -> Some borrow.invocation
          | Observation -> None)
     | _ ->
       Error
         (error Permission_denied "subscription requires a moderator invocation borrow"))
  | Moderator_event id ->
    (match t.queued_event_borrow with
     | Some borrow
       when Agent_protocol.Id.Moderator_execution.equal borrow.receipt.context.id id
            && Agent_protocol.Invocation.equal_observer
                 borrow.receipt.context.source
                 source -> Ok None
     | _ ->
       Error (error Permission_denied "subscription requires its moderator event borrow"))
;;

let schedule_owned t source (schedule : Agent_protocol.Schedule.t) =
  let open Result.Let_syntax in
  let%bind () =
    Extension_invariants.owner
      ~session_id:t.state.identity.session_id
      ~generation:t.state.identity.generation
      schedule.session_id
      schedule.generation
  in
  match schedule.ownership with
  | Some ownership when Agent_protocol.Invocation.equal_observer ownership.source source
    -> Ok ()
  | _ ->
    Error
      (error
         Permission_denied
         "schedule belongs to a different or unbound moderator source")
;;

let provisional_schedule t owner id =
  match Staged_schedules.find t.staged_schedules ~owner ~id with
  | Some value -> Some value
  | None ->
    List.find t.state.schedules ~f:(fun value ->
      Agent_protocol.Id.Schedule.equal value.id id)
;;

let delay_within_limit ~created_at ~due_at max_delay_ms =
  let ns = Agent_protocol.Timestamp.diff_ns due_at created_at in
  (* Compare rounded-up milliseconds, without converting the policy ceiling to
     a span or overflowing the difference between valid timestamp endpoints. *)
  let milliseconds =
    Int64.((ns / 1_000_000L) + if ns % 1_000_000L = 0L then 0L else 1L)
  in
  Int64.(ns >= 0L && milliseconds <= of_int max_delay_ms)
;;

let stage_schedule_mutation_internal
      t
      owner
      source
      previous
      (next : Agent_protocol.Schedule.t)
  =
  let open Result.Let_syntax in
  let%bind _ = subscription_owner_active t owner source in
  let%bind () = schedule_owned t source next in
  let%bind () = Agent_protocol.Schedule.validate_transition ~previous next in
  let%bind () =
    match
      Option.equal
        (fun a b ->
           Jsonaf.exactly_equal
             (Agent_protocol.Schedule.to_json a)
             (Agent_protocol.Schedule.to_json b))
        previous
        (provisional_schedule t owner next.id)
    with
    | true -> Ok ()
    | false -> Error (error Conflict "schedule changed before staging")
  in
  let%bind () =
    match previous with
    | Some _ -> Ok ()
    | None ->
      let limits = t.services.schedule_limits in
      let%bind () = Staged_schedules.validate_limits limits in
      let%bind () =
        match next.ownership with
        | Some ownership
          when Agent_protocol.Job.equal_launch_owner ownership.creator owner
               && Agent_protocol.Timestamp.compare next.created_at (t.services.now ())
                  <= 0
               && delay_within_limit
                    ~created_at:next.created_at
                    ~due_at:next.next_due_at
                    limits.max_delay_ms -> Ok ()
        | _ ->
          Error
            (error
               Permission_denied
               "schedule creation has a different owner or invalid timing")
      in
      let reserved = Staged_schedules.reservations t.staged_schedules in
      let active schedule =
        match schedule.Agent_protocol.Schedule.status with
        | Scheduled | Delivering -> true
        | _ -> false
      in
      let source_active schedule =
        active schedule
        && Option.exists schedule.ownership ~f:(fun ownership ->
          Agent_protocol.Invocation.equal_observer ownership.source source)
      in
      let%bind () =
        match
          List.count t.state.schedules ~f:active + reserved < limits.max_active
          && List.count t.state.schedules ~f:source_active + reserved
             < limits.max_per_source
          && List.length t.state.schedules + reserved < limits.max_retained
        with
        | true -> Ok ()
        | false -> Error (error Resource_limit "schedule admission capacity exhausted")
      in
      Agent_protocol.Json_codec.validate_limits
        ~max_bytes:limits.max_payload_bytes
        ~max_depth:limits.max_payload_depth
        next.payload
  in
  let%map receipt = Staged_schedules.stage t.staged_schedules ~owner ~previous ~next in
  sync_extension_clock t;
  receipt
;;

let create_script_schedule_internal t owner source delay_ms payload misfire =
  let open Result.Let_syntax in
  let%bind _ = subscription_owner_active t owner source in
  let%bind () =
    match delay_ms >= 0 && delay_ms <= t.services.schedule_limits.max_delay_ms with
    | true -> Ok ()
    | false -> Error (error Invalid_request "schedule delay exceeds host policy")
  in
  let created_at = t.services.now () in
  let monotonic_created_at = t.services.monotonic_now () in
  let%bind next_due_at = Agent_protocol.Timestamp.add_ms created_at delay_ms in
  let schedule : Agent_protocol.Schedule.t =
    { id = Agent_protocol.Id.Schedule.create ()
    ; session_id = t.state.identity.session_id
    ; generation = t.state.identity.generation
    ; payload
    ; created_at
    ; next_due_at
    ; misfire
    ; status = Scheduled
    ; delivery_count = 0
    ; last_delivery_at = None
    ; delivery_cancellation = None
    ; ownership = Some { source; creator = owner; subscription = None }
    }
  in
  let%map receipt = stage_schedule_mutation_internal t owner source None schedule in
  Extension_clock.capture
    t.extension_clock
    (Schedule schedule.id)
    ~now:monotonic_created_at
    ~created_at
    ~due_at:next_due_at;
  receipt, schedule
;;

let subscription_owned t source (subscription : Agent_protocol.Subscription.t) =
  let open Result.Let_syntax in
  let%bind () =
    Extension_invariants.owner
      ~session_id:t.state.identity.session_id
      ~generation:t.state.identity.generation
      subscription.context.session_id
      subscription.context.generation
  in
  match subscription.context.source with
  | Some actual when Agent_protocol.Invocation.equal_observer actual source -> Ok ()
  | _ ->
    Error
      (error
         Permission_denied
         "subscription belongs to a different or unbound moderator source")
;;

let provisional_subscription t owner id =
  match Staged_subscriptions.find t.staged_subscriptions ~owner ~id with
  | Some value -> Some value
  | None -> lookup_subscription t id
;;

let subscription_parent_job t =
  Option.bind t.moderator_borrow ~f:(fun borrow ->
    Option.map borrow.job_scope ~f:(fun scope -> scope.job.id, scope.job.attempt))
;;

let stage_subscription_mutation_internal
      ?sampled_time
      t
      owner
      source
      previous
      (next : Agent_protocol.Subscription.t)
  =
  let open Result.Let_syntax in
  let%bind originating = subscription_owner_active t owner source in
  let%bind () = subscription_owned t source next in
  let%bind () =
    match
      Option.equal
        Agent_protocol.Subscription.equal
        previous
        (provisional_subscription t owner next.context.id)
    with
    | true -> Ok ()
    | false -> Error (error Conflict "subscription changed before staging")
  in
  let%bind () = Agent_protocol.Subscription.validate_transition ~previous next in
  let now, monotonic_now =
    Option.value_or_thunk sampled_time ~default:(fun () ->
      let now = t.services.now () in
      now, t.services.monotonic_now ())
  in
  let%bind () =
    match previous with
    | Some _ -> Ok ()
    | None ->
      let%bind () =
        match originating with
        | Some invocation
          when Agent_protocol.Id.Invocation.equal
                 invocation.context.id
                 next.context.invocation_id ->
          (* Origin is established by the live borrowed invocation, not wall
             timestamp ordering across a possible clock adjustment. *)
          Ok ()
        | _ ->
          Error
            (error
               Permission_denied
               "subscription creation requires its dispatched moderator tool")
      in
      let%bind () =
        match
          Option.equal
            (fun (id, attempt) (other_id, other_attempt) ->
               Agent_protocol.Id.Job.equal id other_id && Int.equal attempt other_attempt)
            next.context.parent_job
            (subscription_parent_job t)
        with
        | true -> Ok ()
        | false ->
          Error
            (error Permission_denied "subscription has a different creating job attempt")
      in
      let limits = t.services.subscription_limits in
      let reserved = Staged_subscriptions.reservations t.staged_subscriptions in
      let active =
        List.count t.state.subscriptions ~f:(fun sub -> Option.is_none sub.result)
      in
      let%bind () =
        match
          active + reserved < limits.max_active
          && List.length t.state.subscriptions + reserved < limits.max_retained
        with
        | true -> Ok ()
        | false ->
          Error (error Resource_limit "subscription admission capacity exhausted")
      in
      (match
         Agent_protocol.Timestamp.compare next.context.created_at now <= 0
         && Agent_protocol.Timestamp.compare next.context.deadline now > 0
         && delay_within_limit
              ~created_at:next.context.created_at
              ~due_at:next.context.deadline
              limits.max_lifetime_ms
       with
       | true -> Ok ()
       | false ->
         Error (error Invalid_request "subscription lifetime exceeds host policy"))
  in
  let%bind () =
    match previous with
    | None | Some { result = Some _; _ } -> Ok ()
    | Some previous ->
      let%bind due = subscription_is_due_at t previous ~now ~monotonic_now in
      let%bind () =
        match next.result with
        | None -> Ok ()
        | Some completion ->
          let latest = subscription_terminal_time_at previous completion ~now in
          (match
             Option.exists next.completed_at ~f:(fun at ->
               Agent_protocol.Timestamp.compare at latest > 0)
           with
           | true ->
             Error (error Invalid_request "subscription completion is in the future")
           | false -> Ok ())
      in
      (match due, next.result with
       | true, Some Expired | false, (None | Some (Succeeded _ | Failed _ | Cancelled _))
         -> Ok ()
       | false, Some Expired ->
         Error (error Conflict "subscription has not reached its elapsed deadline")
       | true, _ ->
         Error (error Conflict "subscription deadline has passed; record expiry"))
  in
  let%bind () =
    match next.result, next.context.completion_schema with
    | Some (Succeeded payload), Some schema ->
      let%bind schema =
        Chatmd_shell_spec.Tool_schema.compile schema
        |> Result.map_error ~f:(fun _ ->
          error Invalid_request "invalid subscription completion schema")
      in
      Chatmd_shell_spec.Tool_schema.validate schema payload
      |> Result.map_error ~f:(fun _ ->
        error Invalid_request "subscription completion schema mismatch")
    | _ -> Ok ()
  in
  let%map receipt =
    Staged_subscriptions.stage t.staged_subscriptions ~owner ~previous ~next
  in
  sync_extension_clock t;
  receipt
;;

let create_script_subscription_internal
      t
      owner
      source
      kind
      lifetime_ms
      wake
      completion_schema
  =
  let open Result.Let_syntax in
  let%bind originating = subscription_owner_active t owner source in
  let%bind invocation =
    Result.of_option
      originating
      ~error:
        (error
           Permission_denied
           "subscription creation requires a dispatched moderator tool")
  in
  let%bind () =
    match
      lifetime_ms > 0 && lifetime_ms <= t.services.subscription_limits.max_lifetime_ms
    with
    | true -> Ok ()
    | false -> Error (error Invalid_request "subscription lifetime exceeds host policy")
  in
  let created_at = t.services.now () in
  let monotonic_created_at = t.services.monotonic_now () in
  let%bind deadline = Agent_protocol.Timestamp.add_ms created_at lifetime_ms in
  let%bind subscription =
    Agent_protocol.Subscription.create
      { id = Agent_protocol.Id.Subscription.create ()
      ; session_id = invocation.context.session_id
      ; generation = invocation.context.generation
      ; invocation_id = invocation.context.id
      ; source = Some source
      ; parent_job = subscription_parent_job t
      ; kind
      ; created_at
      ; deadline
      ; completion_schema
      ; wake
      ; ingress_capability = None
      }
  in
  let%map receipt =
    stage_subscription_mutation_internal t owner source None subscription
  in
  Extension_clock.capture
    t.extension_clock
    (Subscription subscription.context.id)
    ~now:monotonic_created_at
    ~created_at
    ~due_at:deadline;
  receipt, subscription
;;

let ingress_owned t source (value : External_ingress.t) =
  let open Result.Let_syntax in
  let%bind () =
    Extension_invariants.owner
      ~session_id:t.state.identity.session_id
      ~generation:t.state.identity.generation
      value.context.session_id
      value.context.generation
  in
  match Agent_protocol.Invocation.equal_observer source value.context.source with
  | true -> Ok ()
  | false ->
    Error
      (error Permission_denied "ingress registration belongs to another moderator source")
;;

let provisional_ingress t owner id =
  match Staged_ingress.find t.staged_ingress ~owner ~id with
  | Some _ as value -> value
  | None -> lookup_ingress t id
;;

let read_script_ingress_internal t owner source id =
  let open Result.Let_syntax in
  let%bind _ = subscription_owner_active t owner source in
  let%bind value =
    provisional_ingress t owner id
    |> Result.of_option
         ~error:(error Invalid_state "ingress registration is not retained")
  in
  let%map () = ingress_owned t source value in
  value
;;

let create_script_ingress_internal
      t
      owner
      source
      subscription_id
      expected_epoch
      namespace
      schema
  =
  let open Result.Let_syntax in
  let%bind _ = subscription_owner_active t owner source in
  let%bind subscription =
    provisional_subscription t owner subscription_id
    |> Result.of_option
         ~error:(error Invalid_state "ingress subscription is not retained")
  in
  let%bind () = subscription_owned t source subscription in
  let%bind () =
    match subscription.epoch = expected_epoch && Option.is_none subscription.result with
    | true -> Ok ()
    | false ->
      Error (error Conflict "ingress subscription is terminal or has a different epoch")
  in
  let%bind due = subscription_is_due t subscription in
  let%bind () =
    match due with
    | false -> Ok ()
    | true ->
      Error (error Conflict "ingress subscription has reached its elapsed deadline")
  in
  let%bind producer =
    t.state.identity.creating_principal
    |> Result.of_option
         ~error:
           (error
              Permission_denied
              "ingress registration requires a recorded creating principal")
  in
  let%bind next =
    External_ingress.create
      { id = Agent_protocol.Id.Capability.create ()
      ; session_id = t.state.identity.session_id
      ; generation = t.state.identity.generation
      ; subscription_id
      ; epoch = expected_epoch
      ; source
      ; producer
      ; namespace
      ; schema
      ; created_at = t.services.now ()
      ; expires_at = subscription.context.deadline
      ; limits = t.services.ingress_limits.registration
      }
      ~subscription
  in
  let%bind () =
    Staged_ingress.check_capacity
      ~limits:t.services.ingress_limits
      ~generation:t.state.identity.generation
      ~now:(t.services.now ())
      ~subscriptions:
        (t.state.subscriptions @ Staged_subscriptions.values t.staged_subscriptions)
      ~values:
        ((next :: t.state.ingress_registrations) @ Staged_ingress.values t.staged_ingress)
  in
  let%map receipt =
    Staged_ingress.stage t.staged_ingress ~owner ~previous:None ~next ~subscription
  in
  receipt, next
;;

let revoke_script_ingress_internal t owner source id reason =
  let open Result.Let_syntax in
  let%bind previous = read_script_ingress_internal t owner source id in
  let%bind subscription =
    provisional_subscription t owner previous.context.subscription_id
    |> Result.of_option
         ~error:(error Invalid_state "ingress subscription is not retained")
  in
  let%bind next = External_ingress.revoke previous ~reason in
  let%map receipt =
    Staged_ingress.stage
      t.staged_ingress
      ~owner
      ~previous:(Some previous)
      ~next
      ~subscription
  in
  receipt, next
;;

let notification_owned t source (delivery : Agent_protocol.Delivery.t) =
  let open Result.Let_syntax in
  let%bind () =
    Extension_invariants.owner
      ~session_id:t.state.identity.session_id
      ~generation:t.state.identity.generation
      delivery.context.session_id
      delivery.context.generation
  in
  match delivery.context.ownership with
  | Some ownership when Agent_protocol.Invocation.equal_observer ownership.source source
    -> Ok ()
  | _ ->
    Error
      (error
         Permission_denied
         "notification belongs to a different or unbound moderator source")
;;

let provisional_notification t owner id =
  match Staged_notifications.find t.staged_notifications ~owner ~id with
  | Some value -> Some value
  | None -> lookup_notification t id
;;

let create_script_notification_internal
      t
      owner
      source
      (correlation : Chat_response.Notification_operations.correlation)
      completion
      wake
      disclosure_pins
  =
  let open Result.Let_syntax in
  let module P = Agent_protocol in
  let%bind _ = subscription_owner_active t owner source in
  let limits = t.services.notification_limits in
  let%bind () = P.Completion.validate completion in
  let%bind () =
    P.Json_codec.validate_limits
      ~max_bytes:limits.max_payload_bytes
      ~max_depth:limits.max_payload_depth
      (P.Completion.to_json completion)
  in
  let same_session session_id generation =
    Extension_invariants.owner
      ~session_id:t.state.identity.session_id
      ~generation:t.state.identity.generation
      session_id
      generation
  in
  let%bind () =
    match correlation.invocation_id with
    | None -> Ok ()
    | Some id ->
      let%bind invocation =
        Result.of_option
          (List.find t.state.invocations ~f:(fun invocation ->
             P.Id.Invocation.equal invocation.context.id id))
          ~error:(error Invalid_request "notification invocation is not retained")
      in
      same_session invocation.context.session_id invocation.context.generation
  in
  let%bind () =
    match correlation.work with
    | None -> Ok ()
    | Some (P.Invocation.Subscription id) ->
      let%bind subscription =
        Result.of_option
          (provisional_subscription t owner id)
          ~error:(error Invalid_request "notification subscription is not retained")
      in
      let%bind () = subscription_owned t source subscription in
      let%bind () =
        match correlation.invocation_id with
        | Some id when P.Id.Invocation.equal id subscription.context.invocation_id ->
          Ok ()
        | _ ->
          Error
            (error
               Permission_denied
               "notification must retain its subscription's invocation")
      in
      (match subscription.result with
       | Some actual when P.Completion.equal actual completion -> Ok ()
       | _ ->
         Error
           (error
              Invalid_request
              "notification differs from its terminal subscription result"))
    | Some (Job id) ->
      let%bind job =
        Result.of_option
          (List.find t.state.jobs ~f:(fun job -> P.Id.Job.equal job.id id))
          ~error:(error Invalid_request "notification job is not retained")
      in
      let%bind () = same_session job.session_id job.generation in
      let%bind result = P.Job.terminal_result job in
      let%bind result =
        Result.of_option
          result
          ~error:(error Invalid_request "notification job is not terminal")
      in
      let%bind matches = P.Stored_completion.matches result completion in
      (match matches with
       | true -> Ok ()
       | false ->
         Error (error Invalid_request "notification differs from its terminal job result"))
  in
  let retained =
    t.state.deliveries @ Staged_notifications.values t.staged_notifications
  in
  let%bind () =
    match correlation.work with
    | None -> Ok ()
    | Some work ->
      (match
         List.exists retained ~f:(fun delivery ->
           Option.exists delivery.context.work ~f:(fun actual ->
             P.Invocation.compare_work actual work = 0))
       with
       | true -> Error (error Conflict "terminal work already has a delivery owner")
       | false -> Ok ())
  in
  let pending delivery =
    match delivery.P.Delivery.status with
    | Pending -> true
    | Committed _ | Failed _ -> false
  in
  let same_source delivery =
    Option.exists delivery.P.Delivery.context.ownership ~f:(fun ownership ->
      P.Invocation.equal_observer ownership.source source)
  in
  let%bind () =
    match
      List.count retained ~f:pending < limits.max_pending
      && List.count retained ~f:(fun delivery -> pending delivery && same_source delivery)
         < limits.max_per_source
      && List.length retained < limits.max_retained
    with
    | true -> Ok ()
    | false -> Error (error Resource_limit "notification admission capacity exhausted")
  in
  let%bind delivery =
    P.Delivery.create
      ?disclosure_pins
      { id = P.Id.Delivery.create ()
      ; session_id = t.state.identity.session_id
      ; generation = t.state.identity.generation
      ; invocation_id = correlation.invocation_id
      ; work = correlation.work
      ; correlation = correlation.key
      ; source = Moderator
      ; completion
      ; wake
      ; created_at = t.services.now ()
      ; ownership = Some { source; creator = owner }
      }
  in
  let%map receipt =
    Staged_notifications.stage t.staged_notifications ~owner ~previous:None ~next:delivery
  in
  receipt, delivery
;;

let finish_script_subscription_internal t owner source id expected_epoch completion =
  let open Result.Let_syntax in
  let%bind _ = subscription_owner_active t owner source in
  let%bind previous =
    Result.of_option
      (provisional_subscription t owner id)
      ~error:(error Invalid_state "subscription is not retained")
  in
  let%bind () = subscription_owned t source previous in
  let now = t.services.now () in
  let monotonic_now = t.services.monotonic_now () in
  let%bind completion =
    match previous.result with
    | Some winner -> Ok winner
    | None ->
      let%map due = subscription_is_due_at t previous ~now ~monotonic_now in
      (match due with
       | true -> Agent_protocol.Completion.Expired
       | false -> completion)
  in
  let%bind next, _ =
    Agent_protocol.Subscription.finish
      previous
      ~expected_epoch
      ~now:(subscription_terminal_time_at previous completion ~now)
      completion
  in
  let%map receipt =
    stage_subscription_mutation_internal
      ~sampled_time:(now, monotonic_now)
      t
      owner
      source
      (Some previous)
      next
  in
  receipt, next
;;

let derive_background_launch t owner =
  let open Result.Let_syntax in
  let%bind () = background_owner_active t owner in
  let%bind launch =
    Job_launch.derive
      ~session_id:t.state.identity.session_id
      ~generation:t.state.identity.generation
      ~invocations:t.state.invocations
      ~events:t.state.moderator_executions
      ~jobs:t.state.jobs
      ~owner
  in
  let%map moderator_source =
    let%bind observer = Runtime_builder.moderator_snapshot_observer t.state.moderator in
    match observer with
    | Some source ->
      (match subscription_owner_active t owner source with
       | Ok _ -> Ok (Some source)
       | Error _ -> Ok None)
    | None -> Ok None
  in
  { launch with moderator_source }
;;

let prepare_background_job t owner request =
  let open Result.Let_syntax in
  let%map launch = derive_background_launch t owner in
  Agent_protocol.Job.
    { id = Agent_protocol.Id.Job.create ()
    ; session_id = t.state.identity.session_id
    ; generation = t.state.identity.generation
    ; kind = Async_tool
    ; payload = Chat_response.Background_request.to_json request
    ; status = Queued
    ; retry_policy = Never
    ; attempt = 0
    ; created_at = t.services.now ()
    ; started_at = None
    ; next_run_at = None
    ; completed_at = None
    ; result = None
    ; delivery = Pending
    ; launch = Some launch
    ; progress = None
    }
;;

let stage_background_job_internal t (job : Agent_protocol.Job.t) capacity =
  let open Result.Let_syntax in
  let result =
    let%bind launch =
      Result.of_option job.launch ~error:(error Invalid_request "job has no launch owner")
    in
    let%bind expected = derive_background_launch t launch.owner in
    let%bind () =
      Extension_invariants.owner
        ~session_id:t.state.identity.session_id
        ~generation:t.state.identity.generation
        job.session_id
        job.generation
    in
    let%bind () =
      match
        ( Agent_protocol.Job.equal_launch launch expected
        , job.kind
        , job.status
        , job.retry_policy
        , job.attempt
        , job.started_at
        , job.next_run_at
        , job.completed_at
        , job.result
        , job.delivery )
      with
      | true, Async_tool, Queued, Never, 0, None, None, None, None, Pending -> Ok ()
      | _ ->
        Error
          (error
             Invalid_request
             "staged launch must be a fresh job with current ancestry")
    in
    let%bind _ = Agent_protocol.Job.of_json (Agent_protocol.Job.to_json job) in
    let%bind () =
      match
        List.exists t.state.jobs ~f:(fun existing ->
          Agent_protocol.Id.Job.equal existing.id job.id)
      with
      | true -> Error (error Conflict "background job already exists")
      | false -> Ok ()
    in
    Staged_jobs.stage t.staged_jobs ~job ~capacity
  in
  match result with
  | Ok () -> Ok ()
  | Error _ as failure ->
    Eio.Cancel.protect capacity.Staged_jobs.abort;
    failure
;;

let claim_idle_invocation t borrow (invocation : Agent_protocol.Invocation.t) =
  let open Result.Let_syntax in
  let%bind () = validate_moderator_borrow t borrow in
  let%bind () =
    match borrow.invocation.context.deadline, invocation.context.deadline with
    | None, _ -> Ok ()
    | Some parent, Some child when Agent_protocol.Timestamp.compare child parent <= 0 ->
      Ok ()
    | _ -> Error (error Conflict "moderator child cannot extend its parent's deadline")
  in
  let%bind () =
    match
      ( borrow.job_scope
      , borrow.operation_id
      , t.state.lifecycle.desired
      , t.state.lifecycle.observed )
    with
    | Some scope, None, _, _
      when borrow.accepts_children && not (borrow.committed || borrow.cancel_requested) ->
      job_scope_can_execute t scope
    | None, None, Running, Idle
      when borrow.accepts_children
           && (not (borrow.committed || borrow.cancel_requested))
           && (not t.state.halted)
           && Option.is_none t.state.failure -> Ok ()
    | _ -> Error (error Conflict "idle moderator cannot admit another invocation")
  in
  let%bind () =
    Extension_invariants.owner
      ~session_id:t.state.identity.session_id
      ~generation:t.state.identity.generation
      invocation.context.session_id
      invocation.context.generation
  in
  let%bind () =
    match
      ( invocation.context.origin
      , invocation.context.parent_invocation
      , invocation.context.parent_job
      , invocation.observation
      , borrow.invocation.observation )
    with
    | Moderator, Some parent, None, Some child, Some observed
      when Agent_protocol.Id.Invocation.equal parent borrow.invocation.context.id
           && Agent_protocol.Invocation.equal_observer child.observer observed.observer ->
      Ok ()
    | Script, Some _, None, _, Some observed ->
      let%bind () =
        active_script_parent t ~owns:(invocation_execution_owned_by borrow) invocation
      in
      compatible_script_observer observed.observer invocation
    | _ -> Error (error Conflict "idle invocation must belong to its observing moderator")
  in
  let%bind admission = invocation_admission_deltas t invocation in
  let%bind dispatched = Agent_protocol.Invocation.dispatch invocation in
  let%bind _ =
    transition
      t
      ~delta:(Session_delta.Batch (admission @ [ Invocation_changed dispatched ]))
      ~payloads:[]
  in
  let execution =
    { owner = Invocation_moderator borrow; dispatched; accepts_children = true }
  in
  t.invocation_executions <- execution :: t.invocation_executions;
  Ok execution
;;

let commit_moderator_invocation t borrow (resolved : Agent_protocol.Invocation.t) snapshot
  =
  let open Result.Let_syntax in
  let%bind () = validate_moderator_borrow t borrow in
  let%bind () =
    match borrow.callback_finished with
    | false -> Ok ()
    | true -> Error (error Conflict "moderator callback has ended")
  in
  let%bind () =
    match
      List.exists t.invocation_executions ~f:(invocation_execution_owned_by borrow)
    with
    | true ->
      Error (error Conflict "moderator child invocations still require completion")
    | false -> Ok ()
  in
  let%bind () =
    match borrow.job_scope, borrow.operation_id with
    | Some scope, None -> job_scope_can_execute t scope
    | Some _, Some _ ->
      Error (error Conflict "job moderator cannot own a model operation")
    | None, Some operation_id ->
      Result.map (running_operation ~allow_stopping:true t operation_id) ~f:ignore
    | None, None ->
      (match t.state.lifecycle.desired, t.state.lifecycle.observed, t.state.failure with
       | Running, Idle, None when not t.state.halted -> Ok ()
       | _ -> Error (error Conflict "idle observation was stopped before acknowledgement"))
  in
  let%bind () =
    if borrow.committed
    then Error (error Already_resolved "moderator invocation is already committed")
    else if
      Agent_protocol.Id.Invocation.compare
        resolved.context.id
        borrow.invocation.context.id
      <> 0
    then Error (error Conflict "resolution does not belong to this moderator borrow")
    else (
      match borrow.kind, resolved.status with
      | Invocation, Resolved _ ->
        Agent_protocol.Invocation.validate_transition
          ~previous:(Some borrow.invocation)
          resolved
      | Observation, (Resolved _ | Published _) ->
        let%bind () =
          match resolved.observation with
          | Some { status = Observed; observer }
            when String.equal
                   observer.script_id
                   snapshot.Session.Moderator_state.Identity_snapshot.script_id
                 && String.equal observer.source_sha256 snapshot.script_source_hash ->
            Ok ()
          | _ ->
            Error
              (error
                 Conflict
                 "observation acknowledgement requires its source-bound moderator \
                  checkpoint")
        in
        Agent_protocol.Invocation.validate_transition
          ~previous:(Some borrow.invocation)
          resolved
      | _ -> Error (error Invalid_state "moderator commit requires a resolved invocation"))
  in
  let owner = Agent_protocol.Job.Invocation borrow.invocation.context.id in
  (match borrow.kind, resolved.status with
   | Invocation, Resolved (Cancelled _) -> abort_staged_work t ~owner
   | _ -> ());
  let%bind job_deltas, job_payloads = staged_work_changes t owner in
  let%bind _ =
    transition
      t
      ~delta:
        (Session_delta.Batch
           ([ Session_delta.Invocation_changed resolved
            ; Moderator_changed
                (Some (Runtime_builder.encode_moderator_snapshot snapshot))
            ]
            @ job_deltas))
      ~payloads:job_payloads
  in
  borrow.committed <- true;
  Ok ()
;;

let uncommitted_borrow_delta t (borrow : moderator_borrow) failure =
  if borrow.committed
  then Ok (Session_delta.Batch [])
  else
    let open Result.Let_syntax in
    let outcome =
      Option.value
        failure
        ~default:
          (Agent_protocol.Invocation.Fail
             { code = "invocation.unhandled"
             ; message = "moderator callback returned without committing a resolution"
             ; retryable = false
             ; details = `Null
             })
    in
    let outcome =
      match borrow.operation_id, t.state.active_operation with
      | Some id, Some { id = current; state = Cancelling; _ }
        when Agent_protocol.Id.Operation.equal id current ->
        Agent_protocol.Invocation.Cancelled "operation cancelled"
      | _ -> outcome
    in
    let%map resolved =
      match borrow.kind with
      | Invocation ->
        Agent_protocol.Invocation.resolve
          borrow.invocation
          ~session_id:t.state.identity.session_id
          ~generation:t.state.identity.generation
          outcome
      | Observation ->
        Agent_protocol.Invocation.fail_observation
          borrow.invocation
          ~reason:
            (match outcome with
             | Cancelled _ -> "observation handler cancelled before acknowledgement"
             | _ -> "observation handler failed before acknowledgement")
    in
    Session_delta.Invocation_changed resolved
;;

let finish_moderator_invocation t borrow failure =
  let open Result.Let_syntax in
  let%bind () = validate_moderator_borrow t borrow in
  borrow.callback_finished <- true;
  abort_staged_work t ~owner:(Invocation borrow.invocation.context.id);
  borrow.cancel <- None;
  borrow.accepts_children <- false;
  let was_committed = borrow.committed in
  let unfinished =
    List.filter t.invocation_executions ~f:(invocation_execution_owned_by borrow)
  in
  let permissions, permission_deltas, permission_payloads =
    cleanup_invocation_permissions
      t
      (borrow.invocation.context.id
       :: List.map unfinished ~f:(fun child -> child.dispatched.context.id))
  in
  let%bind children =
    List.map unfinished ~f:(fun execution ->
      execution.accepts_children <- false;
      abort_staged_work t ~owner:(Invocation execution.dispatched.context.id);
      Agent_protocol.Invocation.cancel
        execution.dispatched
        ~reason:
          (match borrow.operation_id with
           | None -> "idle moderator exited before recording the invocation outcome"
           | Some _ -> "moderator exited before recording the invocation outcome")
      |> Result.map ~f:(fun invocation -> Session_delta.Invocation_changed invocation))
    |> Result.all
  in
  let%bind delta = uncommitted_borrow_delta t borrow failure in
  let%bind () =
    if was_committed && List.is_empty children && List.is_empty permission_deltas
    then Ok ()
    else
      Result.map
        (transition
           t
           ~delta:(Session_delta.Batch (children @ permission_deltas @ [ delta ]))
           ~payloads:permission_payloads)
        ~f:(fun _ -> ())
  in
  resolve_cleaned_permission_waiters t permissions;
  t.invocation_executions
  <- List.filter t.invocation_executions ~f:(fun execution ->
       not (invocation_execution_owned_by borrow execution));
  t.moderator_borrow <- None;
  (match borrow.operation_id with
   | None -> t.idle_moderator_borrowed <- false
   | Some _ -> ());
  if (not was_committed) && Option.is_none failure
  then
    Error
      (error Invalid_state "moderator callback returned without committing a resolution")
  else Ok ()
;;

let operation_state t (operation : Agent_protocol.Operation.t) state =
  { operation with state; updated_at = t.services.now () }
;;

let summary_with_operation t operation =
  Session_state.summary { t.state with active_operation = Some operation }
;;

let lifecycle_for_operation t operation_id =
  Session_state.Lifecycle.
    { desired = t.state.lifecycle.desired; observed = Running_turn operation_id }
;;

let lifecycle_for_compaction t operation_id =
  Session_state.Lifecycle.
    { desired = t.state.lifecycle.desired; observed = Compacting operation_id }
;;

let commit_worker_entry t operation_id entry =
  let open Result.Let_syntax in
  let%bind _ = current_operation t operation_id in
  let protocol_entry = History_codec.to_protocol entry in
  match
    List.find t.state.conversation.canonical_history ~f:(fun existing ->
      History_entry.Id.equal existing.id protocol_entry.id)
  with
  | None ->
    let%map _ = append_history t [ protocol_entry ] in
    ()
  | Some existing ->
    if
      Sexp.equal
        ([%sexp_of: Agent_protocol.History.entry] existing)
        ([%sexp_of: Agent_protocol.History.entry] protocol_entry)
    then Ok ()
    else Error (error Conflict "history ID was committed with a different payload")
;;

let prepare_authoring_input t operation_id materialization history effective =
  let module M = Chat_response.Authoring_materialization in
  let open Result.Let_syntax in
  let%bind _ = running_operation t operation_id in
  let%bind () =
    let committed = t.state.conversation.canonical_history in
    let supplied = History_codec.all_to_protocol ~previous:committed history in
    match List.equal Agent_protocol.History.equal_entry committed supplied with
    | true -> Ok ()
    | false -> Error (error Conflict "authoring input uses stale canonical history")
  in
  let%bind () =
    match
      String.equal
        (M.scope materialization)
        (M.session_scope
           ~session_id:t.state.identity.session_id
           ~generation:t.state.identity.generation)
    with
    | true -> Ok ()
    | false ->
      Error (error Conflict "authoring context belongs to a different session generation")
  in
  let canonical =
    History_codec.canonical_encoder ~previous:t.state.conversation.canonical_history
  in
  let effective =
    List.map effective ~f:(fun value ->
      let module E = Chat_response.Moderation.Effective_entry in
      match value.E.provenance with
      | Canonical -> canonical value.entry
      | Moderator_inserted _ ->
        History_codec.to_protocol ~provenance:Moderator_inserted value.entry
      | Moderator_replacement { target_id; _ } ->
        History_codec.to_protocol ~provenance:(Moderator_replaced target_id) value.entry)
  in
  let%bind references = Session_state.authoring_references t.state in
  let%bind messages =
    M.refresh
      materialization
      ~known:(Chat_response.Authoring_reference_index.receipts references)
      ~effective
  in
  match messages with
  | [] -> Ok []
  | _ ->
    let first =
      Int64.max
        t.state.conversation.next_history_sequence
        t.state.conversation.reserved_history_through
    in
    let count = List.length messages in
    let%bind () =
      match Int64.(first >= 0L && first <= of_int Int.(max_value - count)) with
      | true -> Ok ()
      | false -> Error (error Invalid_state "authoring history sequence overflow")
    in
    let%bind entries =
      List.mapi messages ~f:(fun index message ->
        History_entry.Id.create
          ~namespace:(Agent_protocol.Id.Session.to_string t.state.identity.session_id)
          ~sequence:(Int64.to_int_exn first + index)
        |> Result.map_error ~f:(error Invalid_state)
        |> Result.map ~f:(fun id -> M.entry message ~id))
      |> Result.all
    in
    let%bind returned = History_codec.all_of_protocol entries in
    let%map _ =
      transition
        t
        ~delta:
          (Session_delta.Batch
             [ History_block_reserved Int64.(first + of_int count)
             ; Canonical_entries_appended entries
             ])
        ~payloads:[ Agent_protocol.Event.Durable.Payload.History_appended entries ]
    in
    returned
;;

let publish_invocation_output t operation_id invocation_id entry =
  let open Result.Let_syntax in
  let%bind _ = running_operation ~allow_stopping:true t operation_id in
  let%bind invocation =
    match
      List.find t.state.invocations ~f:(fun i ->
        Agent_protocol.Id.Invocation.compare i.context.id invocation_id = 0)
    with
    | Some i -> Ok i
    | None -> Error (error Invalid_state "invocation has not been admitted")
  in
  let%bind () =
    Extension_invariants.owner
      ~session_id:t.state.identity.session_id
      ~generation:t.state.identity.generation
      invocation.context.session_id
      invocation.context.generation
  in
  let%bind () = Invocation_history.validate_output invocation entry in
  let%bind published =
    Agent_protocol.Invocation.publish_with_history
      invocation
      ~output_entry_id:(History_entry.id entry)
  in
  let protocol_entry = History_codec.to_protocol entry in
  let existing =
    List.find t.state.conversation.canonical_history ~f:(fun e ->
      History_entry.Id.equal e.id protocol_entry.id)
  in
  let%bind () =
    match existing with
    | Some e
      when not
             (Sexp.equal
                ([%sexp_of: Agent_protocol.History.entry] e)
                ([%sexp_of: Agent_protocol.History.entry] protocol_entry)) ->
      Error (error Conflict "output occurrence has a different canonical payload")
    | _ -> Ok ()
  in
  match invocation.status with
  | Published _ -> Ok ()
  | _ ->
    let entries = if Option.is_some existing then [] else [ protocol_entry ] in
    let payloads =
      if List.is_empty entries
      then []
      else [ Agent_protocol.Event.Durable.Payload.History_appended entries ]
    in
    let%map _ =
      transition
        t
        ~delta:
          (Session_delta.Batch
             [ Canonical_entries_appended entries; Invocation_changed published ])
        ~payloads
    in
    ()
;;

let commit_worker_moderator t operation_id moderator =
  let open Result.Let_syntax in
  let%bind _ = running_operation ~allow_stopping:true t operation_id in
  if moderator_is_borrowed t
  then Error (error Conflict "moderator invocation owns the moderator checkpoint")
  else if Option.equal Jsonaf.exactly_equal t.state.moderator moderator
  then Ok ()
  else Result.map (change_moderator t moderator) ~f:(fun _ -> ())
;;

let consume_deferred t operation_id =
  let open Result.Let_syntax in
  let%bind _ = current_operation t operation_id in
  match t.state.lifecycle.desired with
  | Stopped -> Ok []
  | Running ->
    let entries = t.state.conversation.deferred_user_entries in
    let%bind decoded = History_codec.all_of_protocol entries in
    let%map _ = adopt_deferred t in
    decoded
;;

let admit_standalone_delivery_internal t plan =
  let open Result.Let_syntax in
  let%bind () =
    Standalone_delivery.revalidate
      ~state:t.state
      ~staged:(Staged_notifications.values t.staged_notifications)
      ~limits:t.services.notification_limits
      plan
  in
  let%map _ =
    transition t ~delta:(Session_delta.Delivery_changed plan.delivery) ~payloads:[]
  in
  ()
;;

let checked_pending_job_delivery t revision expected =
  let open Result.Let_syntax in
  let module P = Agent_protocol in
  let%bind () =
    match Int64.equal revision t.state.counters.revision with
    | true -> Ok ()
    | false -> Error (error Conflict "background delivery runtime snapshot changed")
  in
  let%bind job =
    List.find t.state.jobs ~f:(fun value -> P.Id.Job.equal value.id expected.P.Job.id)
    |> Result.of_option ~error:(error Invalid_request "background delivery job missing")
  in
  let%bind () =
    match Jsonaf.exactly_equal (P.Job.to_json expected) (P.Job.to_json job) with
    | true -> Ok ()
    | false -> Error (error Conflict "background delivery job changed")
  in
  let%bind () =
    Extension_invariants.owner
      ~session_id:t.state.identity.session_id
      ~generation:t.state.identity.generation
      job.session_id
      job.generation
  in
  let%bind () =
    match job.delivery with
    | Pending -> Ok ()
    | Delivered _ | Not_required | Discarded _ ->
      Error (error Already_resolved "background job delivery is resolved")
  in
  let%bind () =
    match job.status with
    | Succeeded | Failed _ | Cancelled | Interrupted _ -> Ok ()
    | Queued | Running | Waiting_permission _ | Waiting_completion _ ->
      Error (error Conflict "background job is not terminal")
  in
  let%map () = P.Job.validate_result job in
  job
;;

let retire_job_delivery t (job : Agent_protocol.Job.t) =
  let open Result.Let_syntax in
  let module P = Agent_protocol in
  let now = t.services.now () in
  let at =
    match job.completed_at with
    | Some completed when P.Timestamp.compare completed now > 0 -> completed
    | _ -> now
  in
  let job = { job with delivery = Discarded { at; reason = Authority_changed } } in
  let%map _ =
    transition
      t
      ~delta:(Session_delta.Job_changed job)
      ~payloads:[ P.Event.Durable.Payload.Job_state_changed job ]
  in
  ()
;;

let retire_obsolete_moderator_delivery_internal t revision expected =
  let open Result.Let_syntax in
  let%bind job = checked_pending_job_delivery t revision expected in
  let%bind source = Background_job_event.source ~state:t.state job in
  let%bind source =
    Result.of_option
      source
      ~error:
        (error Invalid_request "background delivery has no captured moderator source")
  in
  let%bind installed = Runtime_builder.moderator_snapshot_observer t.state.moderator in
  match Option.equal Agent_protocol.Invocation.equal_observer (Some source) installed with
  | true -> Ok false
  | false ->
    let%map () = retire_job_delivery t job in
    true
;;

let deliver_standalone_completion_internal t revision expected current_capabilities policy
  =
  let open Result.Let_syntax in
  let module P = Agent_protocol in
  let%bind job = checked_pending_job_delivery t revision expected in
  let%bind invocation =
    match job.launch with
    | Some { owner = Invocation id; _ } ->
      List.find t.state.invocations ~f:(fun value ->
        P.Id.Invocation.equal value.context.id id)
      |> Result.of_option
           ~error:(error Invalid_request "standalone completion owner missing")
    | _ -> Error (error Invalid_request "standalone completion has no invocation owner")
  in
  match
    Standalone_completion_contract.authorize
      ~invocation
      ~job
      ~current_capabilities
      ~policy
  with
  | Error { code = Permission_denied; _ } -> retire_job_delivery t job
  | Error error -> Error error
  | Ok () ->
    let load_artifact =
      Option.map t.services.job_results ~f:(fun publisher ->
        Agent_store.Job_result_store.Publisher.load publisher)
    in
    let%bind completion = P.Job.terminal_completion ?load_artifact job in
    let%bind completion =
      Result.of_option
        completion
        ~error:(error Invalid_state "standalone job has no terminal result")
    in
    let now = t.services.now () in
    let%bind plan =
      Standalone_delivery.prepare
        ~state:t.state
        ~notification_limits:t.services.notification_limits
        ~invocation_id:invocation.context.id
        ~job_id:job.id
        ~completion
        ~current_capabilities
        ~delivery_id:(P.Id.Delivery.create ())
        ~now
        ~wake:Request_turn
    in
    let%bind () =
      Standalone_delivery.revalidate
        ~state:t.state
        ~staged:(Staged_notifications.values t.staged_notifications)
        ~limits:t.services.notification_limits
        plan
    in
    let job = { job with delivery = Delivered now } in
    let%map _ =
      transition
        t
        ~delta:(Session_delta.Batch [ Delivery_changed plan.delivery; Job_changed job ])
        ~payloads:[ P.Event.Durable.Payload.Job_state_changed job ]
    in
    ()
;;

let validate_notification_plan t (plan : Notification_delivery.t) =
  let open Result.Let_syntax in
  let%bind () =
    Extension_invariants.owner
      ~session_id:t.state.identity.session_id
      ~generation:t.state.identity.generation
      plan.session_id
      plan.generation
  in
  let%bind installed = Runtime_builder.moderator_snapshot_observer t.state.moderator in
  match
    ( Int64.equal plan.revision t.state.counters.revision
    , installed
    , moderator_is_borrowed t )
  with
  | true, source, false
    when Option.equal Agent_protocol.Invocation.equal_observer source plan.source -> Ok ()
  | _ -> Error (error Conflict "notification snapshot or source changed")
;;

let notification_changes t (plan : Notification_delivery.t) =
  let open Result.Let_syntax in
  let%map deltas, entries, committed =
    List.fold_result
      plan.actions
      ~init:([], [], [])
      ~f:(fun (deltas, entries, committed) action ->
        match action with
        | Notification_delivery.Fail value ->
          Ok (Session_delta.Delivery_changed value :: deltas, entries, committed)
        | Publish value ->
          let%bind id =
            History_entry.Id.create
              ~namespace:
                ("notification/" ^ Agent_protocol.Id.Delivery.to_string value.context.id)
              ~sequence:0
            |> Result.map_error ~f:(error Invalid_state)
          in
          let%bind entry = Notification_history.create ~id value in
          let now = t.services.now () in
          let now =
            if Agent_protocol.Timestamp.compare now value.context.created_at < 0
            then value.context.created_at
            else now
          in
          let%map value =
            Agent_protocol.Delivery.commit ~track_wake:true value ~history_id:id ~now
          in
          ( Session_delta.Delivery_committed (value, entry) :: deltas
          , entry :: entries
          , value :: committed ))
  in
  List.rev deltas, List.rev entries, List.rev committed
;;

let notification_payloads = function
  | [] -> []
  | entries -> [ Agent_protocol.Event.Durable.Payload.History_appended entries ]
;;

let consume_notifications_internal
      ?(wakes = [])
      ?(discarded_wakes = [])
      t
      operation_id
      plan
  =
  let open Result.Let_syntax in
  let%bind _ = running_operation ~allow_stopping:true t operation_id in
  match t.state.lifecycle.desired with
  | Stopped ->
    (* An admitted provider may finish during graceful stop. Its final safe point
       must not fail, consume retained deliveries, or request another turn. *)
    Ok Chat_response.In_memory_stream.Safe_point_input.empty
  | Running ->
    let%bind () =
      match
        List.exists t.invocation_executions ~f:(fun execution ->
          match execution.owner with
          | Foreground id -> Agent_protocol.Id.Operation.equal id operation_id
          | _ -> false)
      with
      | true ->
        Error
          (error Conflict "notification insertion waits for the foreground tool batch")
      | false -> Ok ()
    in
    let%bind () = validate_notification_plan t plan in
    let%bind deltas, entries, committed = notification_changes t plan in
    let deltas =
      deltas
      @ List.map discarded_wakes ~f:(fun value ->
        Session_delta.Delivery_wake_changed value)
    in
    let%bind decoded = History_codec.all_of_protocol entries in
    let%map () =
      match deltas with
      | [] -> Ok ()
      | _ ->
        transition
          t
          ~delta:(Session_delta.Batch deltas)
          ~payloads:(notification_payloads entries)
        |> Result.map ~f:ignore
    in
    let existing =
      match t.notification_inputs with
      | Some (id, ids) when Agent_protocol.Id.Operation.equal id operation_id -> ids
      | _ -> []
    in
    let ids =
      List.map (committed @ wakes) ~f:(fun value ->
        value.Agent_protocol.Delivery.context.id)
    in
    t.notification_inputs <- Some (operation_id, ids @ existing);
    let wake =
      List.exists (committed @ wakes) ~f:(fun value ->
        Agent_protocol.Completion.equal_wake value.context.wake Request_turn)
    in
    Chat_response.In_memory_stream.Safe_point_input.notification_entries
      ~request_turn:wake
      decoded
;;

let notification_wake_deltas t operation_id ~accept =
  let open Result.Let_syntax in
  let ids =
    match t.notification_inputs with
    | Some (id, ids) when Agent_protocol.Id.Operation.equal id operation_id -> ids
    | _ -> []
  in
  List.filter t.state.deliveries ~f:(fun value ->
    List.mem ids value.context.id ~equal:Agent_protocol.Id.Delivery.equal
    &&
    match value.wake_disposition with
    | Some Pending_wake -> true
    | _ -> false)
  |> List.map ~f:(fun value ->
    let%map value =
      match accept with
      | true -> Agent_protocol.Delivery.accept_wake value ~operation_id
      | false ->
        Agent_protocol.Delivery.discard_wake
          value
          ~reason:"foreground ended without admitting this notification wake"
    in
    Session_delta.Delivery_wake_changed value)
  |> Result.all
;;

let worker_ready t operation_id cancel =
  match t.state.active_operation with
  | None ->
    cancel ();
    Ok ()
  | Some operation when Agent_protocol.Id.Operation.compare operation.id operation_id <> 0
    ->
    cancel ();
    Ok ()
  | Some operation ->
    t.active_cancel <- Some cancel;
    (match operation.state with
     | Agent_protocol.Operation.Cancelling ->
       cancel ();
       Ok ()
     | Starting ->
       let operation = operation_state t operation Running in
       let open Result.Let_syntax in
       let%map _ =
         transition
           t
           ~delta:(Session_delta.Active_operation_changed (Some operation))
           ~payloads:
             [ Agent_protocol.Event.Durable.Payload.Session_updated
                 (summary_with_operation t operation)
             ]
       in
       ()
     | Running -> Ok ()
     | Completed | Failed _ | Cancelled | Interrupted _ ->
       cancel ();
       Ok ())
;;

let terminal_lifecycle t =
  Session_state.Lifecycle.
    { desired = t.state.lifecycle.desired
    ; observed =
        (match t.state.lifecycle.desired with
         | Running ->
           (match
              List.find t.state.permissions ~f:(fun permission ->
                Agent_protocol.Permission.equal_state permission.state Pending
                &&
                match permission.owner with
                | Operation _ -> false
                | Invocation id ->
                  List.exists t.invocation_executions ~f:(fun execution ->
                    Agent_protocol.Id.Invocation.equal execution.dispatched.context.id id
                    &&
                    match execution.owner with
                    | Background_job _ -> true
                    | _ -> false))
            with
            | Some permission -> Waiting_for_permission permission.id
            | None -> Idle)
         | Stopped -> Stopped)
    }
;;

let history_prefix ~prefix values =
  let equal left right =
    Sexp.equal
      ([%sexp_of: Agent_protocol.History.entry] left)
      ([%sexp_of: Agent_protocol.History.entry] right)
  in
  List.is_prefix values ~prefix ~equal
;;

let end_session_reason requests =
  List.find_map requests ~f:(function
    | Chat_response.Moderation.Runtime_request.End_session reason -> Some reason
    | Request_turn | Request_compaction -> None)
;;

let completed_lifecycle t requests =
  match end_session_reason requests with
  | None -> terminal_lifecycle t
  | Some _ ->
    Session_state.Lifecycle.
      { desired = Agent_protocol.Session.Stopped; observed = Stopped }
;;

let runtime_request_deltas summary =
  end_session_reason summary.Operation_worker.Summary.runtime_requests
  |> Option.map ~f:(fun reason -> Session_delta.Halt_changed (Some reason))
  |> Option.to_list
;;

let runtime_request_payloads summary =
  end_session_reason summary.Operation_worker.Summary.runtime_requests
  |> Option.map ~f:(fun reason ->
    Agent_protocol.Event.Durable.Payload.Moderator_notification
      (`Object [ "end_session_reason", `String reason ]))
  |> Option.to_list
;;

let completed_delta t operation summary =
  let final_protocol =
    History_codec.all_to_protocol
      ~previous:t.state.conversation.canonical_history
      summary.Operation_worker.Summary.final_history
  in
  let committed = t.state.conversation.canonical_history in
  if not (history_prefix ~prefix:committed final_protocol)
  then Error (error Conflict "worker final history diverges from committed history")
  else (
    let missing = List.drop final_protocol (List.length committed) in
    let operation = operation_state t operation Agent_protocol.Operation.Completed in
    let lifecycle = completed_lifecycle t summary.runtime_requests in
    let moderator_snapshot =
      match t.foreground_moderator, moderator_is_borrowed t with
      | Some _, _ | _, true -> t.state.moderator
      | None, false -> summary.moderator_snapshot
    in
    let deltas =
      [ Option.some_if
          (not (List.is_empty missing))
          (Session_delta.Canonical_entries_appended missing)
      ; Some (Session_delta.Active_operation_changed None)
      ; Some (Session_delta.Lifecycle_changed lifecycle)
      ; Some (Session_delta.Moderator_changed moderator_snapshot)
      ]
      |> List.filter_opt
      |> fun values -> values @ runtime_request_deltas summary
    in
    let payloads =
      [ Option.some_if
          (not (List.is_empty missing))
          (Agent_protocol.Event.Durable.Payload.History_appended missing)
      ; Some (Agent_protocol.Event.Durable.Payload.Operation_completed operation)
      ; Some
          (Agent_protocol.Event.Durable.Payload.Session_state_changed
             { desired_state = lifecycle.desired; observed_state = lifecycle.observed })
      ]
      |> List.filter_opt
      |> fun values -> values @ runtime_request_payloads summary
    in
    Ok (Session_delta.Batch deltas, payloads))
;;

let terminal_delta t operation = function
  | Operation_worker.Completed summary -> completed_delta t operation summary
  | Cancelled cancellation ->
    let operation = operation_state t operation Agent_protocol.Operation.Cancelled in
    let lifecycle = terminal_lifecycle t in
    Ok
      ( Session_delta.Batch [ Active_operation_changed None; Lifecycle_changed lifecycle ]
      , [ Agent_protocol.Event.Durable.Payload.Operation_cancelled operation
        ; Moderator_notification
            (`Object [ "cancellation_reason", `String cancellation.reason ])
        ; Session_state_changed
            { desired_state = lifecycle.desired; observed_state = lifecycle.observed }
        ] )
  | Failed failure ->
    let operation =
      operation_state t operation (Agent_protocol.Operation.Failed failure)
    in
    let lifecycle = terminal_lifecycle t in
    Ok
      ( Session_delta.Batch [ Active_operation_changed None; Lifecycle_changed lifecycle ]
      , [ Agent_protocol.Event.Durable.Payload.Operation_failed operation
        ; Session_state_changed
            { desired_state = lifecycle.desired; observed_state = lifecycle.observed }
        ] )
;;

let is_permission_review_job (job : Agent_protocol.Job.t) =
  match job.payload with
  | `Object fields ->
    List.Assoc.find fields "type" ~equal:String.equal
    |> Option.value_map ~default:false ~f:(function
      | `String value -> String.equal value "permission_review"
      | _ -> false)
  | _ -> false
;;

let pending_for_operation t (operation : Agent_protocol.Operation.t) permission =
  Agent_protocol.Permission.equal_state permission.Agent_protocol.Permission.state Pending
  &&
  match permission.owner with
  | Operation id -> Agent_protocol.Id.Operation.equal id operation.id
  | Invocation id ->
    List.exists t.invocation_executions ~f:(fun execution ->
      Agent_protocol.Id.Invocation.equal id execution.dispatched.context.id
      &&
      match execution.owner with
      | Foreground id -> Agent_protocol.Id.Operation.equal id operation.id
      | Background_job _ -> false
      | Event_moderator borrow ->
        Option.exists
          borrow.receipt.context.operation_id
          ~f:(Agent_protocol.Id.Operation.equal operation.id)
      | Invocation_moderator borrow ->
        Option.exists
          borrow.operation_id
          ~f:(Agent_protocol.Id.Operation.equal operation.id))
    || Option.exists t.moderator_borrow ~f:(fun borrow ->
      Agent_protocol.Id.Invocation.equal id borrow.invocation.context.id
      && Option.exists
           borrow.operation_id
           ~f:(Agent_protocol.Id.Operation.equal operation.id))
;;

let cancel_operation_permission t operation reason permission =
  if pending_for_operation t operation permission
  then Some (cancel_permission t reason permission)
  else None
;;

let interrupt_permission_review_job t reason (job : Agent_protocol.Job.t) =
  if is_permission_review_job job
  then (
    match job.status with
    | Running ->
      Some
        { job with
          status = Interrupted reason
        ; completed_at = Some (t.services.now ())
        ; delivery = Pending
        }
    | Queued ->
      Some
        { job with
          status = Cancelled
        ; completed_at = Some (t.services.now ())
        ; delivery = Pending
        }
    | Waiting_permission _
    | Waiting_completion _
    | Succeeded
    | Failed _
    | Cancelled
    | Interrupted _ -> None)
  else None
;;

let operation_terminal_cleanup t operation outcome =
  let reason =
    match outcome with
    | Operation_worker.Completed _ -> None
    | Cancelled _ -> Some "operation was cancelled"
    | Failed failure -> Some ("operation failed: " ^ failure.message)
  in
  match reason with
  | None -> [], []
  | Some reason ->
    ( List.filter_map
        t.state.permissions
        ~f:(cancel_operation_permission t operation reason)
    , List.filter_map t.state.jobs ~f:(interrupt_permission_review_job t reason) )
;;

let cleanup_delta permissions jobs =
  Session_delta.Batch
    (List.map permissions ~f:(fun permission ->
       Session_delta.Permission_changed permission)
     @ List.map jobs ~f:(fun job -> Session_delta.Job_changed job))
;;

let cleanup_payloads permissions jobs =
  List.map permissions ~f:(fun permission ->
    Agent_protocol.Event.Durable.Payload.Permission_resolved permission)
  @ List.map jobs ~f:(fun job ->
    Agent_protocol.Event.Durable.Payload.Job_state_changed job)
;;

let compaction_generation t =
  if Int.equal t.state.conversation.compaction_generation Int.max_value
  then Error (error Invalid_state "compaction generation overflow")
  else Ok (t.state.conversation.compaction_generation + 1)
;;

let compaction_terminal_base_delta t operation = function
  | Compacted history ->
    let open Result.Let_syntax in
    let%bind generation = compaction_generation t in
    let archive =
      Compaction_archive.reference t.state operation.Agent_protocol.Operation.id
    in
    let history =
      History_codec.all_to_protocol
        ~previous:t.state.conversation.canonical_history
        history
    in
    let operation = operation_state t operation Agent_protocol.Operation.Completed in
    let lifecycle = terminal_lifecycle t in
    Ok
      ( Session_delta.Batch
          [ Compaction_archived archive
          ; Canonical_history_replaced history
          ; Compaction_generation_changed generation
          ; Active_operation_changed None
          ; Lifecycle_changed lifecycle
          ]
      , [ Agent_protocol.Event.Durable.Payload.History_replaced
            (Session_state.history_window history)
        ; Operation_completed operation
        ; Session_state_changed
            { desired_state = lifecycle.desired; observed_state = lifecycle.observed }
        ] )
  | Compaction_cancelled reason ->
    let operation = operation_state t operation Agent_protocol.Operation.Cancelled in
    let lifecycle = terminal_lifecycle t in
    Ok
      ( Session_delta.Batch [ Active_operation_changed None; Lifecycle_changed lifecycle ]
      , [ Agent_protocol.Event.Durable.Payload.Operation_cancelled operation
        ; Moderator_notification (`Object [ "cancellation_reason", `String reason ])
        ; Session_state_changed
            { desired_state = lifecycle.desired; observed_state = lifecycle.observed }
        ] )
  | Compaction_failed failure ->
    let operation =
      operation_state t operation (Agent_protocol.Operation.Failed failure)
    in
    let lifecycle = terminal_lifecycle t in
    Ok
      ( Session_delta.Batch [ Active_operation_changed None; Lifecycle_changed lifecycle ]
      , [ Agent_protocol.Event.Durable.Payload.Operation_failed operation
        ; Session_state_changed
            { desired_state = lifecycle.desired; observed_state = lifecycle.observed }
        ] )
;;

let compaction_terminal_delta t operation outcome =
  let open Result.Let_syntax in
  let%bind delta, payloads = compaction_terminal_base_delta t operation outcome in
  let%bind discarded =
    match outcome with
    | Compacted _ -> Ok []
    | Compaction_cancelled _ | Compaction_failed _ ->
      Observation_follow_up.discard_compaction
        t.state.invocations
        ~operation_id:operation.id
        ~reason:
          (match outcome with
           | Compaction_cancelled _ -> "compaction cancelled"
           | _ -> "compaction failed")
  in
  let%map events =
    match outcome with
    | Compacted _ -> Ok []
    | Compaction_cancelled _ | Compaction_failed _ ->
      Observation_follow_up.discard_event_compaction
        t.state.moderator_executions
        ~operation_id:operation.id
        ~reason:
          (match outcome with
           | Compaction_cancelled _ -> "compaction cancelled"
           | _ -> "compaction failed")
  in
  ( Session_delta.Batch
      (List.map discarded ~f:Observation_follow_up.delta
       @ List.map events ~f:Observation_follow_up.event_delta
       @ [ delta ])
  , payloads )
;;

let compaction_terminal t operation_id outcome =
  match t.state.active_operation with
  | None -> Ok ()
  | Some operation when Agent_protocol.Id.Operation.compare operation.id operation_id <> 0
    -> Ok ()
  | Some operation ->
    let open Result.Let_syntax in
    let outcome =
      match operation.state with
      | Agent_protocol.Operation.Cancelling -> Compaction_cancelled "operation cancelled"
      | _ -> outcome
    in
    let%bind delta, payloads = compaction_terminal_delta t operation outcome in
    let%map _ =
      match transition t ~delta ~payloads, outcome with
      | Ok session, _ -> Ok session
      | Error failure, Compacted _ ->
        let%bind delta, payloads =
          compaction_terminal_delta t operation (Compaction_failed failure)
        in
        transition t ~delta ~payloads
      | Error failure, _ -> Error failure
    in
    t.active_cancel <- None
;;

let compaction_failure exn =
  Agent_protocol.Error.create
    Internal_error
    ~message:("history compaction failed: " ^ Exn.to_string exn)
    ~retryable:true
    ()
;;

exception Compaction_cancel_requested

let compute_compaction t allocator history =
  match
    Context_compaction.Compactor.compact_entries ~allocator ~env:t.compaction_env ~history
  with
  | Error exn -> Compaction_failed (compaction_failure exn)
  | Ok history ->
    (match History_entry.validate ~allocator history with
     | Ok () -> Compacted history
     | Error message ->
       Compaction_failed (error Conflict ("invalid compacted history: " ^ message)))
;;

let run_compaction t operation allocator history =
  let operation_id = operation.Agent_protocol.Operation.id in
  try
    Eio.Switch.run (fun operation_switch ->
      let cancel () = Eio.Switch.fail operation_switch Compaction_cancel_requested in
      match call t ~priority:Priority (Worker_ready (operation_id, cancel)) with
      | Error failure -> Compaction_failed failure
      | Ok () -> compute_compaction t allocator history)
  with
  | Compaction_cancel_requested -> Compaction_cancelled "operation cancelled"
  | Eio.Cancel.Cancelled reason -> Compaction_cancelled (Exn.to_string reason)
  | exn -> Compaction_failed (compaction_failure exn)
;;

let launch_compaction t operation allocator history =
  let operation_id = operation.Agent_protocol.Operation.id in
  Eio.Fiber.fork ~sw:t.sw (fun () ->
    let outcome = run_compaction t operation allocator history in
    ignore
      (call t ~priority:Priority (Compaction_terminal (operation_id, outcome))
       : (unit, Agent_protocol.Error.t) result))
;;

let create_compaction_operation t =
  let timestamp = t.services.now () in
  Agent_protocol.Operation.
    { id = Agent_protocol.Id.Operation.create ()
    ; generation = t.state.identity.generation
    ; kind = Compaction
    ; state = Starting
    ; started_at = timestamp
    ; updated_at = timestamp
    }
;;

let int_of_history_sequence sequence =
  if Int64.(sequence > of_int Int.max_value)
  then Error (error Invalid_state "history sequence exceeds platform allocation range")
  else Ok (Int64.to_int_exn sequence)
;;

let compaction_allocator t first_sequence reserved_through =
  let open Result.Let_syntax in
  let%bind next_sequence = int_of_history_sequence first_sequence in
  let%bind limit_exclusive = int_of_history_sequence reserved_through in
  History_entry.Allocator.create_bounded
    ~namespace:(Agent_protocol.Id.Session.to_string t.state.identity.session_id)
    ~next_sequence
    ~limit_exclusive
  |> Result.map_error ~f:(fun message -> error Invalid_state message)
;;

let reconcile_foreground_invocations t =
  let open Result.Let_syntax in
  let first =
    Int64.max
      t.state.conversation.next_history_sequence
      t.state.conversation.reserved_history_through
  in
  let%bind first_sequence = int_of_history_sequence first in
  let%bind plan =
    Invocation_recovery.plan_foreground
      ~state:t.state
      ~namespace:(Agent_protocol.Id.Session.to_string t.state.identity.session_id)
      ~first_sequence
      ~reason:"foreground worker ended before recording the invocation outcome"
  in
  if List.is_empty plan.deltas
  then Ok ()
  else
    transition
      t
      ~delta:
        (Session_delta.Batch
           (History_block_reserved (Int64.of_int plan.next_sequence) :: plan.deltas))
      ~payloads:
        (if List.is_empty plan.appended
         then []
         else [ Agent_protocol.Event.Durable.Payload.History_appended plan.appended ])
    |> Result.map ~f:(fun _ -> ())
;;

let retain_reconciliation_failure t failure =
  let lifecycle =
    Session_state.Lifecycle.
      { desired = t.state.lifecycle.desired; observed = Failed failure }
  in
  transition
    t
    ~delta:
      (Session_delta.Batch [ Failure_changed (Some failure); Lifecycle_changed lifecycle ])
    ~payloads:
      [ Agent_protocol.Event.Durable.Payload.Session_state_changed
          { desired_state = lifecycle.desired; observed_state = lifecycle.observed }
      ]
  |> Result.map ~f:(fun _ -> ())
;;

let start_compaction ?operation ?(extra_deltas : Session_delta.t list = []) t =
  let open Result.Let_syntax in
  let%bind () = reconcile_foreground_invocations t in
  let first_sequence = t.state.conversation.next_history_sequence in
  if Int64.equal first_sequence Int64.max_value
  then Error (error Invalid_state "history sequence overflow")
  else (
    let reserved_through = Int64.(first_sequence + 1L) in
    let open Result.Let_syntax in
    let%bind allocator = compaction_allocator t first_sequence reserved_through in
    let%bind history =
      History_codec.all_of_protocol t.state.conversation.canonical_history
    in
    let operation =
      Option.value_or_thunk operation ~default:(fun () -> create_compaction_operation t)
    in
    let lifecycle = lifecycle_for_compaction t operation.id in
    let%bind session =
      transition
        t
        ~delta:
          (Session_delta.Batch
             (extra_deltas
              @ [ Session_delta.History_block_reserved reserved_through
                ; Active_operation_changed (Some operation)
                ; Lifecycle_changed lifecycle
                ]))
        ~payloads:
          [ Agent_protocol.Event.Durable.Payload.Operation_started operation
          ; Session_state_changed
              { desired_state = lifecycle.desired; observed_state = lifecycle.observed }
          ]
    in
    launch_compaction t operation allocator history;
    Ok session)
;;

let compact_internal t attachment_id expected_revision =
  let open Result.Let_syntax in
  let%bind _ = write_attachment t attachment_id in
  let%bind () =
    match expected_revision with
    | None -> Ok ()
    | Some revision when Int64.equal revision t.state.counters.revision -> Ok ()
    | Some _ -> Error (error Conflict "session revision does not match")
  in
  match
    t.idle_moderator_borrowed, t.state.active_operation, t.state.lifecycle.observed
  with
  | true, _, _ -> Error (error Conflict "session moderator is processing background work")
  | false, Some _, _ -> Error (error Conflict "a foreground operation is already active")
  | false, None, (Idle | Stopped) -> start_compaction t
  | false, None, _ -> Error (error Invalid_state "session is not ready for compaction")
;;

let history_edit_precondition t revision =
  if not (Int64.equal revision t.state.counters.revision)
  then Error (error Conflict "session revision does not match")
  else if t.idle_moderator_borrowed || Option.is_some t.state.active_operation
  then Error (error Conflict "history cannot change during active work")
  else (
    match t.state.lifecycle.observed with
    | Idle | Stopped -> Ok ()
    | _ -> Error (error Invalid_state "session is not ready for history editing"))
;;

let history_deletion t history_id =
  let open Result.Let_syntax in
  let history = t.state.conversation.canonical_history in
  let%bind canonical = History_codec.all_of_protocol history in
  let%bind retained =
    History_entry.remove_with_tool_pair canonical ~entry_id:history_id
    |> Result.map_error ~f:(error Invalid_request)
  in
  let ids =
    Hash_set.of_list (module History_entry.Id) (List.map retained ~f:History_entry.id)
  in
  let keep entry = Hash_set.mem ids entry.Agent_protocol.History.id in
  let initial = List.take history t.state.conversation.initial_prompt_entry_count in
  let initial_count = List.count initial ~f:keep in
  Ok (List.filter history ~f:keep, initial_count)
;;

let delete_history_internal t attachment_id revision history_id =
  let open Result.Let_syntax in
  let%bind _ = write_attachment t attachment_id in
  let%bind () = history_edit_precondition t revision in
  let%bind history, initial_count = history_deletion t history_id in
  let retained_ids =
    Hash_set.of_list
      (module Agent_protocol.History.Id)
      (List.map history ~f:(fun entry -> entry.Agent_protocol.History.id))
  in
  let forgotten =
    List.filter_map t.state.conversation.canonical_history ~f:(fun entry ->
      Option.some_if (not (Hash_set.mem retained_ids entry.id)) entry.id)
  in
  transition
    t
    ~delta:
      (Session_delta.Batch
         [ Canonical_history_replaced history
         ; Authoring_references_forgotten forgotten
         ; Initial_prompt_count_changed initial_count
         ])
    ~payloads:
      [ Agent_protocol.Event.Durable.Payload.History_replaced
          (Session_state.history_window history)
      ]
;;

let outcome_requests_compaction = function
  | Operation_worker.Completed summary ->
    List.exists summary.runtime_requests ~f:(function
      | Chat_response.Moderation.Runtime_request.Request_compaction -> true
      | Request_turn | End_session _ -> false)
  | Cancelled _ | Failed _ -> false
;;

let follow_up_deltas t (plan : Observation_follow_up.t) =
  List.map plan.invocations ~f:(fun invocation ->
    match Int.equal invocation.context.generation t.state.identity.generation with
    | true -> Session_delta.Invocation_changed invocation
    | false -> Session_delta.Invocation_reconciled invocation)
  @ List.map plan.events ~f:Observation_follow_up.event_delta
;;

let managed_moderator t operation_id =
  let open Result.Let_syntax in
  let%bind observer =
    match t.foreground_moderator with
    | Some (id, observer) when Agent_protocol.Id.Operation.equal id operation_id ->
      Ok observer
    | _ -> Error (error Conflict "foreground moderator routing is not installed")
  in
  let%bind installed = Runtime_builder.moderator_snapshot_observer t.state.moderator in
  match installed with
  | Some installed when Agent_protocol.Invocation.equal_observer observer installed ->
    Ok observer
  | _ -> Error (error Conflict "foreground moderator source changed")
;;

let manage_moderator_follow_up t operation_id observer =
  let open Result.Let_syntax in
  let%bind _ = running_operation t operation_id in
  let%bind installed = Runtime_builder.moderator_snapshot_observer t.state.moderator in
  match installed with
  | Some installed when Agent_protocol.Invocation.equal_observer observer installed ->
    t.foreground_moderator <- Some (operation_id, observer);
    Ok ()
  | _ -> Error (error Conflict "foreground routing requires the installed moderator")
;;

let admit_moderator_turn t operation_id =
  let open Result.Let_syntax in
  let%bind _ = running_operation t operation_id in
  let%bind observer = managed_moderator t operation_id in
  let%bind halted = Runtime_builder.moderator_snapshot_is_halted t.state.moderator in
  let%bind () =
    match halted, moderator_is_borrowed t with
    | true, _ -> Error (error Conflict "halted moderator cannot admit a provider request")
    | false, true ->
      Error (error Conflict "provider admission cannot interrupt a moderator event")
    | false, false -> Ok ()
  in
  let%bind plan = Observation_follow_up.admit_turn ~state:t.state ~observer in
  let%bind notifications = notification_wake_deltas t operation_id ~accept:true in
  match follow_up_deltas t plan @ notifications with
  | [] -> Ok ()
  | deltas ->
    transition t ~delta:(Session_delta.Batch deltas) ~payloads:[] |> Result.map ~f:ignore
;;

let admit_notification_turn t operation_id =
  let open Result.Let_syntax in
  let%bind _ = running_operation t operation_id in
  let%bind () =
    match t.state.lifecycle.desired, t.state.halted, moderator_is_borrowed t with
    | Running, false, false -> Ok ()
    | _ -> Error (error Conflict "session cannot admit a provider request")
  in
  let%bind deltas = notification_wake_deltas t operation_id ~accept:true in
  match deltas with
  | [] -> Ok ()
  | _ ->
    transition t ~delta:(Session_delta.Batch deltas) ~payloads:[] |> Result.map ~f:ignore
;;

let foreground_terminal_requests t operation_id outcome =
  let open Result.Let_syntax in
  match t.foreground_moderator with
  | None -> Ok []
  | Some _ ->
    let%bind observer = managed_moderator t operation_id in
    let end_reason, failed =
      match outcome with
      | Operation_worker.Completed summary ->
        end_session_reason summary.runtime_requests, false
      | Failed _ | Cancelled _ -> None, true
    in
    let%map plan =
      match end_reason with
      | None ->
        Observation_follow_up.finish_foreground
          ~state:t.state
          ~observer
          ~operation_id
          ~failed
      | Some _ ->
        Observation_follow_up.plan
          ~state:t.state
          ~observer:(Some observer)
          ~halted:true
          ~compaction_operation_id:(Agent_protocol.Id.Operation.create ())
    in
    follow_up_deltas t plan
;;

let worker_terminal t operation_id outcome =
  match t.state.active_operation with
  | None -> Ok ()
  | Some operation when Agent_protocol.Id.Operation.compare operation.id operation_id <> 0
    -> Ok ()
  | Some operation ->
    let open Result.Let_syntax in
    let belongs operation =
      Option.exists operation ~f:(Agent_protocol.Id.Operation.equal operation_id)
    in
    let borrow =
      Option.filter t.moderator_borrow ~f:(fun borrow -> belongs borrow.operation_id)
    in
    let event_borrow =
      Option.filter t.queued_event_borrow ~f:(fun borrow ->
        belongs borrow.receipt.context.operation_id)
    in
    let foreground, independent =
      List.partition_tf t.invocation_executions ~f:(fun execution ->
        match execution.owner with
        | Foreground id -> Agent_protocol.Id.Operation.equal id operation_id
        | Invocation_moderator borrow -> belongs borrow.operation_id
        | Event_moderator borrow -> belongs borrow.receipt.context.operation_id
        | Background_job _ -> false)
    in
    let%bind unfinished =
      List.map foreground ~f:(fun execution ->
        execution.accepts_children <- false;
        abort_staged_work t ~owner:(Invocation execution.dispatched.context.id);
        Agent_protocol.Invocation.cancel
          execution.dispatched
          ~reason:"worker exited before recording the invocation outcome"
        |> Result.map ~f:(fun invocation -> Session_delta.Invocation_changed invocation))
      |> Result.all
    in
    let%bind borrow_delta =
      match borrow with
      | None -> Ok (Session_delta.Batch [])
      | Some borrow ->
        abort_staged_work t ~owner:(Invocation borrow.invocation.context.id);
        uncommitted_borrow_delta
          t
          borrow
          (Some
             (Agent_protocol.Invocation.Cancelled
                "worker exited with an active moderator borrow"))
    in
    let%bind event_delta =
      match event_borrow with
      | Some borrow when not borrow.committed ->
        abort_staged_work t ~owner:(Moderator_event borrow.receipt.context.id);
        let%map interrupted =
          Agent_protocol.Moderator_execution.interrupt
            borrow.receipt
            ~reason:"worker exited before recording its moderator event"
        in
        Session_delta.Moderator_execution_changed interrupted
      | _ -> Ok (Session_delta.Batch [])
    in
    let outcome =
      match
        ( Option.is_some borrow
          || Option.is_some event_borrow
          || not (List.is_empty unfinished)
        , outcome )
      with
      | true, Operation_worker.Completed _ ->
        Operation_worker.Failed
          (error Internal_error "worker completed with an active invocation")
      | _ -> outcome
    in
    let%bind delta, payloads = terminal_delta t operation outcome in
    let%bind follow_up = foreground_terminal_requests t operation_id outcome in
    let%bind notifications = notification_wake_deltas t operation_id ~accept:false in
    let permissions, jobs = operation_terminal_cleanup t operation outcome in
    let%bind _ =
      transition
        t
        ~delta:
          (Session_delta.Batch
             (notifications
              @ follow_up
              @ unfinished
              @ [ borrow_delta; event_delta; delta; cleanup_delta permissions jobs ]))
        ~payloads:(payloads @ cleanup_payloads permissions jobs)
    in
    resolve_cleaned_permission_waiters t permissions;
    Option.iter borrow ~f:(fun _ -> t.moderator_borrow <- None);
    Option.iter event_borrow ~f:(fun borrow ->
      borrow.callback_active <- false;
      borrow.cancel <- None);
    Option.iter event_borrow ~f:(fun _ -> t.queued_event_borrow <- None);
    t.foreground_moderator <- None;
    t.notification_inputs <- None;
    t.idle_moderator_borrowed <- moderator_is_borrowed t;
    t.invocation_executions <- independent;
    t.active_cancel <- None;
    (match t.state.lifecycle.desired, t.state.halted with
     | Stopped, _ | _, true -> cancel_independent_moderator t
     | Running, false -> ());
    let%bind () =
      match reconcile_foreground_invocations t with
      | Ok () -> Ok ()
      | Error failure ->
        let%bind () = retain_reconciliation_failure t failure in
        Error failure
    in
    if outcome_requests_compaction outcome
    then Result.map (start_compaction t) ~f:(fun _ -> ())
    else Ok ()
;;

let cancel_operation_internal t attachment_id operation_id =
  let open Result.Let_syntax in
  let%bind _ = write_attachment t attachment_id in
  let%bind operation = current_operation t operation_id in
  match operation.state with
  | Completed | Failed _ | Cancelled | Interrupted _ ->
    Error (error Already_resolved "operation is already terminal")
  | Cancelling -> Ok (Session_state.summary t.state)
  | Starting | Running ->
    let operation = operation_state t operation Cancelling in
    let%bind session =
      transition
        t
        ~delta:(Session_delta.Active_operation_changed (Some operation))
        ~payloads:
          [ Agent_protocol.Event.Durable.Payload.Session_updated
              (summary_with_operation t operation)
          ]
    in
    cancel_event_for_operation t operation.id;
    Option.iter t.active_cancel ~f:(fun cancel -> cancel ());
    Ok session
;;

let worker_failure exn =
  Agent_protocol.Error.create
    Internal_error
    ~message:("foreground worker failed: " ^ Exn.to_string exn)
    ~retryable:true
    ()
;;

let publish_worker_live t buffer ~kind ~payload =
  let event =
    Live_event_buffer.publish
      buffer
      ~anchor_sequence:(Atomic.get t.event_sequence)
      ~timestamp:(t.services.now ())
      ~kind
      ~payload
  in
  broadcast_recoverable t event
;;

let request_review_internal t ~permission ~review =
  let open Result.Let_syntax in
  let%bind response =
    call t (Open_permission (permission, None, Agent_protocol.Permission.Deny))
  in
  let choice, reason =
    match review () with
    | Ok Permission_reviewer.Decision.Allow ->
      Agent_protocol.Permission.Approve_once, None
    | Ok (Deny message) -> Agent_protocol.Permission.Deny, Some message
    | Error (error : Permission_reviewer.Error.t) ->
      Agent_protocol.Permission.Deny, Some error.message
  in
  let%bind (_ : Agent_protocol.Permission.t) =
    call
      t
      ~priority:Priority
      (Resolve_permission_system (permission.id, permission.generation, choice, reason))
  in
  Ok (Eio.Promise.await response)
;;

let review_resolution review =
  match review () with
  | Ok Permission_reviewer.Decision.Allow -> Agent_protocol.Permission.Approve_once, None
  | Ok (Deny message) -> Agent_protocol.Permission.Deny, Some message
  | Error (failure : Permission_reviewer.Error.t) ->
    Agent_protocol.Permission.Deny, Some failure.message
;;

let timeout_review t (permission : Agent_protocol.Permission.t) response seconds review =
  t.sleep seconds;
  let choice, reason = review_resolution review in
  let open Result.Let_syntax in
  let%bind (_ : Agent_protocol.Permission.t) =
    call
      t
      ~priority:Priority
      (Resolve_permission_system (permission.id, permission.generation, choice, reason))
  in
  Ok (Eio.Promise.await response)
;;

let request_permission_with_review_internal
      t
      ~permission
      ~timeout_seconds
      ~fallback
      ~review_on_timeout
  =
  let open Result.Let_syntax in
  let actor_timeout =
    if t.schedule_permission_timeouts && Option.is_none review_on_timeout
    then timeout_seconds
    else None
  in
  let%bind response = call t (Open_permission (permission, actor_timeout, fallback)) in
  match t.schedule_permission_timeouts, timeout_seconds, review_on_timeout with
  | true, Some seconds, Some review ->
    Eio.Fiber.first
      (fun () -> Ok (Eio.Promise.await response))
      (fun () -> timeout_review t permission response seconds review)
  | (false | true), _, _ -> Ok (Eio.Promise.await response)
;;

let with_invocation_claim t claim f =
  let open Result.Let_syntax in
  Eio.Fiber.yield ();
  let%bind execution = Eio.Cancel.protect (fun () -> call t claim) in
  let finish ?requests ?(commit_starts = false) outcome =
    Eio.Cancel.protect (fun () ->
      call t (Finish_invocation (execution, outcome, requests, commit_starts)))
  in
  let failed =
    Agent_protocol.Invocation.Fail
      { code = "invocation.handler_failed"
      ; message = "Tool execution failed."
      ; retryable = false
      ; details = `Null
      }
  in
  let run () =
    match execution.owner with
    | Background_job _ ->
      Chat_response.Runtime_request_scope.collect (fun () ->
        f ~dispatched:execution.dispatched)
    | Foreground _ | Invocation_moderator _ | Event_moderator _ ->
      f ~dispatched:execution.dispatched, []
  in
  match run () with
  | Ok outcome, collected ->
    let outcome, collected, commit_starts =
      match Agent_protocol.Invocation.validate_outcome outcome with
      | Ok () -> outcome, collected, true
      | Error _ ->
        ( Agent_protocol.Invocation.Fail
            { code = "invocation.invalid_output"
            ; message = "Tool execution returned an invalid outcome."
            ; retryable = false
            ; details = `Null
            }
        , []
        , false )
    in
    let requests : Agent_protocol.Invocation.follow_up =
      { request_turn = Chat_response.Runtime_semantics.request_turn collected
      ; request_compaction = Chat_response.Runtime_semantics.request_compaction collected
      ; end_session = Chat_response.Runtime_semantics.should_end_session collected
      }
    in
    finish ~requests ~commit_starts outcome
  | Error failure, _ ->
    let%bind _ = finish failed in
    Error failure
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    let outcome =
      match exn with
      | Eio.Cancel.Cancelled _ ->
        Agent_protocol.Invocation.Cancelled "tool execution cancelled"
      | _ -> failed
    in
    ignore (finish outcome : (Agent_protocol.Invocation.t, Agent_protocol.Error.t) result);
    Stdlib.Printexc.raise_with_backtrace exn backtrace
;;

let with_invocation t operation_id ~invocation f =
  with_invocation_claim t (Claim_invocation (operation_id, invocation)) f
;;

let with_job_scope t ~job_id ~generation ~attempt ~deadline f =
  let open Result.Let_syntax in
  let%bind scope =
    Eio.Cancel.protect (fun () ->
      call t (Claim_job_scope (job_id, generation, attempt, deadline)))
  in
  let active = Atomic.make true in
  let finish () =
    Atomic.set active false;
    Eio.Cancel.protect (fun () -> call t (Finish_job_scope scope))
  in
  let execute ~invocation callback =
    match Atomic.get active with
    | false -> Error (error Conflict "background invocation scope has ended")
    | true ->
      with_invocation_claim
        t
        (Claim_job_invocation (scope, invocation))
        (fun ~dispatched ->
           match Atomic.get active with
           | false -> Error (error Conflict "background invocation scope has ended")
           | true ->
             Eio.Fiber.check ();
             (match Eio.Promise.peek scope.cancelled with
              | Some () -> raise (Eio.Cancel.Cancelled Exit)
              | None ->
                Eio.Fiber.first
                  (fun () -> callback ~dispatched)
                  (fun () ->
                     Eio.Promise.await scope.cancelled;
                     raise (Eio.Cancel.Cancelled Exit))))
  in
  let run () =
    Eio.Cancel.sub (fun context ->
      Exn.protect
        ~finally:(fun () ->
          Atomic.set active false;
          Exn.protect
            ~finally:(fun () -> Eio.Cancel.cancel context Exit)
            ~f:(fun () ->
              Eio.Cancel.protect (fun () ->
                ignore
                  (call t (Seal_job_scope scope) : (unit, Agent_protocol.Error.t) result))))
        ~f:(fun () ->
          let%bind () =
            call
              t
              (Set_job_scope_cancel
                 ( scope
                 , fun () ->
                     match Atomic.get active with
                     | true -> Eio.Cancel.cancel context Exit
                     | false -> () ))
          in
          f ~scope ~execute))
  in
  match run () with
  | result ->
    let%bind () = finish () in
    result
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    ignore (finish () : (unit, Agent_protocol.Error.t) result);
    Stdlib.Printexc.raise_with_backtrace exn backtrace
;;

let run_moderator_callback t borrow f =
  let open Result.Let_syntax in
  let commit ~resolved ~snapshot =
    Eio.Cancel.protect (fun () ->
      call t (Commit_moderator_invocation (borrow, resolved, snapshot)))
  in
  let finish failure =
    Eio.Cancel.protect (fun () -> call t (Finish_moderator_invocation (borrow, failure)))
  in
  let failed message =
    let message =
      if String.is_empty message || String.length message > 16_384
      then "moderator handler failed; diagnostic is outside the invocation message limit"
      else message
    in
    Agent_protocol.Invocation.Fail
      { code = "invocation.handler_failed"; message; retryable = false; details = `Null }
  in
  match f ~dispatched:borrow.invocation ~commit with
  | result ->
    let failure =
      match result with
      | Ok () -> None
      | Error (failure : Agent_protocol.Error.t) -> Some (failed failure.message)
    in
    let%bind () = finish failure in
    result
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    let failure =
      match exn with
      | Eio.Cancel.Cancelled _ ->
        Agent_protocol.Invocation.Cancelled "moderator handler cancelled"
      | _ -> failed (Exn.to_string exn)
    in
    ignore (finish (Some failure) : (unit, Agent_protocol.Error.t) result);
    Stdlib.Printexc.raise_with_backtrace exn backtrace
;;

let run_moderator_borrow t (borrow : moderator_borrow) f =
  match borrow.operation_id with
  | Some _ -> run_moderator_callback t borrow f
  | None ->
    Eio.Cancel.sub (fun context ->
      run_moderator_callback t borrow (fun ~dispatched ~commit ->
        let open Result.Let_syntax in
        let%bind () =
          call
            t
            (Set_idle_moderator_cancel (borrow, fun () -> Eio.Cancel.cancel context Exit))
        in
        f ~dispatched ~commit))
;;

let with_moderator_borrow_unlocked t ~claim f =
  let open Result.Let_syntax in
  (* Once admitted, always reach protected completion even during cancellation. *)
  Eio.Fiber.yield ();
  let%bind borrow = Eio.Cancel.protect (fun () -> call t claim) in
  run_moderator_borrow t borrow f
;;

let with_moderator_gate t f =
  match Chat_response.Execution_gate.with_access t.invocation_gate f with
  | Ok result -> result
  | Error failure ->
    let code =
      match failure with
      | Chat_response.Execution_gate.Resource_limit -> Agent_protocol.Error.Resource_limit
      | Reentrant | Wait_cycle -> Conflict
    in
    Error (error code (Chat_response.Execution_gate.error_message failure))
;;

let with_moderator_checkpoint = with_moderator_gate

let run_queued_event_borrow t borrow f =
  let open Result.Let_syntax in
  let finish interrupted =
    Eio.Cancel.protect (fun () -> call t (Finish_queued_event (borrow, interrupted)))
  in
  let execute () =
    Eio.Cancel.sub (fun context ->
      let active = ref true in
      Exn.protect
        ~finally:(fun () -> active := false)
        ~f:(fun () ->
          let%bind () =
            call
              t
              (Set_queued_event_cancel
                 ( borrow
                 , fun () ->
                     match !active with
                     | true -> Eio.Cancel.cancel context Exit
                     | false -> () ))
          in
          f
            ~borrow
            ~event:borrow.event
            ~commit:(fun ~decision ~notifications ~snapshot ~requests ->
              Eio.Cancel.protect (fun () ->
                call
                  t
                  (Commit_queued_event
                     (borrow, snapshot, requests, decision, notifications))))))
  in
  match execute () with
  | result ->
    let%bind () = finish false in
    let%bind () = result in
    (match borrow.committed with
     | true -> Ok true
     | false ->
       Error (error Invalid_state "queued event returned without a checkpoint commit"))
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    let interrupted =
      match exn with
      | Eio.Cancel.Cancelled _ -> true
      | _ -> false
    in
    ignore (finish interrupted : (unit, Agent_protocol.Error.t) result);
    Stdlib.Printexc.raise_with_backtrace exn backtrace
;;

let with_queued_event_borrow t ~claim f =
  with_moderator_gate t (fun () ->
    let open Result.Let_syntax in
    Eio.Fiber.yield ();
    let%bind claim = claim () in
    let%bind claimed =
      match claim with
      | None -> Ok None
      | Some claim -> Eio.Cancel.protect (fun () -> call t claim)
    in
    match claimed with
    | None -> Ok false
    | Some borrow ->
      run_queued_event_borrow t borrow (fun ~borrow ~event ~commit ->
        f ~borrow ~event ~commit:(fun ~snapshot ~requests ->
          commit ~decision:None ~notifications:[] ~snapshot ~requests)))
;;

let with_delegated_moderator_event
      ?(notifications = fun () -> [])
      t
      ~delegation
      ~event
      ~authorize
      ~snapshot
      f
  =
  with_moderator_gate t (fun () ->
    let open Result.Let_syntax in
    let%bind () = authorize () in
    let%bind snapshot = snapshot () in
    let%bind claimed =
      Eio.Cancel.protect (fun () ->
        call
          t
          (Claim_delegated_event
             (Agent_protocol.Id.Moderator_execution.create (), delegation, snapshot, event)))
    in
    match claimed with
    | Delegated_unavailable -> Ok None
    | Delegated_replay previous ->
      let%map () = authorize () in
      Some previous
    | Delegated_borrow borrow ->
      let%bind _ =
        run_queued_event_borrow t borrow (fun ~borrow ~event ~commit ->
          let active = Atomic.make true in
          let check () =
            let%bind () = authorize () in
            match Atomic.get active with
            | false ->
              Error (error Conflict "delegated policy invocation scope has ended")
            | true -> Ok ()
          in
          let execute ~invocation callback =
            let%bind () = check () in
            let%bind result =
              with_invocation_claim
                t
                (Claim_event_invocation (borrow, invocation))
                (fun ~dispatched ->
                   let%bind () = check () in
                   callback ~dispatched)
            in
            (* Keep the native outcome even if the delegated caller loses access
               while it runs. Rechecking inside the callback after the effect
               would discard known results before the actor could persist them. *)
            let%map () = check () in
            result
          in
          Exn.protect
            ~finally:(fun () -> Atomic.set active false)
            ~f:(fun () ->
              let%bind () = check () in
              f
                ~executing:borrow.receipt
                ~event
                ~execute
                ~commit:(fun ~decision ~snapshot ~requests ->
                  let%bind () = check () in
                  commit
                    ~decision:(Some decision)
                    ~notifications:(notifications ())
                    ~snapshot
                    ~requests)))
      in
      let%bind state = call t State in
      let%bind () = authorize () in
      List.find state.moderator_executions ~f:(fun receipt ->
        Agent_protocol.Id.Moderator_execution.equal
          receipt.context.id
          borrow.receipt.context.id)
      |> Result.of_option
           ~error:(error Internal_error "delegated policy receipt disappeared")
      |> Result.map ~f:Option.some)
;;

let with_idle_queued_moderator_event t ~snapshot f =
  with_queued_event_borrow
    t
    ~claim:(fun () ->
      Ok
        (Some
           (Claim_queued_event
              (Agent_protocol.Id.Moderator_execution.create (), None, snapshot))))
    (fun ~borrow:_ ~event ~commit -> f ~event ~commit)
;;

let with_queued_moderator_retirement t ~id ~snapshot ~reason f =
  with_queued_event_borrow
    t
    ~claim:(fun () -> Ok (Some (Claim_queued_retirement (id, snapshot, reason))))
    (fun ~borrow:_ ~event ~commit ->
       f ~event ~commit:(fun ~snapshot ->
         commit
           ~snapshot
           ~requests:
             { request_turn = false; request_compaction = false; end_session = None }))
;;

let with_event_tools t ~claim f =
  with_queued_event_borrow t ~claim (fun ~borrow ~event ~commit ->
    let active = Atomic.make true in
    let execute ~invocation callback =
      match Atomic.get active with
      | false -> Error (error Conflict "event invocation scope has ended")
      | true ->
        with_invocation_claim t (Claim_event_invocation (borrow, invocation)) callback
    in
    Exn.protect
      ~finally:(fun () -> Atomic.set active false)
      ~f:(fun () ->
        f
          ~executing:borrow.receipt
          ~retirement_reason:borrow.retirement_reason
          ~event
          ~execute
          ~commit))
;;

let with_queued_moderator_event_tools t ~operation_id ~snapshot f =
  with_event_tools
    t
    ~claim:(fun () ->
      Ok
        (Some
           (Claim_queued_event
              (Agent_protocol.Id.Moderator_execution.create (), operation_id, snapshot))))
    f
;;

let with_idle_queued_moderator_event_tools t =
  with_queued_moderator_event_tools t ~operation_id:None
;;

let with_ordinary_moderator_event t ~operation_id ~snapshot ~event f =
  with_event_tools
    t
    ~claim:(fun () ->
      Ok
        (Some
           (Claim_ordinary_event
              ( Agent_protocol.Id.Moderator_execution.create ()
              , operation_id
              , snapshot
              , event ))))
    f
;;

let with_current_moderator_event t ~operation_id ~event ~snapshot f =
  with_event_tools
    t
    ~claim:(fun () ->
      Result.map (snapshot ()) ~f:(fun snapshot ->
        Some
          (Claim_ordinary_event
             ( Agent_protocol.Id.Moderator_execution.create ()
             , operation_id
             , snapshot
             , event ))))
    f
;;

type job_execution =
  { job : Agent_protocol.Job.t
  ; execute : Native_tool_invocation.executor
  ; moderator_execute : Native_tool_invocation.moderator_executor
  ; claim_event : event:Chat_response.Moderation.Event.t -> Moderator_event.claim
  }

let with_job_execution t ~job_id ~generation ~attempt ~deadline f =
  with_job_scope t ~job_id ~generation ~attempt ~deadline (fun ~scope ~execute ->
    let claim_event ~event ~snapshot callback =
      with_event_tools
        t
        ~claim:(fun () ->
          Result.map (snapshot ()) ~f:(fun snapshot ->
            Some
              (Claim_job_event
                 (scope, Agent_protocol.Id.Moderator_execution.create (), snapshot, event))))
        callback
    in
    let moderator_execute ~invocation callback =
      with_moderator_gate t (fun () ->
        with_moderator_borrow_unlocked
          t
          ~claim:(Claim_job_moderator (scope, invocation))
          callback)
    in
    f { job = scope.job; execute; moderator_execute; claim_event })
;;

let with_job_invocations t ~job_id ~generation ~attempt ~deadline f =
  with_job_execution t ~job_id ~generation ~attempt ~deadline (fun execution ->
    f ~job:execution.job ~execute:execution.execute)
;;

let with_current_queued_moderator_event_tools t ~operation_id ~snapshot f =
  with_event_tools
    t
    ~claim:(fun () ->
      Result.map (snapshot ()) ~f:(fun snapshot ->
        match
          snapshot.Session.Moderator_state.Identity_snapshot.queued_internal_events
        with
        | [] -> None
        | _ ->
          Some
            (Claim_queued_event
               (Agent_protocol.Id.Moderator_execution.create (), operation_id, snapshot))))
    f
;;

let with_current_idle_queued_moderator_event_tools t =
  with_current_queued_moderator_event_tools t ~operation_id:None
;;

let with_moderator_invocation t operation_id ~invocation f =
  with_moderator_gate t (fun () ->
    with_moderator_borrow_unlocked
      t
      ~claim:(Claim_moderator_invocation (operation_id, invocation))
      f)
;;

let with_moderator_observation t operation_id ~invocation_id f =
  with_moderator_gate t (fun () ->
    with_moderator_borrow_unlocked
      t
      ~claim:(Claim_moderator_observation (operation_id, invocation_id))
      (fun ~dispatched ~commit -> f ~observing:dispatched ~commit))
;;

let with_selected_moderator_observation t claim f =
  with_moderator_gate t (fun () ->
    let open Result.Let_syntax in
    Eio.Fiber.yield ();
    let%bind borrow = Eio.Cancel.protect (fun () -> call t claim) in
    match borrow with
    | None -> Ok false
    | Some borrow ->
      let%map () =
        run_moderator_borrow t borrow (fun ~dispatched ~commit ->
          f ~observing:dispatched ~commit)
      in
      true)
;;

let with_next_moderator_observation t operation_id ~observer f =
  with_selected_moderator_observation
    t
    (Claim_next_moderator_observation (operation_id, observer))
    f
;;

let with_idle_moderator_observation t ~observer f =
  with_selected_moderator_observation
    t
    (Claim_idle_moderator_observation (observer, false))
    f
;;

let with_idle_moderator_observation_tools t ~observer f =
  with_moderator_gate t (fun () ->
    let open Result.Let_syntax in
    Eio.Fiber.yield ();
    let%bind borrow =
      Eio.Cancel.protect (fun () ->
        call t (Claim_idle_moderator_observation (observer, true)))
    in
    match borrow with
    | None -> Ok false
    | Some borrow ->
      let active = Atomic.make true in
      let execute ~invocation callback =
        match Atomic.get active with
        | false -> Error (error Conflict "idle invocation scope has ended")
        | true ->
          with_invocation_claim t (Claim_idle_invocation (borrow, invocation)) callback
      in
      let%map () =
        run_moderator_borrow t borrow (fun ~dispatched ~commit ->
          Exn.protect
            ~f:(fun () -> f ~observing:dispatched ~execute ~commit)
            ~finally:(fun () -> Atomic.set active false))
      in
      true)
;;

let worker_capabilities t operation_id id_source buffer =
  Operation_worker.Capabilities.
    { id_source =
        History_id_source.as_history_entry_source
          ~committed_through:(fun () ->
            Result.map (call t State) ~f:(fun state ->
              state.conversation.reserved_history_through))
          id_source
    ; commit_entry = (fun entry -> call t (Commit_worker_entry (operation_id, entry)))
    ; prepare_authoring_input =
        (fun materialization ~history ~effective ->
          Eio.Cancel.protect (fun () ->
            call
              t
              (Prepare_authoring_input (operation_id, materialization, history, effective))))
    ; commit_invocation_call =
        (fun ~invocation entry ->
          Eio.Cancel.protect (fun () ->
            call t (Commit_invocation_call (operation_id, invocation, entry))))
    ; publish_invocation_output =
        (fun ~invocation_id entry ->
          Eio.Cancel.protect (fun () ->
            call t (Publish_invocation_output (operation_id, invocation_id, entry))))
    ; commit_moderator =
        (fun snapshot -> call t (Commit_worker_moderator (operation_id, snapshot)))
    ; with_moderator_invocation = with_moderator_invocation t operation_id
    ; with_moderator_observation = with_moderator_observation t operation_id
    ; with_next_moderator_observation = with_next_moderator_observation t operation_id
    ; with_moderator_event =
        (fun ~snapshot ~event ->
          with_current_moderator_event
            t
            ~operation_id:(Some operation_id)
            ~event
            ~snapshot)
    ; with_queued_moderator_event =
        with_current_queued_moderator_event_tools t ~operation_id:(Some operation_id)
    ; manage_moderator_follow_up =
        (fun ~observer -> call t (Manage_moderator_follow_up (operation_id, observer)))
    ; admit_moderator_turn = (fun () -> call t (Admit_moderator_turn operation_id))
    ; admit_notification_turn = (fun () -> call t (Admit_notification_turn operation_id))
    ; with_invocation = with_invocation t operation_id
    ; consume_deferred = (fun () -> call t (Consume_deferred operation_id))
    ; request_permission =
        (fun ~permission ~timeout_seconds ~fallback ~review_on_timeout ->
          request_permission_with_review_internal
            t
            ~permission
            ~timeout_seconds
            ~fallback
            ~review_on_timeout)
    ; request_review =
        (fun ~permission ~review -> request_review_internal t ~permission ~review)
    ; responder_available =
        (fun () ->
          call t Has_writer_attachment |> Result.ok |> Option.value ~default:false)
    ; invocation_granted =
        (fun ~tool_name ~identity_digest ->
          call t (Invocation_granted (tool_name, identity_digest))
          |> Result.ok
          |> Option.value ~default:false)
    ; publish_live = publish_worker_live t buffer
    }
;;

let run_worker_operation worker input capabilities cancelled =
  match
    Eio.Fiber.first
      (fun () ->
         Eio.Switch.run (fun sw ->
           `Finished (Operation_worker.run worker ~sw ~input capabilities)))
      (fun () ->
         Eio.Promise.await cancelled;
         `Cancelled)
  with
  | `Finished outcome -> outcome
  | `Cancelled -> Operation_worker.Cancelled { reason = "operation cancelled" }
;;

let run_worker t worker operation history =
  let operation_id = operation.Agent_protocol.Operation.id in
  let id_source =
    History_id_source.create
      ~namespace:(Agent_protocol.Id.Session.to_string t.state.identity.session_id)
      ~block_size:64
      ~reserve:(fun ~count -> call t (Reserve_history_block count))
  in
  match id_source with
  | Error failure -> Operation_worker.Failed failure
  | Ok id_source ->
    let buffer =
      Live_event_buffer.create
        ~capacity:2_048
        ~session_id:t.state.identity.session_id
        ~operation_id
    in
    let capabilities = worker_capabilities t operation_id id_source buffer in
    let input =
      Operation_worker.Input.
        { session_id = t.state.identity.session_id
        ; session_generation = t.state.identity.generation
        ; operation
        ; history
        }
    in
    let cancelled, cancel_resolver = Eio.Promise.create () in
    let cancel () = ignore (Eio.Promise.try_resolve cancel_resolver ()) in
    (try
       match call t ~priority:Priority (Worker_ready (operation_id, cancel)) with
       | Error failure -> Operation_worker.Failed failure
       | Ok () -> run_worker_operation worker input capabilities cancelled
     with
     | Eio.Cancel.Cancelled reason ->
       Operation_worker.Cancelled { reason = Exn.to_string reason }
     | exn -> Operation_worker.Failed (worker_failure exn))
;;

let launch_worker t operation =
  let operation_id = operation.Agent_protocol.Operation.id in
  let history =
    let open Result.Let_syntax in
    let%bind () = reconcile_foreground_invocations t in
    History_codec.all_of_protocol t.state.conversation.canonical_history
  in
  match history with
  | Error failure ->
    Eio.Fiber.fork ~sw:t.sw (fun () ->
      ignore
        (call t ~priority:Priority (Worker_terminal (operation_id, Failed failure))
         : (unit, Agent_protocol.Error.t) result))
  | Ok history ->
    Eio.Fiber.fork ~sw:t.sw (fun () ->
      let outcome =
        match t.operation_worker with
        | Some worker -> run_worker t worker operation history
        | None ->
          Operation_worker.Failed
            (error Invalid_state "session has no foreground operation worker")
      in
      ignore
        (call t ~priority:Priority (Worker_terminal (operation_id, outcome))
         : (unit, Agent_protocol.Error.t) result))
;;

let create_turn_operation t reason =
  let timestamp = t.services.now () in
  Agent_protocol.Operation.
    { id = Agent_protocol.Id.Operation.create ()
    ; generation = t.state.identity.generation
    ; kind = Turn reason
    ; state = Starting
    ; started_at = timestamp
    ; updated_at = timestamp
    }
;;

let submit_idle_message ?(extra_deltas = []) t entry =
  let open Result.Let_syntax in
  let%bind () = reconcile_foreground_invocations t in
  let operation = create_turn_operation t User_submit in
  let lifecycle = lifecycle_for_operation t operation.id in
  let delta =
    Session_delta.Batch
      (extra_deltas
       @ [ Session_delta.Canonical_entries_appended [ entry ]
         ; Active_operation_changed (Some operation)
         ; Lifecycle_changed lifecycle
         ])
  in
  let payloads =
    [ Agent_protocol.Event.Durable.Payload.History_appended [ entry ]
    ; Operation_started operation
    ; Session_state_changed
        { desired_state = lifecycle.desired; observed_state = lifecycle.observed }
    ]
  in
  let open Result.Let_syntax in
  let%bind session = transition t ~delta ~payloads in
  launch_worker t operation;
  Ok
    { session
    ; history_id = entry.id
    ; disposition = Started
    ; operation_id = Some operation.id
    }
;;

let submit_deferred_message ?(extra_deltas = []) t entry =
  let open Result.Let_syntax in
  let%map session = defer_history ~extra_deltas t [ entry ] in
  { session; history_id = entry.id; disposition = Deferred; operation_id = None }
;;

let submit_authorized_message ?(extra_deltas = []) t entry =
  if t.state.halted
  then Error (error Invalid_state "session is halted")
  else if Option.is_some t.state.failure
  then Error (error Invalid_state "session has failed")
  else if
    not (Agent_protocol.Session.equal_desired_state t.state.lifecycle.desired Running)
  then Error (error Invalid_state "session is not running")
  else (
    match
      t.idle_moderator_borrowed, t.state.active_operation, t.state.lifecycle.observed
    with
    | true, _, _ -> submit_deferred_message ~extra_deltas t entry
    | false, Some _, _
    | false, None, (Running_turn _ | Compacting _ | Waiting_for_permission _) ->
      submit_deferred_message ~extra_deltas t entry
    | false, None, Idle -> submit_idle_message ~extra_deltas t entry
    | ( false
      , None
      , (Stopped | Queued_for_slot | Starting | Recovering | Stopping | Failed _) ) ->
      Error (error Invalid_state "session is not ready to accept a turn"))
;;

let submit_message t attachment_id entry =
  Result.bind (write_attachment t attachment_id) ~f:(fun _ ->
    submit_authorized_message t entry)
;;

let submit_managed_message t reference key request_sha256 generation max_receipts entry =
  let module M = Managed_submission in
  let open Result.Let_syntax in
  let%bind () =
    match t.state.spec.delegation with
    | Some current
      when Agent_store.Delegation_store.Reference.equal current reference
           && Agent_protocol.Id.Session.equal
                reference.child_session_id
                t.state.identity.session_id -> Ok ()
    | _ ->
      Error (error Permission_denied "delegation.send: child relationship does not match")
  in
  let%bind candidate =
    M.create
      ~reference
      ~key
      ~request_sha256
      ~generation
      ~history_id:entry.Agent_protocol.History.id
      ~now:(t.services.now ())
  in
  match List.find t.state.managed_submissions ~f:(M.same_key candidate) with
  | Some receipt ->
    (match String.equal receipt.request_sha256 request_sha256 with
     | true -> Ok receipt
     | false -> Error (error Conflict "managed send key was used for a different request"))
  | None ->
    let%bind () =
      match
        ( Int.equal generation t.state.identity.generation
        , entry.role
        , entry.kind
        , entry.provenance )
      with
      | true, User, Message, Canonical -> Ok ()
      | _ -> Error (error Conflict "managed message generation or input is invalid")
    in
    let%bind () =
      match max_receipts with
      | Some maximum
        when maximum < 1 || List.length t.state.managed_submissions >= maximum ->
        Error (error Invalid_state "managed submission receipt capacity reached")
      | None | Some _ -> Ok ()
    in
    let%bind _ =
      submit_authorized_message
        t
        entry
        ~extra_deltas:[ Managed_submission_admitted candidate ]
    in
    List.find t.state.managed_submissions ~f:(M.same_key candidate)
    |> Result.of_option
         ~error:(error Invalid_state "managed send did not retain its receipt")
;;

let find_permission t permission_id =
  List.find t.state.permissions ~f:(fun permission ->
    Agent_protocol.Id.Permission.compare permission.id permission_id = 0)
  |> Result.of_option ~error:(error Invalid_request "permission request does not exist")
;;

let permission_state = function
  | Agent_protocol.Permission.Deny -> Agent_protocol.Permission.Denied
  | Approve_once | Approve_session | Approve_prefix | Durable_exact -> Approved
;;

let grant_scope = function
  | Agent_protocol.Permission.Approve_session -> Some Agent_protocol.Grant.Exact_session
  | Approve_prefix -> Some Prefix_session
  | Durable_exact -> Some Durable_exact
  | Approve_once | Deny -> None
;;

let grant_identity permission scope =
  match scope with
  | Agent_protocol.Grant.Prefix_session ->
    Ok
      (Permission_policy.prefix_identity_digest
         ~tool_name:permission.Agent_protocol.Permission.tool_name)
  | Exact_session | Durable_exact ->
    permission.runtime_identity
    |> Result.of_option
         ~error:(error Invalid_state "permission request has no runtime identity")
;;

let grant_for_resolution
      (t : t)
      (permission : Agent_protocol.Permission.t)
      principal_id
      choice
  =
  match grant_scope choice with
  | None -> Ok None
  | Some scope ->
    let open Result.Let_syntax in
    let%bind principal_id =
      principal_id
      |> Result.of_option
           ~error:(error Permission_denied "grant approval requires a principal")
    in
    let%map identity_digest = grant_identity permission scope in
    Some
      Agent_protocol.Grant.
        { id = Agent_protocol.Id.Grant.create ()
        ; session_id = t.state.identity.session_id
        ; principal_id
        ; tool_name = permission.tool_name
        ; identity_digest
        ; scope
        ; state = Active
        ; created_at = t.services.now ()
        ; expires_at = None
        ; revoked_at = None
        ; revocation_reason = None
        }
;;

let permission_owner_active t (permission : Agent_protocol.Permission.t) =
  match permission.owner with
  | Operation id ->
    (match t.state.active_operation with
     | Some operation when not (Agent_protocol.Id.Operation.equal operation.id id) ->
       Error (error Conflict "permission request belongs to another active operation")
     | _ -> Ok ())
  | Invocation id ->
    let live =
      List.exists t.invocation_executions ~f:(fun execution ->
        execution.accepts_children
        && Agent_protocol.Id.Invocation.equal id execution.dispatched.context.id
        && execution.dispatched.context.generation = permission.generation
        &&
        match execution.owner with
        | Foreground id ->
          Option.exists t.state.active_operation ~f:(fun operation ->
            Agent_protocol.Id.Operation.equal operation.id id
            &&
            match operation.state with
            | Cancelling -> false
            | _ -> true)
        | Background_job scope -> Result.is_ok (job_scope_can_execute t scope)
        | Invocation_moderator borrow ->
          (not (borrow.committed || borrow.cancel_requested))
          && Option.exists t.moderator_borrow ~f:(phys_equal borrow)
          &&
            (match borrow.job_scope, borrow.operation_id with
            | Some scope, None -> Result.is_ok (job_scope_can_execute t scope)
            | Some _, Some _ -> false
            | None, None ->
              t.idle_moderator_borrowed && Option.is_none t.state.active_operation
            | None, Some id ->
              Option.exists t.state.active_operation ~f:(fun operation ->
                Agent_protocol.Id.Operation.equal operation.id id
                &&
                match operation.state with
                | Cancelling -> false
                | _ -> true))
        | Event_moderator borrow ->
          t.idle_moderator_borrowed
          && borrow.callback_active
          && (not (borrow.committed || borrow.cancel_requested))
          && Option.exists t.queued_event_borrow ~f:(phys_equal borrow)
          &&
            (match borrow.job_scope, borrow.receipt.context.operation_id with
            | Some scope, None -> Result.is_ok (job_scope_can_execute t scope)
            | Some _, Some _ -> false
            | None, None -> Option.is_none t.state.active_operation
            | None, Some id ->
              Option.exists t.state.active_operation ~f:(fun operation ->
                Agent_protocol.Id.Operation.equal id operation.id
                &&
                match operation.state with
                | Cancelling -> false
                | _ -> true)))
      || Option.exists t.moderator_borrow ~f:(fun borrow ->
        borrow.accepts_children
        && (match borrow.kind with
            | Invocation -> true
            | Observation -> false)
        && (not (borrow.committed || borrow.cancel_requested))
        && Agent_protocol.Id.Invocation.equal id borrow.invocation.context.id
        && Int.equal borrow.invocation.context.generation permission.generation
        &&
        match borrow.job_scope, borrow.operation_id with
        | Some scope, None -> Result.is_ok (job_scope_can_execute t scope)
        | None, None | Some _, Some _ -> false
        | None, Some id ->
          Option.exists t.state.active_operation ~f:(fun operation ->
            Agent_protocol.Id.Operation.equal operation.id id
            &&
            match operation.state with
            | Cancelling -> false
            | _ -> true))
    in
    (match t.state.lifecycle.desired, live, t.state.halted, t.state.failure with
     | Running, true, false, None -> Ok ()
     | _ -> Error (error Conflict "permission requires a live invocation owner"))
;;

let resolve_permission
      t
      (permission : Agent_protocol.Permission.t)
      ~principal_id
      ~choice
      ~reason
      ~resume_observed
  =
  let resolution =
    Agent_protocol.Permission.
      { choice; principal_id; resolved_at = t.services.now (); reason }
  in
  let permission =
    { permission with state = permission_state choice; resolution = Some resolution }
  in
  let open Result.Let_syntax in
  let%bind () =
    match choice with
    | Agent_protocol.Permission.Deny -> Ok ()
    | Approve_once | Approve_session | Approve_prefix | Durable_exact ->
      permission_owner_active t permission
  in
  let%bind grant = grant_for_resolution t permission principal_id choice in
  let lifecycle =
    Session_state.Lifecycle.
      { desired = t.state.lifecycle.desired
      ; observed =
          permission_resume_observed
            t
            ~resolved:[ permission.id ]
            ~fallback:resume_observed
      }
  in
  let%bind _ =
    transition
      t
      ~delta:
        (Session_delta.Batch
           ([ Session_delta.Permission_changed permission
            ; Session_delta.Lifecycle_changed lifecycle
            ]
            @ Option.to_list
                (Option.map grant ~f:(fun value -> Session_delta.Grant_changed value))))
      ~payloads:
        ([ Agent_protocol.Event.Durable.Payload.Permission_resolved permission
         ; Agent_protocol.Event.Durable.Payload.Session_state_changed
             { desired_state = lifecycle.desired; observed_state = lifecycle.observed }
         ]
         @ Option.to_list
             (Option.map grant ~f:(fun value ->
                Agent_protocol.Event.Durable.Payload.Grant_created value)))
  in
  Option.iter (Map.find !(t.permission_waiters) permission.id) ~f:(fun waiter ->
    Eio.Promise.resolve waiter.resolver resolution);
  t.permission_waiters := Map.remove !(t.permission_waiters) permission.id;
  Ok permission
;;

let schedule_permission_expiry t permission timeout_seconds fallback =
  if t.schedule_permission_timeouts
  then
    Option.iter timeout_seconds ~f:(fun seconds ->
      Eio.Fiber.fork ~sw:t.sw (fun () ->
        t.sleep seconds;
        let promise, resolver = Eio.Promise.create () in
        let request =
          Expire_permission
            (permission.Agent_protocol.Permission.id, permission.generation, fallback)
        in
        match
          Mailbox.push t.mailbox ~priority:Priority (Pack (None, request, resolver))
        with
        | Error _ -> ()
        | Ok () ->
          ignore (Eio.Promise.await promise : (unit, Agent_protocol.Error.t) result)))
;;

let open_permission t (permission : Agent_protocol.Permission.t) timeout_seconds fallback =
  let open Result.Let_syntax in
  let%bind () = permission_owner_active t permission in
  if
    Agent_protocol.Id.Session.compare permission.session_id t.state.identity.session_id
    <> 0
  then Error (error Invalid_request "permission request belongs to another session")
  else if permission.generation <> t.state.identity.generation
  then Error (error Conflict "permission request belongs to a stale session generation")
  else if not (Agent_protocol.Permission.equal_state permission.state Pending)
  then Error (error Invalid_request "new permission request must be pending")
  else if
    not
      (List.mem permission.choices fallback ~equal:Agent_protocol.Permission.equal_choice)
  then Error (error Invalid_request "permission fallback choice is not offered")
  else if
    List.exists t.state.permissions ~f:(fun previous ->
      Agent_protocol.Id.Permission.equal previous.id permission.id)
  then Error (error Conflict "permission request identity is already retained")
  else (
    let response, resolver = Eio.Promise.create () in
    let resume_observed =
      match t.state.lifecycle.observed with
      | Waiting_for_permission id ->
        Option.value_map
          (Map.find !(t.permission_waiters) id)
          ~default:t.state.lifecycle.observed
          ~f:(fun waiter -> waiter.resume_observed)
      | observed -> observed
    in
    let lifecycle =
      Session_state.Lifecycle.
        { desired = t.state.lifecycle.desired
        ; observed = Waiting_for_permission permission.id
        }
    in
    let open Result.Let_syntax in
    let%bind _ =
      transition
        t
        ~delta:
          (Session_delta.Batch
             [ Permission_changed permission; Lifecycle_changed lifecycle ])
        ~payloads:
          [ Agent_protocol.Event.Durable.Payload.Permission_requested permission
          ; Session_state_changed
              { desired_state = lifecycle.desired; observed_state = lifecycle.observed }
          ]
    in
    t.permission_waiters
    := Map.set
         !(t.permission_waiters)
         ~key:permission.id
         ~data:{ resolver; resume_observed };
    schedule_permission_expiry t permission timeout_seconds fallback;
    Ok response)
;;

let respond_permission_internal
      t
      attachment_id
      principal_id
      permission_id
      permission_generation
      choice
      reason
  =
  let open Result.Let_syntax in
  let%bind _ = write_attachment t attachment_id in
  let%bind permission = find_permission t permission_id in
  if permission.generation <> permission_generation
  then Error (error Conflict "permission generation does not match")
  else if not (Agent_protocol.Permission.equal_state permission.state Pending)
  then Error (error Already_resolved "permission has already been resolved")
  else if
    not (List.mem permission.choices choice ~equal:Agent_protocol.Permission.equal_choice)
  then Error (error Invalid_request "permission choice is not offered")
  else (
    match Map.find !(t.permission_waiters) permission_id with
    | None -> Error (error Interrupted "permission continuation is unavailable")
    | Some waiter ->
      resolve_permission
        t
        permission
        ~principal_id
        ~choice
        ~reason
        ~resume_observed:waiter.resume_observed)
;;

let resolve_permission_system t permission_id generation choice reason =
  let open Result.Let_syntax in
  let%bind permission = find_permission t permission_id in
  if permission.generation <> generation
  then Error (error Conflict "permission generation does not match")
  else if not (Agent_protocol.Permission.equal_state permission.state Pending)
  then Error (error Already_resolved "permission has already been resolved")
  else if
    not (List.mem permission.choices choice ~equal:Agent_protocol.Permission.equal_choice)
  then Error (error Invalid_request "permission choice is not offered")
  else (
    match Map.find !(t.permission_waiters) permission_id with
    | None -> Error (error Interrupted "permission continuation is unavailable")
    | Some waiter ->
      resolve_permission
        t
        permission
        ~principal_id:None
        ~choice
        ~reason
        ~resume_observed:waiter.resume_observed)
;;

let expire_permission_internal t permission_id generation fallback =
  match
    find_permission t permission_id, Map.find !(t.permission_waiters) permission_id
  with
  | Ok permission, Some waiter
    when permission.generation = generation
         && Agent_protocol.Permission.equal_state permission.state Pending ->
    resolve_permission
      t
      permission
      ~principal_id:None
      ~choice:fallback
      ~reason:(Some "permission request timed out")
      ~resume_observed:waiter.resume_observed
    |> Result.map ~f:(fun _ -> ())
  | _ -> Ok ()
;;

let revoke_grant t attachment_id grant_id reason =
  with_writer t attachment_id (fun () ->
    let open Result.Let_syntax in
    let%bind update =
      Security_grant.revoke
        ~now:(t.services.now ())
        ~reason
        ~session_id:t.state.identity.session_id
        ~creating_principal:t.state.identity.creating_principal
        ~generic:t.state.grants
        ~shell:t.state.shell
        grant_id
    in
    let delta, grant =
      match update with
      | Security_grant.Generic grant -> Session_delta.Grant_changed grant, grant
      | Shell (shell, grant) -> Session_delta.Shell_changed shell, grant
    in
    let%map session =
      transition
        t
        ~delta
        ~payloads:[ Agent_protocol.Event.Durable.Payload.Grant_revoked grant ]
    in
    grant, session)
;;

let replace_shell_approval_grants t approval_grants =
  let shell = { t.state.shell with approval_grants } in
  transition t ~delta:(Session_delta.Shell_changed shell) ~payloads:[]
  |> Result.map ~f:(fun _ -> ())
;;

let add_shell_manifest_grant t grant =
  let manifest_grants = grant :: t.state.shell.manifest_grants in
  let shell = { t.state.shell with manifest_grants } in
  let projected =
    Security_grant.project_manifest
      ~now:(t.services.now ())
      ~session_id:t.state.identity.session_id
      ~creating_principal:t.state.identity.creating_principal
      grant
  in
  transition
    t
    ~delta:(Session_delta.Shell_changed shell)
    ~payloads:[ Agent_protocol.Event.Durable.Payload.Grant_created projected ]
  |> Result.map ~f:(fun _ -> ())
;;

let change_job t attachment_id (job : Agent_protocol.Job.t) =
  with_writer t attachment_id (fun () ->
    match
      List.exists t.job_scopes ~f:(fun scope ->
        Agent_protocol.Id.Job.equal scope.job.id job.id)
    with
    | true -> Error (error Conflict "job execution owns its current record")
    | false ->
      transition
        t
        ~delta:(Session_delta.Job_changed job)
        ~payloads:[ Agent_protocol.Event.Durable.Payload.Job_state_changed job ])
;;

let find_job t job_id =
  List.find t.state.jobs ~f:(fun job -> Agent_protocol.Id.Job.compare job.id job_id = 0)
  |> Result.of_option ~error:(error Invalid_request "job was not found")
;;

let job_with_progress t (job : Agent_protocol.Job.t) =
  let progress =
    List.find_map t.job_scopes ~f:(fun scope ->
      match
        scope.active
        && (not scope.cancel_requested)
        && Agent_protocol.Id.Job.equal scope.job.id job.id
        && Int.equal scope.job.generation job.generation
        && Int.equal scope.job.attempt job.attempt
      with
      | true -> Job_progress_buffer.snapshot scope.progress
      | false -> None)
  in
  { job with progress }
;;

let publish_job_progress_internal t id progress =
  match
    List.find t.invocation_executions ~f:(fun execution ->
      execution.accepts_children
      && Agent_protocol.Id.Invocation.equal execution.dispatched.context.id id)
  with
  | None -> ()
  | Some execution ->
    let scope =
      match execution.owner with
      | Background_job scope -> Some scope
      | Invocation_moderator borrow -> borrow.job_scope
      | Event_moderator borrow -> borrow.job_scope
      | Foreground _ -> None
    in
    Option.iter scope ~f:(fun scope ->
      match job_scope_can_execute t scope with
      | Ok () -> Job_progress_buffer.update scope.progress progress
      | Error _ -> ())
;;

let validate_job_generation t (job : Agent_protocol.Job.t) generation =
  if job.generation <> generation || t.state.identity.generation <> generation
  then Error (error Conflict "job belongs to a stale session generation")
  else Ok ()
;;

let update_jobs ?(subscriptions = []) t jobs =
  let timer_ids =
    List.filter_map subscriptions ~f:(fun next ->
      match
        next.Agent_protocol.Subscription.result, lookup_subscription t next.context.id
      with
      | Some _, Some previous -> previous.timer_id
      | _ -> None)
    |> Hash_set.of_list (module Agent_protocol.Id.Schedule)
  in
  let cancelled_timers =
    List.filter_map t.state.schedules ~f:(fun schedule ->
      match Hash_set.mem timer_ids schedule.id, schedule.status with
      | true, (Agent_protocol.Schedule.Scheduled | Delivering) ->
        Some { schedule with status = Agent_protocol.Schedule.Cancelled }
      | _ -> None)
  in
  let cancelled =
    List.filter jobs ~f:(fun job ->
      match job.Agent_protocol.Job.status with
      | Cancelled | Interrupted _ -> true
      | Queued
      | Running
      | Waiting_permission _
      | Waiting_completion _
      | Succeeded
      | Failed _ -> false)
  in
  let owns (scope : job_scope) =
    List.exists cancelled ~f:(fun job ->
      Agent_protocol.Id.Job.equal scope.job.id job.Agent_protocol.Job.id
      && Int.equal scope.job.generation job.generation
      && Int.equal scope.job.attempt job.attempt)
  in
  let children =
    List.filter t.invocation_executions ~f:(fun execution ->
      List.exists t.job_scopes ~f:(fun scope ->
        job_execution_owned_by scope execution && owns scope))
  in
  let permissions, deltas, payloads =
    cleanup_invocation_permissions
      t
      (List.map children ~f:(fun child -> child.dispatched.context.id)
       @
       match t.moderator_borrow with
       | Some borrow when Option.exists borrow.job_scope ~f:owns ->
         [ borrow.invocation.context.id ]
       | _ -> [])
  in
  let open Result.Let_syntax in
  let%map session =
    transition
      t
      ~delta:
        (Session_delta.Batch
           (List.map jobs ~f:(fun job -> Session_delta.Job_changed job)
            @ List.map subscriptions ~f:(fun value ->
              Session_delta.Subscription_changed value)
            @ List.map cancelled_timers ~f:(fun value ->
              Session_delta.Schedule_changed value)
            @ deltas))
      ~payloads:
        (List.map jobs ~f:(fun job ->
           Agent_protocol.Event.Durable.Payload.Job_state_changed job)
         @ payloads
         @ List.map cancelled_timers ~f:(fun schedule ->
           Agent_protocol.Event.Durable.Payload.Schedule_cancelled schedule))
  in
  cancel_job_scopes t cancelled;
  resolve_cleaned_permission_waiters t permissions;
  session
;;

let update_job t job = update_jobs t [ job ]

let add_job t (job : Agent_protocol.Job.t) =
  if Agent_protocol.Id.Session.compare job.session_id t.state.identity.session_id <> 0
  then Error (error Invalid_request "job belongs to another session")
  else if job.generation <> t.state.identity.generation
  then Error (error Conflict "job belongs to a stale session generation")
  else if
    List.exists t.state.jobs ~f:(fun existing ->
      Agent_protocol.Id.Job.compare existing.id job.id = 0)
  then Error (error Conflict "job ID is already present")
  else Result.map (update_job t job) ~f:(fun _ -> job)
;;

let job_is_due t (job : Agent_protocol.Job.t) =
  Option.value_map job.next_run_at ~default:true ~f:(fun next_run_at ->
    Agent_protocol.Timestamp.compare next_run_at (t.services.now ()) <= 0)
;;

let claim_job t job_id generation =
  let open Result.Let_syntax in
  let%bind job = find_job t job_id in
  let%bind () = validate_job_generation t job generation in
  let%bind () =
    match
      List.exists t.job_scopes ~f:(fun scope ->
        Agent_protocol.Id.Job.equal scope.job.id job_id)
    with
    | false -> Ok ()
    | true -> Error (error Conflict "job's prior execution scope has not finished")
  in
  match job.status with
  | Agent_protocol.Job.Queued when job_is_due t job ->
    if job.attempt = Int.max_value
    then Error (error Invalid_state "job attempt counter overflow")
    else (
      let job =
        { job with
          status = Running
        ; attempt = job.attempt + 1
        ; started_at = Some (t.services.now ())
        ; next_run_at = None
        ; result = None
        }
      in
      let%map _ = update_job t job in
      Some job)
  | Queued -> Ok None
  | Running
  | Waiting_permission _
  | Waiting_completion _
  | Succeeded
  | Failed _
  | Cancelled
  | Interrupted _ -> Ok None
;;

let job_failure message =
  Agent_protocol.Error.create Internal_error ~message ~retryable:false ()
;;

let retry_limits = function
  | Agent_protocol.Job.Never -> None
  | Safe_retry { max_attempts; backoff_ms } | Idempotent { max_attempts; backoff_ms; _ }
    -> Some (max_attempts, backoff_ms)
;;

let retry_at t backoff_ms =
  let now = t.services.now () in
  let span = Time_ns.Span.of_ms (Float.of_int backoff_ms) in
  Agent_protocol.Timestamp.to_time_ns now
  |> Fn.flip Time_ns.add span
  |> Agent_protocol.Timestamp.of_time_ns
;;

let retry_result message = `Object [ "last_error", `String message ]

let retry_job t (job : Agent_protocol.Job.t) message backoff_ms =
  { job with
    status = Queued
  ; started_at = None
  ; next_run_at = Some (retry_at t backoff_ms)
  ; completed_at = None
  ; result = Some (retry_result message)
  }
;;

let terminal_job t (job : Agent_protocol.Job.t) status result =
  let delivery =
    match job.delivery with
    | Agent_protocol.Job.Not_required -> Agent_protocol.Job.Not_required
    | Pending | Delivered _ -> Pending
    | Discarded _ as delivery -> delivery
  in
  { job with status; result; completed_at = Some (t.services.now ()); delivery }
;;

let validate_job_dependency t job =
  Job_dependency.validate
    ~invocations:t.state.invocations
    ~events:t.state.moderator_executions
    ~jobs:t.state.jobs
    ~subscriptions:t.state.subscriptions
    job
;;

let retained_subscription t id =
  Result.of_option
    (lookup_subscription t id)
    ~error:(error Invalid_state "subscription dependency is missing")
;;

let cancelled_work_dependencies t (parent : Agent_protocol.Job.t) =
  let rec walk seen (job : Agent_protocol.Job.t) =
    let open Result.Let_syntax in
    match job.status with
    | Waiting_completion dependency ->
      let%bind () = validate_job_dependency t job in
      (match dependency.work with
       | Subscription id ->
         let%bind subscription = retained_subscription t id in
         (match subscription.result with
          | Some _ -> Ok ([], [])
          | None ->
            let%map terminal, _ =
              Agent_protocol.Subscription.finish
                subscription
                ~expected_epoch:subscription.epoch
                ~now:
                  (subscription_terminal_time
                     t
                     subscription
                     (Cancelled "owning job stopped waiting"))
                (Cancelled "owning job stopped waiting")
            in
            [], [ terminal ])
       | Job id ->
         let key = Agent_protocol.Id.Job.to_string id in
         (match Set.mem seen key with
          | true -> Error (error Invalid_state "job dependency cycle")
          | false ->
            let%bind child = find_job t id in
            let%bind terminal = Agent_protocol.Job.terminal_result child in
            (match terminal with
             | Some _ -> Ok ([], [])
             | None ->
               let%map rest, subscriptions = walk (Set.add seen key) child in
               ( terminal_job
                   t
                   child
                   Cancelled
                   (background_terminal_result
                      child
                      (Cancelled "owning job stopped waiting"))
                 :: rest
               , subscriptions ))))
    | _ -> Ok ([], [])
  in
  walk (String.Set.singleton (Agent_protocol.Id.Job.to_string parent.id)) parent
;;

let complete_job_outcome t (job : Agent_protocol.Job.t) = function
  | Runtime_builder.Model_succeeded result ->
    terminal_job t job Agent_protocol.Job.Succeeded (Some result)
  | Model_failed message ->
    (match retry_limits job.retry_policy with
     | Some (max_attempts, backoff_ms) when job.attempt < max_attempts ->
       retry_job t job message backoff_ms
     | None | Some _ -> terminal_job t job (Failed (job_failure message)) None)
;;

let validate_job_attempt (job : Agent_protocol.Job.t) attempt =
  match Int.equal job.attempt attempt with
  | true -> Ok ()
  | false -> Error (error Conflict "job callback belongs to a stale execution attempt")
;;

let claim_job_scope t job_id generation attempt deadline =
  let open Result.Let_syntax in
  let%bind job = find_job t job_id in
  let%bind () = validate_job_generation t job generation in
  let%bind () = validate_job_attempt job attempt in
  let%bind () =
    match
      ( job.status
      , List.exists t.job_scopes ~f:(fun scope ->
          Agent_protocol.Id.Job.equal scope.job.id job_id) )
    with
    | Running, false -> Ok ()
    | _ -> Error (error Conflict "job must be running without another execution owner")
  in
  let cancelled, cancel_signal = Eio.Promise.create () in
  let scope =
    { job
    ; progress = Job_progress_buffer.create ()
    ; deadline
    ; cancelled
    ; cancel_signal
    ; active = true
    ; cancel_requested = false
    ; unfinished_on_return = false
    ; cancel = None
    }
  in
  (* Validate before exposing the scope; undo the process-local reservation on
     rejection. No durable state or runnable invocation is produced here. *)
  t.job_scopes <- scope :: t.job_scopes;
  match job_scope_can_execute t scope with
  | Ok () -> Ok scope
  | Error failure ->
    t.job_scopes
    <- List.filter t.job_scopes ~f:(fun other -> not (phys_equal scope other));
    Error failure
;;

let claim_job_invocation t scope (invocation : Agent_protocol.Invocation.t) =
  let open Result.Let_syntax in
  let%bind () = job_scope_can_execute t scope in
  let%bind () =
    match
      Agent_protocol.Id.Session.equal scope.job.session_id invocation.context.session_id
      && Int.equal scope.job.generation invocation.context.generation
    with
    | true -> Ok ()
    | false ->
      Error (error Conflict "invocation belongs to another job session or generation")
  in
  let%bind () =
    match invocation.context.parent_job with
    | Some id
      when Agent_protocol.Id.Job.equal id scope.job.id
           && Agent_protocol.Invocation.equal_origin invocation.context.origin Script
           && Option.is_none invocation.context.parent_invocation
           && Option.is_none invocation.context.provider_call_id
           && Option.is_none invocation.context.call_entry_id
           && Option.is_none invocation.parent_event -> Ok ()
    | None -> active_script_parent t ~owns:(job_execution_owned_by scope) invocation
    | Some _ -> Error (error Conflict "invocation does not belong to this job scope")
  in
  let%bind () =
    match scope.deadline, invocation.context.deadline with
    | None, _ -> Ok ()
    | Some parent, Some child when Agent_protocol.Timestamp.compare child parent <= 0 ->
      Ok ()
    | _ -> Error (error Conflict "job invocation cannot extend its execution deadline")
  in
  let%bind () =
    match invocation.context.deadline with
    | Some deadline
      when Agent_protocol.Timestamp.compare (t.services.now ()) deadline >= 0 ->
      Error (error Conflict "job invocation deadline has elapsed")
    | _ -> Ok ()
  in
  let%bind admission = invocation_admission_deltas t invocation in
  let%bind dispatched = Agent_protocol.Invocation.dispatch invocation in
  let%bind _ =
    transition
      t
      ~delta:(Session_delta.Batch (admission @ [ Invocation_changed dispatched ]))
      ~payloads:[]
  in
  let execution = { owner = Background_job scope; dispatched; accepts_children = true } in
  t.invocation_executions <- execution :: t.invocation_executions;
  Ok execution
;;

let finish_job_scope t scope =
  let open Result.Let_syntax in
  let%bind () =
    match List.mem t.job_scopes scope ~equal:phys_equal with
    | true -> Ok ()
    | false -> Error (error Conflict "background execution scope has ended")
  in
  scope.active <- false;
  scope.cancel <- None;
  let%bind () =
    match t.moderator_borrow with
    | Some borrow
      when borrow.callback_finished
           && Option.exists borrow.job_scope ~f:(phys_equal scope) ->
      finish_moderator_invocation
        t
        borrow
        (Some
           (Agent_protocol.Invocation.Cancelled
              "background scope ended before moderator cleanup committed"))
    | _ -> Ok ()
  in
  let%bind () =
    match t.queued_event_borrow with
    | Some borrow
      when (not borrow.callback_active)
           && Option.exists borrow.job_scope ~f:(phys_equal scope) ->
      finish_queued_event t borrow true
    | _ -> Ok ()
  in
  let%bind () =
    match job_has_moderator t scope with
    | false -> Ok ()
    | true -> Error (error Conflict "background callback left an owned moderator running")
  in
  let children = List.filter t.invocation_executions ~f:(job_execution_owned_by scope) in
  let permissions, permission_deltas, payloads =
    cleanup_invocation_permissions
      t
      (List.map children ~f:(fun child -> child.dispatched.context.id))
  in
  let%bind deltas =
    List.map children ~f:(fun execution ->
      execution.accepts_children <- false;
      abort_staged_work t ~owner:(Invocation execution.dispatched.context.id);
      Agent_protocol.Invocation.cancel
        execution.dispatched
        ~reason:"background scope exited before recording the invocation outcome"
      |> Result.map ~f:(fun invocation -> Session_delta.Invocation_changed invocation))
    |> Result.all
  in
  let%bind () =
    match deltas @ permission_deltas with
    | [] -> Ok ()
    | deltas ->
      transition t ~delta:(Session_delta.Batch deltas) ~payloads |> Result.map ~f:ignore
  in
  resolve_cleaned_permission_waiters t permissions;
  t.invocation_executions
  <- List.filter t.invocation_executions ~f:(fun execution ->
       not (job_execution_owned_by scope execution));
  t.job_scopes <- List.filter t.job_scopes ~f:(fun other -> not (phys_equal scope other));
  match scope.unfinished_on_return || not (List.is_empty children) with
  | false -> Ok ()
  | true -> Error (error Conflict "background callback left unfinished invocations")
;;

let running_job_for_completion t job_id generation attempt =
  let open Result.Let_syntax in
  let%bind job = find_job t job_id in
  let%bind () = validate_job_generation t job generation in
  let%bind () = validate_job_attempt job attempt in
  let%bind () =
    match
      List.exists t.job_scopes ~f:(fun scope ->
        Agent_protocol.Id.Job.equal scope.job.id job_id)
    with
    | false -> Ok ()
    | true -> Error (error Conflict "job execution scope still owns its invocations")
  in
  match job.status with
  | Agent_protocol.Job.Running -> Ok job
  | Queued | Waiting_permission _ | Waiting_completion _ ->
    Error (error Conflict "job is not running")
  | Succeeded | Failed _ | Cancelled | Interrupted _ ->
    Error (error Already_resolved "job is already terminal")
;;

let complete_job t job_id generation attempt outcome =
  let open Result.Let_syntax in
  let%bind job = running_job_for_completion t job_id generation attempt in
  let job = complete_job_outcome t job outcome in
  let%map _ = update_job t job in
  job
;;

let finish_background_scopes t job_id generation attempt =
  List.filter t.job_scopes ~f:(fun scope ->
    (not scope.active)
    && Agent_protocol.Id.Job.equal scope.job.id job_id
    && Int.equal scope.job.generation generation
    && Int.equal scope.job.attempt attempt)
  |> List.fold_result ~init:() ~f:(fun () scope ->
    match finish_job_scope t scope with
    | Ok () -> Ok ()
    | Error failure ->
      (match List.mem t.job_scopes scope ~equal:phys_equal with
       | true -> Error failure
       | false -> Ok ()))
;;

let defer_background_job t job_id generation attempt dependency =
  let open Result.Let_syntax in
  let%bind () = finish_background_scopes t job_id generation attempt in
  let%bind job = running_job_for_completion t job_id generation attempt in
  let next = { job with status = Waiting_completion dependency; result = None } in
  let%bind () = validate_job_dependency t next in
  let%map _ = update_job t next in
  next
;;

let complete_background_job
      ?(waiting = false)
      ?completed_at
      t
      job_id
      generation
      attempt
      completion
  =
  let open Result.Let_syntax in
  let%bind job = find_job t job_id in
  let%bind () = validate_job_generation t job generation in
  let%bind () = validate_job_attempt job attempt in
  let%bind () =
    match job.kind with
    | Async_tool -> Ok ()
    | _ -> Error (error Invalid_request "generic completion requires an async tool job")
  in
  let%bind () = Agent_protocol.Completion.validate completion in
  let completion, storage_limited =
    match t.services.job_results with
    | Some publisher
      when Result.is_error
             (Agent_store.Job_result_store.Publisher.check_completion
                publisher
                completion) ->
      ( (match completion with
         | Cancelled _ | Expired -> completion
         | Succeeded _ | Failed _ ->
           Agent_protocol.Completion.Failed
             { code = "background.result_limit"
             ; message = "The completed result exceeds the host storage limit."
             ; retryable = false
             ; details = `Null
             })
      , true )
    | _ -> completion, false
  in
  let%bind () = finish_background_scopes t job_id generation attempt in
  let%bind job =
    match job.status, waiting with
    | Waiting_completion _, true -> Ok job
    | _ -> running_job_for_completion t job_id generation attempt
  in
  let encoded = Agent_protocol.Completion.to_json completion in
  let%bind cancelled, subscriptions =
    match completion with
    | Cancelled _ | Expired -> cancelled_work_dependencies t job
    | Succeeded _ | Failed _ -> Ok ([], [])
  in
  let terminal status =
    let terminal = terminal_job t job status (Some encoded) in
    match completed_at with
    | None -> terminal
    | Some at -> { terminal with completed_at = Some at }
  in
  let running = job in
  let job =
    match completion with
    | Agent_protocol.Completion.Succeeded _ -> terminal Succeeded
    | Cancelled _ -> terminal Cancelled
    | Expired ->
      terminal (Failed (error Resource_limit "background job deadline elapsed"))
    | Failed failure ->
      let retry =
        match job.status with
        | Waiting_completion _ -> None
        | _ -> retry_limits job.retry_policy
      in
      (match failure.retryable, retry with
       | true, Some (maximum, backoff_ms) when job.attempt < maximum ->
         { (retry_job t job failure.message backoff_ms) with result = Some encoded }
       | _ ->
         terminal
           (Failed
              (Agent_protocol.Error.create
                 Internal_error
                 ~message:failure.message
                 ~retryable:failure.retryable
                 ~data:encoded
                 ())))
  in
  let persist stored =
    let status =
      match job.status, stored with
      | Failed failure, Agent_protocol.Stored_completion.Artifact _ ->
        Agent_protocol.Job.Failed { failure with data = `Null }
      | status, _ -> status
    in
    let job =
      { job with status; result = Some (Agent_protocol.Stored_completion.to_json stored) }
    in
    let%map _ = update_jobs ~subscriptions t (job :: cancelled) in
    job
  in
  match job.status, t.services.job_results, storage_limited with
  | Queued, _, _ | _, None, _ | _, _, true ->
    persist (Agent_protocol.Stored_completion.Inline completion)
  | _, Some publisher, false ->
    Agent_store.Job_result_store.Publisher.publish
      publisher
      ~jobs:t.state.jobs
      ~job:running
      ~now:(t.services.now ())
      completion
      ~persist
;;

let recover_background_results t max_count max_total_bytes =
  let open Result.Let_syntax in
  let%bind () =
    match
      t.job_scopes, t.invocation_executions, t.moderator_borrow, t.queued_event_borrow
    with
    | [], [], None, None -> Ok ()
    | _ -> Error (error Conflict "result recovery requires an idle execution host")
  in
  match t.services.job_results with
  | None -> Ok ()
  | Some publisher ->
    let%bind restored =
      Agent_store.Job_result_store.Publisher.restore
        publisher
        ~jobs:t.state.jobs
        ~generation:t.state.identity.generation
        ~max_count
        ~max_total_bytes
    in
    List.fold_result restored ~init:() ~f:(fun () (previous, completion, completed_at) ->
      let%bind job = find_job t previous.id in
      match job.status with
      | Running | Waiting_completion _ ->
        let%bind () = validate_job_dependency t job in
        complete_background_job
          ~waiting:true
          ~completed_at
          t
          job.id
          job.generation
          job.attempt
          completion
        |> Result.map ~f:ignore
      | _ -> Ok ())
;;

let refresh_background_job t job_id generation attempt =
  let open Result.Let_syntax in
  let%bind job = find_job t job_id in
  let%bind () = validate_job_generation t job generation in
  let%bind () = validate_job_attempt job attempt in
  match job.status with
  | Waiting_completion dependency ->
    let%bind () = validate_job_dependency t job in
    let selected =
      Option.bind t.services.job_results ~f:(fun publisher ->
        Agent_store.Job_result_store.Publisher.pending_completion publisher ~job)
    in
    (match selected with
     | Some completion ->
       complete_background_job ~waiting:true t job_id generation attempt completion
     | None ->
       let%bind completion, completed_at =
         match dependency.work with
         | Subscription id ->
           let%map target = retained_subscription t id in
           target.result, target.completed_at
         | Job id ->
           let%bind target = find_job t id in
           let load_artifact =
             Option.map t.services.job_results ~f:(fun publisher ->
               Agent_store.Job_result_store.Publisher.load publisher)
           in
           let%map completion =
             match Agent_protocol.Job.terminal_completion ?load_artifact target with
             | Ok completion -> Ok completion
             | Error failure when not failure.retryable ->
               Ok
                 (Some
                    (Agent_protocol.Completion.Failed
                       { code = "background.artifact_unavailable"
                       ; message =
                           "The saved background result is unavailable or failed \
                            verification."
                       ; retryable = false
                       ; details = `Null
                       }))
             | Error _
               when Agent_protocol.Timestamp.compare
                      (t.services.now ())
                      dependency.deadline
                    >= 0 -> Ok (Some Agent_protocol.Completion.Expired)
             | Error _ as failure -> failure
           in
           completion, target.completed_at
       in
       (match completion with
        | Some completion ->
          let completion =
            match completed_at with
            | Some at when Agent_protocol.Timestamp.compare at dependency.deadline <= 0 ->
              completion
            | _ -> Agent_protocol.Completion.Expired
          in
          let%bind completion = Job_dependency.completion dependency completion in
          complete_background_job ~waiting:true t job_id generation attempt completion
        | None
          when Agent_protocol.Timestamp.compare (t.services.now ()) dependency.deadline
               >= 0 ->
          complete_background_job ~waiting:true t job_id generation attempt Expired
        | None -> Ok job))
  | _ -> Ok job
;;

let check_expected_moderator_checkpoint t = function
  | None -> Ok ()
  | Some expected ->
    (match t.state.moderator with
     | Some installed
       when Jsonaf.exactly_equal
              installed
              (Runtime_builder.encode_moderator_snapshot expected) -> Ok ()
     | _ -> Error (error Conflict "moderator checkpoint changed before external delivery"))
;;

let deliver_job t job_id generation expected expected_job moderator_snapshot =
  let open Result.Let_syntax in
  let%bind () = check_expected_moderator_checkpoint t expected in
  let%bind () =
    match moderator_is_borrowed t with
    | true -> Error (error Conflict "moderator callback owns the checkpoint")
    | false -> Ok ()
  in
  let%bind job = find_job t job_id in
  let%bind () =
    match expected_job with
    | None -> Ok ()
    | Some expected_job ->
      if
        Jsonaf.exactly_equal
          (Agent_protocol.Job.to_json expected_job)
          (Agent_protocol.Job.to_json job)
      then Ok ()
      else Error (error Conflict "job changed before completion delivery")
  in
  let%bind () = validate_job_generation t job generation in
  match job.status, job.delivery with
  | (Succeeded | Failed _ | Cancelled | Interrupted _), Agent_protocol.Job.Pending ->
    let%bind () =
      match job.kind, job.launch with
      | Async_tool, Some _ ->
        (match expected, moderator_snapshot with
         | Some before, Some after ->
           let observer : Agent_protocol.Invocation.observer =
             { script_id = before.script_id; source_sha256 = before.script_source_hash }
           in
           let%bind frame = Background_job_event.frame ~state:t.state ~observer job in
           let%bind event =
             Chat_response.Background_delivery.capture frame
             |> Session.Snapshot.of_value
             |> Result.map_error ~f:Agent_protocol.Error.invalid_request
           in
           let appended =
             { before with
               queued_internal_events = before.queued_internal_events @ [ event ]
             }
           in
           (match
              Jsonaf.exactly_equal
                (Runtime_builder.encode_moderator_snapshot appended)
                after
            with
            | true -> Ok ()
            | false ->
              Error
                (error
                   Conflict
                   "background delivery must append its captured terminal frame only"))
         | _ ->
           Error
             (error Conflict "background delivery requires an exact moderator checkpoint"))
      | _ -> Ok ()
    in
    let job = { job with delivery = Delivered (t.services.now ()) } in
    let%map _ =
      transition
        t
        ~delta:
          (Session_delta.Batch [ Job_changed job; Moderator_changed moderator_snapshot ])
        ~payloads:[ Agent_protocol.Event.Durable.Payload.Job_state_changed job ]
    in
    job
  | (Queued | Running | Waiting_permission _ | Waiting_completion _), _ ->
    Error (error Conflict "job is not terminal")
  | _, (Not_required | Delivered _ | Discarded _) ->
    Error (error Already_resolved "job delivery is resolved")
;;

let cancel_job_internal t job_id =
  let open Result.Let_syntax in
  let%bind job = find_job t job_id in
  let%bind job =
    match job.status with
    | Waiting_completion _ -> refresh_background_job t job_id job.generation job.attempt
    | _ -> Ok job
  in
  match job.status, job.delivery with
  | _, Agent_protocol.Job.Discarded _ -> Ok job
  | (Queued | Running | Waiting_permission _ | Waiting_completion _ | Interrupted _), _ ->
    let%bind cancelled, subscriptions = cancelled_work_dependencies t job in
    let job =
      { job with
        status = Cancelled
      ; result = background_terminal_result job (Cancelled "job cancelled")
      ; completed_at = Some (t.services.now ())
      ; delivery =
          (match job.delivery with
           | Not_required -> Not_required
           | _ -> Pending)
      }
    in
    let%map _ = update_jobs ~subscriptions t (job :: cancelled) in
    job
  | (Succeeded | Failed _ | Cancelled), _ -> Ok job
;;

let interrupt_job t job_id generation attempt reason =
  let open Result.Let_syntax in
  let%bind job = find_job t job_id in
  let%bind () = validate_job_generation t job generation in
  let%bind () = validate_job_attempt job attempt in
  match job.status with
  | Agent_protocol.Job.Running ->
    let job =
      { job with
        status = Interrupted reason
      ; result =
          background_terminal_result
            job
            (Failed
               { code = "background.interrupted"
               ; message = reason
               ; retryable = false
               ; details = `Null
               })
      ; completed_at = Some (t.services.now ())
      ; delivery =
          (match job.delivery with
           | Not_required -> Not_required
           | _ -> Pending)
      }
    in
    let%map _ = update_job t job in
    job
  | Queued
  | Waiting_permission _
  | Waiting_completion _
  | Succeeded
  | Failed _
  | Cancelled
  | Interrupted _ -> Ok job
;;

let change_schedule t attachment_id event schedule =
  let payload =
    match event with
    | `Created -> Agent_protocol.Event.Durable.Payload.Schedule_created schedule
    | `Cancelled -> Schedule_cancelled schedule
  in
  with_writer t attachment_id (fun () ->
    transition t ~delta:(Session_delta.Schedule_changed schedule) ~payloads:[ payload ])
;;

let add_schedule t schedule =
  if
    Agent_protocol.Id.Session.compare
      schedule.Agent_protocol.Schedule.session_id
      t.state.identity.session_id
    <> 0
  then Error (error Invalid_request "schedule belongs to another session")
  else if schedule.generation <> t.state.identity.generation
  then Error (error Conflict "schedule belongs to a stale session generation")
  else if
    List.exists t.state.schedules ~f:(fun existing ->
      Agent_protocol.Id.Schedule.compare existing.id schedule.id = 0)
  then Error (error Conflict "schedule ID is already present")
  else
    let open Result.Let_syntax in
    let%map _ =
      transition
        t
        ~delta:(Session_delta.Schedule_changed schedule)
        ~payloads:[ Agent_protocol.Event.Durable.Payload.Schedule_created schedule ]
    in
    schedule
;;

let find_schedule t schedule_id =
  List.find t.state.schedules ~f:(fun schedule ->
    Agent_protocol.Id.Schedule.compare schedule.id schedule_id = 0)
  |> Result.of_option ~error:(error Invalid_request "schedule was not found")
;;

let validate_schedule_generation t schedule generation =
  if
    schedule.Agent_protocol.Schedule.generation <> generation
    || t.state.identity.generation <> generation
  then Error (error Conflict "schedule belongs to a stale session generation")
  else Ok ()
;;

let update_schedule t schedule =
  transition
    t
    ~delta:(Session_delta.Schedule_changed schedule)
    ~payloads:[ Agent_protocol.Event.Durable.Payload.Schedule_state_changed schedule ]
;;

let schedule_is_due t (schedule : Agent_protocol.Schedule.t) =
  match schedule.ownership with
  | None ->
    Ok (Agent_protocol.Timestamp.compare schedule.next_due_at (t.services.now ()) <= 0)
  | Some _ ->
    Extension_clock.is_due
      t.extension_clock
      (Schedule schedule.id)
      ~now:(t.services.monotonic_now ())
;;

let due_schedules t =
  let open Result.Let_syntax in
  let%map schedules =
    List.filter_map t.state.schedules ~f:(fun schedule ->
      match schedule.Agent_protocol.Schedule.status with
      | Scheduled when Int.equal schedule.generation t.state.identity.generation ->
        Some
          (schedule_is_due t schedule
           |> Result.map ~f:(fun due -> Option.some_if due schedule))
      | _ -> None)
    |> Result.all
    |> Result.map ~f:List.filter_opt
  in
  t.state.lifecycle.observed, schedules
;;

let claim_schedule t schedule_id generation =
  let open Result.Let_syntax in
  let%bind schedule = find_schedule t schedule_id in
  let%bind () = validate_schedule_generation t schedule generation in
  match schedule.status with
  | Agent_protocol.Schedule.Scheduled ->
    let%bind due = schedule_is_due t schedule in
    (match due with
     | false -> Ok None
     | true ->
       let schedule = { schedule with status = Delivering } in
       let%map _ = update_schedule t schedule in
       Some schedule)
  | Delivering | Delivered | Cancelled | Failed _ -> Ok None
;;

let retry_schedule t schedule_id generation =
  let open Result.Let_syntax in
  let%bind schedule = find_schedule t schedule_id in
  let%bind () = validate_schedule_generation t schedule generation in
  match schedule.status with
  | Agent_protocol.Schedule.Delivering ->
    let schedule = { schedule with status = Scheduled } in
    let%map _ = update_schedule t schedule in
    schedule
  | Scheduled -> Ok schedule
  | Delivered | Cancelled | Failed _ ->
    Error (error Already_resolved "schedule is already terminal")
;;

let ingress_enqueue_available t =
  match
    moderator_is_borrowed t || t.idle_moderator_borrowed, t.state.halted, t.state.failure
  with
  | true, _, _ -> Error (error Conflict "moderator callback owns the ingress checkpoint")
  | _, true, _ | _, _, Some _ ->
    Error (error Invalid_state "moderator cannot accept external data")
  | false, false, None -> Ok ()
;;

let ingress_new_admission t candidate =
  let open Result.Let_syntax in
  let%bind subscription =
    lookup_subscription t candidate.External_ingress.context.subscription_id
    |> Result.of_option
         ~error:(error Invalid_state "ingress subscription is not retained")
  in
  let%bind due = subscription_is_due t subscription in
  let%bind () =
    match due with
    | true ->
      Error (error Invalid_state "ingress subscription has reached its elapsed deadline")
    | false -> Ok ()
  in
  Staged_ingress.check_capacity
    ~limits:t.services.ingress_limits
    ~generation:t.state.identity.generation
    ~now:(t.services.now ())
    ~subscriptions:
      (t.state.subscriptions @ Staged_subscriptions.values t.staged_subscriptions)
    ~values:
      ((candidate :: t.state.ingress_registrations)
       @ Staged_ingress.values t.staged_ingress)
;;

let prepare_ingress_submission_internal
      t
      source
      producer
      registration_id
      namespace
      key
      payload
  =
  let open Result.Let_syntax in
  let%bind () = validate_installed_observer t (Some source) in
  let%bind decision =
    Ingress_submission.prepare
      ~state:t.state
      ~source
      ~producer
      ~registration_id
      ~namespace
      ~key
      ~payload
      ~now:(t.services.now ())
      ~create_event_id:Agent_protocol.Id.Ingress_event.create
  in
  match decision with
  | Duplicate _ -> Ok decision
  | Enqueue proposal ->
    let%bind () = ingress_enqueue_available t in
    let%map () = ingress_new_admission t proposal.candidate in
    decision
;;

let commit_ingress_submission_internal t proposal before snapshot =
  let open Result.Let_syntax in
  let%bind () = ingress_enqueue_available t in
  let%bind () = validate_installed_observer t (Some proposal.Ingress_submission.source) in
  let%bind () = check_expected_moderator_checkpoint t (Some before) in
  let%bind next, receipt =
    Ingress_submission.revalidate ~state:t.state ~now:(t.services.now ()) proposal
  in
  let%bind () = ingress_new_admission t next in
  let%bind frame = External_ingress.delivery_frame next receipt in
  let%bind event =
    Chat_response.Ingress_delivery.capture frame
    |> Session.Snapshot.of_value
    |> Result.map_error ~f:Agent_protocol.Error.invalid_request
  in
  let expected =
    { before with queued_internal_events = before.queued_internal_events @ [ event ] }
  in
  let%bind () =
    match
      String.equal before.script_id proposal.source.script_id
      && String.equal before.script_source_hash proposal.source.source_sha256
      && Jsonaf.exactly_equal
           (Runtime_builder.encode_moderator_snapshot expected)
           (Runtime_builder.encode_moderator_snapshot snapshot)
    with
    | true -> Ok ()
    | false ->
      Error (error Conflict "ingress delivery must append its captured data frame only")
  in
  let%map _ =
    transition
      t
      ~payloads:[]
      ~delta:
        (Session_delta.Batch
           [ Ingress_changed next
           ; Moderator_changed (Some (Runtime_builder.encode_moderator_snapshot snapshot))
           ])
  in
  receipt
;;

let complete_schedule
      t
      schedule_id
      generation
      expected
      expected_schedule
      moderator_snapshot
  =
  let open Result.Let_syntax in
  let%bind () = check_expected_moderator_checkpoint t expected in
  let%bind () =
    match moderator_is_borrowed t with
    | true -> Error (error Conflict "moderator callback owns the checkpoint")
    | false -> Ok ()
  in
  let%bind schedule = find_schedule t schedule_id in
  let%bind () =
    match expected_schedule with
    | None -> Ok ()
    | Some expected_schedule ->
      if
        Jsonaf.exactly_equal
          (Agent_protocol.Schedule.to_json expected_schedule)
          (Agent_protocol.Schedule.to_json schedule)
      then Ok ()
      else Error (error Conflict "schedule changed before event delivery")
  in
  let%bind () = validate_schedule_generation t schedule generation in
  match schedule.status with
  | Agent_protocol.Schedule.Delivering ->
    let%bind () =
      match schedule.ownership, expected with
      | None, _ -> Ok ()
      | Some ownership, Some before ->
        let%bind event =
          Chat_response.Schedule_delivery.capture schedule
          |> Result.bind ~f:Session.Snapshot.of_value
          |> Result.map_error ~f:Agent_protocol.Error.invalid_request
        in
        let after =
          { before with
            queued_internal_events = before.queued_internal_events @ [ event ]
          }
        in
        (match
           String.equal before.script_id ownership.source.script_id
           && String.equal before.script_source_hash ownership.source.source_sha256
           && Option.equal
                Jsonaf.exactly_equal
                moderator_snapshot
                (Some (Runtime_builder.encode_moderator_snapshot after))
         with
         | true -> Ok ()
         | false ->
           Error
             (error Conflict "owned timer delivery must append its captured frame only"))
      | Some _, None ->
        Error (error Conflict "owned timer delivery requires its installed checkpoint")
    in
    if schedule.delivery_count = Int.max_value
    then Error (error Invalid_state "schedule delivery count overflow")
    else (
      let schedule =
        { schedule with
          status = Delivered
        ; delivery_count = schedule.delivery_count + 1
        ; last_delivery_at =
            Some
              (match schedule.ownership with
               | None -> t.services.now ()
               | Some _ ->
                 let now = t.services.now () in
                 (match Agent_protocol.Timestamp.compare now schedule.created_at < 0 with
                  | true -> schedule.created_at
                  | false -> now))
        }
      in
      let%map _ =
        transition
          t
          ~delta:
            (Session_delta.Batch
               [ Schedule_changed schedule; Moderator_changed moderator_snapshot ])
          ~payloads:
            [ Agent_protocol.Event.Durable.Payload.Schedule_state_changed schedule ]
      in
      schedule)
  | Scheduled -> Error (error Conflict "schedule delivery was not claimed")
  | Delivered | Cancelled | Failed _ ->
    Error (error Already_resolved "schedule is already terminal")
;;

let fail_schedule t schedule_id generation failure =
  let open Result.Let_syntax in
  let%bind schedule = find_schedule t schedule_id in
  let%bind () = validate_schedule_generation t schedule generation in
  match schedule.status with
  | Agent_protocol.Schedule.Scheduled | Delivering ->
    let schedule = { schedule with status = Failed failure } in
    let%map _ = update_schedule t schedule in
    schedule
  | Delivered | Cancelled | Failed _ ->
    Error (error Already_resolved "schedule is already terminal")
;;

let skip_schedule t schedule_id generation =
  let open Result.Let_syntax in
  let%bind schedule = find_schedule t schedule_id in
  let%bind () = validate_schedule_generation t schedule generation in
  match schedule.status with
  | Agent_protocol.Schedule.Scheduled ->
    let schedule = { schedule with status = Delivered } in
    let%map _ = update_schedule t schedule in
    schedule
  | Delivering -> Error (error Conflict "schedule delivery is already claimed")
  | Delivered | Cancelled | Failed _ ->
    Error (error Already_resolved "schedule is already terminal")
;;

let cancel_schedule_internal t schedule_id =
  let open Result.Let_syntax in
  let%bind schedule = find_schedule t schedule_id in
  match schedule.status with
  | Agent_protocol.Schedule.Scheduled ->
    let schedule = { schedule with status = Cancelled } in
    let%map _ =
      transition
        t
        ~delta:(Session_delta.Schedule_changed schedule)
        ~payloads:[ Agent_protocol.Event.Durable.Payload.Schedule_cancelled schedule ]
    in
    schedule
  | Delivering | Delivered | Cancelled | Failed _ ->
    Error (error Already_resolved "schedule is already terminal")
;;

let claim_idle_moderator t =
  if not (idle_moderator_eligible t)
  then Ok None
  else
    History_codec.all_of_protocol t.state.conversation.canonical_history
    |> Result.map ~f:(fun history ->
      t.idle_moderator_borrowed <- true;
      Some history)
;;

let notification_payload message =
  Agent_protocol.Event.Durable.Payload.Moderator_notification
    (`Object [ "message", `String message ])
;;

let drain_payloads (drain : Runtime_builder.moderator_drain) =
  List.map drain.Runtime_builder.notifications ~f:notification_payload
;;

let start_idle_turn_unchecked
      ?(extra_deltas = [])
      ?(extra_payloads = [])
      ?(notification_wakes = [])
      t
      (drain : Runtime_builder.moderator_drain)
      ~reason
      ~adopt_deferred
  =
  let open Result.Let_syntax in
  let%bind () = reconcile_foreground_invocations t in
  let operation = create_turn_operation t reason in
  let lifecycle = lifecycle_for_operation t operation.id in
  let%bind wake_deltas =
    List.map notification_wakes ~f:(fun value ->
      Agent_protocol.Delivery.accept_wake value ~operation_id:operation.id
      |> Result.map ~f:(fun value -> Session_delta.Delivery_wake_changed value))
    |> Result.all
  in
  let deferred = t.state.conversation.deferred_user_entries in
  let deltas =
    extra_deltas
    @ [ Session_delta.Moderator_changed drain.Runtime_builder.moderator_snapshot
      ; Active_operation_changed (Some operation)
      ; Lifecycle_changed lifecycle
      ]
    @ wake_deltas
    |> fun values ->
    if adopt_deferred then Session_delta.Deferred_entries_adopted :: values else values
  in
  let payloads =
    drain_payloads drain
    @ (if adopt_deferred && not (List.is_empty deferred)
       then [ Agent_protocol.Event.Durable.Payload.History_appended deferred ]
       else [])
    @ extra_payloads
    @ [ Agent_protocol.Event.Durable.Payload.Operation_started operation
      ; Session_state_changed
          { desired_state = lifecycle.desired; observed_state = lifecycle.observed }
      ]
  in
  let%map _ = transition t ~delta:(Session_delta.Batch deltas) ~payloads in
  launch_worker t operation
;;

let suppress_follow_up_deltas t deltas ~reason =
  let open Result.Let_syntax in
  List.map deltas ~f:(function
    | (Session_delta.Invocation_changed next | Invocation_reconciled next) as delta ->
      let%bind original =
        List.find t.state.invocations ~f:(fun value ->
          Agent_protocol.Id.Invocation.equal value.context.id next.context.id)
        |> Result.of_option
             ~error:(error Invalid_state "automatic-turn invocation is missing")
      in
      let%map values = Observation_follow_up.discard [ original ] ~reason in
      (match values with
       | [] -> delta
       | value :: _ -> Observation_follow_up.delta value)
    | (Moderator_execution_changed next | Moderator_execution_reconciled next) as delta ->
      let%bind original =
        List.find t.state.moderator_executions ~f:(fun value ->
          Agent_protocol.Id.Moderator_execution.equal value.context.id next.context.id)
        |> Result.of_option ~error:(error Invalid_state "automatic-turn event is missing")
      in
      let%map values = Observation_follow_up.discard_events [ original ] ~reason in
      (match values with
       | [] -> delta
       | value :: _ -> Observation_follow_up.event_delta value)
    | delta -> Ok delta)
  |> Result.all
;;

let start_idle_turn
      ?(extra_deltas = [])
      ?(extra_payloads = [])
      ?(notification_wakes = [])
      t
      (drain : Runtime_builder.moderator_drain)
      ~reason
      ~adopt_deferred
  =
  let reason =
    match adopt_deferred, t.state.conversation.deferred_user_entries with
    | true, _ :: _ -> Agent_protocol.Operation.User_submit
    | _ -> reason
  in
  let decision =
    match reason, t.state.automatic_turn_budget with
    | (Agent_protocol.Operation.User_submit | Administrative | Recovery_retry), _
    | _, None -> Chat_response.Automatic_turn_policy.Allow_automatic_turn
    | (Moderator_request | Idle_followup), Some budget ->
      (match budget.policy.honor_request_turn with
       | true -> Automatic_turn_budget.decide budget ~now:(t.services.now ())
       | false ->
         Suppress_automatic_turn
           { notice_key = "budget:request-turn-disabled"
           ; notice_text = "Automatic follow-up turns are disabled by runtime policy."
           })
  in
  match decision with
  | Allow_automatic_turn ->
    start_idle_turn_unchecked
      ~extra_deltas
      ~extra_payloads
      ~notification_wakes
      t
      drain
      ~reason
      ~adopt_deferred
  | Suppress_automatic_turn { notice_key; notice_text } ->
    let open Result.Let_syntax in
    let%bind extra_deltas =
      suppress_follow_up_deltas t extra_deltas ~reason:notice_text
    in
    let%bind wake_deltas =
      List.map notification_wakes ~f:(fun value ->
        Agent_protocol.Delivery.discard_wake value ~reason:notice_text
        |> Result.map ~f:(fun value -> Session_delta.Delivery_wake_changed value))
      |> Result.all
    in
    transition
      t
      ~delta:
        (Session_delta.Batch
           (extra_deltas @ wake_deltas @ [ Moderator_changed drain.moderator_snapshot ]))
      ~payloads:
        (extra_payloads
         @ drain_payloads drain
         @ [ Agent_protocol.Event.Durable.Payload.Moderator_notification
               (`Object [ "key", `String notice_key; "message", `String notice_text ])
           ])
    |> Result.map ~f:ignore
;;

let stop_from_idle_moderator
      ?(extra_deltas : Session_delta.t list = [])
      t
      (drain : Runtime_builder.moderator_drain)
      reason
  =
  let open Result.Let_syntax in
  let jobs = stopped_jobs t Cancel in
  let permissions =
    pending_invocation_permissions t ~matches:(fun _ -> true)
    |> List.map ~f:(cancel_permission t "session ended")
  in
  let changed invocation =
    List.exists extra_deltas ~f:(function
      | Session_delta.Invocation_changed updated | Invocation_reconciled updated ->
        Agent_protocol.Id.Invocation.equal
          updated.context.id
          invocation.Agent_protocol.Invocation.context.id
      | _ -> false)
  in
  let%bind discarded =
    Observation_follow_up.discard
      (List.filter t.state.invocations ~f:(fun invocation -> not (changed invocation)))
      ~reason:"moderator ended session"
  in
  let%bind events =
    Observation_follow_up.discard_events
      (List.filter t.state.moderator_executions ~f:(fun event ->
         not
           (List.exists extra_deltas ~f:(function
              | Session_delta.Moderator_execution_changed updated
              | Moderator_execution_reconciled updated ->
                Agent_protocol.Id.Moderator_execution.equal
                  event.context.id
                  updated.context.id
              | _ -> false))))
      ~reason:"moderator ended session"
  in
  let extra_deltas =
    extra_deltas
    @ List.map discarded ~f:Observation_follow_up.delta
    @ List.map events ~f:Observation_follow_up.event_delta
    @ List.map jobs ~f:(fun job -> Session_delta.Job_changed job)
    @ List.map permissions ~f:(fun permission ->
      Session_delta.Permission_changed permission)
  in
  let lifecycle = Session_state.Lifecycle.{ desired = Stopped; observed = Stopped } in
  let payloads =
    drain_payloads drain
    @ List.map jobs ~f:(fun job ->
      Agent_protocol.Event.Durable.Payload.Job_state_changed job)
    @ List.map permissions ~f:(fun permission ->
      Agent_protocol.Event.Durable.Payload.Permission_resolved permission)
    @ [ Agent_protocol.Event.Durable.Payload.Moderator_notification
          (`Object [ "end_session_reason", `String reason ])
      ; Session_state_changed
          { desired_state = lifecycle.desired; observed_state = lifecycle.observed }
      ]
  in
  let%map _ =
    transition
      t
      ~delta:
        (Session_delta.Batch
           (extra_deltas
            @ [ Session_delta.Moderator_changed drain.moderator_snapshot
              ; Halt_changed (Some reason)
              ; Lifecycle_changed lifecycle
              ]))
      ~payloads
  in
  cancel_job_scopes t jobs;
  resolve_cleaned_permission_waiters t permissions
;;

let deliver_idle_notifications_internal t (proposal : Notification_delivery.idle) =
  let open Result.Let_syntax in
  match idle_moderator_eligible t with
  | false -> Ok false
  | true ->
    let%bind () = validate_notification_plan t proposal.pending in
    let%bind halted = Runtime_builder.moderator_snapshot_is_halted t.state.moderator in
    (match halted with
     | true -> Ok false
     | false ->
       let%bind follow_up =
         Observation_follow_up.plan
           ~state:t.state
           ~observer:proposal.pending.source
           ~halted:false
           ~compaction_operation_id:(Agent_protocol.Id.Operation.create ())
       in
       (match follow_up.action with
        | Stop _ | Compact -> Ok false
        | Checkpoint | Turn ->
          let%bind deltas, entries, committed = notification_changes t proposal.pending in
          let deltas =
            deltas
            @ List.map proposal.discarded_wakes ~f:(fun value ->
              Session_delta.Delivery_wake_changed value)
          in
          let notification_wakes =
            proposal.wakes
            @ List.filter committed ~f:(fun value ->
              match value.wake_disposition with
              | Some Pending_wake -> true
              | _ -> false)
          in
          (match deltas, notification_wakes with
           | [], [] -> Ok false
           | _ ->
             let extra_deltas = deltas @ follow_up_deltas t follow_up in
             let extra_payloads = notification_payloads entries in
             let drain : Runtime_builder.moderator_drain =
               { moderator_snapshot = t.state.moderator
               ; runtime_requests = []
               ; notifications = []
               ; remaining_events = false
               }
             in
             let adopt_deferred =
               not (List.is_empty t.state.conversation.deferred_user_entries)
             in
             let needs_turn =
               adopt_deferred
               || (not (List.is_empty notification_wakes))
               ||
               match follow_up.action with
               | Turn -> true
               | _ -> false
             in
             let%map () =
               match needs_turn, t.operation_worker with
               | false, _ ->
                 transition
                   t
                   ~delta:(Session_delta.Batch extra_deltas)
                   ~payloads:extra_payloads
                 |> Result.map ~f:ignore
               | true, None ->
                 Error
                   (error Invalid_state "notification wake requires an installed worker")
               | true, Some _ ->
                 start_idle_turn
                   ~extra_deltas
                   ~extra_payloads
                   ~notification_wakes
                   t
                   drain
                   ~reason:Idle_followup
                   ~adopt_deferred
             in
             true)))
;;

let apply_observation_follow_up t =
  if not (idle_actor_available t)
  then Ok false
  else
    let open Result.Let_syntax in
    let%bind observer = Runtime_builder.moderator_snapshot_observer t.state.moderator in
    let%bind halted = Runtime_builder.moderator_snapshot_is_halted t.state.moderator in
    let compaction = create_compaction_operation t in
    let%bind plan =
      Observation_follow_up.plan
        ~state:t.state
        ~observer
        ~halted
        ~compaction_operation_id:compaction.id
    in
    let extra_deltas = follow_up_deltas t plan in
    let drain : Runtime_builder.moderator_drain =
      { moderator_snapshot = t.state.moderator
      ; runtime_requests = []
      ; notifications = []
      ; remaining_events = false
      }
    in
    let resume_deferred =
      (not halted) && not (List.is_empty t.state.conversation.deferred_user_entries)
    in
    let%map () =
      match plan.action, extra_deltas with
      | Checkpoint, _ when resume_deferred && Option.is_none t.operation_worker ->
        Error (error Invalid_state "deferred user turn requires an installed worker")
      | Checkpoint, _ when resume_deferred ->
        (* Queued/background callbacks can defer a user submission without
           requesting a moderator turn. Resume it once the callback releases
           its borrow; no notification or later event should be required. *)
        start_idle_turn ~extra_deltas t drain ~reason:User_submit ~adopt_deferred:true
      | Checkpoint, [] -> Ok ()
      | Checkpoint, _ ->
        transition t ~delta:(Session_delta.Batch extra_deltas) ~payloads:[]
        |> Result.map ~f:ignore
      | Stop reason, _ -> stop_from_idle_moderator ~extra_deltas t drain reason
      | Compact, _ ->
        start_compaction ~operation:compaction ~extra_deltas t |> Result.map ~f:ignore
      | Turn, _ when Option.is_none t.operation_worker ->
        Error (error Invalid_state "follow-up turn requires an installed worker")
      | Turn, _ ->
        start_idle_turn
          ~extra_deltas
          t
          drain
          ~reason:Moderator_request
          ~adopt_deferred:(not (List.is_empty t.state.conversation.deferred_user_entries))
    in
    resume_deferred || not (List.is_empty extra_deltas)
;;

let checkpoint_idle_moderator t (drain : Runtime_builder.moderator_drain) =
  transition
    t
    ~delta:(Session_delta.Moderator_changed drain.Runtime_builder.moderator_snapshot)
    ~payloads:(drain_payloads drain)
  |> Result.map ~f:(fun _ -> ())
;;

let complete_running_idle_moderator t (drain : Runtime_builder.moderator_drain) =
  let requests = drain.Runtime_builder.runtime_requests in
  match t.state.conversation.deferred_user_entries with
  | _ :: _ -> start_idle_turn t drain ~reason:User_submit ~adopt_deferred:true
  | [] when Chat_response.Runtime_semantics.request_compaction requests ->
    let open Result.Let_syntax in
    let%bind () = checkpoint_idle_moderator t drain in
    Result.map (start_compaction t) ~f:(fun _ -> ())
  | [] when Chat_response.Runtime_semantics.request_turn requests ->
    start_idle_turn t drain ~reason:Moderator_request ~adopt_deferred:false
  | [] -> checkpoint_idle_moderator t drain
;;

let complete_idle_moderator t (drain : Runtime_builder.moderator_drain) =
  if (not t.idle_moderator_borrowed) || moderator_is_borrowed t
  then Error (error Conflict "session moderator is not borrowed for idle work")
  else (
    let result =
      match Chat_response.Runtime_semantics.should_end_session drain.runtime_requests with
      | Some reason -> stop_from_idle_moderator t drain reason
      | None
        when Agent_protocol.Session.equal_desired_state t.state.lifecycle.desired Running
        -> complete_running_idle_moderator t drain
      | None -> checkpoint_idle_moderator t drain
    in
    t.idle_moderator_borrowed <- false;
    result)
;;

let fail_idle_moderator t failure =
  if moderator_is_borrowed t
  then Error (error Conflict "observation callback owns the idle moderator")
  else (
    t.idle_moderator_borrowed <- false;
    let lifecycle =
      Session_state.Lifecycle.
        { desired = t.state.lifecycle.desired
        ; observed = Agent_protocol.Session.Failed failure
        }
    in
    transition
      t
      ~delta:
        (Session_delta.Batch
           [ Failure_changed (Some failure); Lifecycle_changed lifecycle ])
      ~payloads:
        [ Agent_protocol.Event.Durable.Payload.Session_state_changed
            { desired_state = lifecycle.desired; observed_state = lifecycle.observed }
        ]
    |> Result.map ~f:(fun _ -> ()))
;;

let lease_generation t = Int64.(t.state.counters.owner_lease_generation + 1L)

let reclaim_token_sha256 token =
  Digestif.SHA256.digest_string token |> Digestif.SHA256.to_hex
;;

let reclaim_token_matches lease token =
  match lease.Agent_protocol.Session.Owner_lease.reclaim_token_sha256 with
  | None -> false
  | Some expected ->
    Option.value_map
      (Digestif.SHA256.of_hex_opt expected)
      ~default:false
      ~f:(fun expected ->
        Digestif.SHA256.equal expected (Digestif.SHA256.digest_string token))
;;

let owner_principal_matches t lease principal_id =
  let expected =
    Option.first_some
      lease.Agent_protocol.Session.Owner_lease.principal_id
      t.state.identity.creating_principal
  in
  Option.both expected principal_id
  |> Option.exists ~f:(fun (expected, actual) ->
    Agent_protocol.Id.Principal.compare expected actual = 0)
;;

let owner_lease t generation ~principal_id ~reclaim_token =
  match t.state.spec.protocol.liveness with
  | Agent_protocol.Session.Owner_bound _ ->
    let now = t.services.now () |> Agent_protocol.Timestamp.to_time_ns in
    let span = Time_ns.Span.of_ms (Float.of_int t.owner_lease_duration_ms) in
    let expires_at = Time_ns.add now span |> Agent_protocol.Timestamp.of_time_ns in
    Some
      Agent_protocol.Session.Owner_lease.
        { generation
        ; expires_at
        ; disconnect_grace_until = None
        ; principal_id
        ; reclaim_token_sha256 = Option.map reclaim_token ~f:reclaim_token_sha256
        }
  | Detached | Process_bound -> None
;;

let owner_attachment t =
  List.find t.state.attachments ~f:(fun attachment ->
    Agent_protocol.Session.equal_attachment_mode attachment.mode Owner_read_write)
;;

let lease_deadline lease =
  Option.value
    lease.Agent_protocol.Session.Owner_lease.disconnect_grace_until
    ~default:lease.expires_at
;;

let schedule_owner_expiry_at t generation deadline =
  Option.iter t.owner_timer_cancel ~f:(fun resolver -> Eio.Promise.resolve resolver ());
  let cancelled, cancel = Eio.Promise.create () in
  t.owner_timer_cancel <- Some cancel;
  let now = t.services.now () |> Agent_protocol.Timestamp.to_time_ns in
  let deadline = Agent_protocol.Timestamp.to_time_ns deadline in
  let delay = Time_ns.diff deadline now |> Time_ns.Span.to_sec |> Float.max 0. in
  Eio.Fiber.fork ~sw:t.sw (fun () ->
    let expired =
      Eio.Fiber.first
        (fun () ->
           t.sleep delay;
           true)
        (fun () ->
           Eio.Promise.await cancelled;
           false)
    in
    if expired
    then (
      let promise, resolver = Eio.Promise.create () in
      let packed = Pack (None, Owner_expired generation, resolver) in
      match Mailbox.push t.mailbox ~priority:Priority packed with
      | Error _ -> ()
      | Ok () ->
        ignore (Eio.Promise.await promise : (unit, Agent_protocol.Error.t) result)))
;;

let schedule_owner_lease t lease =
  schedule_owner_expiry_at
    t
    lease.Agent_protocol.Session.Owner_lease.generation
    (lease_deadline lease)
;;

let disconnect_grace_deadline t disconnect_grace_ms =
  let now = t.services.now () |> Agent_protocol.Timestamp.to_time_ns in
  Time_ns.add now (Time_ns.Span.of_ms (Float.of_int disconnect_grace_ms))
  |> Agent_protocol.Timestamp.of_time_ns
;;

let mark_owner_disconnected t attachment disconnect_grace_ms =
  let open Result.Let_syntax in
  let%bind lease =
    attachment.Agent_protocol.Session.Attachment.owner_lease
    |> Result.of_option ~error:(error Invalid_state "owner attachment has no lease")
  in
  let lease =
    let grace = disconnect_grace_deadline t disconnect_grace_ms in
    let disconnect_grace_until =
      if Agent_protocol.Timestamp.compare grace lease.expires_at <= 0
      then grace
      else lease.expires_at
    in
    { lease with disconnect_grace_until = Some disconnect_grace_until }
  in
  let attachment = { attachment with owner_lease = Some lease } in
  let%map _ =
    transition
      t
      ~delta:(Session_delta.Attachment_added attachment)
      ~payloads:[ Agent_protocol.Event.Durable.Payload.Attachment_owner_changed None ]
  in
  schedule_owner_lease t lease
;;

let owner_expired t generation =
  t.owner_timer_cancel <- None;
  match owner_attachment t with
  | None -> Ok ()
  | Some attachment ->
    (match attachment.owner_lease with
     | None -> Ok ()
     | Some lease when not (Int64.equal lease.generation generation) -> Ok ()
     | Some lease ->
       let now = t.services.now () in
       let deadline = lease_deadline lease in
       if Agent_protocol.Timestamp.compare deadline now > 0
       then (
         schedule_owner_expiry_at t generation deadline;
         Ok ())
       else (
         match lease.disconnect_grace_until, t.state.spec.protocol.liveness with
         | None, Owner_bound { disconnect_grace_ms; _ } ->
           mark_owner_disconnected t attachment disconnect_grace_ms
         | Some _, Owner_bound { stop_mode; _ } ->
           let open Result.Let_syntax in
           let%bind _ =
             transition
               t
               ~delta:(Session_delta.Attachment_removed attachment.id)
               ~payloads:
                 [ Agent_protocol.Event.Durable.Payload.Attachment_owner_changed None ]
           in
           Result.map (stop_internal t stop_mode) ~f:(fun _ -> ())
         | _, (Detached | Process_bound) -> Ok ()))
;;

let renew_owner t attachment_id expected_generation =
  let open Result.Let_syntax in
  let%bind attachment = write_attachment t attachment_id in
  match attachment.mode, attachment.owner_lease with
  | Owner_read_write, Some lease ->
    if not (Int64.equal lease.generation expected_generation)
    then Error (error Lease_stale "owner lease generation does not match")
    else if Option.is_some lease.disconnect_grace_until
    then Error (error Lease_stale "disconnected owner lease must be reclaimed")
    else if Agent_protocol.Timestamp.compare lease.expires_at (t.services.now ()) <= 0
    then Error (error Lease_stale "owner lease has expired")
    else (
      let generation = lease_generation t in
      let%bind owner_lease =
        owner_lease t generation ~principal_id:lease.principal_id ~reclaim_token:None
        |> Result.of_option ~error:(error Invalid_state "owner lease is unavailable")
      in
      let owner_lease =
        { owner_lease with reclaim_token_sha256 = lease.reclaim_token_sha256 }
      in
      let attachment = { attachment with owner_lease = Some owner_lease } in
      let%map session =
        transition
          t
          ~delta:
            (Session_delta.Batch
               [ Attachment_added attachment; Owner_lease_generation_changed generation ])
          ~payloads:
            [ Agent_protocol.Event.Durable.Payload.Attachment_owner_changed
                (Some attachment)
            ]
      in
      schedule_owner_lease t owner_lease;
      owner_lease, session)
  | Owner_read_write, None -> Error (error Invalid_state "owner attachment has no lease")
  | (Read_write | Read_only), _ ->
    Error (error Permission_denied "attachment does not own the session")
;;

let owner_reclaim_authorized t lease ~principal_id ~reclaim_token =
  owner_principal_matches t lease principal_id
  || Option.exists reclaim_token ~f:(reclaim_token_matches lease)
;;

let install_subscriber t attachment_id subscribe =
  let subscriber =
    Option.some_if subscribe (Subscriber.create ~capacity:t.subscriber_capacity)
  in
  Option.iter subscriber ~f:(fun value ->
    Eio.Mutex.use_rw ~protect:true t.subscriber_mutex (fun () ->
      t.subscribers := Map.set !(t.subscribers) ~key:attachment_id ~data:value));
  subscriber
;;

let validate_attachment_capacity t ~replacing =
  if replacing || List.length t.state.attachments < t.max_attachments
  then Ok ()
  else Error (error Resource_limit "session attachment limit reached")
;;

let attach_owner t subscribe ~principal_id ~reclaim_token =
  let open Result.Let_syntax in
  let%bind () =
    match owner_attachment t with
    | Some { owner_lease = Some lease; _ }
      when Agent_protocol.Timestamp.compare (lease_deadline lease) (t.services.now ())
           <= 0 -> owner_expired t lease.generation
    | _ -> Ok ()
  in
  let previous = owner_attachment t in
  let%bind () = validate_attachment_capacity t ~replacing:(Option.is_some previous) in
  let%bind () =
    match previous with
    | None -> Ok ()
    | Some { owner_lease = None; _ } ->
      Error (error Conflict "session already has an owner attachment")
    | Some { owner_lease = Some lease; _ } ->
      (match lease.disconnect_grace_until with
       | None -> Error (error Conflict "session already has an active owner attachment")
       | Some _ ->
         if owner_reclaim_authorized t lease ~principal_id ~reclaim_token
         then Ok ()
         else Error (error Permission_denied "owner reclaim credential is invalid"))
  in
  let issued_reclaim_token = t.services.create_reclaim_token () in
  let%bind () =
    if String.is_empty issued_reclaim_token
    then
      Error (error Invalid_state "owner reclaim token generator returned an empty token")
    else Ok ()
  in
  let generation = lease_generation t in
  let%bind lease =
    owner_lease t generation ~principal_id ~reclaim_token:(Some issued_reclaim_token)
    |> Result.of_option ~error:(error Invalid_state "owner lease is unavailable")
  in
  let attachment_id = t.services.create_attachment_id () in
  let attachment =
    Agent_protocol.Session.Attachment.
      { id = attachment_id
      ; session_id = t.state.identity.session_id
      ; mode = Owner_read_write
      ; owner_lease = Some lease
      }
  in
  let deltas =
    Option.to_list
      (Option.map previous ~f:(fun previous ->
         Session_delta.Attachment_removed previous.id))
    @ [ Session_delta.Attachment_added attachment
      ; Session_delta.Owner_lease_generation_changed generation
      ]
  in
  let%bind _ =
    transition
      t
      ~delta:(Session_delta.Batch deltas)
      ~payloads:
        [ Agent_protocol.Event.Durable.Payload.Attachment_owner_changed (Some attachment)
        ]
  in
  schedule_owner_lease t lease;
  let subscriber = install_subscriber t attachment_id subscribe in
  Ok (attachment, subscriber, current_snapshot t, Some issued_reclaim_token)
;;

let attach_nonowner t mode subscribe =
  let open Result.Let_syntax in
  let%bind () = validate_attachment_capacity t ~replacing:false in
  let attachment_id = t.services.create_attachment_id () in
  let attachment =
    Agent_protocol.Session.Attachment.
      { id = attachment_id
      ; session_id = t.state.identity.session_id
      ; mode
      ; owner_lease = None
      }
  in
  let%bind _ =
    transition t ~delta:(Session_delta.Attachment_added attachment) ~payloads:[]
  in
  let subscriber = install_subscriber t attachment_id subscribe in
  Ok (attachment, subscriber, current_snapshot t, None)
;;

let attach t mode subscribe principal_id reclaim_token =
  match mode, t.state.spec.protocol.liveness with
  | Agent_protocol.Session.Owner_read_write, Agent_protocol.Session.Owner_bound _ ->
    attach_owner t subscribe ~principal_id ~reclaim_token
  | Owner_read_write, (Detached | Process_bound) -> attach_nonowner t mode subscribe
  | (Read_write | Read_only), _ -> attach_nonowner t mode subscribe
;;

let detach t attachment_id =
  match
    List.find t.state.attachments ~f:(fun value ->
      Agent_protocol.Id.Attachment.compare value.id attachment_id = 0)
  with
  | None -> Error (error Invalid_request "attachment is not active")
  | Some attachment ->
    Eio.Mutex.use_rw ~protect:true t.subscriber_mutex (fun () ->
      Option.iter (Map.find !(t.subscribers) attachment_id) ~f:Subscriber.close;
      t.subscribers := Map.remove !(t.subscribers) attachment_id);
    (match attachment.mode, t.state.spec.protocol.liveness with
     | Owner_read_write, Owner_bound { disconnect_grace_ms; _ } ->
       mark_owner_disconnected t attachment disconnect_grace_ms
     | _, _ ->
       Result.map
         (transition
            t
            ~delta:(Session_delta.Attachment_removed attachment_id)
            ~payloads:[])
         ~f:(fun _ -> ()))
;;

let handle : type a. t -> a request -> (a, Agent_protocol.Error.t) result =
  fun t -> function
  | Manage_moderator_follow_up (operation_id, observer) ->
    manage_moderator_follow_up t operation_id observer
  | Admit_moderator_turn operation_id -> admit_moderator_turn t operation_id
  | Admit_notification_turn operation_id -> admit_notification_turn t operation_id
  | Claim_delegated_event (id, delegation, snapshot, event) ->
    claim_delegated_event t id delegation snapshot event
  | Claim_job_event (scope, id, snapshot, event) ->
    claim_job_event t scope id snapshot event
  | Claim_job_moderator (scope, invocation) -> claim_job_moderator t scope invocation
  | Claim_ordinary_event (id, operation_id, snapshot, event) ->
    claim_ordinary_event t id operation_id snapshot event
  | Claim_queued_event (id, operation_id, snapshot) ->
    claim_queued_event t id operation_id snapshot
  | Claim_queued_retirement (id, snapshot, reason) ->
    claim_queued_retirement t id snapshot reason
  | Commit_queued_event (borrow, snapshot, requests, decision, notifications) ->
    let open Result.Let_syntax in
    let%bind () = validate_queued_event_borrow t borrow in
    with_staged_transaction t (Moderator_event borrow.receipt.context.id) (fun () ->
      commit_queued_event t borrow snapshot requests decision notifications)
  | Finish_queued_event (borrow, interrupted) -> finish_queued_event t borrow interrupted
  | Set_queued_event_cancel (borrow, cancel) ->
    Result.map (queued_event_can_commit t borrow) ~f:(fun () ->
      borrow.cancel <- Some cancel)
  | Commit_invocation_call (operation_id, invocation, entry) ->
    commit_invocation_call t operation_id invocation entry
  | Claim_invocation (operation_id, invocation) ->
    claim_invocation t operation_id invocation
  | Claim_idle_invocation (borrow, invocation) ->
    claim_idle_invocation t borrow invocation
  | Claim_event_invocation (borrow, invocation) ->
    claim_event_invocation t borrow invocation
  | Claim_job_scope (id, generation, attempt, deadline) ->
    claim_job_scope t id generation attempt deadline
  | Claim_job_invocation (scope, invocation) ->
    (match t.moderator_borrow with
     | Some borrow
       when Option.exists borrow.job_scope ~f:(phys_equal scope)
            && Option.exists invocation.context.parent_invocation ~f:(fun parent ->
              Agent_protocol.Id.Invocation.equal parent borrow.invocation.context.id
              || List.exists t.invocation_executions ~f:(fun execution ->
                invocation_execution_owned_by borrow execution
                && Agent_protocol.Id.Invocation.equal
                     parent
                     execution.dispatched.context.id)) ->
       claim_idle_invocation t borrow invocation
     | _ -> claim_job_invocation t scope invocation)
  | Finish_job_scope scope -> finish_job_scope t scope
  | Seal_job_scope scope ->
    (match List.mem t.job_scopes scope ~equal:phys_equal with
     | false -> Error (error Conflict "background execution scope has ended")
     | true ->
       scope.active <- false;
       scope.unfinished_on_return
       <- job_has_moderator t scope
          || List.exists t.invocation_executions ~f:(job_execution_owned_by scope);
       signal_job_scope scope;
       cancel_job_moderators t scope;
       Ok ())
  | Set_job_scope_cancel (scope, cancel) ->
    let open Result.Let_syntax in
    let%map () = job_scope_can_execute t scope in
    scope.cancel <- Some cancel
  | Finish_invocation (execution, outcome, requests, commit_starts) ->
    let open Result.Let_syntax in
    let%bind () =
      match List.mem t.invocation_executions execution ~equal:phys_equal with
      | true -> Ok ()
      | false -> Error (error Conflict "invocation callback no longer owns its result")
    in
    with_staged_transaction t (Invocation execution.dispatched.context.id) (fun () ->
      finish_invocation t execution outcome requests commit_starts)
  | Claim_moderator_invocation (operation_id, invocation) ->
    claim_moderator_invocation t operation_id invocation
  | Claim_moderator_observation (operation_id, invocation_id) ->
    claim_moderator_observation t (Some operation_id) invocation_id
  | Claim_next_moderator_observation (operation_id, observer) ->
    claim_next_moderator_observation t (Some operation_id) observer
  | Claim_idle_moderator_observation (observer, tools) ->
    if idle_moderator_eligible t
    then
      Result.map (claim_next_moderator_observation t None observer) ~f:(fun borrow ->
        Option.iter borrow ~f:(fun borrow -> borrow.accepts_children <- tools);
        borrow)
    else Ok None
  | Commit_moderator_invocation (borrow, resolved, snapshot) ->
    let open Result.Let_syntax in
    let%bind () = validate_moderator_borrow t borrow in
    with_staged_transaction t (Invocation borrow.invocation.context.id) (fun () ->
      commit_moderator_invocation t borrow resolved snapshot)
  | Finish_moderator_invocation (borrow, failure) ->
    finish_moderator_invocation t borrow failure
  | Set_idle_moderator_cancel (borrow, cancel) ->
    let open Result.Let_syntax in
    let%bind () = validate_moderator_borrow t borrow in
    (match
       ( borrow.job_scope
       , borrow.operation_id
       , t.state.lifecycle.desired
       , t.state.lifecycle.observed )
     with
     | Some scope, None, _, _ ->
       let%map () = job_scope_can_execute t scope in
       borrow.cancel <- Some cancel
     | None, None, Running, Idle ->
       borrow.cancel <- Some cancel;
       Ok ()
     | _ ->
       Error (error Conflict "idle observation was stopped before callback execution"))
  | Commit_extensions (generation, revision, changes) ->
    commit_extensions_internal t generation revision changes
  | State -> Ok t.state
  | Prepare_ingress_submission (source, producer, registration_id, namespace, key, payload)
    ->
    prepare_ingress_submission_internal
      t
      source
      producer
      registration_id
      namespace
      key
      payload
  | Commit_ingress_submission (proposal, before, snapshot) ->
    commit_ingress_submission_internal t proposal before snapshot
  | Due_schedules -> due_schedules t
  | Read_job id -> Result.map (find_job t id) ~f:(job_with_progress t)
  | Publish_job_progress (id, progress) ->
    publish_job_progress_internal t id progress;
    Ok ()
  | Prepare_background_job (owner, request) -> prepare_background_job t owner request
  | Stage_background_job (job, capacity) -> stage_background_job_internal t job capacity
  | Stage_subscription_mutation (owner, source, previous, next) ->
    stage_subscription_mutation_internal t owner source previous next
  | Create_script_subscription (owner, source, kind, lifetime_ms, wake, completion_schema)
    ->
    create_script_subscription_internal
      t
      owner
      source
      kind
      lifetime_ms
      wake
      completion_schema
  | Select_subscription_mutations (owner, source, receipts) ->
    let open Result.Let_syntax in
    let%bind _ = subscription_owner_active t owner source in
    Staged_subscriptions.select
      t.staged_subscriptions
      ~owner
      ~receipts
      ~lookup:(lookup_subscription t)
  | Abort_subscription_mutation (owner, receipt) ->
    let open Result.Let_syntax in
    let%map () = Staged_subscriptions.abort t.staged_subscriptions ~owner ~receipt in
    sync_extension_clock t
  | Finish_script_subscription (owner, source, id, expected_epoch, completion) ->
    finish_script_subscription_internal t owner source id expected_epoch completion
  | Read_script_subscription (owner, source, id) ->
    let open Result.Let_syntax in
    let%bind _ = subscription_owner_active t owner source in
    let%bind subscription =
      Result.of_option
        (provisional_subscription t owner id)
        ~error:(error Invalid_state "subscription is not retained")
    in
    let%map () = subscription_owned t source subscription in
    subscription
  | Expire_subscriptions -> expire_subscriptions_internal t
  | Create_script_ingress (owner, source, id, epoch, namespace, schema) ->
    create_script_ingress_internal t owner source id epoch namespace schema
  | Revoke_script_ingress (owner, source, id, reason) ->
    revoke_script_ingress_internal t owner source id reason
  | Read_script_ingress (owner, source, id) ->
    read_script_ingress_internal t owner source id
  | Abort_ingress_mutation (owner, receipt) ->
    Staged_ingress.abort t.staged_ingress ~owner ~receipt
  | Select_ingress_mutations (owner, source, receipts) ->
    let open Result.Let_syntax in
    let%bind _ = subscription_owner_active t owner source in
    Staged_ingress.select
      t.staged_ingress
      ~owner
      ~receipts
      ~lookup:(lookup_ingress t)
      ~subscription:
        (ingress_subscription_lookup
           t
           (Staged_subscriptions.values t.staged_subscriptions))
  | Create_script_notification
      (owner, source, correlation, completion, wake, disclosure_pins) ->
    create_script_notification_internal
      t
      owner
      source
      correlation
      completion
      wake
      disclosure_pins
  | Read_script_notification (owner, source, id) ->
    let open Result.Let_syntax in
    let%bind _ = subscription_owner_active t owner source in
    let%bind value =
      Result.of_option
        (provisional_notification t owner id)
        ~error:(error Invalid_state "notification is not retained")
    in
    let%map () = notification_owned t source value in
    value
  | Select_notification_mutations (owner, source, receipts) ->
    let open Result.Let_syntax in
    let%bind _ = subscription_owner_active t owner source in
    Staged_notifications.select
      t.staged_notifications
      ~owner
      ~receipts
      ~lookup:(lookup_notification t)
  | Abort_notification_mutation (owner, receipt) ->
    Staged_notifications.abort t.staged_notifications ~owner ~receipt
  | Stage_schedule_mutation (owner, source, previous, next) ->
    stage_schedule_mutation_internal t owner source previous next
  | Create_script_schedule (owner, source, delay_ms, payload, misfire) ->
    create_script_schedule_internal t owner source delay_ms payload misfire
  | Select_schedule_mutations (owner, source, receipts) ->
    let open Result.Let_syntax in
    let%bind _ = subscription_owner_active t owner source in
    Staged_schedules.select t.staged_schedules ~owner ~receipts ~lookup:(fun id ->
      List.find t.state.schedules ~f:(fun value ->
        Agent_protocol.Id.Schedule.equal value.id id))
  | Abort_schedule_mutation (owner, receipt) ->
    let open Result.Let_syntax in
    let%map () = Staged_schedules.abort t.staged_schedules ~owner ~receipt in
    sync_extension_clock t
  | Read_script_schedule (owner, source, id) ->
    let open Result.Let_syntax in
    let%bind _ = subscription_owner_active t owner source in
    let%bind value =
      Result.of_option
        (provisional_schedule t owner id)
        ~error:(error Invalid_state "schedule is not retained")
    in
    let%map () = schedule_owned t source value in
    value
  | Select_background_jobs (owner, ids) ->
    let open Result.Let_syntax in
    let%bind () = background_owner_active t owner in
    Staged_jobs.select t.staged_jobs ~owner ~ids
  | Abort_background_job (owner, id) -> Staged_jobs.abort t.staged_jobs ~owner ~id
  | Has_staged_background_job (owner, id) ->
    let open Result.Let_syntax in
    let%map () = background_owner_active t owner in
    Staged_jobs.contains t.staged_jobs ~owner ~id
  | Read_script_job (owner, id) ->
    let open Result.Let_syntax in
    let%bind () = background_owner_active t owner in
    let%bind staged = Staged_jobs.find t.staged_jobs ~owner ~id in
    (match staged with
     | Some job -> Ok job
     | None ->
       let%bind job = find_job t id in
       let%map () = validate_job_generation t job t.state.identity.generation in
       job_with_progress t job)
  | Read_script_job_result (owner, expected) ->
    let open Result.Let_syntax in
    let%bind () = background_owner_active t owner in
    let%bind job = find_job t expected.id in
    let%bind () = validate_job_generation t job t.state.identity.generation in
    let%bind () =
      match
        Jsonaf.exactly_equal
          (Agent_protocol.Job.to_json job)
          (Agent_protocol.Job.to_json expected)
      with
      | true -> Ok ()
      | false -> Error (error Conflict "job changed before artifact materialization")
    in
    let load_artifact =
      Option.map t.services.job_results ~f:(fun publisher ->
        Agent_store.Job_result_store.Publisher.load publisher)
    in
    let%bind completion = Agent_protocol.Job.terminal_completion ?load_artifact job in
    Result.of_option
      completion
      ~error:(error Invalid_state "job has no terminal completion")
  | Cancel_script_job (owner, id) ->
    let open Result.Let_syntax in
    let%bind () = background_owner_active t owner in
    let%bind staged =
      Staged_jobs.cancel t.staged_jobs ~owner ~id ~now:(t.services.now ())
    in
    (match staged with
     | Some _ -> Ok ()
     | None ->
       let%bind job = find_job t id in
       let%bind () = validate_job_generation t job t.state.identity.generation in
       Result.map (cancel_job_internal t id) ~f:ignore)
  | Snapshot -> Ok (current_snapshot t)
  | Set_operation_worker worker -> set_operation_worker t worker
  | Enable_automatic_turn_budget policy ->
    (match
       t.state.automatic_turn_budget, t.state.active_operation, moderator_is_borrowed t
     with
     | Some budget, _, _
       when Chat_response.Runtime_semantics.equal_policy budget.policy policy -> Ok ()
     | _, None, false ->
       transition
         t
         ~delta:(Session_delta.Automatic_turn_budget_enabled policy)
         ~payloads:[]
       |> Result.map ~f:ignore
     | _ ->
       Error (error Conflict "cannot enable automatic-turn accounting during active work"))
  | Set_automatic_turn_pauses conditions ->
    (match
       t.state.active_operation, moderator_is_borrowed t, t.idle_moderator_borrowed
     with
     | None, false, false ->
       let open Result.Let_syntax in
       let%bind budget =
         t.state.automatic_turn_budget
         |> Result.of_option
              ~error:(error Invalid_state "automatic-turn policy is not enabled")
       in
       let next = Automatic_turn_budget.with_pauses budget conditions in
       (match Automatic_turn_budget.equal budget next with
        | true -> Ok ()
        | false ->
          transition
            t
            ~delta:
              (Session_delta.Automatic_turn_pauses_changed
                 next.policy.budget.pause_conditions)
            ~payloads:[]
          |> Result.map ~f:ignore)
     | _ ->
       Error (error Conflict "pause changes wait for active moderator or foreground work"))
  | Change_moderator moderator -> change_moderator t moderator
  | Change_workspace workspace -> change_workspace t workspace
  | Shell_approval_grants -> Ok t.state.shell.approval_grants
  | Replace_shell_approval_grants approval_grants ->
    replace_shell_approval_grants t approval_grants
  | Shell_manifest_grants -> Ok t.state.shell.manifest_grants
  | Add_shell_manifest_grant grant -> add_shell_manifest_grant t grant
  | Reset (attachment_id, expected_revision, options) ->
    reset_internal t attachment_id expected_revision options
  | Upgrade_prompt (attachment_id, expected_revision, target_revision) ->
    upgrade_prompt_internal t attachment_id expected_revision target_revision
  | Commit_administration (attachment_id, expected_revision, kind, state) ->
    commit_administration t attachment_id expected_revision kind state
  | Start (attachment_id, expected_parent_stop_epoch) ->
    with_writer t attachment_id (fun () -> start_internal ?expected_parent_stop_epoch t)
  | Start_initial_delegated (reference, expected_parent_stop_epoch) ->
    (match t.state.spec.delegation with
     | Some current
       when Agent_store.Delegation_store.Reference.equal current reference
            && Agent_protocol.Id.Session.equal
                 reference.child_session_id
                 t.state.identity.session_id ->
       (match t.state.pending_initial_start with
        | true -> start_internal ?expected_parent_stop_epoch t
        | false -> Ok (Session_state.summary t.state))
     | _ ->
       Error
         (error Permission_denied "delegation.start: child relationship does not match"))
  | Fail_initial_delegated (reference, failure) ->
    (match t.state.spec.delegation with
     | Some current
       when Agent_store.Delegation_store.Reference.equal current reference
            && Agent_protocol.Id.Session.equal
                 reference.child_session_id
                 t.state.identity.session_id ->
       (match t.state.pending_initial_start with
        | false -> Ok (Session_state.summary t.state)
        | true ->
          let open Result.Let_syntax in
          let%map _ =
            transition
              t
              ~delta:
                (Batch
                   [ Initial_start_consumed
                   ; Lifecycle_changed { desired = Stopped; observed = Failed failure }
                   ; Failure_changed (Some failure)
                   ])
              ~payloads:
                [ Session_state_changed
                    { desired_state = Stopped; observed_state = Failed failure }
                ]
          in
          Session_state.summary t.state)
     | _ ->
       Error
         (error Permission_denied "delegation.start: child relationship does not match"))
  | Queue_start attachment_id ->
    with_writer t attachment_id (fun () -> queue_start_internal t)
  | Activate_queued_start -> activate_queued_start t
  | Stop (attachment_id, mode) ->
    with_writer t attachment_id (fun () -> stop_internal t mode)
  | Stop_delegated (reference, mode) ->
    (match t.state.spec.delegation with
     | Some current
       when Agent_store.Delegation_store.Reference.equal current reference
            && Agent_protocol.Id.Session.equal
                 reference.child_session_id
                 t.state.identity.session_id -> stop_internal t mode
     | _ ->
       Error
         (error Permission_denied "delegation.stop: child relationship does not match"))
  | Stop_managed (reference, key, mode, generation, max_receipts) ->
    stop_managed_internal t reference key mode generation max_receipts
  | Append_history (attachment_id, entries) ->
    with_writer t attachment_id (fun () -> append_history t entries)
  | Stop_delegated_at_epoch (reference, epoch, force) ->
    (match t.state.spec.delegation with
     | Some current
       when Agent_store.Delegation_store.Reference.equal current reference
            && Agent_protocol.Id.Session.equal
                 reference.child_session_id
                 t.state.identity.session_id ->
       (match t.state.parent_stop_epoch with
        | Some previous when (not force) && Int64.(epoch <= previous) ->
          Ok (Session_state.summary t.state)
        | _ -> stop_internal ~parent_stop_epoch:epoch t Cancel)
     | _ ->
       Error
         (error Permission_denied "delegation.stop: child relationship does not match"))
  | Defer_history (attachment_id, entries) ->
    with_writer t attachment_id (fun () -> defer_history t entries)
  | Submit_message (attachment_id, entry) -> submit_message t attachment_id entry
  | Submit_managed_message (reference, key, fingerprint, generation, maximum, entry) ->
    submit_managed_message t reference key fingerprint generation maximum entry
  | Compact (attachment_id, expected_revision) ->
    compact_internal t attachment_id expected_revision
  | Delete_history (attachment_id, revision, history_id) ->
    delete_history_internal t attachment_id revision history_id
  | Adopt_deferred -> adopt_deferred t
  | Reserve_history_block count -> reserve_history_block t count
  | Commit_worker_entry (operation_id, entry) -> commit_worker_entry t operation_id entry
  | Prepare_authoring_input (operation_id, materialization, history, effective) ->
    prepare_authoring_input t operation_id materialization history effective
  | Publish_invocation_output (operation_id, invocation_id, entry) ->
    publish_invocation_output t operation_id invocation_id entry
  | Consume_deferred operation_id -> consume_deferred t operation_id
  | Consume_notifications (operation_id, plan) ->
    consume_notifications_internal t operation_id plan
  | Deliver_idle_notifications plan -> deliver_idle_notifications_internal t plan
  | Admit_standalone_delivery plan -> admit_standalone_delivery_internal t plan
  | Retire_obsolete_moderator_delivery (revision, job) ->
    retire_obsolete_moderator_delivery_internal t revision job
  | Deliver_standalone_completion (revision, job, capabilities, policy) ->
    deliver_standalone_completion_internal t revision job capabilities policy
  | Consume_initial_notifications (operation_id, proposal) ->
    consume_notifications_internal
      ~wakes:proposal.wakes
      ~discarded_wakes:proposal.discarded_wakes
      t
      operation_id
      proposal.pending
  | Commit_worker_moderator (operation_id, snapshot) ->
    commit_worker_moderator t operation_id snapshot
  | Has_writer_attachment -> Ok (has_writer_attachment t)
  | Authorize_writer attachment_id -> with_writer t attachment_id (fun () -> Ok ())
  | Invocation_granted (tool_name, identity_digest) ->
    Ok (invocation_granted t ~tool_name ~identity_digest)
  | Worker_ready (operation_id, cancel) -> worker_ready t operation_id cancel
  | Worker_terminal (operation_id, outcome) -> worker_terminal t operation_id outcome
  | Compaction_terminal (operation_id, outcome) ->
    compaction_terminal t operation_id outcome
  | Cancel_operation (attachment_id, operation_id) ->
    cancel_operation_internal t attachment_id operation_id
  | Open_permission (permission, timeout_seconds, fallback) ->
    open_permission t permission timeout_seconds fallback
  | Respond_permission
      (attachment_id, principal_id, permission_id, generation, choice, reason) ->
    respond_permission_internal
      t
      attachment_id
      principal_id
      permission_id
      generation
      choice
      reason
  | Resolve_permission_system (permission_id, generation, choice, reason) ->
    resolve_permission_system t permission_id generation choice reason
  | Expire_permission (permission_id, generation, fallback) ->
    expire_permission_internal t permission_id generation fallback
  | Revoke_grant (attachment_id, grant_id, reason) ->
    revoke_grant t attachment_id grant_id reason
  | Change_job (attachment_id, job) -> change_job t attachment_id job
  | Add_job job -> add_job t job
  | Claim_job (job_id, generation) -> claim_job t job_id generation
  | Complete_job (job_id, generation, attempt, outcome) ->
    complete_job t job_id generation attempt outcome
  | Complete_background_job (job_id, generation, attempt, outcome) ->
    complete_background_job t job_id generation attempt outcome
  | Defer_background_job (job_id, generation, attempt, dependency) ->
    defer_background_job t job_id generation attempt dependency
  | Refresh_background_job (job_id, generation, attempt) ->
    refresh_background_job t job_id generation attempt
  | Recover_background_results (max_count, max_total_bytes) ->
    recover_background_results t max_count max_total_bytes
  | Deliver_job (job_id, generation, expected, expected_job, moderator_snapshot) ->
    deliver_job t job_id generation expected expected_job moderator_snapshot
  | Cancel_job_internal job_id -> cancel_job_internal t job_id
  | Cancel_job (attachment_id, job_id) ->
    with_writer t attachment_id (fun () -> cancel_job_internal t job_id)
  | Interrupt_job (job_id, generation, attempt, reason) ->
    interrupt_job t job_id generation attempt reason
  | Change_schedule (attachment_id, event, schedule) ->
    change_schedule t attachment_id event schedule
  | Add_schedule schedule -> add_schedule t schedule
  | Cancel_schedule_internal schedule_id -> cancel_schedule_internal t schedule_id
  | Claim_schedule (schedule_id, generation) -> claim_schedule t schedule_id generation
  | Retry_schedule (schedule_id, generation) -> retry_schedule t schedule_id generation
  | Complete_schedule
      (schedule_id, generation, expected, expected_schedule, moderator_snapshot) ->
    complete_schedule
      t
      schedule_id
      generation
      expected
      expected_schedule
      moderator_snapshot
  | Fail_schedule (schedule_id, generation, failure) ->
    fail_schedule t schedule_id generation failure
  | Skip_schedule (schedule_id, generation) -> skip_schedule t schedule_id generation
  | Claim_idle_moderator -> claim_idle_moderator t
  | Apply_observation_follow_up -> apply_observation_follow_up t
  | Complete_idle_moderator drain -> complete_idle_moderator t drain
  | Fail_idle_moderator failure -> fail_idle_moderator t failure
  | Attach (mode, subscribe, principal_id, reclaim_token) ->
    attach t mode subscribe principal_id reclaim_token
  | Detach attachment_id -> detach t attachment_id
  | Renew_owner (attachment_id, generation) -> renew_owner t attachment_id generation
  | Owner_expired generation -> owner_expired t generation
  | Checkpoint persist -> persist t.state
  | Quiescent_checkpoint inspect ->
    (match
       ( t.state.active_operation
       , t.active_cancel
       , t.invocation_executions
       , t.job_scopes
       , t.moderator_borrow
       , t.queued_event_borrow
       , t.foreground_moderator
       , t.idle_moderator_borrowed
       , Staged_jobs.is_empty t.staged_jobs
         && Staged_subscriptions.is_empty t.staged_subscriptions
         && Staged_ingress.is_empty t.staged_ingress
         && Staged_schedules.is_empty t.staged_schedules
         && Staged_notifications.is_empty t.staged_notifications
       , Active_calls.snapshot t.active_calls )
     with
     | None, None, [], [], None, None, None, false, true, ([], []) ->
       Result.map (inspect t.state) ~f:Option.some
     | _ -> Ok None)
  | Shutdown ->
    abort_all_staged_work t;
    Option.iter t.owner_timer_cancel ~f:(fun resolver -> Eio.Promise.resolve resolver ());
    t.owner_timer_cancel <- None;
    Option.iter t.active_cancel ~f:(fun cancel -> cancel ());
    List.iter t.job_scopes ~f:cancel_job_scope;
    Option.iter t.queued_event_borrow ~f:(fun borrow ->
      borrow.callback_active <- false;
      borrow.cancel_requested <- true;
      Option.iter borrow.cancel ~f:(fun cancel -> cancel ()));
    Eio.Mutex.use_rw ~protect:true t.subscriber_mutex (fun () ->
      Map.iter !(t.subscribers) ~f:Subscriber.close;
      t.subscribers := Map.Poly.empty);
    Map.iter !(t.permission_waiters) ~f:(fun waiter ->
      Eio.Promise.resolve
        waiter.resolver
        Agent_protocol.Permission.
          { choice = Deny
          ; principal_id = None
          ; resolved_at = t.services.now ()
          ; reason = Some "session actor shut down"
          });
    t.permission_waiters := Map.Poly.empty;
    t.stopped <- true;
    Ok ()
;;

let rec reject_pending mailbox =
  match Mailbox.pop mailbox with
  | None -> ()
  | Some (Pack (_, _, resolver)) ->
    Eio.Promise.resolve
      resolver
      (Error
         (Agent_protocol.Error.create
            Server_shutting_down
            ~message:"session actor is shut down"
            ~retryable:true
            ()));
    reject_pending mailbox
;;

let rec run t =
  match Mailbox.pop t.mailbox with
  | None -> ()
  | Some (Pack (command_audit, request, resolver)) ->
    t.command_audit <- command_audit;
    Eio.Promise.resolve resolver (handle t request);
    t.command_audit <- None;
    if t.stopped
    then (
      Mailbox.close t.mailbox;
      reject_pending t.mailbox)
    else run t
;;

let create_with_owner_lease_duration
      ~schedule_permission_timeouts
      ~sw
      ~clock
      ~mailbox_capacity
      ~owner_lease_duration_ms
      ~max_attachments
      ~subscriber_capacity
      ~compaction_env
      ~(initial_state : Session_state.t)
      ~persistence
      ~operation_worker
      ~services
  =
  if max_attachments <= 0 then invalid_arg "max_attachments must be positive";
  if subscriber_capacity <= 0 then invalid_arg "subscriber_capacity must be positive";
  (match Staged_subscriptions.validate_limits services.subscription_limits with
   | Ok () -> ()
   | Error error -> invalid_arg error.message);
  (match Staged_ingress.validate_limits services.ingress_limits with
   | Ok () -> ()
   | Error error -> invalid_arg error.message);
  (match Staged_schedules.validate_limits services.schedule_limits with
   | Ok () -> ()
   | Error error -> invalid_arg error.message);
  (match Staged_notifications.validate_limits services.notification_limits with
   | Ok () -> ()
   | Error error -> invalid_arg error.message);
  let t =
    { sw
    ; sleep = (fun seconds -> Eio.Time.sleep clock seconds)
    ; mailbox = Mailbox.create ~capacity:mailbox_capacity
    ; persistence
    ; services
    ; subscriber_mutex = Eio.Mutex.create ()
    ; active_calls = Active_calls.create ()
    ; subscribers = ref Map.Poly.empty
    ; permission_waiters = ref Map.Poly.empty
    ; operation_worker
    ; compaction_env
    ; owner_lease_duration_ms
    ; max_attachments
    ; subscriber_capacity
    ; schedule_permission_timeouts
    ; owner_timer_cancel = None
    ; active_cancel = None
    ; idle_moderator_borrowed = false
    ; moderator_borrow = None
    ; queued_event_borrow = None
    ; foreground_moderator = None
    ; invocation_executions = []
    ; job_scopes = []
    ; staged_jobs = Staged_jobs.create ()
    ; staged_subscriptions = Staged_subscriptions.create ()
    ; staged_ingress = Staged_ingress.create ()
    ; staged_schedules = Staged_schedules.create ()
    ; staged_notifications = Staged_notifications.create ()
    ; notification_inputs = None
    ; extension_clock = Extension_clock.create ()
    ; invocation_gate = Chat_response.Execution_gate.create ()
    ; event_sequence = Atomic.make initial_state.counters.event_sequence
    ; state = initial_state
    ; stopped = false
    ; command_audit = None
    }
  in
  sync_extension_clock t;
  (* A persisted actor outlives the tool/script that created it. Its future
     operations acquire their own budgets and delegated authority; inheriting
     this caller's lexical ChatML frame would poison them after creation ends. *)
  Chatml_execution.without_ambient_context (fun () ->
    Eio.Fiber.fork ~sw (fun () -> run t));
  Option.iter (owner_attachment t) ~f:(fun attachment ->
    Option.iter attachment.owner_lease ~f:(schedule_owner_lease t));
  t
;;

let create
      ~sw
      ~clock
      ~mailbox_capacity
      ~compaction_env
      ~initial_state
      ~persistence
      ~operation_worker
      ~services
  =
  create_with_owner_lease_duration
    ~schedule_permission_timeouts:true
    ~sw
    ~clock
    ~mailbox_capacity
    ~owner_lease_duration_ms:60_000
    ~max_attachments:1_024
    ~subscriber_capacity:512
    ~compaction_env
    ~initial_state
    ~persistence
    ~operation_worker
    ~services
;;

let snapshot t = call t Snapshot
let state t = call t State
let due_schedules t = call t Due_schedules

let commit_extensions t ~generation ~expected_revision changes =
  call t (Commit_extensions (generation, expected_revision, changes))
;;

let enable_automatic_turn_budget t policy = call t (Enable_automatic_turn_budget policy)
let set_automatic_turn_pauses t conditions = call t (Set_automatic_turn_pauses conditions)

let set_operation_worker t worker =
  call t ~priority:Priority (Set_operation_worker worker)
;;

let change_moderator t moderator = call t (Change_moderator moderator)
let shell_approval_grants t = call t Shell_approval_grants

let replace_shell_approval_grants t approval_grants =
  call t (Replace_shell_approval_grants approval_grants)
;;

let shell_manifest_grants t = call t Shell_manifest_grants
let add_shell_manifest_grant t grant = call t (Add_shell_manifest_grant grant)

let commit_administration t ~command_audit ~attachment_id ~expected_revision ~kind state =
  call
    t
    ?command_audit
    (Commit_administration (attachment_id, expected_revision, kind, state))
;;

let reset t ~attachment_id ~expected_revision options =
  call t ~priority:Priority (Reset (attachment_id, expected_revision, options))
;;

let reset_with_command_audit t ~command_audit ~attachment_id ~expected_revision options =
  call
    t
    ~priority:Priority
    ~command_audit
    (Reset (attachment_id, expected_revision, options))
;;

let upgrade_prompt t ~attachment_id ~expected_revision ~target_revision =
  call
    t
    ~priority:Priority
    (Upgrade_prompt (attachment_id, expected_revision, target_revision))
;;

let upgrade_prompt_with_command_audit
      t
      ~command_audit
      ~attachment_id
      ~expected_revision
      ~target_revision
  =
  call
    t
    ~priority:Priority
    ~command_audit
    (Upgrade_prompt (attachment_id, expected_revision, target_revision))
;;

let start ?expected_parent_stop_epoch t ~attachment_id =
  call t (Start (attachment_id, expected_parent_stop_epoch))
;;

let start_initial_delegated ?expected_parent_stop_epoch t ~reference =
  call t (Start_initial_delegated (reference, expected_parent_stop_epoch))
;;

let fail_initial_delegated t ~reference failure =
  call t ~priority:Priority (Fail_initial_delegated (reference, failure))
;;

let replace_workspace t workspace = call t ~priority:Priority (Change_workspace workspace)
let queue_start t ~attachment_id = call t (Queue_start attachment_id)

let start_with_command_audit ?expected_parent_stop_epoch t ~command_audit ~attachment_id =
  call t ~command_audit (Start (attachment_id, expected_parent_stop_epoch))
;;

let queue_start_with_command_audit t ~command_audit ~attachment_id =
  call t ~command_audit (Queue_start attachment_id)
;;

let activate_queued_start t = call t ~priority:Priority Activate_queued_start
let stop t ~attachment_id ~mode = call t ~priority:Priority (Stop (attachment_id, mode))

let stop_with_command_audit t ~command_audit ~attachment_id ~mode =
  call t ~priority:Priority ~command_audit (Stop (attachment_id, mode))
;;

let stop_delegated t ~reference ~mode =
  call t ~priority:Priority (Stop_delegated (reference, mode))
;;

let stop_managed t ~reference ~key ~mode ~generation ~max_receipts =
  call
    t
    ~priority:Priority
    (Stop_managed (reference, key, mode, generation, max_receipts))
;;

let stop_delegated_at_epoch ?(force = false) t ~reference ~epoch =
  call t ~priority:Priority (Stop_delegated_at_epoch (reference, epoch, force))
;;

let append_history t ~attachment_id entries =
  call t (Append_history (attachment_id, entries))
;;

let defer_history t ~attachment_id entries =
  call t (Defer_history (attachment_id, entries))
;;

let submit_message t ~attachment_id entry = call t (Submit_message (attachment_id, entry))

let submit_managed_message
      t
      ~reference
      ~key
      ~request_sha256
      ~generation
      ~max_receipts
      entry
  =
  Eio.Cancel.protect (fun () ->
    call
      t
      (Submit_managed_message
         (reference, key, request_sha256, generation, max_receipts, entry)))
;;

let submit_message_with_command_audit t ~command_audit ~attachment_id entry =
  call t ~command_audit (Submit_message (attachment_id, entry))
;;

let compact t ~attachment_id ~expected_revision =
  call t (Compact (attachment_id, expected_revision))
;;

let compact_with_command_audit t ~command_audit ~attachment_id ~expected_revision =
  call t ~command_audit (Compact (attachment_id, expected_revision))
;;

let delete_history t ?command_audit ~attachment_id ~expected_revision history_id =
  call t ?command_audit (Delete_history (attachment_id, expected_revision, history_id))
;;

let adopt_deferred t = call t Adopt_deferred
let reserve_history_block t ~count = call t (Reserve_history_block count)

let commit_worker_entry t ~operation_id entry =
  call t (Commit_worker_entry (operation_id, entry))
;;

let consume_deferred t ~operation_id = call t (Consume_deferred operation_id)

let consume_notifications t ~operation_id plan =
  call t (Consume_notifications (operation_id, plan))
;;

let deliver_idle_notifications t plan = call t (Deliver_idle_notifications plan)

let admit_standalone_delivery t plan =
  Eio.Cancel.protect (fun () -> call t (Admit_standalone_delivery plan))
;;

let retire_obsolete_moderator_delivery t ~revision ~job =
  Eio.Cancel.protect (fun () ->
    call t (Retire_obsolete_moderator_delivery (revision, job)))
;;

let deliver_standalone_completion t ~revision ~job ~current_capabilities ~policy =
  Eio.Cancel.protect (fun () ->
    call t (Deliver_standalone_completion (revision, job, current_capabilities, policy)))
;;

let consume_initial_notifications t ~operation_id plan =
  call t (Consume_initial_notifications (operation_id, plan))
;;

let cancel_operation t ~attachment_id ~operation_id =
  call t ~priority:Priority (Cancel_operation (attachment_id, operation_id))
;;

let cancel_operation_with_command_audit t ~command_audit ~attachment_id ~operation_id =
  call
    t
    ~priority:Priority
    ~command_audit
    (Cancel_operation (attachment_id, operation_id))
;;

let request_permission t ~permission ~timeout_seconds ~fallback =
  let open Result.Let_syntax in
  let%bind response = call t (Open_permission (permission, timeout_seconds, fallback)) in
  Ok (Eio.Promise.await response)
;;

let request_permission_with_review_fallback
      t
      ~permission
      ~timeout_seconds
      ~fallback
      ~review_on_timeout
  =
  request_permission_with_review_internal
    t
    ~permission
    ~timeout_seconds
    ~fallback
    ~review_on_timeout:(Some review_on_timeout)
;;

let request_review t ~permission ~review = request_review_internal t ~permission ~review

let resolve_permission_as_system t ~permission_id ~permission_generation ~choice ~reason =
  call
    t
    ~priority:Priority
    (Resolve_permission_system (permission_id, permission_generation, choice, reason))
;;

let expire_permission t ~permission_id ~permission_generation ~fallback =
  call
    t
    ~priority:Priority
    (Expire_permission (permission_id, permission_generation, fallback))
;;

let respond_permission
      t
      ~attachment_id
      ~principal_id
      ~permission_id
      ~permission_generation
      ~choice
      ~reason
  =
  call
    t
    ~priority:Priority
    (Respond_permission
       (attachment_id, principal_id, permission_id, permission_generation, choice, reason))
;;

let respond_permission_with_command_audit
      t
      ~command_audit
      ~attachment_id
      ~principal_id
      ~permission_id
      ~permission_generation
      ~choice
      ~reason
  =
  call
    t
    ~priority:Priority
    ~command_audit
    (Respond_permission
       (attachment_id, principal_id, permission_id, permission_generation, choice, reason))
;;

let revoke_grant t ~attachment_id ~grant_id ~reason =
  call t (Revoke_grant (attachment_id, grant_id, reason))
;;

let revoke_grant_with_command_audit t ~command_audit ~attachment_id ~grant_id ~reason =
  call t ~command_audit (Revoke_grant (attachment_id, grant_id, reason))
;;

let change_job t ~attachment_id job = call t (Change_job (attachment_id, job))
let add_job t job = call t (Add_job job)
let read_job t ~job_id = call t (Read_job job_id)

let publish_job_progress t ~invocation_id progress =
  match Job_progress_buffer.valid progress with
  | false -> ()
  | true ->
    let _promise, resolver = Eio.Promise.create () in
    ignore
      (Mailbox.try_push
         t.mailbox
         ~priority:Transient
         (Pack (None, Publish_job_progress (invocation_id, progress), resolver))
       : bool)
;;

let claim_job t ~job_id ~generation = call t (Claim_job (job_id, generation))

let complete_job t ~job_id ~generation ~attempt outcome =
  call t ~priority:Priority (Complete_job (job_id, generation, attempt, outcome))
;;

let complete_background_job t ~job_id ~generation ~attempt outcome =
  call
    t
    ~priority:Priority
    (Complete_background_job (job_id, generation, attempt, outcome))
;;

let defer_background_job t ~job_id ~generation ~attempt dependency =
  call
    t
    ~priority:Priority
    (Defer_background_job (job_id, generation, attempt, dependency))
;;

let refresh_background_job t ~job_id ~generation ~attempt =
  call t ~priority:Priority (Refresh_background_job (job_id, generation, attempt))
;;

let recover_background_results t ~max_count ~max_total_bytes =
  call t ~priority:Priority (Recover_background_results (max_count, max_total_bytes))
;;

let deliver_job ?expected ?expected_job t ~job_id ~generation ~moderator_snapshot =
  call t (Deliver_job (job_id, generation, expected, expected_job, moderator_snapshot))
;;

let cancel_job_internal t ~job_id = call t ~priority:Priority (Cancel_job_internal job_id)
let authorize_writer t ~attachment_id = call t (Authorize_writer attachment_id)

let cancel_job t ?command_audit ~attachment_id ~job_id () =
  call t ~priority:Priority ?command_audit (Cancel_job (attachment_id, job_id))
;;

let cancel_job_internal_with_command_audit t ~command_audit ~job_id =
  call t ~priority:Priority ~command_audit (Cancel_job_internal job_id)
;;

let interrupt_job t ~job_id ~generation ~attempt ~reason =
  call t ~priority:Priority (Interrupt_job (job_id, generation, attempt, reason))
;;

let change_schedule t ~attachment_id ~event schedule =
  call t (Change_schedule (attachment_id, event, schedule))
;;

let change_schedule_with_command_audit t ~command_audit ~attachment_id ~event schedule =
  call t ~command_audit (Change_schedule (attachment_id, event, schedule))
;;

let add_schedule t schedule = call t (Add_schedule schedule)

let cancel_schedule_internal t ~schedule_id =
  call t (Cancel_schedule_internal schedule_id)
;;

let claim_schedule t ~schedule_id ~generation =
  call t (Claim_schedule (schedule_id, generation))
;;

let retry_schedule t ~schedule_id ~generation =
  call t ~priority:Priority (Retry_schedule (schedule_id, generation))
;;

let complete_schedule
      ?expected
      ?expected_schedule
      t
      ~schedule_id
      ~generation
      ~moderator_snapshot
  =
  call
    t
    (Complete_schedule
       (schedule_id, generation, expected, expected_schedule, moderator_snapshot))
;;

let fail_schedule t ~schedule_id ~generation failure =
  call t (Fail_schedule (schedule_id, generation, failure))
;;

let skip_schedule t ~schedule_id ~generation =
  call t (Skip_schedule (schedule_id, generation))
;;

let claim_idle_moderator t = call t Claim_idle_moderator
let apply_observation_follow_up t = call t Apply_observation_follow_up
let apply_moderator_follow_up t = call t Apply_observation_follow_up

let invocation_granted t ~tool_name ~identity_digest =
  call t (Invocation_granted (tool_name, identity_digest))
;;

let complete_idle_moderator t drain =
  call t ~priority:Priority (Complete_idle_moderator drain)
;;

let fail_idle_moderator t failure =
  call t ~priority:Priority (Fail_idle_moderator failure)
;;

let attach_with_snapshot t ~principal_id ~reclaim_token ~mode ~subscribe =
  call t (Attach (mode, subscribe, principal_id, reclaim_token))
;;

let attach_with_snapshot_and_command_audit
      t
      ~command_audit
      ~principal_id
      ~reclaim_token
      ~mode
      ~subscribe
  =
  call t ~command_audit (Attach (mode, subscribe, principal_id, reclaim_token))
;;

let attach t ~mode ~subscribe =
  Result.map
    (attach_with_snapshot t ~principal_id:None ~reclaim_token:None ~mode ~subscribe)
    ~f:(fun (attachment, subscriber, _, _) -> attachment, subscriber)
;;

let detach t attachment_id = call t ~priority:Priority (Detach attachment_id)

let detach_with_command_audit t ~command_audit attachment_id =
  call t ~priority:Priority ~command_audit (Detach attachment_id)
;;

let renew_owner t ~attachment_id ~lease_generation =
  call t ~priority:Priority (Renew_owner (attachment_id, lease_generation))
;;

let renew_owner_with_command_audit t ~command_audit ~attachment_id ~lease_generation =
  call t ~priority:Priority ~command_audit (Renew_owner (attachment_id, lease_generation))
;;

let publish_recoverable t event = broadcast_recoverable t event
let checkpoint t ~persist = call t ~priority:Priority (Checkpoint persist)
let with_quiescent_state t ~f = call t ~priority:Priority (Quiescent_checkpoint f)

let prepare_ingress_submission
      t
      ~source
      ~producer
      ~registration_id
      ~namespace
      ~key
      ~payload
  =
  call
    t
    (Prepare_ingress_submission
       (source, producer, registration_id, namespace, key, payload))
;;

let commit_ingress_submission t proposal ~before ~snapshot =
  call t (Commit_ingress_submission (proposal, before, snapshot))
;;

module For_testing = struct
  let deliver_compaction_result t ~operation_id ~history =
    call t ~priority:Priority (Compaction_terminal (operation_id, Compacted history))
  ;;
end

let shutdown t =
  ignore (call t ~priority:Priority Shutdown : (unit, Agent_protocol.Error.t) result)
;;
