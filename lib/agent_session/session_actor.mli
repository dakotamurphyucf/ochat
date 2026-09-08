open! Core

(** Single-owner session actor shared by embedded and daemon execution. *)

type persistence =
  { commit :
      command_audit:string option
      -> previous:Session_state.t
      -> Session_transition.t
      -> (unit, Agent_protocol.Error.t) result
  }

type services =
  { now : unit -> Agent_protocol.Timestamp.t
  ; create_attachment_id : unit -> Agent_protocol.Id.Attachment.t
  ; create_reclaim_token : unit -> string
  ; state_committed : Session_state.t -> Agent_protocol.Event.Durable.t list -> unit
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

type t

(** Mailbox-level regression support; not exposed by any wire method. *)
module For_testing : sig
  (** Deliver a completed worker result through the real priority mailbox.
      Returning acknowledges actor consumption, including rejection of stale IDs. *)
  val deliver_compaction_result
    :  t
    -> operation_id:Agent_protocol.Id.Operation.t
    -> history:History_entry.t list
    -> (unit, Agent_protocol.Error.t) result
end

val create
  :  sw:Eio.Switch.t
  -> clock:_ Eio.Time.clock
  -> mailbox_capacity:int
  -> compaction_env:Eio_unix.Stdenv.base option
  -> initial_state:Session_state.t
  -> persistence:persistence
  -> operation_worker:Operation_worker.t option
  -> services:services
  -> t

val create_with_owner_lease_duration
  :  schedule_permission_timeouts:bool
  -> sw:Eio.Switch.t
  -> clock:_ Eio.Time.clock
  -> mailbox_capacity:int
  -> owner_lease_duration_ms:int
  -> max_attachments:int
  -> subscriber_capacity:int
  -> compaction_env:Eio_unix.Stdenv.base option
  -> initial_state:Session_state.t
  -> persistence:persistence
  -> operation_worker:Operation_worker.t option
  -> services:services
  -> t

val snapshot : t -> (Agent_protocol.Snapshot.t, Agent_protocol.Error.t) result
val state : t -> (Session_state.t, Agent_protocol.Error.t) result

module Extension_change : sig
  type t =
    | Invocation of Agent_protocol.Invocation.t
    | Subscription of Agent_protocol.Subscription.t
    | Delivery of Agent_protocol.Delivery.t
    | Publish of Agent_protocol.Delivery.t * Agent_protocol.History.entry
    | Start_job of Agent_protocol.Job.t
    | Schedule of Agent_protocol.Schedule.t
    | Moderator_state of Jsonaf.t option
end

(** Host-internal, revision-checked atomic commit of already admitted extension
    work. Jobs become visible to scheduling only after persistence succeeds.
    This is not a caller authorization service and must not be exposed as a raw
    model tool or external RPC. Runtime borrow/admission and disclosure checks
    belong to the dispatch service. Publication requires an idle safe point;
    active-turn integration must hand off through the worker boundary. *)
val commit_extensions
  :  t
  -> generation:int
  -> expected_revision:int64
  -> Extension_change.t list
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

(** [set_operation_worker] installs or removes the process-local runtime
    capability. It does not mutate durable session state. Callers may remove
    the worker only while no foreground operation is active. *)
val set_operation_worker
  :  t
  -> Operation_worker.t option
  -> (unit, Agent_protocol.Error.t) result

val change_moderator
  :  t
  -> Jsonaf.t option
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

(** Shell stores use these synchronous actor operations so executor-visible
    success always follows the durable session transaction. *)
val shell_approval_grants
  :  t
  -> (Session.Shell_state.Approval_grant.persisted list, Agent_protocol.Error.t) result

val replace_shell_approval_grants
  :  t
  -> Session.Shell_state.Approval_grant.persisted list
  -> (unit, Agent_protocol.Error.t) result

val shell_manifest_grants
  :  t
  -> (Session.Shell_state.Manifest_grant.persisted list, Agent_protocol.Error.t) result

val add_shell_manifest_grant
  :  t
  -> Session.Shell_state.Manifest_grant.persisted
  -> (unit, Agent_protocol.Error.t) result

val reset
  :  t
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> expected_revision:int64
  -> reset_options
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val reset_with_command_audit
  :  t
  -> command_audit:string
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> expected_revision:int64
  -> reset_options
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

(** [commit_administration t ... candidate] rechecks the stopped writer/revision
    boundary and commits prepared state with an independently retained archive.
    Failed persistence leaves the previous state installed. Preparation must not
    have mutated the actor; candidates carry the captured revision. *)
val commit_administration
  :  t
  -> command_audit:string option
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> expected_revision:int64
  -> kind:Session_state.Compaction_archive.kind
  -> Session_state.t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val upgrade_prompt
  :  t
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> expected_revision:int64
  -> target_revision:Agent_protocol.Id.Prompt_revision.t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val upgrade_prompt_with_command_audit
  :  t
  -> command_audit:string
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> expected_revision:int64
  -> target_revision:Agent_protocol.Id.Prompt_revision.t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val start
  :  t
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

(** [replace_workspace t workspace] durably installs a verified replacement
    workspace while the caller holds the session's administrative boundary. *)
val replace_workspace
  :  t
  -> Workspace_instance.t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val start_with_command_audit
  :  t
  -> command_audit:string
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val queue_start
  :  t
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val queue_start_with_command_audit
  :  t
  -> command_audit:string
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

(** [activate_queued_start t] advances an already accepted queued start after
    the server acquires its capacity. *)
val activate_queued_start : t -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val stop
  :  t
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> mode:Agent_protocol.Session.stop_mode
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val stop_with_command_audit
  :  t
  -> command_audit:string
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> mode:Agent_protocol.Session.stop_mode
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val append_history
  :  t
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> Agent_protocol.History.entry list
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val defer_history
  :  t
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> Agent_protocol.History.entry list
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val submit_message
  :  t
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> Agent_protocol.History.entry
  -> (submission, Agent_protocol.Error.t) result

val submit_message_with_command_audit
  :  t
  -> command_audit:string
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> Agent_protocol.History.entry
  -> (submission, Agent_protocol.Error.t) result

(** [delete_history t ... id] removes one canonical occurrence and its matching
    tool call/result occurrence. Requires an idle/stopped writable session and
    an exact revision; rejects borrowed moderator work. Commits before broadcast. *)
val delete_history
  :  t
  -> ?command_audit:string
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> expected_revision:int64
  -> Agent_protocol.History.Id.t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

(** [compact t ~attachment_id ~expected_revision] starts an asynchronous summary
    without changing canonical history until the successful terminal commit.
    That commit references an independently retained pre-compaction archive.
    An accepted cancellation wins over a concurrently finishing summary: publish
    [Operation_cancelled], preserve history and compaction generation, and discard
    the summary. This also applies when cancellation is accepted before the
    summary worker registers its cancellation callback. Cancellation acknowledgement
    precedes the terminal event; it does not mean the worker has finished.
    Provider failures without cancellation publish [Operation_failed]. *)
val compact
  :  t
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> expected_revision:int64 option
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val compact_with_command_audit
  :  t
  -> command_audit:string
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> expected_revision:int64 option
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val adopt_deferred : t -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val reserve_history_block
  :  t
  -> count:int
  -> (History_id_source.reservation, Agent_protocol.Error.t) result

val commit_worker_entry
  :  t
  -> operation_id:Agent_protocol.Id.Operation.t
  -> History_entry.t
  -> (unit, Agent_protocol.Error.t) result

val consume_deferred
  :  t
  -> operation_id:Agent_protocol.Id.Operation.t
  -> (History_entry.t list, Agent_protocol.Error.t) result

val cancel_operation
  :  t
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> operation_id:Agent_protocol.Id.Operation.t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val cancel_operation_with_command_audit
  :  t
  -> command_audit:string
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> operation_id:Agent_protocol.Id.Operation.t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val request_permission
  :  t
  -> permission:Agent_protocol.Permission.t
  -> timeout_seconds:float option
  -> fallback:Agent_protocol.Permission.choice
  -> (Agent_protocol.Permission.resolution, Agent_protocol.Error.t) result

(** [request_permission_with_review_fallback] uses [review_on_timeout] only
    after an unresolved interactive request reaches its deadline. Embedded
    actors run the callback in the waiting fiber; daemon actors rely on their
    durable deadline scheduler. *)
val request_permission_with_review_fallback
  :  t
  -> permission:Agent_protocol.Permission.t
  -> timeout_seconds:float option
  -> fallback:Agent_protocol.Permission.choice
  -> review_on_timeout:
       (unit -> (Permission_reviewer.Decision.t, Permission_reviewer.Error.t) result)
  -> (Agent_protocol.Permission.resolution, Agent_protocol.Error.t) result

(** [request_review t] durably opens [permission], executes the reviewer
    outside the actor, durably records the fail-closed resolution, and only
    then resumes the waiting operation. *)
val request_review
  :  t
  -> permission:Agent_protocol.Permission.t
  -> review:(unit -> (Permission_reviewer.Decision.t, Permission_reviewer.Error.t) result)
  -> (Agent_protocol.Permission.resolution, Agent_protocol.Error.t) result

(** Resolves an existing pending permission without a client attachment.
    Daemon-owned timeout/reviewer services use this compare-and-set path. *)
val resolve_permission_as_system
  :  t
  -> permission_id:Agent_protocol.Id.Permission.t
  -> permission_generation:int
  -> choice:Agent_protocol.Permission.choice
  -> reason:string option
  -> (Agent_protocol.Permission.t, Agent_protocol.Error.t) result

(** [expire_permission t] applies the configured timeout choice through the
    same actor compare-and-set path as an interactive response. *)
val expire_permission
  :  t
  -> permission_id:Agent_protocol.Id.Permission.t
  -> permission_generation:int
  -> fallback:Agent_protocol.Permission.choice
  -> (unit, Agent_protocol.Error.t) result

val respond_permission
  :  t
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> principal_id:Agent_protocol.Id.Principal.t option
  -> permission_id:Agent_protocol.Id.Permission.t
  -> permission_generation:int
  -> choice:Agent_protocol.Permission.choice
  -> reason:string option
  -> (Agent_protocol.Permission.t, Agent_protocol.Error.t) result

val respond_permission_with_command_audit
  :  t
  -> command_audit:string
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> principal_id:Agent_protocol.Id.Principal.t option
  -> permission_id:Agent_protocol.Id.Permission.t
  -> permission_generation:int
  -> choice:Agent_protocol.Permission.choice
  -> reason:string option
  -> (Agent_protocol.Permission.t, Agent_protocol.Error.t) result

val revoke_grant
  :  t
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> grant_id:Agent_protocol.Id.Grant.t
  -> reason:string
  -> (Agent_protocol.Grant.t * Agent_protocol.Session.t, Agent_protocol.Error.t) result

val revoke_grant_with_command_audit
  :  t
  -> command_audit:string
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> grant_id:Agent_protocol.Id.Grant.t
  -> reason:string
  -> (Agent_protocol.Grant.t * Agent_protocol.Session.t, Agent_protocol.Error.t) result

val change_job
  :  t
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> Agent_protocol.Job.t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val add_job
  :  t
  -> Agent_protocol.Job.t
  -> (Agent_protocol.Job.t, Agent_protocol.Error.t) result

val claim_job
  :  t
  -> job_id:Agent_protocol.Id.Job.t
  -> generation:int
  -> (Agent_protocol.Job.t option, Agent_protocol.Error.t) result

val complete_job
  :  t
  -> job_id:Agent_protocol.Id.Job.t
  -> generation:int
  -> Runtime_builder.model_job_outcome
  -> (Agent_protocol.Job.t, Agent_protocol.Error.t) result

val deliver_job
  :  t
  -> job_id:Agent_protocol.Id.Job.t
  -> generation:int
  -> moderator_snapshot:Jsonaf.t option
  -> (Agent_protocol.Job.t, Agent_protocol.Error.t) result

(** [authorize_writer t ~attachment_id] checks the current actor-owned attachment
    mode and lease before a host performs mutation preparation. Actor mutations
    must also retain their own authorization checks. *)
val authorize_writer
  :  t
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> (unit, Agent_protocol.Error.t) result

(** [cancel_job t ~attachment_id ~job_id ()] authorizes the attachment and its
    live lease atomically with cancellation. Internal schedulers use the
    separate privileged operation below. *)
val cancel_job
  :  t
  -> ?command_audit:string
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> job_id:Agent_protocol.Id.Job.t
  -> unit
  -> (Agent_protocol.Job.t, Agent_protocol.Error.t) result

val cancel_job_internal
  :  t
  -> job_id:Agent_protocol.Id.Job.t
  -> (Agent_protocol.Job.t, Agent_protocol.Error.t) result

val cancel_job_internal_with_command_audit
  :  t
  -> command_audit:string
  -> job_id:Agent_protocol.Id.Job.t
  -> (Agent_protocol.Job.t, Agent_protocol.Error.t) result

val interrupt_job
  :  t
  -> job_id:Agent_protocol.Id.Job.t
  -> generation:int
  -> reason:string
  -> (Agent_protocol.Job.t, Agent_protocol.Error.t) result

val change_schedule
  :  t
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> event:[ `Created | `Cancelled ]
  -> Agent_protocol.Schedule.t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val change_schedule_with_command_audit
  :  t
  -> command_audit:string
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> event:[ `Created | `Cancelled ]
  -> Agent_protocol.Schedule.t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

(** Internal ChatML schedule mutations bypass client attachments but remain
    generation-checked actor transactions. *)
val add_schedule
  :  t
  -> Agent_protocol.Schedule.t
  -> (Agent_protocol.Schedule.t, Agent_protocol.Error.t) result

val cancel_schedule_internal
  :  t
  -> schedule_id:Agent_protocol.Id.Schedule.t
  -> (Agent_protocol.Schedule.t, Agent_protocol.Error.t) result

(** Scheduler-only compare-and-set transitions. They reject stale session
    generations and never require a client attachment. *)
val claim_schedule
  :  t
  -> schedule_id:Agent_protocol.Id.Schedule.t
  -> generation:int
  -> (Agent_protocol.Schedule.t option, Agent_protocol.Error.t) result

(** [retry_schedule] releases a recovered in-flight delivery claim. Since a
    [Delivering] state has no committed queue-acceptance transaction, restart
    may safely make that occurrence runnable again. *)
val retry_schedule
  :  t
  -> schedule_id:Agent_protocol.Id.Schedule.t
  -> generation:int
  -> (Agent_protocol.Schedule.t, Agent_protocol.Error.t) result

val complete_schedule
  :  t
  -> schedule_id:Agent_protocol.Id.Schedule.t
  -> generation:int
  -> moderator_snapshot:Jsonaf.t option
  -> (Agent_protocol.Schedule.t, Agent_protocol.Error.t) result

val fail_schedule
  :  t
  -> schedule_id:Agent_protocol.Id.Schedule.t
  -> generation:int
  -> Agent_protocol.Error.t
  -> (Agent_protocol.Schedule.t, Agent_protocol.Error.t) result

val skip_schedule
  :  t
  -> schedule_id:Agent_protocol.Id.Schedule.t
  -> generation:int
  -> (Agent_protocol.Schedule.t, Agent_protocol.Error.t) result

(** Atomically select and claim one deferred observation in an idle, running,
    unblocked session. The callback runs outside the mailbox under exclusive
    moderator ownership, without creating a foreground operation. Returns false
    when unavailable or no matching observation remains. Selection is ordered by
    creation time and invocation ID, and bound to the exact observer source.
    The source must also match the committed moderator snapshot. Missing or
    replaced sources fail before a claim, leaving receipts/state unchanged.

    The callback must prospectively commit the observation acknowledgement and
    moderator snapshot together. Use [retain_follow_up] in the manager so runtime
    requests survive until a host applies them durably after releasing ownership.
    Failure/cancellation records observation failure without altering native
    results; successful acknowledgements are never replayed. The callback does
    not acquire foreground native-tool authority. This API does not schedule a
    turn, install a wakeup or apply retained follow-up requests. *)
val with_idle_moderator_observation
  :  t
  -> observer:Agent_protocol.Invocation.observer
  -> (observing:Agent_protocol.Invocation.t
      -> commit:
           (resolved:Agent_protocol.Invocation.t
            -> snapshot:Session.Moderator_state.Identity_snapshot.t
            -> (unit, Agent_protocol.Error.t) result)
      -> (unit, Agent_protocol.Error.t) result)
  -> (bool, Agent_protocol.Error.t) result

(** Idle observation handoff with a native invocation executor tied to this
    borrow. Only Moderator children of the observing invocation, with the same
    source-bound observation intent and session generation, can be admitted.
    Calls and results are persisted outside provider history. The executor grants
    no tool authorization: route it through [Native_tool_invocation.run_scoped].
    It expires when the callback returns or commits and rejects calls after stop.
    Cancel-stop interrupts active calls; outcomes still need protected persistence.
    Acknowledgement is rejected until all child outcomes have been saved. Failed
    saves leave interruption evidence and are retired when the borrow finishes.
    This does not install Tool.call, ordinary-event or foreground authority. *)
val with_idle_moderator_observation_tools
  :  t
  -> observer:Agent_protocol.Invocation.observer
  -> (observing:Agent_protocol.Invocation.t
      -> execute:Native_tool_invocation.executor
      -> commit:
           (resolved:Agent_protocol.Invocation.t
            -> snapshot:Session.Moderator_state.Identity_snapshot.t
            -> (unit, Agent_protocol.Error.t) result)
      -> (unit, Agent_protocol.Error.t) result)
  -> (bool, Agent_protocol.Error.t) result

(** [claim_idle_moderator] acquires the process-local exclusive moderator
    borrow only when the running session is idle and unblocked. *)
val claim_idle_moderator
  :  t
  -> (History_entry.t list option, Agent_protocol.Error.t) result

(** At an idle safe point, coalesce retained requests and atomically accept them
    with scheduling/stop. A compaction-plus-turn request retains its turn until
    the next idle safe point. Stop discards outstanding requests. Returns whether
    receipts changed; false while unavailable or no work remains. No observer
    executes here. The host must install the worker before admitting a turn. *)
val apply_observation_follow_up : t -> (bool, Agent_protocol.Error.t) result

(** [complete_idle_moderator] durably checkpoints the moderator result and
    schedules any resulting foreground work before releasing the borrow. *)
val complete_idle_moderator
  :  t
  -> Runtime_builder.moderator_drain
  -> (unit, Agent_protocol.Error.t) result

(** [fail_idle_moderator] releases the borrow and fails the session closed. *)
val fail_idle_moderator
  :  t
  -> Agent_protocol.Error.t
  -> (unit, Agent_protocol.Error.t) result

val attach
  :  t
  -> mode:Agent_protocol.Session.attachment_mode
  -> subscribe:bool
  -> ( Agent_protocol.Session.Attachment.t * Subscriber.t option
       , Agent_protocol.Error.t )
       result

(** Atomically installs the subscription and captures its replay boundary.
    Events committed after the returned snapshot are queued to [subscriber]. *)
val attach_with_snapshot
  :  t
  -> principal_id:Agent_protocol.Id.Principal.t option
  -> reclaim_token:string option
  -> mode:Agent_protocol.Session.attachment_mode
  -> subscribe:bool
  -> ( Agent_protocol.Session.Attachment.t
       * Subscriber.t option
       * Agent_protocol.Snapshot.t
       * string option
       , Agent_protocol.Error.t )
       result

val attach_with_snapshot_and_command_audit
  :  t
  -> command_audit:string
  -> principal_id:Agent_protocol.Id.Principal.t option
  -> reclaim_token:string option
  -> mode:Agent_protocol.Session.attachment_mode
  -> subscribe:bool
  -> ( Agent_protocol.Session.Attachment.t
       * Subscriber.t option
       * Agent_protocol.Snapshot.t
       * string option
       , Agent_protocol.Error.t )
       result

val detach : t -> Agent_protocol.Id.Attachment.t -> (unit, Agent_protocol.Error.t) result

val detach_with_command_audit
  :  t
  -> command_audit:string
  -> Agent_protocol.Id.Attachment.t
  -> (unit, Agent_protocol.Error.t) result

val renew_owner
  :  t
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> lease_generation:int64
  -> ( Agent_protocol.Session.Owner_lease.t * Agent_protocol.Session.t
       , Agent_protocol.Error.t )
       result

val renew_owner_with_command_audit
  :  t
  -> command_audit:string
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> lease_generation:int64
  -> ( Agent_protocol.Session.Owner_lease.t * Agent_protocol.Session.t
       , Agent_protocol.Error.t )
       result

val publish_recoverable : t -> Agent_protocol.Event.Recoverable.t -> unit

(** [checkpoint t ~persist] invokes [persist] with the authoritative durable
    state while serialized with all actor transitions. *)
val checkpoint
  :  t
  -> persist:(Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> (unit, Agent_protocol.Error.t) result

val shutdown : t -> unit
