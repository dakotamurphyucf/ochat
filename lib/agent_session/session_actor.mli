open! Core

(** Single-owner session actor shared by embedded and daemon execution. *)

type persistence =
  { commit :
      command_audit:Document_schema.Document.t option
      -> previous:Session_state.t
      -> Session_transition.t
      -> (unit, Agent_protocol.Error.t) result
  ; archive_reference :
      previous:Session_state.t
      -> kind:Session_state.Compaction_archive.kind
      -> Agent_protocol.Id.Operation.t
      -> (Session_state.Compaction_archive.t, Agent_protocol.Error.t) result
  }

type services =
  { now : unit -> Agent_protocol.Timestamp.t
  ; monotonic_now : unit -> Mtime.t
  ; create_attachment_id : unit -> Agent_protocol.Id.Attachment.t
  ; create_reclaim_token : unit -> string
  ; state_committed : Session_state.t -> Agent_protocol.Event.Durable.t list -> unit
  ; job_results : Agent_store.Job_result_store.Publisher.t option
    (** Optional session-owned artifact publication/loading. Absence keeps inline
        publication and fails explicitly when an existing artifact needs loading. *)
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

type t

module History_edit_result : sig
  type t =
    { session : Agent_protocol.Session.t
    ; history_id : Agent_protocol.History.Id.t
    ; content_revision : Agent_protocol.History.Content_revision.t
    ; archived_revision : int64
    ; continuation : Agent_protocol.History_edit.Continuation.t
    }
end

module History_continuation_result : sig
  type t =
    { session : Agent_protocol.Session.t
    ; continuation : Agent_protocol.History_edit.Continuation.t
    }
end

module Runtime_retirement : sig
  (** A process-local foreground cleanup join. The actor remains available to
      inference and worker finalizers until the terminal checkpoint finishes. *)
  type t

  val is_finished : t -> bool

  (** Await outside the actor mailbox and runtime owner mutex. Terminal commit
      errors are returned; unexpected failures retain their original backtrace. *)
  val await : t -> (unit, Agent_protocol.Error.t) result
end

(** Exclude new user, deferred, automatic, start and compaction admission and cancel the
    active foreground operation, including a worker that has not acknowledged
    readiness yet. Returns immediately with a join completed after its actual
    worker scope and terminal checkpoint. Concurrent requests share the join.
    Does not change durable desired state or discard deferred messages. Successful
    [closing=true] permanently excludes admission, including across an ordinary
    unload already in progress. For reusable retirement, successful detachment or
    worker installation reopens admission after the retiring operation has joined.
    Safe points return no additional deferred/notification input after retirement
    begins. Background lifetimes must be cancelled/joined separately by the owner. *)
val retire_runtime_worker
  :  t
  -> closing:bool
  -> (Runtime_retirement.t, Agent_protocol.Error.t) result

module Compaction_inference : sig
  (** Trusted process-local selected auxiliary lifetime. Operation and complete
      selection are captured by durable compaction admission. The callback runs
      after Worker_ready, outside the actor mailbox; it must not initialize a
      runtime or reselect a provider. Its bracket joins inference work before
      releasing tracking/resources, preserves primary errors/cancellation, and
      propagates cleanup failure after otherwise successful work. *)
  type t =
    { with_execution :
        'a.
        operation:Agent_protocol.Operation.t
        -> selection:Inference.Selection.t
        -> (Inference_client.Execution.t -> ('a, Agent_protocol.Error.t) Result.t)
        -> ('a, Agent_protocol.Error.t) Result.t
    }
end

(** Install an explicitly owned host auxiliary port. None retains direct actor
    fixture support for an explicitly installed selected runtime, otherwise
    compaction reports configuration unavailable. No ambient provider exists. *)
val set_compaction_inference
  :  t
  -> Compaction_inference.t option
  -> (unit, Agent_protocol.Error.t) Result.t

module Inference_owner : sig
  (** One actual constructed graph, with no model/tool authority. The process-local
      token survives worker detachment and authorized resource borrowing. *)
  type t
end

(** Open before graph constructors. Fresh graph source, issuing actor and current
    generation are captured; it cannot be reconstructed from durable state. *)
val open_inference_owner
  :  t
  -> source:Transcript.Source_id.t
  -> (Inference_owner.t, Agent_protocol.Error.t) Result.t

(** Serialized original-ledger admission and durable ACK precede handle return.
    Optional associations require actual per-execution evidence; no inference from
    the actor's current operation. Untracked admission also ACKs its ordinal. *)
val admit_inference
  :  t
  -> owner:Inference_owner.t
  -> relation:Transcript.Scope.relation
  -> operation_id:Agent_protocol.Id.Operation.t option
  -> invocation_id:Agent_protocol.Id.Invocation.t option
  -> configuration:Inference.Observation.Configuration.t
  -> (Inference_ledger.Handle.t, Agent_protocol.Error.t) Result.t

val acknowledge_inference
  :  t
  -> owner:Inference_owner.t
  -> handle:Inference_ledger.Handle.t
  -> Inference_runtime.Attempt.t
  -> (unit, Agent_protocol.Error.t) Result.t

(** Exact retained-handle observations may arrive after inference ends. Missing
    retired/untracked rows stay absent. Context estimate remains separate from
    usage and must carry the designated context identity/preparation. *)
val observe_inference
  :  t
  -> handle:Inference_ledger.Handle.t
  -> Inference.Observation.t
  -> (unit, Agent_protocol.Error.t) Result.t

(** Late revisions may use an EXACT retained admitted handle under this still
    live graph owner. Missing/future/unallocated scope rejects; no handle is
    constructed from incoming identifiers or lifetime tombstone index. *)
val observe_owned_inference
  :  t
  -> owner:Inference_owner.t
  -> Inference.Observation.t
  -> (unit, Agent_protocol.Error.t) result

val complete_inference
  :  t
  -> owner:Inference_owner.t
  -> handle:Inference_ledger.Handle.t
  -> Inference_client.Completion.t
  -> (unit, Agent_protocol.Error.t) Result.t

(** Called once after allocation's joined lifetime. Routing is removed even if
    interruption ACK fails; the bounded durable row remains recovery authority.
    Uses the actual live Attempt.delivery when acknowledged. Without that owned
    instance, Prepared is definitely not submitted and Running conservatively
    possibly submitted. No provider terminal is manufactured. *)
val release_inference
  :  t
  -> owner:Inference_owner.t
  -> handle:Inference_ledger.Handle.t
  -> (unit, Agent_protocol.Error.t) Result.t

val seal_inference_owner
  :  t
  -> owner:Inference_owner.t
  -> (unit, Agent_protocol.Error.t) Result.t

(** Only after the actual graph excludes new calls and joins. Reconciles bounded
    retained residual rows; success removes the owner. Actor/writer must be live. *)
val finish_inference_owner
  :  t
  -> owner:Inference_owner.t
  -> (unit, Agent_protocol.Error.t) Result.t

(** Explicit constructor-only mutations under the actor-issued preparation.
    Each request captures the token; the actor checks its issuer/current policy
    before persistence. Ordinary APIs above never infer constructor custody. *)
module Constructor_mutations : sig
  val admit_inference
    :  t
    -> preparation:Run_preparation.t
    -> owner:Inference_owner.t
    -> relation:Transcript.Scope.relation
    -> operation_id:Agent_protocol.Id.Operation.t option
    -> invocation_id:Agent_protocol.Id.Invocation.t option
    -> configuration:Inference.Observation.Configuration.t
    -> (Inference_ledger.Handle.t, Agent_protocol.Error.t) Result.t

  val acknowledge_inference
    :  t
    -> preparation:Run_preparation.t
    -> owner:Inference_owner.t
    -> handle:Inference_ledger.Handle.t
    -> Inference_runtime.Attempt.t
    -> (unit, Agent_protocol.Error.t) Result.t

  val observe_inference
    :  t
    -> preparation:Run_preparation.t
    -> handle:Inference_ledger.Handle.t
    -> Inference.Observation.t
    -> (unit, Agent_protocol.Error.t) Result.t

  val observe_owned_inference
    :  t
    -> preparation:Run_preparation.t
    -> owner:Inference_owner.t
    -> Inference.Observation.t
    -> (unit, Agent_protocol.Error.t) result

  val complete_inference
    :  t
    -> preparation:Run_preparation.t
    -> owner:Inference_owner.t
    -> handle:Inference_ledger.Handle.t
    -> Inference_client.Completion.t
    -> (unit, Agent_protocol.Error.t) Result.t

  val release_inference
    :  t
    -> preparation:Run_preparation.t
    -> owner:Inference_owner.t
    -> handle:Inference_ledger.Handle.t
    -> (unit, Agent_protocol.Error.t) Result.t

  val finish_inference_owner
    :  t
    -> preparation:Run_preparation.t
    -> owner:Inference_owner.t
    -> (unit, Agent_protocol.Error.t) Result.t

  val replace_shell_approval_grants
    :  t
    -> preparation:Run_preparation.t
    -> Session.Shell_state.Approval_grant.persisted list
    -> (unit, Agent_protocol.Error.t) result

  val add_shell_manifest_grant
    :  t
    -> preparation:Run_preparation.t
    -> Session.Shell_state.Manifest_grant.persisted
    -> (unit, Agent_protocol.Error.t) result
end

(** Exclusive activation/recovery only, never read/query. Reconciles outstanding
    Prepared/Running using honest conservative delivery before fresh constructors. *)
val reconcile_inference_recovery : t -> (unit, Agent_protocol.Error.t) Result.t

module Initialization_scope : sig
  (** Process-local capability issued only to a trusted Pending constructor.
      Bound to one actor, stable source/complete Selection and generation; it is
      never reconstructed from durable Pending state or exposed by RPC. *)
  type t
end

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

(** One mailbox observation captures retained state and transient active calls
    from the same owner instant. Never activates or acknowledges work. *)
val observe
  :  t
  -> (Session_state.t * Agent_protocol.Snapshot.t, Agent_protocol.Error.t) result

val state : t -> (Session_state.t, Agent_protocol.Error.t) result

(** Trusted host ingress bridge. Producer must come from authenticated transport
    context. Preparation is read-only; commit requires the exact captured state
    and only the proposed queue append, saving receipt and snapshot together.
    Duplicate acknowledgements never enqueue. These are not raw model/RPC APIs. *)
val prepare_ingress_submission
  :  t
  -> source:Agent_protocol.Invocation.observer
  -> producer:Agent_protocol.Id.Principal.t
  -> registration_id:Agent_protocol.Id.Capability.t
  -> namespace:string
  -> key:Agent_protocol.Idempotency_key.t
  -> payload:Jsonaf.t
  -> (Ingress_submission.decision, Agent_protocol.Error.t) result

val commit_ingress_submission
  :  t
  -> Ingress_submission.t
  -> before:Session.Moderator_state.Identity_snapshot.t
  -> snapshot:Session.Moderator_state.Identity_snapshot.t
  -> (External_ingress.receipt, Agent_protocol.Error.t) result

(** Host-only moderator registration transactions. Creation derives the producer
    from the session's recorded creating principal; scripts cannot supply it.
    Mutations require the actual live moderator owner/source and are invisible
    until selected and committed with its checkpoint/outcome. *)
val create_script_ingress
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> source:Agent_protocol.Invocation.observer
  -> subscription_id:Agent_protocol.Id.Subscription.t
  -> expected_epoch:int
  -> namespace:string
  -> schema:Jsonaf.t
  -> (int * External_ingress.t, Agent_protocol.Error.t) result

val revoke_script_ingress
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> source:Agent_protocol.Invocation.observer
  -> id:Agent_protocol.Id.Capability.t
  -> reason:string
  -> (int * External_ingress.t, Agent_protocol.Error.t) result

val read_script_ingress
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> source:Agent_protocol.Invocation.observer
  -> id:Agent_protocol.Id.Capability.t
  -> (External_ingress.t, Agent_protocol.Error.t) result

val select_ingress_mutations
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> source:Agent_protocol.Invocation.observer
  -> receipts:int list
  -> (unit, Agent_protocol.Error.t) result

val abort_ingress_mutation
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> receipt:int
  -> (unit, Agent_protocol.Error.t) result

(** Host-only transactional background admission. The caller captures the opaque
    request from the current borrowed tool authority before preparing a job.
    Preparation derives ancestry from a live callback and does not reserve or
    execute work. Do not expose these functions as raw RPCs or model tools. *)
val prepare_background_job_launch
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> Chat_response.Background_request.t
  -> (Agent_protocol.Job.t, Agent_protocol.Error.t) result

(** Transfer a shared-capacity reservation for the unchanged prepared job to its
    live owner. Failure releases the reservation; publish/abort callbacks must be
    infallible and idempotent. Staging is invisible to durable job readers. *)
val stage_background_job
  :  t
  -> job:Agent_protocol.Job.t
  -> capacity:Staged_jobs.capacity
  -> (unit, Agent_protocol.Error.t) result

(** Select exactly the job IDs in the surviving transaction effect log. Saving
    the owner outcome/checkpoint atomically persists these jobs, then publishes
    reservations. Rejection or callback exit releases provisional reservations. *)
val select_background_jobs
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> ids:Agent_protocol.Id.Job.t list
  -> (unit, Agent_protocol.Error.t) result

(** Idempotent catch-rollback cleanup, also allowed after the callback ends. *)
val abort_background_job
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> id:Agent_protocol.Id.Job.t
  -> (unit, Agent_protocol.Error.t) result

val has_staged_background_job
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> id:Agent_protocol.Id.Job.t
  -> (bool, Agent_protocol.Error.t) result

(** Host-only subscription transaction adapter. The actor verifies an actual
    live moderator invocation/event borrow and its installed source. Creations
    must belong to that currently dispatched moderator invocation; updates must
    retain its original source/session/generation. The host must supply the
    original declaration's completion schema and lifetime choice, never raw model
    ownership fields. Staging checks shared admission quotas and transitions.
    Selected changes save with the owning moderator checkpoint, and are discarded
    on rollback, failed save, scope exit, stop or shutdown. *)
val stage_subscription_mutation
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> source:Agent_protocol.Invocation.observer
  -> previous:Agent_protocol.Subscription.t option
  -> next:Agent_protocol.Subscription.t
  -> (int, Agent_protocol.Error.t) result

(** Atomically construct and stage a subscription under the actual moderator
    borrow. The actor generates identity/time and captures its creating job
    attempt; neither the script nor its service chooses ancestry. *)
val create_script_subscription
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> source:Agent_protocol.Invocation.observer
  -> kind:string
  -> lifetime_ms:int
  -> wake:Agent_protocol.Completion.wake
  -> completion_schema:Jsonaf.t option
  -> (int * Agent_protocol.Subscription.t, Agent_protocol.Error.t) result

val select_subscription_mutations
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> source:Agent_protocol.Invocation.observer
  -> receipts:int list
  -> (unit, Agent_protocol.Error.t) result

(** Actor-owned completion selection. Uses the subscription's elapsed deadline,
    preserves a retained winner, normalizes terminal wall timestamps, and returns
    a staged receipt to commit with the moderator checkpoint. *)
val finish_script_subscription
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> source:Agent_protocol.Invocation.observer
  -> id:Agent_protocol.Id.Subscription.t
  -> expected_epoch:int
  -> Agent_protocol.Completion.t
  -> (int * Agent_protocol.Subscription.t, Agent_protocol.Error.t) result

val abort_subscription_mutation
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> receipt:int
  -> (unit, Agent_protocol.Error.t) result

(** Source-checked read of this owner's provisional state or retained state.
    Unbound legacy subscriptions and other moderator sources are rejected. *)
val read_script_subscription
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> source:Agent_protocol.Invocation.observer
  -> id:Agent_protocol.Id.Subscription.t
  -> (Agent_protocol.Subscription.t, Agent_protocol.Error.t) result

(** Host scheduler sweep of retained subscriptions whose absolute deadline has
    passed. Expiry advances the epoch and cancels a linked outstanding schedule
    in the same durable transaction. Terminal winners and linked jobs are kept.
    Runs without borrowing/loading a moderator, including stopped sessions and
    retained older generations. Failed persistence changes nothing and may be
    retried; a sweep with no due work does not advance the session revision. *)
val expire_subscriptions : t -> (int, Agent_protocol.Error.t) result

(** Host-only source-bound schedule transaction adapter. Creation captures the
    actual moderator invocation/event and enforces shared admission budgets.
    Selected mutations save with that owner's checkpoint; abort and stop release
    reservations. Legacy unowned schedules cannot be claimed through this API. *)
val create_script_schedule
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> source:Agent_protocol.Invocation.observer
  -> delay_ms:int
  -> payload:Jsonaf.t
  -> misfire:Agent_protocol.Schedule.misfire
  -> (int * Agent_protocol.Schedule.t, Agent_protocol.Error.t) result

val stage_schedule_mutation
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> source:Agent_protocol.Invocation.observer
  -> previous:Agent_protocol.Schedule.t option
  -> next:Agent_protocol.Schedule.t
  -> (int, Agent_protocol.Error.t) result

val select_schedule_mutations
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> source:Agent_protocol.Invocation.observer
  -> receipts:int list
  -> (unit, Agent_protocol.Error.t) result

val abort_schedule_mutation
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> receipt:int
  -> (unit, Agent_protocol.Error.t) result

val read_script_schedule
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> source:Agent_protocol.Invocation.observer
  -> id:Agent_protocol.Id.Schedule.t
  -> (Agent_protocol.Schedule.t, Agent_protocol.Error.t) result

(** Read a job with optional live progress from its current attempt. Progress
    disappears when the worker scope ends and is never a durable result. *)
val read_job
  :  t
  -> job_id:Agent_protocol.Id.Job.t
  -> (Agent_protocol.Job.t, Agent_protocol.Error.t) result

(** Nonblocking, lossy progress ingress. Only the actual live native invocation's
    background scope may accept it. Invalid/oversized updates and mailbox pressure
    drop progress without changing tool completion or durable state. *)
val publish_job_progress
  :  t
  -> invocation_id:Agent_protocol.Id.Invocation.t
  -> Ochat_function.Progress.t
  -> unit

(** Host-internal scoped reads/cancellation. The active caller sees its own
    provisional jobs and current-generation durable jobs of this session.
    The script host must project/redact results rather than expose raw payloads.
    Cancellation is immediate and is not reversed by catching a later error. *)
val read_script_job
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> id:Agent_protocol.Id.Job.t
  -> (Agent_protocol.Job.t, Agent_protocol.Error.t) result

val cancel_script_job
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> id:Agent_protocol.Id.Job.t
  -> (unit, Agent_protocol.Error.t) result

(** Load the exact terminal record checked by the script host. Rechecks active
    ownership, generation and record identity before bounded verified IO. *)
val read_script_job_result
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> expected:Agent_protocol.Job.t
  -> (Agent_protocol.Completion.t, Agent_protocol.Error.t) result

(** Stage an owned pending delivery without publishing history or waking a turn.
    The actor validates the live moderator borrow, source, references, terminal
    work result and shared capacity. Selected intents commit with the handler;
    discarded or failed handlers leave no notification. Host service disclosure
    checks must precede calls involving job results. *)
val create_script_notification
  :  ?disclosure_pins:(string * string) list
  -> t
  -> owner:Agent_protocol.Job.launch_owner
  -> source:Agent_protocol.Invocation.observer
  -> correlation:Chat_response.Notification_operations.correlation
  -> completion:Agent_protocol.Completion.t
  -> wake:Agent_protocol.Completion.wake
  -> (int * Agent_protocol.Delivery.t, Agent_protocol.Error.t) result

val read_script_notification
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> source:Agent_protocol.Invocation.observer
  -> id:Agent_protocol.Id.Delivery.t
  -> (Agent_protocol.Delivery.t, Agent_protocol.Error.t) result

val select_notification_mutations
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> source:Agent_protocol.Invocation.observer
  -> receipts:int list
  -> (unit, Agent_protocol.Error.t) result

val abort_notification_mutation
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> receipt:int
  -> (unit, Agent_protocol.Error.t) result

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

(** Host-owned invocation service for one already claimed background job attempt.
    The callback receives the actor's actual job record and an expiring executor.
    Only one scope may own a job at a time. Roots require Script origin and that
    job's [parent_job], without foreground/event/provider identities. Descendants
    require an active Script parent in this same scope and cannot widen deadlines.
    The host derives [deadline] from durable admission/resource policy; it is an
    admission ceiling, not a replacement for worker timeouts.

    Invocations and outcomes use normal actor persistence and permission ownership,
    without creating a foreground operation or provider history. Use the executor
    through [Native_tool_invocation.run_scoped] or the equivalent owned script
    adapter: this scope grants no tool capabilities, admission or policy bypass.
    Cancellation/interrupt closes the callback's Eio cancellation context after the
    durable job transition; cancelled permission waiters are resolved after save.
    Foreground completion does not retire this job's invocations.

    The callback must join its work. Returning expires the executor and cancels
    in-flight invocation callbacks, including callers on another switch;
    unfinished invocations are cancelled durably and reported as
    an error. Job completion/retry must wait until this scope is released. A failed
    cleanup save retains an inactive owner for reconciliation, preventing reuse.
    Does not perform scheduler dispatch or transactional launch admission. *)
type job_execution =
  { job : Agent_protocol.Job.t
  ; execute : Native_tool_invocation.executor
  ; moderator_execute : Native_tool_invocation.moderator_executor
  ; claim_event : event:Chat_response.Moderation.Event.t -> Moderator_event.claim
  }

(** Retain job-attempt ownership across native calls and moderator events. Event
    claims use the actor's moderator gate and persist job identity with the
    checkpoint receipt. Returned services expire at callback completion. *)
val with_job_execution
  :  t
  -> job_id:Agent_protocol.Id.Job.t
  -> generation:int
  -> attempt:int
  -> deadline:Agent_protocol.Timestamp.t option
  -> (job_execution -> ('a, Agent_protocol.Error.t) result)
  -> ('a, Agent_protocol.Error.t) result

val with_job_invocations
  :  t
  -> job_id:Agent_protocol.Id.Job.t
  -> generation:int
  -> attempt:int
  -> deadline:Agent_protocol.Timestamp.t option
  -> (job:Agent_protocol.Job.t
      -> execute:Native_tool_invocation.executor
      -> ('a, Agent_protocol.Error.t) result)
  -> ('a, Agent_protocol.Error.t) result

(** Enable qualified host follow-up accounting once. Reinstalling the same policy
    is a read-only success; replacement cannot reset a retained budget. *)
val enable_automatic_turn_budget
  :  t
  -> Chat_response.Runtime_semantics.policy
  -> (unit, Agent_protocol.Error.t) result

(** Host-only pause/resume at a quiescent boundary. Persists pause flags without
    replacing limits, resetting counts or altering already admitted work. *)
val set_automatic_turn_pauses
  :  t
  -> Chat_response.Runtime_semantics.pause_condition list
  -> (unit, Agent_protocol.Error.t) result

(** [set_operation_worker] installs or removes the process-local runtime
    capability. It does not mutate durable session state. Callers may remove
    the worker only while no foreground operation is active. *)
val set_operation_worker
  :  t
  -> Operation_worker.t option
  -> (unit, Agent_protocol.Error.t) result

(** Install or clear the actual selected runtime and inference execution together.
    Some/None mismatches reject; Pending initialization rejects installation.
    The constructor and legacy worker-only setter retain no inferred execution.
    No callback reenters the runtime owner's mutex during compaction. *)
val set_runtime_worker
  :  t
  -> worker:Operation_worker.t option
  -> inference:Inference_client.Execution.t option
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
  -> command_audit:Document_schema.Document.t
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
  -> command_audit:Document_schema.Document.t option
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> expected_revision:int64
  -> kind:Session_state.Compaction_archive.kind
  -> Session_state.t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

(** Trusted owner-fenced administration. Validate the original writer/CAS basis
    BEFORE resource retirement. The reconciled commit accepts only ledger,
    counters and updated timestamp changes caused by that retirement; complete
    opaque configuration and all other fields must still match [expected].
    The original candidate is validated before overlaying the actual ledger,
    preserving retained records/ordinals across the candidate generation. *)
val validate_administration_basis
  :  t
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> expected:Session_state.t
  -> (unit, Agent_protocol.Error.t) result

val commit_reconciled_administration
  :  t
  -> command_audit:Document_schema.Document.t option
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> expected:Session_state.t
  -> kind:Session_state.Compaction_archive.kind
  -> Session_state.t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

(** Issue one exclusive constructor scope against the actual Pending basis.
    Requires a captured target. Actor-owned jobs/schedules, tracking counters,
    reservations and acknowledged shell grants may advance without replacing the
    basis. Accepted authorized Stop permanently revokes even a stopped no-op. *)
val begin_initialization
  :  t
  -> expected:Session_state.t
  -> (Initialization_scope.t, Agent_protocol.Error.t) result

(** Same existing constructor scope, explicitly bound to this run preparation.
    Scoped commits recheck current custody and acknowledge only their own changes. *)
val begin_run_initialization
  :  t
  -> preparation:Run_preparation.t
  -> expected:Session_state.t
  -> (Initialization_scope.t, Agent_protocol.Error.t) result

(** Retire the issuing actor's scope idempotently, without a durable write. Wrong
    actors reject. The host must call this under cancellation protection on every
    exit before exposing the constructed runtime. *)
val end_initialization
  :  t
  -> scope:Initialization_scope.t
  -> (unit, Agent_protocol.Error.t) result

(** Admit a fresh queued Model_call under the active constructor capability.
    Uses ordinary job validation and atomic source binding. *)
val add_initialization_model_job
  :  t
  -> scope:Initialization_scope.t
  -> Agent_protocol.Job.t
  -> (Agent_protocol.Job.t, Agent_protocol.Error.t) result

(** Admit and claim a fresh immediate, Not_required Model_call in one serialized
    actor request. Reuses ordinary admission/claim invariants; their two ordered
    durable commits remain crash-safe under Pending recovery. Stop cannot
    interleave between them. *)
val start_initialization_model_job
  :  t
  -> scope:Initialization_scope.t
  -> Agent_protocol.Job.t
  -> (Agent_protocol.Job.t, Agent_protocol.Error.t) result

(** Check active scope and its actually admitted running job's exact ID,
    generation and attempt. Retired/revoked/stale scopes yield false; another
    actor's token rejects. No authority is derived from persisted Pending. *)
val initialization_model_job_is_current
  :  t
  -> scope:Initialization_scope.t
  -> job_id:Agent_protocol.Id.Job.t
  -> generation:int
  -> attempt:int
  -> (bool, Agent_protocol.Error.t) result

(** Capture / complete only a running attempt actually admitted by this scope.
    Capability checks and durable mutation share the same actor request. *)
val capture_initialization_recipe_target
  :  t
  -> scope:Initialization_scope.t
  -> job_id:Agent_protocol.Id.Job.t
  -> generation:int
  -> attempt:int
  -> target:Inference.Request.Target.t
  -> limits:Document_schema.Limits.t
  -> (unit, Agent_protocol.Error.t) result

val complete_initialization_model_job
  :  t
  -> scope:Initialization_scope.t
  -> job_id:Agent_protocol.Id.Job.t
  -> generation:int
  -> attempt:int
  -> Runtime_builder.model_job_outcome
  -> (Agent_protocol.Job.t, Agent_protocol.Error.t) result

(** Trusted completion retains the scope's stable basis and requires it active.
    Only initialized history/count/reservations, moderator and shell reviewer
    checkpoints are overlaid. Actual jobs/bindings/schedules, tracking commits
    and shell grants remain authoritative. Marks Ready without a second archive. *)
val complete_initialization
  :  t
  -> scope:Initialization_scope.t
  -> candidate:Session_state.t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

(** Record failed activation under the active scope's stable basis. Retains
    Pending and actual durable effects/configuration; no rollback is claimed. *)
val fail_initialization
  :  t
  -> scope:Initialization_scope.t
  -> Agent_protocol.Error.t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val upgrade_prompt
  :  t
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> expected_revision:int64
  -> target_revision:Agent_protocol.Id.Prompt_revision.t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val upgrade_prompt_with_command_audit
  :  t
  -> command_audit:Document_schema.Document.t
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> expected_revision:int64
  -> target_revision:Agent_protocol.Id.Prompt_revision.t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val start
  :  ?expected_parent_stop_epoch:int64
  -> t
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

(** Consume an admitted generated child's initial start intent, if still pending.
    The trusted host must load and authorize its runtime first. Retries after a
    completed start or explicit stop preserve the current lifecycle. *)
val start_initial_delegated
  :  ?expected_parent_stop_epoch:int64
  -> t
  -> reference:Agent_store.Delegation_store.Reference.t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

(** Persist a failed initial activation without deleting the child. A competing
    start or stop that already consumed the intent takes precedence. *)
val fail_initial_delegated
  :  t
  -> reference:Agent_store.Delegation_store.Reference.t
  -> Agent_protocol.Error.t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

(** [replace_workspace t workspace] durably installs a verified replacement
    workspace while the caller holds the session's administrative boundary. *)
val replace_workspace
  :  t
  -> Workspace_instance.t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val start_with_command_audit
  :  ?expected_parent_stop_epoch:int64
  -> t
  -> command_audit:Document_schema.Document.t
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val queue_start
  :  t
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val queue_start_with_command_audit
  :  t
  -> command_audit:Document_schema.Document.t
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
  -> command_audit:Document_schema.Document.t
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> mode:Agent_protocol.Session.stop_mode
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

(** Stop and record the observed parent counter in one transaction. Already
    reconciled epochs do not stop a subsequently restarted child. [force] is for
    exclusive recovery before publication, including revocation without an epoch
    advance. Callers must use the verified private parent relationship. *)
val stop_delegated_at_epoch
  :  ?force:bool
  -> t
  -> reference:Agent_store.Delegation_store.Reference.t
  -> epoch:int64
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

(** Internal owned-child lifecycle propagation. Atomically matches the child's
    persisted private relationship before using the same durable stop/cancellation
    transition as a writer request. Does not create an attachment, grant approval
    rights, or require the parent to remain running. The host must first establish
    management authority and the applicable lifetime policy from its private
    ledger; a public session ID or model-provided reference is not sufficient.
    Stop acknowledgement is not a resource-cleanup join. *)
val stop_delegated
  :  t
  -> reference:Agent_store.Delegation_store.Reference.t
  -> mode:Agent_protocol.Session.stop_mode
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

(** Atomically admit stop intent and its immutable retry receipt. A matching key
    replays before generation/readiness/capacity checks and never stops again,
    including after explicit restart or administrative replacement. Changed modes
    conflict. The host must authenticate current management authority first.
    Admission is not a join of runtime or descendant cleanup. *)
val stop_managed
  :  t
  -> reference:Agent_store.Delegation_store.Reference.t
  -> key:Agent_protocol.Idempotency_key.t
  -> mode:Agent_protocol.Session.stop_mode
  -> generation:int
  -> max_receipts:int option
  -> (Managed_stop.t, Agent_protocol.Error.t) result

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
  -> ?timing:Agent_protocol.Pending_input.Timing.t
  -> submitting_principal:Agent_protocol.Id.Principal.t
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> Agent_protocol.History.entry
  -> (submission, Agent_protocol.Error.t) result

(** Called by the restoring host before any runtime can execute. Durable old
    live runs become Interrupted with immutable Unconfirmed owner evidence and
    terminal receipts; installation epoch is unchanged and no effect replays. *)
val reconcile_run_recovery : t -> (unit, Agent_protocol.Error.t) result

(** Native action provider for the exact currently held actor event borrow.
    Returns None for unrelated events, and rechecks the current host policy.
    Execution authority belongs to the admitted host scope, independently of
    transport attachment lifetime; initial admission still requires a writer. The owning actor closes the service before releasing the borrow. *)
val run_actions
  :  t
  -> Agent_protocol.Moderator_execution.t
  -> (Run_action_service.t option, Agent_protocol.Error.t) result

(** Captured constructor's staged work, using existing admission/cancellation
    validation plus exact preparation custody in the same mailbox commit. *)
val add_run_constructor_job
  :  t
  -> preparation:Run_preparation.t
  -> Agent_protocol.Job.t
  -> (Agent_protocol.Job.t, Agent_protocol.Error.t) result

val add_run_constructor_schedule
  :  t
  -> preparation:Run_preparation.t
  -> Agent_protocol.Schedule.t
  -> (Agent_protocol.Schedule.t, Agent_protocol.Error.t) result

val cancel_run_constructor_schedule
  :  t
  -> preparation:Run_preparation.t
  -> schedule_id:Agent_protocol.Id.Schedule.t
  -> (Agent_protocol.Schedule.t, Agent_protocol.Error.t) result

(** Attributable constructor/user history reservation; same serialized custody
    check as its durable high-water-mark commit. *)
val reserve_run_history_block
  :  t
  -> preparation:Run_preparation.t
  -> count:int
  -> (History_id_source.reservation, Agent_protocol.Error.t) result

(** Synchronous constructor work carries the exact live admission custody.
    Admission/claim and terminal updates recheck the issuer and current policy;
    no preparation is inferred from the actor's ambient state. *)
val start_run_constructor_model_job
  :  t
  -> preparation:Run_preparation.t
  -> Agent_protocol.Job.t
  -> (Agent_protocol.Job.t, Agent_protocol.Error.t) result

val complete_run_constructor_model_job
  :  t
  -> preparation:Run_preparation.t
  -> job_id:Agent_protocol.Id.Job.t
  -> generation:int
  -> attempt:int
  -> Runtime_builder.model_job_outcome
  -> (Agent_protocol.Job.t, Agent_protocol.Error.t) result

(** Captures only the exact constructor model job's source/recipe selection;
    actual generation/attempt and token custody are checked before persistence. *)
val capture_run_constructor_model_source
  :  t
  -> preparation:Run_preparation.t
  -> job_id:Agent_protocol.Id.Job.t
  -> generation:int
  -> target:Inference.Request.Target.t
  -> limits:Document_schema.Limits.t
  -> (unit, Agent_protocol.Error.t) result

val capture_run_constructor_recipe_target
  :  t
  -> preparation:Run_preparation.t
  -> job_id:Agent_protocol.Id.Job.t
  -> generation:int
  -> attempt:int
  -> target:Inference.Request.Target.t
  -> limits:Document_schema.Limits.t
  -> (unit, Agent_protocol.Error.t) result

val interrupt_run_constructor_job
  :  t
  -> preparation:Run_preparation.t
  -> job_id:Agent_protocol.Id.Job.t
  -> generation:int
  -> attempt:int
  -> reason:string
  -> (Agent_protocol.Job.t, Agent_protocol.Error.t) result

(** Captured runtime's moderator install in the preparation's acknowledged
    constructor transaction. Matching checkpoints produce no durable advance. *)
val checkpoint_run_moderator
  :  t
  -> preparation:Run_preparation.t
  -> Jsonaf.t option
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

(** Install the captured runtime's accounting policy under constructor custody.
    Matching policies do not advance the durable revision. *)
val enable_run_constructor_turn_budget
  :  t
  -> preparation:Run_preparation.t
  -> Chat_response.Runtime_semantics.policy
  -> (unit, Agent_protocol.Error.t) result

(** Final checked admission consumes original input from the actual issuing
    preparation. Always releases custody. Distinguishes definite no-run rejection
    from an attempted uncertain final persistence; exceptions/cancellation retain
    their primary backtrace and still close custody. *)
val admit_prepared_run
  :  t
  -> command_audit:Document_schema.Document.t option
  -> preparation:Run_preparation.t
  -> scope:Run_admission.Scope.t
  -> session:Agent_protocol.Session_ref.t
  -> entry:Agent_protocol.History.entry option
  -> (Run_admission_outcome.t, Agent_protocol.Error.t) result

(** Issue exclusive ephemeral custody after original CAS/current policy/writer
    checks. Receipt retries allocate nothing. Every host exit must end custody. *)
val begin_run_preparation
  :  t
  -> authorize:(Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> principal_id:Agent_protocol.Id.Principal.t
  -> request:Agent_protocol.Run_start.t
  -> request_sha256:string
  -> (Run_preparation.Decision.t, Agent_protocol.Error.t) result

val end_run_preparation : t -> Run_preparation.t -> (unit, Agent_protocol.Error.t) result

(** Current host policy, target and writer are checked in one mailbox read before
    receipt lookup. [authorize] is host-owned and non-yielding. No runtime load,
    history allocation or authority reconstruction occurs on this path. *)
val run_start_receipt
  :  t
  -> authorize:(Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> principal_id:Agent_protocol.Id.Principal.t
  -> request:Agent_protocol.Run_start.t
  -> request_sha256:string
  -> (Agent_protocol.Run_receipt.t option, Agent_protocol.Error.t) result

(** Internal host-authenticated run admission. The host prepares [entry] from
    exactly the request's user input through its existing runtime parser, or None
    for the actual pending authored startup. The scope rechecks current host
    authority in the actor. Current live writer visibility precedes receipt retry;
    an old receipt never creates a new execution scope. New admission persists
    run/receipt with the actual history+operation or startup checkpoint before
    existing execution starts. Busy user input is rejected rather than deferred
    without a bound owner. Persistence uncertainty is reconciled by the same key. *)
val admit_run
  :  t
  -> scope:Run_admission.Scope.t
  -> request:Agent_protocol.Run_start.t
  -> session:Agent_protocol.Session_ref.t
  -> request_sha256:string
  -> entry:Agent_protocol.History.entry option
  -> (Agent_protocol.Run_receipt.t, Agent_protocol.Error.t) result

(** Same atomic admission with the existing command-audit persistence carrier. *)
val admit_run_with_command_audit
  :  t
  -> command_audit:Document_schema.Document.t
  -> scope:Run_admission.Scope.t
  -> request:Agent_protocol.Run_start.t
  -> session:Agent_protocol.Session_ref.t
  -> request_sha256:string
  -> entry:Agent_protocol.History.entry option
  -> (Agent_protocol.Run_receipt.t, Agent_protocol.Error.t) result

val submit_message_with_command_audit
  :  t
  -> ?timing:Agent_protocol.Pending_input.Timing.t
  -> command_audit:Document_schema.Document.t
  -> submitting_principal:Agent_protocol.Id.Principal.t
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> Agent_protocol.History.entry
  -> (submission, Agent_protocol.Error.t) result

(** Internal authenticated management admission. The host must validate the
    calling parent's current relationship/authority first. Atomically binds the
    exact child reference, request key/hash and accepted input to a durable receipt.
    Identical retries return the retained receipt, even after a later stop/reset;
    changed input conflicts. New input preserves normal stopped/failed rejection.
    [max_receipts=None] is an explicit trusted-host choice without a count ceiling.
    This does not create a client attachment or grant approval rights. *)
val submit_managed_message
  :  t
  -> reference:Agent_store.Delegation_store.Reference.t
  -> key:Agent_protocol.Idempotency_key.t
  -> request_sha256:string
  -> generation:int
  -> max_receipts:int option
  -> Agent_protocol.History.entry
  -> (Managed_submission.t, Agent_protocol.Error.t) result

(** [delete_history t ... id] removes one canonical occurrence and its matching
    tool call/result occurrence. Requires an idle/stopped writable session and
    an exact revision; rejects foreground and borrowed moderator publication owners.
    Independent background jobs remain live and publish against current state.
    Archives the exact prior admitted document as [Delete] before committing;
    archive or journal failures publish no history change. Commits before broadcast. *)
val delete_history
  :  t
  -> ?command_audit:Document_schema.Document.t
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> expected_revision:int64
  -> Agent_protocol.History.Id.t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

(** Serialized cancel/edit/adopt arbitration. Current authorized host principal
    and its pure public projection are required; original submitting ownership
    is checked again against the admitted durable wrapper/disposition. *)
val cancel_pending
  :  t
  -> ?command_audit:Document_schema.Document.t
  -> principal:Agent_protocol.Id.Principal.t
  -> project:Pending_inspection.projection
  -> Agent_protocol.Pending_control.Cancel_request.t
  -> (Agent_protocol.Pending_control.Result.t, Agent_protocol.Error.t) result

val replace_pending
  :  t
  -> ?command_audit:Document_schema.Document.t
  -> principal:Agent_protocol.Id.Principal.t
  -> project:Pending_inspection.projection
  -> Agent_protocol.Pending_control.Replace_request.t
  -> (Agent_protocol.Pending_control.Result.t, Agent_protocol.Error.t) result

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
  -> command_audit:Document_schema.Document.t
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

(** Commit a disclosure-checked proposal at this foreground operation's input
    boundary. Saves delivery/history atomically and retains consumed wake IDs for
    the next actual model admission or terminal disposition. *)
val consume_notifications
  :  t
  -> operation_id:Agent_protocol.Id.Operation.t
  -> Notification_delivery.t
  -> ( Chat_response.In_memory_stream.Safe_point_input.batch
       , Agent_protocol.Error.t )
       result

(** Commit eligible idle data and settle its requested wake with actual operation
    admission, or with budget rejection. Existing pending wake receipts never
    insert a second history entry. Stop/compaction intent takes precedence. *)
val deliver_idle_notifications
  :  t
  -> Notification_delivery.idle
  -> (bool, Agent_protocol.Error.t) result

(** Persist a privately prepared standalone completion intent after checking the
    current actor snapshot, owned job and shared notification quotas. This does
    not insert history or wake a model; stop does not erase retained results. *)
val admit_standalone_delivery
  :  t
  -> Standalone_delivery.t
  -> (unit, Agent_protocol.Error.t) result

(** Under the host's moderator gate, recheck the exact pending terminal job and
    current state revision. Retire delivery if its captured moderator source was
    replaced or removed; return [true] after durable retirement. [false] leaves the
    matching source eligible for normal event delivery. Missing provenance, stale
    requests and save failures do not retire work. No result artifact is loaded. *)
val retire_obsolete_moderator_delivery
  :  t
  -> revision:int64
  -> job:Agent_protocol.Job.t
  -> (bool, Agent_protocol.Error.t) result

(** Host adapter path under a pinned runtime lease. Recheck revision, actual owner,
    job and current authority before bounded artifact loading. Save the checked
    intent with its job's delivered marker atomically; publish only at a safe point.
    If current publisher or dependency authority is unavailable, durably discard
    the pending delivery without loading its artifact or changing its result.
    Stale requests and other failures leave pending delivery retryable. *)
val deliver_standalone_completion
  :  t
  -> revision:int64
  -> job:Agent_protocol.Job.t
  -> current_capabilities:Chat_response.Tool_capability.t
  -> policy:Chat_response.One_off_request.policy
  -> (unit, Agent_protocol.Error.t) result

(** Before the first provider call, claim eligible restored wakes for this
    already-started operation and insert ready new data. The worker appends only
    returned new entries to its input snapshot. Actual before-model admission
    settles the claims; terminal cleanup discards any unadmitted requests. *)
val consume_initial_notifications
  :  t
  -> operation_id:Agent_protocol.Id.Operation.t
  -> Notification_delivery.idle
  -> ( Chat_response.In_memory_stream.Safe_point_input.batch
       , Agent_protocol.Error.t )
       result

val cancel_operation
  :  t
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> operation_id:Agent_protocol.Id.Operation.t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val cancel_operation_with_command_audit
  :  t
  -> command_audit:Document_schema.Document.t
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> operation_id:Agent_protocol.Id.Operation.t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

(** Persist an approval request before waiting outside the actor. Invocation-owned
    requests require a live actor-dispatched callback, including idle event and
    observation native calls. Their owner is immutable, stop/callback cancellation
    resolves pending requests, and a completed callback cannot open a new request.
    Legacy operation-owned requests retain their existing host behavior. *)
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

(** Read the actor's current unexpired generic invocation grants, using the same
    exact/prefix matching as foreground worker authorization. *)
val invocation_granted
  :  t
  -> tool_name:string
  -> identity_digest:string
  -> (bool, Agent_protocol.Error.t) result

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
  -> command_audit:Document_schema.Document.t
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
  -> command_audit:Document_schema.Document.t
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> grant_id:Agent_protocol.Id.Grant.t
  -> reason:string
  -> (Agent_protocol.Grant.t * Agent_protocol.Session.t, Agent_protocol.Error.t) result

val change_job
  :  t
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> Agent_protocol.Job.t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

(** Trusted host selection capture. The caller resolves explicit source or
    migration policy first. Successful return acknowledges the complete durable
    selection before runtime activation; no backend lookup or authority grant. *)
val capture_inference_target
  :  t
  -> target:Inference.Request.Target.t
  -> limits:Document_schema.Limits.t
  -> (unit, Agent_protocol.Error.t) Result.t

(** Migration capture uses an explicitly approved target for this exact retained
    job. It never derives from the parent's current selection. *)
val capture_model_job_source
  :  t
  -> job_id:Agent_protocol.Id.Job.t
  -> generation:int
  -> target:Inference.Request.Target.t
  -> limits:Document_schema.Limits.t
  -> (unit, Agent_protocol.Error.t) Result.t

(** Persist the root recipe's effective target before its first model/moderator
    effect. Only the exact running attempt may capture; repetition must retain
    the complete same target, including future fields and presence. *)
val capture_recipe_target
  :  t
  -> job_id:Agent_protocol.Id.Job.t
  -> generation:int
  -> attempt:int
  -> target:Inference.Request.Target.t
  -> limits:Document_schema.Limits.t
  -> (unit, Agent_protocol.Error.t) Result.t

val add_job
  :  t
  -> Agent_protocol.Job.t
  -> (Agent_protocol.Job.t, Agent_protocol.Error.t) result

val claim_job
  :  t
  -> job_id:Agent_protocol.Id.Job.t
  -> generation:int
  -> (Agent_protocol.Job.t option, Agent_protocol.Error.t) result

(** Complete only the exact claimed attempt. Late callbacks from an older retry
    cannot finish or mutate the newer attempt, including its delivery state. *)
val complete_job
  :  t
  -> job_id:Agent_protocol.Id.Job.t
  -> generation:int
  -> attempt:int
  -> Runtime_builder.model_job_outcome
  -> (Agent_protocol.Job.t, Agent_protocol.Error.t) result

(** Complete an Async_tool job's exact claimed attempt after its invocation scope
    releases. Retries cleanup of an inactive scope when its previous cleanup save
    failed; active callbacks remain ineligible. Persists the full [Completion]
    envelope in [Job.result], with a matching success/failure/cancel status;
    expiration is a resource-limit failure.
    Retries require both an explicitly configured retry policy and a retryable
    tool failure. A worker cannot complete an attempt after it enters a durable
    dependency wait; only dependency reconciliation may finish that wait.
    Legacy model-job result encoding is unchanged. *)
val complete_background_job
  :  t
  -> job_id:Agent_protocol.Id.Job.t
  -> generation:int
  -> attempt:int
  -> Agent_protocol.Completion.t
  -> (Agent_protocol.Job.t, Agent_protocol.Error.t) result

(** Release a completed worker into a durable wait on its actual target's Pending
    job. Revalidates target ownership, generation, attempt and deadline. *)
val defer_background_job
  :  t
  -> job_id:Agent_protocol.Id.Job.t
  -> generation:int
  -> attempt:int
  -> Agent_protocol.Job.dependency
  -> (Agent_protocol.Job.t, Agent_protocol.Error.t) result

(** Reconcile a persisted dependency from saved terminal data or its deadline.
    Never reruns work. Validates the captured completion schema/result budget and
    preserves the original deadline. Terminal dependencies do not trigger parent
    auto-retries. Expiry cancels unfinished owned dependencies atomically. *)
val refresh_background_job
  :  t
  -> job_id:Agent_protocol.Id.Job.t
  -> generation:int
  -> attempt:int
  -> (Agent_protocol.Job.t, Agent_protocol.Error.t) result

(** Recover verified private result preparations before startup interruption.
    Requires an idle execution host. Uses current generation/attempt ownership and
    normal completion persistence without replaying a tool. Bounds the number of
    scanned intents and their aggregate selected payload size. *)
val recover_background_results
  :  t
  -> max_count:int
  -> max_total_bytes:int
  -> (unit, Agent_protocol.Error.t) result

(** Enqueues an exact retained terminal attempt through the existing moderator
    checkpoint. Rechecks actual Pending custody, source and current run authority;
    denial retires that run truthfully before returning the original denial.
    Saves Enqueued and the exact queue append in one durable transaction. *)
val enqueue_run_job_delivery
  :  t
  -> delivery:Run_job_delivery.t
  -> before:Session.Moderator_state.Identity_snapshot.t
  -> after:Jsonaf.t
  -> (unit, Agent_protocol.Error.t) result

(** Optional expected values perform checkpoint and complete job-record comparison
    in the same mailbox transaction as delivery. Scheduler ingress supplies both. *)
val deliver_job
  :  ?expected:Session.Moderator_state.Identity_snapshot.t
  -> ?expected_job:Agent_protocol.Job.t
  -> t
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
  -> ?command_audit:Document_schema.Document.t
  -> ?expected_generation:int
  -> ?expected_attempt:int
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
  -> command_audit:Document_schema.Document.t
  -> job_id:Agent_protocol.Id.Job.t
  -> (Agent_protocol.Job.t, Agent_protocol.Error.t) result

(** Interrupt only the exact claimed attempt; stale cancellation/cleanup must not
    interrupt a newer retry. Recovered workers use the persisted attempt. *)
val interrupt_job
  :  t
  -> job_id:Agent_protocol.Id.Job.t
  -> generation:int
  -> attempt:int
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
  -> command_audit:Document_schema.Document.t
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

(** Atomically checks a supplied occurrence before cancelling. Omission retains
    the legacy cancel-current-ID contract. *)
val cancel_schedule
  :  t
  -> ?command_audit:Document_schema.Document.t
  -> ?expected_generation:int
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> schedule_id:Agent_protocol.Id.Schedule.t
  -> unit
  -> (Agent_protocol.Schedule.t, Agent_protocol.Error.t) result

val cancel_schedule_internal
  :  t
  -> schedule_id:Agent_protocol.Id.Schedule.t
  -> (Agent_protocol.Schedule.t, Agent_protocol.Error.t) result

(** Scheduler-only compare-and-set transitions. They reject stale session
    generations and never require a client attachment. *)
val due_schedules
  :  t
  -> ( Agent_protocol.Session.observed_state * Agent_protocol.Schedule.t list
       , Agent_protocol.Error.t )
       result

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

(** Optional expected values compare the old checkpoint and captured schedule
    before saving delivery plus the new moderator checkpoint atomically. *)
val complete_schedule
  :  ?expected:Session.Moderator_state.Identity_snapshot.t
  -> ?expected_schedule:Agent_protocol.Schedule.t
  -> t
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

(** Claim the head of an exact installed checkpoint while idle. The callback runs
    outside the mailbox under exclusive moderator ownership and receives the
    selected event snapshot for comparison in the manager's [authorize] callback.
    The claim is durable before script execution. [commit] atomically persists
    the prospective checkpoint, completed receipt and retained runtime requests;
    ownership remains held until the callback returns after local installation.

    Failure or cancellation retains terminal evidence without changing the old
    checkpoint. Unsettled failed/interrupted claims block further queued execution
    for that source/generation, including after unrelated checkpoint edits. This
    handoff does not retire failed heads. A failed terminal save retains the borrow
    until recovery.
    Stop-cancel interrupts the callback; late or escaped commits are rejected.
    Returns false while the session is unavailable; empty, stale or previously
    claimed checkpoints return an error. This internal handoff grants no native
    tool authority and does not apply scheduling intent or start a model turn. *)
val with_idle_queued_moderator_event
  :  t
  -> snapshot:Session.Moderator_state.Identity_snapshot.t
  -> (event:Session.Snapshot.t
      -> commit:
           (snapshot:Session.Moderator_state.Identity_snapshot.t
            -> requests:Agent_protocol.Invocation.follow_up
            -> (unit, Agent_protocol.Error.t) result)
      -> (unit, Agent_protocol.Error.t) result)
  -> (bool, Agent_protocol.Error.t) result

(** Queued-event handoff with an actor-owned native executor. Direct children
    of this Running event require matching source observation intent. Script
    descendants require a still-active parent owned by this exact event borrow,
    the same session/generation and no wider deadline. Any observer must match the
    event source; logical Script nodes may omit observation intent.
    Route the executor through [Native_tool_invocation.run_scoped] for capability,
    policy and disclosure checks. Commit waits for all recorded child outcomes;
    failed outcome saves are cancelled at callback cleanup. Scope expires on
    callback return, commit or stop; no foreground/history authority is granted. *)
val with_idle_queued_moderator_event_tools
  :  t
  -> snapshot:Session.Moderator_state.Identity_snapshot.t
  -> (executing:Agent_protocol.Moderator_execution.t
      -> retirement_reason:string option
      -> event:Session.Snapshot.t
      -> execute:Native_tool_invocation.executor
      -> commit:
           (snapshot:Session.Moderator_state.Identity_snapshot.t
            -> requests:Agent_protocol.Invocation.follow_up
            -> (unit, Agent_protocol.Error.t) result)
      -> (unit, Agent_protocol.Error.t) result)
  -> (bool, Agent_protocol.Error.t) result

(** Ordinary v1 lifecycle or foreground boundary event under the shared event
    owner. None operation ownership is limited to startup/resume on a running idle
    session. Other phases require the actual active operation. Claim is saved
    before execution; checkpoint, outcome and request intent commit atomically.
    The handler retains the actor borrow through infallible manager installation,
    and native calls use the exact event-owned executor. No event queue head is
    consumed. Failure retains evidence without replaying external effects. *)
val with_ordinary_moderator_event
  :  t
  -> operation_id:Agent_protocol.Id.Operation.t option
  -> snapshot:Session.Moderator_state.Identity_snapshot.t
  -> event:Chat_response.Moderation.Event.t
  -> (executing:Agent_protocol.Moderator_execution.t
      -> retirement_reason:string option
      -> event:Session.Snapshot.t
      -> execute:Native_tool_invocation.executor
      -> commit:
           (snapshot:Session.Moderator_state.Identity_snapshot.t
            -> requests:Agent_protocol.Invocation.follow_up
            -> (unit, Agent_protocol.Error.t) result)
      -> (unit, Agent_protocol.Error.t) result)
  -> (bool, Agent_protocol.Error.t) result

(** Read the manager checkpoint after acquiring exclusive moderator ownership,
    outside the actor mailbox. This prevents another concurrent tool from changing
    the checkpoint between its read and event admission. The callback must only
    read the manager snapshot. Fixed-snapshot variants above remain available for
    explicit checkpoint assertions. *)
val with_current_moderator_event
  :  t
  -> operation_id:Agent_protocol.Id.Operation.t option
  -> event:Chat_response.Moderation.Event.t
  -> Moderator_event.claim

val with_current_idle_queued_moderator_event_tools : t -> Moderator_event.claim

(** Trusted host handoff for a parent's live delegated policy check. [authorize]
    must validate the private relation and owned child invocation candidate after entering
    the parent gate. The handoff rechecks before entering [f], before and after
    native admission, before decision commit and before disclosing native outcomes
    or saved decisions. A denied disclosure retains the already committed outcome
    and checkpoint. [authorize] must be repeatable and must not run the policy
    handler itself. Native implementations must still check after their own
    internal approval waits; this wrapper cannot intercept arbitrary host code.
    The actor persists claim before executing [f]; [commit] saves parent snapshot,
    runtime intent, decision and [notifications] atomically and retains ownership through callback
    return. Native calls are parent event-owned and must use the normal scoped
    capability/policy dispatcher. No child invocation is fabricated in this actor.

    An identical completed retry returns its receipt without calling [f]. Failed
    or interrupted handlers never replay. Stop-cancel interrupts active callbacks;
    late commits and escaped executors reject. None means parent unavailable.
    An unrelated foreground completion preserves this borrow and its requests;
    a foreground end-session outcome cancels it.
    Factory mediation remains responsible for validating and enforcing the child's
    decision. Completed retries do not publish notifications again. *)
val with_delegated_moderator_event
  :  ?notifications:(unit -> string list)
  -> t
  -> delegation:Agent_protocol.Moderator_execution.delegation
  -> event:Chat_response.Moderation.Event.t
  -> authorize:(unit -> (unit, Agent_protocol.Error.t) result)
  -> snapshot:
       (unit
        -> (Session.Moderator_state.Identity_snapshot.t, Agent_protocol.Error.t) result)
  -> (executing:Agent_protocol.Moderator_execution.t
      -> event:Session.Snapshot.t
      -> execute:Native_tool_invocation.executor
      -> commit:
           (decision:Agent_protocol.Moderator_execution.Decision.t
            -> snapshot:Session.Moderator_state.Identity_snapshot.t
            -> requests:Agent_protocol.Invocation.follow_up
            -> (unit, Agent_protocol.Error.t) result)
      -> (unit, Agent_protocol.Error.t) result)
  -> (Agent_protocol.Moderator_execution.t option, Agent_protocol.Error.t) result

(** Serialize external checkpoint preparation with owned moderator execution.
    This does not run a handler or grant tool authority. Delivery commits must
    also compare the prepared [expected] checkpoint in the actor mailbox. *)
val with_moderator_checkpoint
  :  t
  -> (unit -> ('a, Agent_protocol.Error.t) result)
  -> ('a, Agent_protocol.Error.t) result

(** Explicitly retire a retained failed/interrupted queue head at its original
    checkpoint. Available while quiescent running-idle or stopped, without an
    active callback/permission. The callback prepares a manager queue-only change;
    [commit] saves retirement and the new checkpoint atomically. It holds exclusive
    ownership through local installation, preserves the original failure, and
    grants no execution or scheduling authority. Uncommitted retirement releases
    ownership without changing the receipt/queue and can be retried. A changed
    checkpoint or already retired receipt is rejected. No handler is rerun. *)
val with_queued_moderator_retirement
  :  t
  -> id:Agent_protocol.Id.Moderator_execution.t
  -> snapshot:Session.Moderator_state.Identity_snapshot.t
  -> reason:string
  -> (event:Session.Snapshot.t
      -> commit:
           (snapshot:Session.Moderator_state.Identity_snapshot.t
            -> (unit, Agent_protocol.Error.t) result)
      -> (unit, Agent_protocol.Error.t) result)
  -> (bool, Agent_protocol.Error.t) result

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
    borrow. Direct Moderator children of the observing invocation require the same
    source-bound observation intent and session generation. Script descendants
    require an active parent owned by this exact borrow, the same generation,
    a non-widening deadline and compatible observation intent. Logical Script
    nodes may omit observations; they cannot gain provider/event/job identities.
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

(** At an idle safe point, coalesce retained event and observation requests into
    one action and atomically accept them
    with scheduling/stop. A compaction-plus-turn request retains its turn until
    the next idle safe point. Stop discards outstanding requests. A user message
    deferred during an idle callback can start a user turn even without explicit
    moderator intent; adoption and admission commit together. Returns whether
    receipts changed or deferred user work was resumed; false while unavailable
    or no work remains. No observer
    executes here. The host must install the worker before admitting a turn. *)
val apply_moderator_follow_up : t -> (bool, Agent_protocol.Error.t) result

(** Compatibility name for [apply_moderator_follow_up]; also consumes event intent. *)
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
  -> command_audit:Document_schema.Document.t
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
  -> command_audit:Document_schema.Document.t
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
  -> command_audit:Document_schema.Document.t
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

(** Invoke [f] with authoritative state only when no foreground operation,
    invocation/job execution, moderator borrow, staged launch or active call
    retains process-local execution ownership. Serialized with actor transitions;
    [None] means defer. The callback must not reenter this actor. *)
val with_quiescent_state
  :  t
  -> f:(Session_state.t -> ('a, Agent_protocol.Error.t) result)
  -> ('a option, Agent_protocol.Error.t) result

(** Organization-only edit; accepts streaming and stopped sessions without activation. *)
val update_metadata
  :  t
  -> ?command_audit:Document_schema.Document.t
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> expected_metadata_revision:int64
  -> patch:Agent_protocol.Session_metadata.Patch.t
  -> unit
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

(** Host-only authorization/resolver ports used outside the actor mailbox for root configuration. *)
val set_configuration_policy
  :  t
  -> Configuration_policy.t
  -> (unit, Agent_protocol.Error.t) Result.t

val configuration
  :  t
  -> (Agent_protocol.Session_configuration.t, Agent_protocol.Error.t) Result.t

(** Writer-authorized atomic selection/receipt update; active captured work stays pinned. *)
val update_configuration
  :  t
  -> ?command_audit:Document_schema.Document.t
  -> Agent_protocol.Session_configuration.Update_request.t
  -> (Agent_protocol.Session_configuration.t, Agent_protocol.Error.t) Result.t

(** Trusted host composition only. Borrow the root-owned group admission authority;
    no permissive default exists. Must be installed for loaded and stopped actors. *)
val set_organization_admission
  :  t
  -> Session_organization_admission.t
  -> (unit, Agent_protocol.Error.t) result

(** Host caller authenticates principal and binds connection attachment before
    enqueue/replay. Actor repeats current writer/mode/lease validation. Group admission
    serializes only persistence; acknowledged install and post-unlock publication are protected across org unlock,
    event/index callbacks follow unlock. Does not activate runtime. *)
val update_organization
  :  t
  -> ?command_audit:Document_schema.Document.t
  -> principal:Agent_protocol.Principal.t
  -> Agent_protocol.Session_organization.Request.t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

(** The accepted edit and optional Starting Turn are one durable transition.
    Save_only invokes no worker/preparation/recovery port. Publication precedes
    worker launch, within the command's protected commit handoff. Started means
    admitted host operation, not provider dispatch; owner interruption is recovered
    through existing operation/receipt reconciliation and never rolled back. *)

val edit_history
  :  t
  -> ?command_audit:Document_schema.Document.t
  -> Agent_protocol.History_edit.Edit_request.t
  -> (History_edit_result.t, Agent_protocol.Error.t) result

val continue_history
  :  t
  -> ?command_audit:Document_schema.Document.t
  -> Agent_protocol.History_edit.Continue_request.t
  -> (History_continuation_result.t, Agent_protocol.Error.t) result

module Lifecycle_fence : sig
  type t
end

(** One mailbox turn validates the current writer attachment (when supplied),
    expected generation and canonical revision before excluding fresh execution,
    runtime construction and user canonical mutations. Worker/inference/background
    finalizers remain admitted so external runtime retirement can join. A rejected
    attempt does not install a fence or retire the runtime. *)
val begin_lifecycle
  :  t
  -> attachment_id:Agent_protocol.Id.Attachment.t option
  -> expected_generation:int
  -> expected_revision:int64
  -> (Lifecycle_fence.t, Agent_protocol.Error.t) Result.t

(** Read the final canonical state after Runtime_owner.close_and_wait has joined;
    validates issuing actor/current fence. This permits honest tracking-ACK drift
    during retirement; original user anchor is validated at begin_lifecycle. *)
val lifecycle_state
  :  t
  -> Lifecycle_fence.t
  -> (Session_state.t, Agent_protocol.Error.t) Result.t

(** Release only after proven absence of lifecycle effects AND before permanent
    Runtime_owner retirement has begun. Once runtime close starts, host detaches
    and closes this actor even when publication fails without effects, retaining
    the unchanged durable index for a fresh owner. Installed archive or
    removal and uncertain publication retain the fence until actor closure.
    Restored sessions are not released into execution: they are retired and
    subsequently inspected through the retained reader. Explicit resume commits
    its gate through indexed ownership before a fresh activating load. *)
val abort_lifecycle : t -> Lifecycle_fence.t -> (unit, Agent_protocol.Error.t) Result.t

(** Mark irreversible runtime retirement before closing its owner. The fence
    cannot subsequently be aborted, including after cancellation or no-effect
    publication failure. The host must retire the actor/store or retain this
    fenced owner for an actual cleanup retry. *)
val retire_lifecycle : t -> Lifecycle_fence.t -> (unit, Agent_protocol.Error.t) Result.t

(** Current mailbox preflight before runtime construction; does not grant a
    future execution lifetime. Actual owner/scope installation must revalidate
    under the same admission rules after any waits. *)
val check_runtime_admission : t -> (unit, Agent_protocol.Error.t) Result.t
