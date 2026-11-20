open! Core

(** Single-owner session actor shared by embedded and daemon execution.

    Requests return typed domain failures. An unexpected owner exception or
    cancellation instead terminates the actor and is re-raised by its in-flight
    caller with the original backtrace; it also propagates to the owning switch.
    Queued and later calls return Server_shutting_down. The owner closes its
    mailbox and retires process-local execution proofs before releasing callers.
    A failure after persistence may already have committed the request; an
    exceptional reply neither rolls it back nor permits automatic replay. *)

module Canonical_persistence : sig
  (** SAME concrete backend and physical commit closure, issued together by
      Session_persistence.actor_persistence_with_startup_disposal. No public
      constructor, custody success, writer retry or execution authority. *)
  type t = Canonical_backend_binding_data.t
end

type persistence =
  { commit :
      ?observation:Session_commit_observation.t
      -> command_audit:Document_schema.Document.t option
      -> previous:Session_state.t
      -> Session_transition.t
      -> (unit, Agent_protocol.Error.t) result
    (** Forward [observation] to canonical persistence when supplied. A host that
        ignores it retains the existing commit-success contract, but its failure
        is an unknown write outcome and cannot authorize a cleanup retry. A host
        rejecting before its backend call may record that exact no-writer fact
        through [Session_commit_observation.Host.observe_entry]. Only the real
        canonical writer belongs inside [Host.with_writer]; in-memory success
        must not manufacture a journal acknowledgement. *)
  ; archive_reference :
      previous:Session_state.t
      -> kind:Session_state.Compaction_archive.kind
      -> Agent_protocol.Id.Operation.t
      -> (Session_state.Compaction_archive.t, Agent_protocol.Error.t) result
  }

type services =
  { now : unit -> Agent_protocol.Timestamp.t
    (** Trusted NONYIELDING wall-clock sampler, also used by the final provider
        submission check after all composed refreshes. Implementations must not
        suspend; this host contract is not mechanically enforced. Unexpected
        clock exceptions propagate. Final original/physical checks follow it. *)
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
  ; goal_descendant_profile : Goal_descendants.Profile.t
    (** Trusted immutable host profile captured from actual delegation depth,
        child-count and child-byte bounds. Used only for genuinely new Goal
        roots; persisted accounting keeps its captured profile. *)
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
      inference and worker finalizers until the terminal checkpoint finishes.
      The actual terminal result is retained even when its worker returned before
      retirement began. This join reports that result; an error does not imply a
      persisted terminal Operation or successful resource closure. *)
  type t

  val is_finished : t -> bool

  (** Pure nonwaiting observation of this SAME actual foreground terminal
      checkpoint. True only after successful completion; pending, typed errors,
      and escaping exceptions are false. Does not consume the result, certify
      resource closure, or grant execution. Safe for an Owner's coverage check. *)
  val is_successful : t -> bool

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

module Retirement_cleanup : sig
  type actor = t
  type t

  module Failure : sig
    type t =
      | Protocol of Agent_protocol.Error.t
      | Raised of exn * Stdlib.Printexc.raw_backtrace
  end

  module Snapshot : sig
    type t

    type progress =
      | Pending
      | Retryable
      | Terminal
      | Joined
    [@@deriving equal, sexp_of]

    (** Next permissible action, not global recoverability. Retryable if any
        captured exact operation is permitted, even alongside immutable failures;
        otherwise Pending while an original selected producer/dependency is
        unfinished; otherwise Terminal for known immutable negatives; Joined only
        at the final selected-parent cutoff and exact retained-cell observation.
        Empty current fields, early factual Settled or released bookkeeping alone
        never establish Joined. Unrelated surviving custody is outside selection. *)
    val progress : t -> progress

    (** Every actual current negative of captured duties, in original duty order.
        Does not turn Deferred/Pending into an invented failure. Immutable original
        snapshots retain their exact exception/aggregate/rawtrace forever. *)
    val failures : t -> Failure.t list
  end

  (** SAME actual foreground join, not another cancellation/worker owner. *)
  val retirement : t -> Runtime_retirement.t

  (** Synchronous domain-checked mutable-holder observation; does not drive work.
      Snapshot projections themselves inspect immutable values only. *)
  val snapshot : t -> (Snapshot.t, Agent_protocol.Error.t) result

  (** External operation, outside mailbox/Owner locks. [after] must be this
      holder's exact latest snapshot. Reject wrong Actor/domain/stale/concurrent
      calls before effects; reserve same-domain holder before first yield. Drive
      only selected objects through existing exact operations/schedulers, no
      arbitrary callback, new Begin/Init/admission/provider work or current sweep.

      First advance keeps existing constructor -> callback -> idle/descendant ->
      factual order and may be provisionally Pending with no failures. Owner then
      awaits SAME foreground join in its existing position and advances again.
      Actual selected promise/producer/exit/finish/ACK objects determine cutoff;
      caller cannot assert parent completion. Final advance refreshes SAME cells
      and actual initial dependencies before Joined. Non-factual positive duties
      never replay; factual positives stay provisional until this cutoff.

      Admission reserves the holder before yielding. Actual admitted cleanup is
      protected: caller cancellation is deferred until that cleanup finishes.
      Live callback/constructor/idle/foreground producers return Pending here;
      advance never waits for their lexical exit or foreground join. Actual
      existing cleanup/canonical work can await its real owned IO/ACKs.

      Actual failures are snapshot DATA; Actor/domain/stale/concurrent admission refusal
      is outer Error. A
      failed prefix leaves its originally selected tail Deferred. A later explicit
      advance resumes permitted exact failed/deferred legs only. Released callback
      with pending suffix stays Pending; completed postrelease failure is Terminal
      and never replays writer/recurrence/publication. Cancelled observation does
      not overwrite actual producer report. Completed snapshots never mutate. *)
  val advance
    :  t
    -> actor:actor
    -> after:Snapshot.t
    -> (Snapshot.t, Agent_protocol.Error.t) result
end

(** Atomic selected capture at the SAME actual retirement election, BEFORE
    fallible before_unload/background joins can strand the original Actor tail.
    Retain original Operation/completion and selected actual objects before actor
    cancel/clear. Live source callbacks are selected through ORIGINAL Actor-owned
    physical initialization/lifecycle parents with same issuer/domain and no Job
    scope; foreground callbacks use original Operation association separately.
    No caller-supplied source identity or authority/basis refresh is needed.

    Uses the existing actual runtime retirement and preserves its cancellation,
    strengthening and foreground result semantics. Existing retire_runtime_worker
    remains compatible and EVERY old/new issuance retains original raw selection.
    Duplicate same election returns SAME capture, never selects current fields or
    rebinds a replacement parent. No public prewriter selector/test observer/global
    enrollment seal. *)
val retire_runtime_worker_with_cleanup
  :  t
  -> closing:bool
  -> (Retirement_cleanup.t, Agent_protocol.Error.t) result

(** Outside the Actor mailbox and ownership-state/registry locks, request
    cancellation on this Actor's SAME retained native ACK and join its existing
    bounded factual cleanup, then retry an already sealed prewriter Slot.
    Preserves positive partial progress on error/cancellation. Does not infer a
    no-writer outcome for Active or attempted-unknown Slots; lifecycle fencing
    rejects every outstanding Slot/ACK. Source owners call this before cancelling
    the Actor switch or closing the Store, after cutting fresh runtime admission. *)
val drain_descendant_cleanup : t -> (unit, Agent_protocol.Error.t) result

(** Explicit factual retirement of this Actor's already ended queued callback.
    Call outside runtime/registry mutexes after the real callback exit joins and
    before worker replacement. Reuses its frozen terminal/Run/Init facts, never
    executes the callback again or renews its admission. Proven prewriter failure
    may reprepare; an entered/unknown writer retains its exact candidate and
    failure for existing recovery. An active callback is not completed by this
    drain. Positive receipt/State ACK releases its own borrow before publication. *)
val drain_callback_retirement : t -> (unit, Agent_protocol.Error.t) result

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
        -> borrow_agent_actor:
             (unit -> (Operator_authorization.t option, Agent_protocol.Error.t) result)
        -> (Inference_client.Execution.t
            -> guidance:Inference.Request.Guidance.t option
            -> reserved_input_tokens:int
            -> ('a, Agent_protocol.Error.t) Result.t)
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
  type t = Inference_graph_owner.t
end

(** Outside-mailbox factual sweep: progress all actual initial confirmations
    first, then correct the SAME selected graph. Deferred retains its duty and
    is not positive cleanup. Current execution permission is not required;
    exact Actor/graph ownership remains. *)
val settle_inference_owner_usage
  :  t
  -> owner:Inference_owner.t
  -> (Inference_graph_owner.Factual_usage.Drain_outcome.t, Agent_protocol.Error.t) result

(** Domain-first, nonyielding negative cleanup read for actual lifecycle owners.
    Includes pending facts and an installed native-finish ACK awaiting final
    clear. Foreign domain raises Invalid_argument before any mutable scan;
    unavailable cell evidence conservatively remains pending. No permission or
    remote work is acquired. *)
val inference_usage_cleanup_pending : t -> bool

(** One finite outside-mailbox initial-first factual sweep over actual
    registrations. [Deferred] retains unfinished work; it is neither cleanup
    success nor execution authority. Errors and cancellation remain unchanged.
    Idle maintenance may continue its original polling path after [Deferred];
    admission and teardown must use the strict [drain_inference_usage] projection. *)
val poll_inference_usage
  :  t
  -> (Inference_graph_owner.Factual_usage.Drain_outcome.t, Agent_protocol.Error.t) result

(** Outside-mailbox initial-first factual cleanup over actual registrations.
    Deferred initial confirmation is a retryable failure, never cleanup success.
    Execution policy/source retirement does not revoke this factual duty. *)
val drain_inference_usage : t -> (unit, Agent_protocol.Error.t) result

module Held_input_scope : sig
  (** One actor-issued rich input attempt. Same Eio domain only. Parsing ports
      expire once the joined parsed result is recorded, consumed, or closed.
      Parsed DATA is not authority and cannot reconstruct this scope. *)
  type t

  (** Return the SAME issued immutable service record, only while Parsing and
      still owned. Never accepts a caller-supplied record as execution proof. *)
  val execution
    :  t
    -> (Runtime_input_preparation.Execution.t, Agent_protocol.Error.t) result
end

module Input_graph : sig
  (** Selected factual ACKs for an actual graph registered to this input scope.
      Original expiry denies new effects but does not reauthorize or prevent
      completion/release of already admitted work. Foreign/displaced custody
      rejects; ordinary and constructor graph APIs retain their own contracts. *)
  val open_owner
    :  t
    -> scope:Held_input_scope.t
    -> source:Transcript.Source_id.t
    -> (Inference_owner.t, Agent_protocol.Error.t) result

  val acknowledge_inference
    :  t
    -> scope:Held_input_scope.t
    -> owner:Inference_owner.t
    -> handle:Inference_ledger.Handle.t
    -> Inference_runtime.Attempt.t
    -> (unit, Agent_protocol.Error.t) result

  val observe_inference
    :  t
    -> scope:Held_input_scope.t
    -> owner:Inference_owner.t
    -> handle:Inference_ledger.Handle.t
    -> Inference.Observation.t
    -> (unit, Agent_protocol.Error.t) result

  val observe_owned_inference
    :  t
    -> scope:Held_input_scope.t
    -> owner:Inference_owner.t
    -> Inference.Observation.t
    -> (unit, Agent_protocol.Error.t) result

  val complete_inference
    :  t
    -> scope:Held_input_scope.t
    -> owner:Inference_owner.t
    -> handle:Inference_ledger.Handle.t
    -> Inference_client.Completion.t
    -> (unit, Agent_protocol.Error.t) result

  val release_inference
    :  t
    -> scope:Held_input_scope.t
    -> owner:Inference_owner.t
    -> handle:Inference_ledger.Handle.t
    -> (unit, Agent_protocol.Error.t) result

  val seal_inference_owner
    :  t
    -> scope:Held_input_scope.t
    -> owner:Inference_owner.t
    -> (unit, Agent_protocol.Error.t) result

  val finish_inference_owner
    :  t
    -> scope:Held_input_scope.t
    -> owner:Inference_owner.t
    -> (unit, Agent_protocol.Error.t) result
end

(** Open before graph constructors. Fresh graph source, issuing actor and current
    generation are captured; it cannot be reconstructed from durable state. *)
val open_inference_owner
  :  t
  -> source:Transcript.Source_id.t
  -> (Inference_owner.t, Agent_protocol.Error.t) Result.t

(** Open through the exact live preparation. An unselected owner additionally
    requires the preparation's actual held initialization or authored scope;
    preparation data alone does not permit construction. *)
val open_prepared_inference_owner
  :  t
  -> preparation:Run_preparation.t
  -> source:Transcript.Source_id.t
  -> (Inference_owner.t, Agent_protocol.Error.t) result

(** Serialized original-ledger admission and durable ACK precede handle return.
    Optional associations require actual per-execution evidence; no inference from
    the actor's current operation. Untracked admission also ACKs its ordinal.
    Inherited or unavailable delegated Goal origin requires the allocation
    sibling and rejects here before canonical admission. *)
val admit_inference
  :  t
  -> owner:Inference_owner.t
  -> relation:Transcript.Scope.relation
  -> operation_id:Agent_protocol.Id.Operation.t option
  -> invocation_id:Agent_protocol.Id.Invocation.t option
  -> configuration:Inference.Observation.Configuration.t
  -> (Inference_ledger.Handle.t, Agent_protocol.Error.t) Result.t

(** Guard-preserving allocation through the actual canonical owner. Inherited
    work retains native ancestor admission and original submission guards. *)
val admit_inference_allocation
  :  t
  -> owner:Inference_owner.t
  -> relation:Transcript.Scope.relation
  -> operation_id:Agent_protocol.Id.Operation.t option
  -> invocation_id:Agent_protocol.Id.Invocation.t option
  -> configuration:Inference.Observation.Configuration.t
  -> (Inference_allocation.t, Agent_protocol.Error.t) Result.t

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

(** Called after allocation's joined lifetime. Only a positive factual ACK
    retires the exact live registration. Joined exact-handle membership remains
    only while its canonical row is retained, for factual correction without
    provider permission. A failed release retains its genuine
    graph/handle custody for the existing owner or canonical recovery; it grants
    no retry through an unavailable physical writer. Uses the actual live
    Attempt.delivery when acknowledged. Without that instance, Prepared is
    definitely not submitted and Running conservatively possibly submitted.
    No provider terminal is manufactured. *)
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

(** Explicit constructor mutations under the actor-issued preparation. Fresh
    admission checks current authorization. Joined factual mutations use the
    actual retained graph, canonical registration and original preparation
    custody without granting further provider use. *)
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

  (** Guard-preserving allocation through the actual canonical owner. Inherited
      work retains native ancestor admission and original submission guards. *)
  val admit_inference_allocation
    :  t
    -> preparation:Run_preparation.t
    -> owner:Inference_owner.t
    -> relation:Transcript.Scope.relation
    -> operation_id:Agent_protocol.Id.Operation.t option
    -> invocation_id:Agent_protocol.Id.Invocation.t option
    -> configuration:Inference.Observation.Configuration.t
    -> (Inference_allocation.t, Agent_protocol.Error.t) Result.t

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

  val replace_shell_extension_snapshots
    :  t
    -> preparation:Run_preparation.t
    -> Session.Shell_state.Extension_snapshot.t list
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

module Goal_callback_control : sig
  module Disposition : sig
    type t =
      | Held of Agent_protocol.Goal.Reason.t
      | Abandoned of Agent_protocol.Goal.Reason.t
    [@@deriving equal, sexp_of]
  end

  (** Factual same-checkpoint native control ACK, never execution permission.
      Issued only by the actual joined Init or installed lifecycle owner. *)
  type t

  val receipt : t -> Goal_command_receipt.t
  val disposition : t -> Disposition.t

  (** Exact retained passive Wait DATA; no delivery or execution authority. *)
  val waiting : t -> Agent_protocol.Run_wake.t option
end

module Goal_callback_completion : sig
  (** Closed factual completion after the actual lifecycle joins. A real terminal
      settlement has precedence over its earlier control ACK. Neither variant
      grants execution, installation, delivery or startup handoff. *)
  type t =
    | Controlled of Goal_callback_control.t
    | Settled of Agent_protocol.Goal_episode.Settlement.t
end

module Held_authored_result : sig
  type t =
    | Started of Agent_protocol.Run_receipt.t
    | Controlled of Goal_callback_control.t
    | Settled of Agent_protocol.Goal_episode.Settlement.t
end

module Initialization_scope : sig
  type actor = t

  module Purpose : sig
    type t =
      | Pending_initialization
      | Runtime_construction
    [@@deriving equal, sexp_of]
  end

  (** Process-local capability issued only to a trusted Pending initializer or
      authenticated Ready resource construction.
      Bound to one actor, stable source/complete Selection and generation; it is
      never reconstructed from durable Pending state or exposed by RPC. *)
  type t

  (** Pure association to this SAME original scope's factual callback parent;
      no permission renewal. Snapshot is synchronous and creating-domain checked. *)
  val callback_cleanup : t -> Callback_cleanup.Initialization.t

  module Source_handoff : sig
    (** Factual SAME Pending Init positive ACK, never reconstructed from State.
        Survives lexical scope end; grants no execution or source permission. *)
    type t

    val before_source : t -> Agent_protocol.Run_source.t option
    val after_source : t -> Agent_protocol.Run_source.t option

    (** Domain-first association with the issuing Actor, actual optional Manager,
        generation/resource specification and full installed source in a Ready
        state. The Factory separately checks current authority and the Cell
        verifies the same native unit and Product binding. *)
    val check
      :  t
      -> actor:actor
      -> moderator_manager:Chat_response.Moderator_manager.t option
      -> expected:Session_state.t
      -> (unit, Agent_protocol.Error.t) result
  end

  (** Available only after the actual successful Pending completion ACK. Missing
      or uncertain persistence and an ordinary Ready checkpoint cannot issue it.
      Exact source options include the no-moderator None-to-None case. *)
  val source_handoff : t -> (Source_handoff.t, Agent_protocol.Error.t) result
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

(** Actual Factory construction with its read-only source issuer identity. The
    issuer key is never supplied to the Actor. Identity is fixed at creation;
    ordinary constructors have none and cannot issue deferred-source completion.
    All controls retain [create_with_owner_lease_duration]'s meaning. *)
val create_with_runtime_source_owner
  :  runtime_source_owner:Runtime_source_owner.Identity.t
  -> ordinary_execution_admission:(unit -> (unit, Agent_protocol.Error.t) result)
  -> schedule_permission_timeouts:bool
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

(** First-party canonical counterpart. Validates SAME physical commit closure,
    writer session and creating domain before Actor startup. Old constructors
    remain unsupported for failed-startup disposal; no generic availability
    callback or alternative persistence adapter is admitted by this binding. *)
val create_with_bound_runtime_source_owner
  :  canonical_persistence:Canonical_persistence.t
  -> runtime_source_owner:Runtime_source_owner.Identity.t
  -> ordinary_execution_admission:(unit -> (unit, Agent_protocol.Error.t) result)
  -> schedule_permission_timeouts:bool
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

(** Read-only immutable issuer association, not an issuer projection or resource
    permission. The source host uses it to check its private Factory key against
    this exact resource Actor. Same issuing Eio domain; performs no Actor request,
    callback or Store I/O. *)
val runtime_source_owner_identity : t -> Runtime_source_owner.Identity.t option

val snapshot : t -> (Agent_protocol.Snapshot.t, Agent_protocol.Error.t) result

(** One mailbox observation captures retained state and transient active calls
    from the same owner instant. Never activates or acknowledges work. *)
val observe
  :  t
  -> (Session_state.t * Agent_protocol.Snapshot.t, Agent_protocol.Error.t) result

val state : t -> (Session_state.t, Agent_protocol.Error.t) result

module Resource_observation : sig
  (** Request-local facts captured by one mailbox observation on the issuing
      Actor. The observation carries no source, execution or cleanup authority. *)
  type t

  (** The immutable original State returned by the producer. *)
  val state : t -> Session_state.t

  (** Checks the creating domain before reading the same Actor. Rejects a
      stopped or finished Actor, a lifecycle reservation, and runtime retirement.
      Returns the actual current accepted State when the session ID, generation,
      creating principal, Spec resource identity, optional Run source
      installation, initialization and pending-start fields, owner lease
      generation, and local and parent Stop epochs match. Metadata and usage
      changes can therefore return a newer State while [state] stays fixed.

      This check does not yield, invoke callbacks, or query the mailbox or
      Store. A pending write may install after the check reads the still
      accepted State. The returned facts hold until the next yield. Callers
      must still check the current source and fingerprint on the returned
      State, the exact host association and physical source, and existing
      per-effect guards. *)
  val check_current : t -> (Session_state.t, Agent_protocol.Error.t) result
end

(** Observes accepted State through this Actor's mailbox. May yield. The host
    calls this after its existing request admission. Capturing the observation
    creates no admission and activates no runtime. *)
val observe_resource_state : t -> (Resource_observation.t, Agent_protocol.Error.t) result

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

module Callback_scope : sig
  (** Exact actor-issued legacy borrow and installed Manager callback. No public
      constructor or decoder; saved source/origin facts grant no authority. *)
  type t

  module Completion = Callback_cleanup.Completion

  (** Same acquired producer, including after expiry or Actor death. No authority. *)
  val completion : t -> Completion.t

  val executing : t -> Agent_protocol.Moderator_execution.t
  val source : t -> Agent_protocol.Run_source.t
  val origin : t -> Agent_protocol.Inference_origin.t option

  (** Exact full checkpoint captured at this callback's successful ACK, before
      publication. None means no ACK; Some None is an acknowledged absent
      checkpoint. This is retained factual data, including after callback expiry,
      and grants no effect authority. It never queries or re-encodes a Manager. *)
  val committed_checkpoint : t -> Jsonaf.t option option
end

(** A callback holds its actual actor borrow through checkpoint commit and
    cleanup. Commit persists the outcome, full prospective queue and explicit
    notifications in the same Event acknowledgement. Notifications returned in a
    host outcome must not be published again after this commit.
    Failure retains honest failed/interrupted evidence; effects are not replayed. *)
type legacy_callback_handler =
  scope:Callback_scope.t
  -> commit:
       (snapshot:Session.Moderator_state.Identity_snapshot.t
        -> requests:Agent_protocol.Invocation.follow_up
        -> notifications:string list
        -> (unit, Agent_protocol.Error.t) result)
  -> (unit, Agent_protocol.Error.t) result

(** Exact installed Manager callback admission. True means checkpoint committed
    and cleanup completed; false means physical actor unavailable. Expired,
    foreign or mismatching callbacks reject rather than granting a legacy fallback.
    These factual producer wrappers require the Actor's creating domain and
    reject a foreign domain before gate or claim acquisition. General Actor RPC
    transport is unchanged.
    [capture_cleanup] runs once for an actual installed claim ACK, outside the
    mailbox, protected and before refresh/user code. It must not yield. The
    acquired-scope finish bracket already exists. This includes publication
    failure before a successful claim reply; rejected/uninstalled claims never
    capture. Capture exceptions skip user code and retain their raw failure.
    An already failed claim remains primary; its independent capture failure and
    actual finish outcome remain observable. The same optional transport applies
    to [job_execution.claim_legacy_callback]; it creates no Job parent. *)
type legacy_callback_claim =
  ?capture_cleanup:(Callback_cleanup.Completion.t -> unit)
  -> callback:Chat_response.Moderator_manager.Callback.t
  -> legacy_callback_handler
  -> (bool, Agent_protocol.Error.t) result

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
module Job_runtime_owner : sig
  (** Projection of SAME active job_scope created by with_job_execution. No
      decoder/constructor, and no authority from a Job DTO or allocator closure. *)
  type t

  module Attribution : sig
    type t =
      | Ordinary
      | Goal
  end

  (** Snapshot routing only, using the same retained-parent lookup as actual
      scope issuance. Historical missing inference classification never becomes
      Unbound. Conflicting/unavailable ancestry rejects. This acquires no scope,
      permission, runtime or Job claim; the Actor repeats actual ownership checks. *)
  val classify
    :  Session_state.t
    -> Agent_protocol.Job.t
    -> (Attribution.t, Agent_protocol.Error.t) result

  (** Snapshot-only fresh-claim selection using the SAME retained Run lookup.
      A queued Job of a current Held Goal suspends without claiming, retiring or
      advancing its attempt. Running/terminal factual work stays eligible here;
      its resource/effect owners retain their independent current guards.
      Stale or unavailable queued ownership rejects, never becomes Ordinary.
      This value grants no scope or execution permission. *)
  val selection
    :  Session_state.t
    -> Agent_protocol.Job.t
    -> (Goal_execution_selection.t, Agent_protocol.Error.t) result

  (** Immutable causal routing DATA. Historical missing inference provenance
      still retains genuine Run ancestry; Ordinary is not an inference Unbound
      classification. The Goal branch needs actual source/scope/currentness;
      Ordinary cannot acquire a Goal later.
      Creating Eio domain only; a foreign-domain call raises Invalid_argument. *)
  val attribution : t -> Attribution.t

  (** Immutable actual scope deadline DATA, creating Eio domain only. *)
  val deadline : t -> Agent_protocol.Timestamp.t option
end

type job_execution =
  { job : Agent_protocol.Job.t
  ; runtime_owner : Job_runtime_owner.t
  ; allocate_inference : Inference_allocation.Port.t
  ; model_jobs : Model_job_admission.t
  ; execute : Native_tool_invocation.executor
  ; moderator_execute : Native_tool_invocation.moderator_executor
  ; claim_event : event:Chat_response.Moderation.Event.t -> Moderator_event.claim
  ; claim_legacy_callback : legacy_callback_claim
  }

(** Retain job-attempt ownership across native calls and moderator events. Event
    claims use the actor's moderator gate and persist job identity with the
    checkpoint receipt. [claim_legacy_callback] admits only actual Boundary
    selections from the installed Manager under this exact job attempt. Missing
    historical custody remains unavailable. Returned services expire at callback
    completion; none reconstruct original authentication from a Job ID. *)
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
  :  ?actor:Operator_authorization.t
  -> t
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
    No callback reenters the runtime owner's mutex during compaction. When an
    original [actor] is supplied, check it at actual worker installation after
    any queued constructor checkpoints; legacy omission remains unbound.
    [moderator_manager] retains the exact installed compiled resource for callback
    ownership checks. It requires a runtime worker and is cleared by worker-only
    replacement, removal or retirement. It never grants source authority by itself.
    [initial_startup] names only this authenticated ordinary first extension startup,
    with actual Ready/Running source, no Run/held/Init owner and no prior lifecycle
    claim. Its exact worker/Manager is admitted at physical install into the single
    pending lifecycle slot. Callback claim consumes it once and drops the runtime
    binding; resource replacement/removal, Stop, source loss and Actor closure retire
    it. Existing lifecycle custody cannot be overwritten. Omission keeps existing
    behavior; invalid supplied startup rejects rather than creating an ambient
    runtime original. Full native currentness/capability/host policy still applies
    at each effect. Construction errors before installation retire the speculative
    admission, including raised exceptions. *)
module Ordinary_attachment_basis = Ordinary_runtime_basis

(** [expected_ordinary] is a retained root's original factual source/Stop basis.
    It is checked inside the actual Actor install, after existing original and
    execution checks. It grants no authority, cannot bypass Pending, and prevents
    a Stop committed before Owner unload from admitting a stale warm worker.
    [unmoderated_source] is the SAME Builder fact for this final physical worker;
    omission at actual replacement clears classification. It grants no effect. *)
val set_runtime_worker
  :  ?unmoderated_source:Runtime_builder.Unmoderated_source.t
  -> ?actor:Operator_authorization.t
  -> ?initial_startup:Runtime_builder.Authored_start.t
  -> ?moderator_manager:Chat_response.Moderator_manager.t
  -> ?expected_ordinary:Ordinary_attachment_basis.t
  -> t
  -> worker:Operation_worker.t option
  -> inference:Inference_client.Execution.t option
  -> (unit, Agent_protocol.Error.t) result

(** Durably installs the supplied checkpoint, including an unchanged value.
    Publishes a public Session_updated position after commit so subscribers can
    reach the returned revision. Private moderator state is not disclosed.
    [actor], when supplied by authenticated construction, is rechecked at actual
    actor admission; completed earlier effects are not rolled back. *)
val change_moderator
  :  ?actor:Operator_authorization.t
  -> t
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

(** Trusted live shell code owners persist their admitted snapshots through the
    existing shell transaction. Other shell fields retain the actor's current
    values. This grants no execution or snapshot-admission authority; the caller
    must reject expired code/resource owners before invoking it. Constructor
    callers use [Constructor_mutations] and update their own candidate shell
    projection only after the durable operation succeeds. *)
val replace_shell_extension_snapshots
  :  t
  -> Session.Shell_state.Extension_snapshot.t list
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

(** Issue one exclusive constructor scope against its exact lifecycle/source
    basis. Default purpose is Pending_initialization. Runtime_construction requires
    actual Ready and current original custody or a genuine Run preparation; this
    legacy no-proof entry cannot issue it. Complete/fail initialization reject
    Runtime_construction without changing Ready; end retires either purpose.
    Requires a captured target. Actor-owned jobs/schedules, tracking counters,
    reservations and acknowledged shell grants may advance without replacing the
    basis. Accepted authorized Stop permanently revokes even a stopped no-op. *)
val begin_initialization
  :  ?purpose:Initialization_scope.Purpose.t
  -> t
  -> expected:Session_state.t
  -> (Initialization_scope.t, Agent_protocol.Error.t) result

(** Actual authenticated initialization admission. The scope still supplies
    lifecycle/job ownership; the retained original actor grants no source read. *)
val begin_initialization_authenticated
  :  ?purpose:Initialization_scope.Purpose.t
  -> t
  -> actor:Operator_authorization.t
  -> expected:Session_state.t
  -> (Initialization_scope.t, Agent_protocol.Error.t) result

(** Borrow an attenuated original proof from this exact active initialization.
    A retained proof expires with the initialization owner; no ambient authority
    is installed and legacy initialization returns None. *)
val initialization_agent_actor
  :  t
  -> scope:Initialization_scope.t
  -> (Operator_authorization.t option, Agent_protocol.Error.t) result

(** Same existing constructor scope, explicitly bound to this run preparation.
    Scoped commits recheck current custody and acknowledge only their own changes. *)
val begin_run_initialization
  :  ?purpose:Initialization_scope.Purpose.t
  -> t
  -> preparation:Run_preparation.t
  -> expected:Session_state.t
  -> (Initialization_scope.t, Agent_protocol.Error.t) result

(** Consume the exact joined Init's known control. Before returning, release
    its callback startup hold through the canonical aggregate planner for an
    already accepted Finish; a validated terminal receipt returns Settled.
    Closed construction may retain the control fact, but this grants no fresh
    execution. Failed/unknown writer and foreign source/candidate reject. Actual
    callback/publication exceptions propagate. None still requires every normal
    fresh guard. No pending work or passive Wait is converted into settlement. *)
val initialization_goal_completion
  :  t
  -> scope:Initialization_scope.t
  -> (Goal_callback_completion.t option, Agent_protocol.Error.t) result

(** Retire the issuing actor's scope idempotently, without a durable write. Wrong
    actors reject. The host must call this under cancellation protection on every
    exit before exposing the constructed runtime. *)
val end_initialization
  :  t
  -> scope:Initialization_scope.t
  -> (unit, Agent_protocol.Error.t) result

(** Negative cleanup for this actor-issued constructor's exact resource.
    Valid after end_initialization. Removes only its acknowledged selected queue/
    Schedule loans still bound to this physical Manager/full source. Missing rows
    are idempotent; a foreign actor or contradictory Manager rejects. No durable
    fact is deleted and no original authority is required or renewed. The host
    composes this with the resource's original close, which must always run. *)
val retire_initialization_resource
  :  t
  -> scope:Initialization_scope.t
  -> moderator_manager:Chat_response.Moderator_manager.t
  -> (unit, Agent_protocol.Error.t) result

(** Actor-issued ports over the same exact constructor token. Admission classifies
    only new pre-episode work; ambiguous existing Run origin denies. Allocation
    additionally requires this scope's exact actual Running Model_call attempt,
    including while Pending initialization has the session stopped. Retained
    callbacks reject after end/stop/source change; no IDs mint custody. *)
val initialization_model_admission
  :  t
  -> scope:Initialization_scope.t
  -> Model_job_admission.t

(** One synchronous constructor producer, installed before its admission RPC.
    The supplied function runs once outside the mailbox. Its actual joined exit
    retains cleanup facts without masking the original exception/cancellation.
    No producer outcome grants further provider permission. *)
type initialization_model_producer

val with_initialization_model_producer
  :  t
  -> scope:Initialization_scope.t
  -> f:(initialization_model_producer -> 'a)
  -> 'a

val initialization_model_producer_admission
  :  initialization_model_producer
  -> Model_job_admission.t

val claim_initialization_model_call
  :  t
  -> initialization_model_producer
  -> (Agent_protocol.Job.t, Agent_protocol.Error.t) result

(** Same-domain producer handoff before a returned execution result can be lost
    while joining its sibling cancellation fiber. Stores the first result before
    clock sampling, retaining any clock diagnostic without replacing that result.
    No mailbox call, writer, or permission is issued. Repeated handoff keeps the
    original result; completion below uses this same selection. *)
val retain_initialization_model_call_result
  :  t
  -> initialization_model_producer
  -> Runtime_builder.model_job_outcome
  -> (unit, Agent_protocol.Error.t) result

val complete_initialization_model_call
  :  t
  -> initialization_model_producer
  -> Runtime_builder.model_job_outcome
  -> (Agent_protocol.Job.t, Agent_protocol.Error.t) result

(** Actual joined cancellation of the synchronous execution leg. *)
val interrupt_initialization_model_call
  :  t
  -> initialization_model_producer
  -> (Agent_protocol.Job.t, Agent_protocol.Error.t) result

(** Same-domain nonyielding negative eligibility read. It performs no cleanup. *)
val constructor_model_retirement_pending : t -> bool

(** Finite factual pass after actual producer/source/worker joins. Runs outside
    Owner locks, before callback retirement. Uncertain writes retain their exact
    candidate and original failure; this never re-executes a provider. *)
val drain_constructor_model_retirement : t -> (unit, Agent_protocol.Error.t) result

val initialization_model_allocation
  :  t
  -> scope:Initialization_scope.t
  -> job:Agent_protocol.Job.t
  -> Inference_allocation.Port.t

(** Claim only an already acknowledged immediate Model_call in this exact
    active initializer's admitted set. Rechecks current source and original
    custody, retains the existing preparation, and never reclassifies origin. *)
val claim_initialization_model_job
  :  t
  -> scope:Initialization_scope.t
  -> job_id:Agent_protocol.Id.Job.t
  -> generation:int
  -> (Agent_protocol.Job.t, Agent_protocol.Error.t) result

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

(** Cancellation-watch liveness for this scope's exact already claimed Running
    Model job, generation and attempt. It retains both original authorizations,
    source/custody and admitted policy/deadline checks. An owned inference ACK's
    Store revision change alone is not cancellation; automatic budget Hold does
    not cancel admitted work, while explicit control revokes the actual custody.
    This query grants no execution permission. Fresh attempt checks must use
    [initialization_model_job_is_current] and the actual allocation/submission
    ports. Ordinary initializer behavior is unchanged. *)
val initialization_model_job_is_running
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

(** Trusted Pending completion retains the scope's stable basis and requires it
    active. Runtime_construction rejects without changing Ready.
    Only initialized history/count/reservations, moderator and shell reviewer
    checkpoints are overlaid. Actual jobs/bindings/schedules, tracking commits
    and shell grants remain authoritative. The optional actual runtime Manager
    binds only this owner's acknowledged constructor Jobs; missing completed
    no-delivery rows remain unavailable. Permission objects are not renewed.
    Failed ACK retains prior bindings; ACK installs handoff before publication.
    Marks Ready without a second archive. *)
val complete_initialization
  :  ?moderator_manager:Chat_response.Moderator_manager.t
  -> t
  -> scope:Initialization_scope.t
  -> candidate:Session_state.t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

(** Checkpoint only this actual Ready construction and hand off its acknowledged
    constructor Jobs to the intended runtime resource. Uses the same original
    Admission and optional Run preparation; no permission is renewed. Missing
    completed no-delivery rows remain unavailable. Failed ACK preserves the
    prior basis and bindings; ACK installs them before reconciliation/publication.
    Exact unchanged checkpoints with no binding change perform no write.
    Pending scopes must use [complete_initialization]. *)
val checkpoint_runtime_construction
  :  ?moderator_manager:Chat_response.Moderator_manager.t
  -> t
  -> scope:Initialization_scope.t
  -> Jsonaf.t option
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

(** Record failed Pending activation under the active scope's stable basis.
    Runtime_construction rejects without changing Ready. Retains
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

(** [actor], when present, is checked again at actual actor admission after
    runtime construction. Omission preserves the principal-only host API. *)
val start
  :  ?actor:Operator_authorization.t
  -> ?expected_parent_stop_epoch:int64
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
  :  ?actor:Operator_authorization.t
  -> ?expected_parent_stop_epoch:int64
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

(** Proof-bearing ingress used by actual authenticated transports. Repeats the
    original current Send_messages scope and real writer checks. Its principal
    is derived from that object. Live custody is installed only after admission;
    deferred input retains its exact proof until its actual later adoption.
    A single expired deferred proof rejects adoption; an ambiguous multi-entry
    batch is unbound rather than selecting one caller's authority. Explicit Run
    operation grants use their own admitted owner. The principal-only API above
    remains explicitly unbound. *)
val submit_message_authenticated
  :  t
  -> actor:Operator_authorization.t
  -> ?timing:Agent_protocol.Pending_input.Timing.t
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> Agent_protocol.History.entry
  -> (submission, Agent_protocol.Error.t) result

(** Called by the restoring host before any runtime can execute. Durable old
    live runs become Interrupted with immutable Unconfirmed owner evidence and
    terminal receipts; installation epoch is unchanged and no effect replays.
    The same recovery transaction also holds enabled retained goals for source
    review without consuming a user command receipt or constructing a runtime. *)
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
    constructor transaction. The exact runtime Manager can bind only that owner's
    acknowledged constructor Job subset without renewing its permission. Matching
    checkpoints and unchanged bindings produce no durable advance; omitted Manager
    never grants a physical resource. Failed ACK preserves subset and bindings. *)
val checkpoint_run_moderator
  :  ?moderator_manager:Chat_response.Moderator_manager.t
  -> t
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
    preparation. Releases custody or transfers its SAME close obligation to the
    retained inherited admission until an actual no-writer proof/canonical ACK.
    Distinguishes definite no-run rejection
    from an attempted uncertain final persistence; exceptions/cancellation retain
    their primary backtrace; uncertain canonical writers retain actual custody. *)
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

val begin_run_preparation_authenticated
  :  t
  -> actor:Operator_authorization.t
  -> authorize:(Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> request:Agent_protocol.Run_start.t
  -> request_sha256:string
  -> (Run_preparation.Decision.t, Agent_protocol.Error.t) result

(** Internal authorized host command, without a client attachment. The actual
    original authentication must be current and carry Send_messages. [authorize]
    is additional non-yielding current host policy, not a substitute for proof.
    The actor computes the disjoint canonical digest and retains the original
    proof/policy in its ephemeral preparation through final commit. No controller
    ID, decoded record or durable receipt grants this custody. *)
val begin_headless_run_preparation
  :  t
  -> actor:Operator_authorization.t
  -> authorize:(Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> request:Run_admission_request.Headless.t
  -> (Run_preparation.Decision.t, Agent_protocol.Error.t) result

(** Idempotent caller exit. A preparation transferred to the exact inherited
    admission retains its closing owner across uncertain persistence; only that
    owner settles no-writer/ACK. This does not keep fresh execution authorized. *)
val end_run_preparation : t -> Run_preparation.t -> (unit, Agent_protocol.Error.t) result

(** Issue NEW effect-deferred provenance under this Actor's existing preparation
    owner and Factory identity captured at Actor creation. Original request/CAS,
    current Send_messages/host policy, retained receipt and actual execution
    quiescence are checked before issuance. Retained is DATA and creates no new
    deferred owner. Ordinary preparation cannot be retrofitted. The host still
    acquires the real source/resource lifetime; no runtime is loaded here. *)
val begin_effect_deferred_headless_preparation
  :  t
  -> actor:Operator_authorization.t
  -> authorize:(Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> request:Run_admission_request.Headless.t
  -> (Run_preparation.Deferred.Decision.t, Agent_protocol.Error.t) result

(** Negative-only host entry before ordinary runtime effects or execution
    resources escape. Irreversibly excludes this Actor's actual active deferred
    attempt, including when the ordinary caller omits a preparation. It is a
    no-op when no deferred cell exists and returns no permission or certificate.
    Authoritative fresh capability gates repeat exclusion inside the mailbox. *)
val exclude_effect_deferred_execution : t -> (unit, Agent_protocol.Error.t) result

(** Seal the exact continuously owned deferred preparation without fresh effect
    authorization. A logical [end_run_preparation]/source close cannot construct
    this half or prove resource cleanup. The real Factory must separately join
    its target and parent leases before completing it with its hidden issuer. *)
val close_effect_deferred_preparation
  :  t
  -> Run_preparation.Deferred.t
  -> (Run_preparation.Closed_attempt.t, Agent_protocol.Error.t) result

module No_effects_observation : sig
  (** Actual target observation for one genuinely closed, effect-deferred
      request. Neither decoded absence nor recovery can issue it. No recurrence
      origin, retry, execution authority or durable proof constructor is exposed. *)
  type t

  (** Current read policy and original visibility, followed by exact pure basis.
      Trusted host callbacks must not yield. This authorizes no new work. *)
  val check_current : t -> (unit, Agent_protocol.Error.t) result

  (** Callback-free same Actor/Store Current, closed source/preparation and exact
      retained-request absence. Current read authorization remains separately
      mandatory; this observation is not permission to rotate or admit a Run. *)
  val check_basis : t -> (unit, Agent_protocol.Error.t) result
end

(** Authenticate the Factory-completed exact closed attempt against this actual
    live Store/writer and fresh retained state. Requires current transcript
    visibility and nonyielding host read policy, repeated after Store observation.
    Known/unknown Run admission, excluded or consumed execution, foreign issuer,
    poisoned/dead/displaced owner and missing successful source completion reject.
    No runtime loads and no retry/rotation occurs. Same issuing Eio domain. *)
val observe_no_run_effects
  :  t
  -> actor:Operator_authorization.t
  -> authorize:(Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> completion:Run_preparation.Source_completion.t
  -> (No_effects_observation.t, Agent_protocol.Error.t) result

module Source_binding : sig
  (** Actor-issued captured source/preparation custody, not a runtime lease or
      execution authority. No serialization or caller-supplied guard issuer.
      Belongs to the issuing Eio domain; release is irreversible and idempotent. *)
  type t

  val source : t -> Runtime_source_preparation.t
  val close : t -> unit

  (** Repeats the original policy, then the callback-free basis. *)
  val check_current : t -> (unit, Agent_protocol.Error.t) result

  (** Non-yielding and callback-free: original proof presence, live
      actor/preparation, exact retained source selection and current revision.
      Original authentication currentness and policy run in [check_current];
      this basis alone grants neither. *)
  val check_basis : t -> (unit, Agent_protocol.Error.t) result
end

(** Bind one authentic static source selection to the exact live authenticated
    preparation. Requires no active operation or moderator borrow and an Idle or
    Stopped target. Checks the pinned definition/revision/invocation; installed-tree
    verification remains the host source owner's responsibility. Ordinary ACKs,
    lifecycle/runtime retirement, replacement and owner exit close the binding.
    An exact owned preparation ACK can advance its basis. A closed binding cannot
    be rebound. This neither constructs nor proves a Runtime_owner lease. *)
val bind_run_source
  :  t
  -> preparation:Run_preparation.t
  -> source:Runtime_source_preparation.t
  -> (Source_binding.t, Agent_protocol.Error.t) result

module Recurrence_retry : sig
  (** Process-local ownership of one explicit retry, from source preparation
      through controller acknowledgement. Neither serializable nor recoverable
      from a retained execution row. *)
  type t

  (** Original immutable intent, predecessor and derived next request are DATA.
      No getter returns an execution/admission permit or a new Closed token. *)
  val intent : t -> Recurrence_execution_intent.t

  val previous : t -> Recurrence_execution_attempt.t
  val next_attempt : t -> Recurrence_execution_attempt.t

  (** Exact issuing Eio domain, actual target/Store/negative-cell identity and
      phase-specific basis. No policy/clock/Store RPC or source admission here.
      Rejects displaced, attempted/unconfirmed or unavailable retry custody. *)
  val check_basis : t -> (unit, Agent_protocol.Error.t) result
end

(** The runtime owner calls this after reserving its existing source lane. Validate
    original target NoEffects against intent/previous, exact actual Store/Actor,
    then consume that observation into a newly authenticated deferred preparation.
    The next immutable Attempt is derived from this request's exact principal,
    key/CAS/input/mode/generation/SHA and previous ordinal+1; nothing is rebased.
    The returned pair is issued together; the same opaque retry is retained in
    Owner.Deferred_source/Report before pending cancellation can escape. *)
val begin_recurrence_retry_preparation
  :  t
  -> observation:No_effects_observation.t
  -> actor:Operator_authorization.t
  -> authorize:(Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> intent:Recurrence_execution_intent.t
  -> previous:Recurrence_execution_attempt.t
  -> request:Run_admission_request.Headless.t
  -> (Recurrence_retry.t * Run_preparation.Deferred.t, Agent_protocol.Error.t) result

(** Only a retry closed successfully before its controller write can resume.
    Fresh authorization must have the same frozen principal. The original next request,
    key/CAS/SHA/ordinal are reused, never supplied/replaced by the caller. Consume
    the exact previous source completion into a fresh actual Deferred; return the
    same retry handle. No appended controller ordinal or target write occurs. *)
val resume_recurrence_retry_preparation
  :  t
  -> retry:Recurrence_retry.t
  -> actor:Operator_authorization.t
  -> authorize:(Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> (Recurrence_retry.t * Run_preparation.Deferred.t, Agent_protocol.Error.t) result

(** The Factory calls this after real target and parent cleanup have succeeded and
    existing close_effect_deferred/Closed_attempt.complete produced completion.
    Validates the same deferred cell, request and Factory issuer. Success retains
    the same handle for resuming source preparation. Acknowledged, attempted,
    invalid or foreign retries return an error and cannot resume.
    Existing factual close still happens before this and never depends on success
    here. Error must not replace the Factory's original failure or exception.
    The public Source_completion type is already opaque and its constructor needs
    both Actor's owner and Factory's private key; no callback/Boolean join claim. *)
val complete_recurrence_retry_source
  :  t
  -> retry:Recurrence_retry.t
  -> completion:Run_preparation.Source_completion.t
  -> (unit, Agent_protocol.Error.t) result

(** Shared controller operation for a distinct target or the controller itself. Exactly
    one opaque retry plus its current next binding is required; Selected retry
    binding cannot admit a Run before this operation's known exact rotation ACK.
    Actual source/target policy, current controller definition/control/source/birth,
    fresh grace, both originals and final pure bases remain mandatory. No target
    Store/Actor RPC runs inside the controller mailbox. Controller.same_store is
    checked against the physically retained target controller association.

    Pure catalog.rotate_execution constructs one resolve_retry+append candidate.
    Immediately before the actual writer attempt, exception-safely consume the
    phase into a pending writer. Only exact candidate ACK releases the next
    binding and installs immutable ACK fact, before publication. Self-target
    forwards the same next preparation through persistence and ACK installation;
    original expected_revision and request_sha256 remain unchanged.

    Exact duplicate row is DATA only. It cannot establish a missing ACK fact or
    restore/release a closed preparation. Reopened controller/target observes the
    retained row through existing record/read APIs, with no activation custody. *)
val rotate_prepared_recurrence_execution
  :  t
  -> retry:Recurrence_retry.t
  -> next:Source_binding.t
  -> actor:Operator_authorization.t
  -> authorize:
       (Session_state.t
        -> Recurrence_definition.t
        -> (unit, Agent_protocol.Error.t) result)
  -> (Recurrence_execution.t, Agent_protocol.Error.t) result

(* A pending, attempted or invalid retry cannot issue another no-effects
   observation. After controller acknowledgement, the next attempt may qualify
   through the ordinary joined-cleanup and Store-absence checks. The Actor
   enforces this independently of Factory bookkeeping and public close calls. *)

(** Early refusal before ordinary prepared-runtime resource acquisition. Checks
    the exact current actor preparation, original policy and retry ACK phase.
    Returns no permission across a yield; actual construction/admission must keep
    its existing later checks. Pending/closed/displaced retry custody rejects. *)
val check_prepared_run_execution
  :  t
  -> preparation:Run_preparation.t
  -> (unit, Agent_protocol.Error.t) result

module Held_run : sig
  (** Actual actor-issued continuous startup custody for an inert acknowledged
      Run. Neutral with respect to recurrence/Goal ownership. No decoder, issuer
      or scope constructor; a retained receipt never recreates this value.
      Requires the separate genuine runtime-owner Prepared lifetime for eventual
      activation. Closing its source binding preserves the acknowledged evidence
      and last write disposition without rollback or no-effects permission. *)
  type t

  val admission : t -> Agent_protocol.Run_receipt.t

  (** Callback-free exact live actor/source/preparation and still-Admitted
      Run/receipt. Neither authenticates the original nor starts execution. *)
  val check_basis : t -> (unit, Agent_protocol.Error.t) result

  module Activation : sig
    (** One-shot continuation of this exact actor-issued held owner. The old
        quiescent Held/source checks cease to work after consumption. Own ACKs
        retain the same preparation and immutable admission; unrelated writes,
        Stop, replacement and closure revoke startup. No receipt/ID constructor.
        This is not a reconstructed Run authority or an execution result. *)
    type t

    val check_basis : t -> (unit, Agent_protocol.Error.t) result
    val close : t -> unit

    (** Historical fact from this actual owner's immutable root-operation ACK
        proof. Remains true after that Operation retires; neither checks live
        authority nor permits execution, resource loading or replacement. The
        runtime owner must separately retain its exact installed resources and
        ordinary lease admission checks. Same issuing Eio domain, otherwise
        raises [Invalid_argument]. *)
    val handoff_acknowledged : t -> bool
  end

  module Admission : sig
    type held = t

    (** Retained is exact confirmed DATA only, including a known ACK whose
        continuous source owner closed during persistence. Rejected means no
        admission write was attempted; it is not a no-effects proof. Uncertain
        retains attempted-write uncertainty and forbids a replacement admission. *)
    type t =
      | Held of held
      | Retained of Agent_protocol.Run_receipt.t
      | Rejected of Agent_protocol.Error.t
      | Uncertain of Agent_protocol.Error.t
  end
end

module Recurrence_goal_run_guard : sig
  (** Scoped exact controller prewriter capability for this one captured Run.
      Bound to its actual original authority, attempt and lower Goal scope. *)
  type t
end

val capture_recurrence_goal_run_guard
  :  t
  -> actor:Operator_authorization.t
  -> authorize:
       (Session_state.t
        -> Recurrence_definition.t
        -> (unit, Agent_protocol.Error.t) result)
  -> intent:Recurrence_execution_intent.t
  -> attempt:Recurrence_execution_attempt.t
  -> (Recurrence_goal_run_guard.t option, Agent_protocol.Error.t) result

(** Trusted host inert admission using the exact actor-issued source binding and
    its original authenticated preparation. Its canonical request must match the
    complete occurrence intent/attempt and exact actual server/session target.
    New Run, Admission and reciprocal origin
    commit atomically before returning live Held custody. No normal Run authority,
    operation/history allocation, runtime load, initialization or worker launch.
    Retained lookup verifies exact origin/attempt and returns DATA only; closed
    or recovered custody must use existing observation/reconciliation, never
    recreate Held. Own ACK advances only this exact preparation before publish.
    Source binding alone is not installed-artifact or runtime-lease authority;
    the host must supply those genuine owners before any eventual activation. *)
val admit_held_recurrence_run
  :  ?goal_guard:Recurrence_goal_run_guard.t
  -> t
  -> binding:Source_binding.t
  -> intent:Recurrence_execution_intent.t
  -> attempt:Recurrence_execution_attempt.t
  -> session:Agent_protocol.Session_ref.t
  -> (Held_run.Admission.t, Agent_protocol.Error.t) result

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
    without a bound owner. Persistence uncertainty is reconciled by the same key.

    A NEW Authored_start rejects any existing admitted lifecycle slot. Its first
    Starting ACK installs the live Run binding and, only for a genuinely
    authenticated scope, the one-shot original lifecycle loan before publication.
    A later Idle-write failure preserves that acknowledged binding/loan and Run
    receipt; it does not roll back startup. Receipt reconciliation remains DATA
    and never replays the callback or creates a loan. Running/Starting after that
    failure is not yet idle-claim eligible; this API adds no automatic recovery.
    A scope without an original object creates no legacy-None lifecycle loan. *)
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

val submit_message_authenticated_with_command_audit
  :  t
  -> actor:Operator_authorization.t
  -> ?timing:Agent_protocol.Pending_input.Timing.t
  -> command_audit:Document_schema.Document.t
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

(** Same internal managed admission under the exact actual native calling
    scope already authenticated by Factory. Retains that lexical borrow for
    negative admitted-operation refresh outside the mailbox and pure currentness
    before a new inherited Turn writer. No client attachment, source grant,
    agent-creation right or arbitrary authorization callback is supplied. Receipt
    replay and busy/deferred no-Turn behavior match [submit_managed_message]. *)
val submit_managed_message_native
  :  t
  -> borrowed:Native_tool_invocation.borrowed
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

(** Actual authenticated compaction admission. Retains the exact original proof
    only in the newly ACKed compaction operation; the actor checks it at dispatch.
    Completion/failed persistence cannot extend the physical worker epoch. *)
val compact_authenticated
  :  t
  -> actor:Operator_authorization.t
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> expected_revision:int64 option
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val compact_authenticated_with_command_audit
  :  t
  -> actor:Operator_authorization.t
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

(** Factual maintenance of an exact due queued Job. Reuses actual retained
    Run authority retirement, preserving stale generation errors and writer
    failures. Never claims an execution attempt, even if a held Goal becomes
    Enabled while its original authority callback yields. Acquires no runtime,
    source or capacity. Actual startup-owned deferred Jobs retain their owner. *)
val reconcile_job_authority
  :  t
  -> job_id:Agent_protocol.Id.Job.t
  -> generation:int
  -> (unit, Agent_protocol.Error.t) result

(** Scheduled [Queued] descendants with [Pending] or [Delivered] delivery
    remain queued ([Ok None], without a write) while their exact acknowledged
    held-authored startup owner is still live. After its final handoff ACK,
    ordinary Job claiming applies. Synchronous [Not_required] calls and factual
    completion/cancellation are unchanged; no deferral creates execution rights. *)
val claim_job
  :  t
  -> job_id:Agent_protocol.Id.Job.t
  -> generation:int
  -> (Agent_protocol.Job.t option, Agent_protocol.Error.t) result

(** Complete only the exact claimed attempt. The actual terminal Job, retry
    custody and owning Run evidence/aggregate settlement share one durable ACK;
    the raw Model result is preserved. An unrepresentable retry time terminates
    with the original failure, as exhausted retries do. No original authority is
    renewed. Late callbacks cannot mutate a newer retry or terminal winner. *)
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
    tool failure. Retry timing uses the observed completion time; an
    unrepresentable endpoint preserves the original terminal failure. Recovery
    retains the stored completion timestamp separately from retry timing. A worker
    cannot complete an attempt after it enters a durable dependency wait; only
    dependency reconciliation may finish that wait.
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
    Saves Enqueued and the exact queue append in one durable transaction,
    preserving the full checkpoint carrier and retained original Run origin even
    when retry has replaced the latest Job attempt. *)
val enqueue_run_job_delivery
  :  t
  -> delivery:Run_job_delivery.t
  -> before:Session.Moderator_state.Identity_snapshot.t
  -> after:Jsonaf.t
  -> (unit, Agent_protocol.Error.t) result

(** Optional expected values perform checkpoint and complete Job-record comparison
    in the same mailbox transaction as delivery. Actual legacy Manager delivery
    requires both, compares known snapshots independently from the full retained
    queue, and appends exactly the existing completion payload. Current/Expired
    terminal custody transfers before Delivered retires its parent; missing
    historical custody stays unavailable. The queue loan installs only after ACK,
    before publication. Named Run source conflicts reject rather than downgrade.
    The Manager's expected value compares every known snapshot field; complete
    carrier metadata and unfamiliar fields are independently validated/preserved.
    A supplied complete incoming carrier remains strict, never repaired. *)
val deliver_job
  :  ?expected:Session.Moderator_state.Identity_snapshot.t
  -> ?expected_job:Agent_protocol.Job.t
  -> ?moderator_manager:Chat_response.Moderator_manager.t
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
    live lease atomically with cancellation. Owned Model cancellation records
    the exact current or planned occurrence and Run settlement in that same ACK.
    An already Interrupted Model remains unchanged. Cancellation does not imply
    rollback of external effects. Internal schedulers use the privileged operation
    below. *)
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

(** Interrupt only the exact claimed attempt; stale cleanup cannot interrupt a
    newer retry or overwrite a terminal winner. An owned Model's actual attempt
    evidence and Run settlement share its Job ACK without renewing execution
    authority or claiming effect rollback. Recovered workers use the persisted
    attempt. *)
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
    before saving delivery plus the new moderator checkpoint atomically. Legacy
    positive creator evidence or an unowned queue lifetime requires the actual
    installed Manager resource, exact expected record/checkpoint and one raw
    append. Only genuinely historical absence retains the data-only route.
    Completed delivery persists truthful facts even if original custody expired;
    the corresponding Expired queue loan rejects later execution. No custody is
    restored from missing inventory. Owned v1 compares all known snapshot fields
    and appends only its exact Timer frame, preserving full queue metadata/raw
    extensions without creating an original admission from ownership DATA. *)
val complete_schedule
  :  ?expected:Session.Moderator_state.Identity_snapshot.t
  -> ?expected_schedule:Agent_protocol.Schedule.t
  -> ?moderator_manager:Chat_response.Moderator_manager.t
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
    An exact v1 Job/Timer wake that resumes a Run installs that live Run binding's
    authenticated original loan at the claim ACK, before publication. This parent
    takes precedence over an unrelated foreground drainer. Missing original
    custody remains unavailable; neither retained Run facts nor a drainer lend it.
    Failed claims discard speculative loans; callback completion, Stop and owner
    closure retire installed loans. No-wake callbacks retain contextual admission.

    Failure or cancellation retains terminal evidence without changing the old
    checkpoint. Unsettled failed/interrupted claims block further queued execution
    for that source/generation, including after unrelated checkpoint edits. This
    handoff does not retire failed heads. A failed terminal save retains the borrow
    until recovery.
    Stop-cancel interrupts the callback; late or escaped commits are rejected.
    Returns false while the session is unavailable, including while a prospective
    governed admission retains its actual occupancy; empty, stale or previously
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
      -> policy_caller:Delegated_policy_caller.t option
      -> model_jobs:Model_job_admission.t
      -> allocate_inference:Inference_allocation.Port.t
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
      -> policy_caller:Delegated_policy_caller.t option
      -> model_jobs:Model_job_admission.t
      -> allocate_inference:Inference_allocation.Port.t
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

(** Validate the opaque Caller against THIS actual child Actor and its existing
    request owner. Nonyielding, domain confined; Factory still checks private
    source, reservation and selected tools before and after parent waits. *)
val check_delegated_policy_caller
  :  t
  -> Script_tool_calls.preparation
  -> (unit, Agent_protocol.Error.t) result

module Native_setup : sig
  (** Lexical actual Factory source setup; no Caller/Goal/Init/lifecycle or
      execution permission. Same creating domain and registered target token. *)
  type t

  val check : t -> (unit, Agent_protocol.Error.t) result
  val close : t -> unit
end

(** Trusted Factory setup. Checks the creating domain before invoking authorize
    outside the mailbox. The caller must capture a successful token into cleanup
    custody before pending cancellation or another authorization check can escape.
    Current private source/grant checks remain required around later waits; the
    token never replaces them. *)
val begin_native_setup
  :  t
  -> source_owner:Runtime_source_owner.t
  -> reference:Agent_store.Delegation_store.Reference.t
  -> source:Runtime_builder.Source.t
  -> expected:Session_state.t
  -> authorize:(unit -> (unit, Agent_protocol.Error.t) result)
  -> (Native_setup.t, Agent_protocol.Error.t) result

(** Same target cleanup remains valid after negative [Native_setup.close]. *)
val end_native_setup : t -> setup:Native_setup.t -> (unit, Agent_protocol.Error.t) result

(** Actual registered graph only. Unbound identity still cannot allocate. *)
val open_native_setup_inference_owner
  :  t
  -> setup:Native_setup.t
  -> source:Transcript.Source_id.t
  -> (Inference_owner.t, Agent_protocol.Error.t) result

module Policy_restoration : sig
  (** Lexical actual Actor admission, issued for a fresh authenticated delegated
      request. It is neither Init, Run nor ordinary execution authority. Factory
      owns actual writer/source/profile checks before and after setup waits. *)
  type t

  val check : t -> (unit, Agent_protocol.Error.t) result
  val close : t -> unit

  module Published : sig
    (** Process-local association retained by the actual Owner baseline Cell
        lease. No old Caller/governance/authorize is retained as permission.
        Fresh captures require their own genuine current request. *)
    type t

    val check : t -> (unit, Agent_protocol.Error.t) result
    val close : t -> unit
    val product_identity : t -> Runtime_builder.Product_identity.t
  end
end

module Delegated_policy_claim : sig
  type actor = t

  module Access : sig
    type t =
      | Running
      | Policy_only of Runtime_builder.Policy_access.t
      | Restored_unloaded of Policy_restoration.Published.t
  end

  module Capture : sig
    type t

    type disposition =
      | Waiting
      | Retained
      | Released

    (** Nonyielding same-domain association only, before the first claim RPC.
        Grants no effect or source permission. The owning host registers this
        exact capture with its actual resource loan before invoking the wrapper. *)
    val create : actor -> access:Access.t -> (t, Agent_protocol.Error.t) result

    (** Released requires the actual wrapper join, never cancellation alone. *)
    val disposition : t -> disposition

    (** One-shot host extent, before holder registration or Manager waits.
        Bracket all selected runtime use and the Actor wrapper; finally records
        genuine join on return/error/exception/cancellation. Reentry rejects
        without changing the owning extent. This grants no admission. *)
    val with_extent
      :  t
      -> (unit -> ('a, Agent_protocol.Error.t) result)
      -> ('a, Agent_protocol.Error.t) result

    (** Nonyielding negative guard for ONLY this same actual Policy_only callback.
        Requires Activated claim, Entered extent, current Product/source/Stop
        basis, live uncommitted/unretired borrow and its genuine original admission.
        Rejects real halt/failure, cancellation, terminal ACK, join and foreign
        Actor/domain. Grants no admission or permission; reads no Manager snapshot
        and forces no lifecycle callback. Original authorization's documented
        nonyielding guards remain mandatory; unexpected host exceptions propagate.
        Running captures reject. Used only by the trusted host's local tool view. *)
    val check_policy_effect_current : actor -> t -> (unit, Agent_protocol.Error.t) result

    val cancel_waiting : t -> unit
  end

  (** Same-domain factual state only. Pending claims exclude resource retirement. *)
  val pending : actor -> bool

  (** Call after the actual wrapper joins. Exact no-writer cleanup or the same
      callback terminal ACK releases ownership; unknown retains its first outcome
      and never resubmits the writer or executes a callback. *)
  val drain : actor -> (unit, Agent_protocol.Error.t) result
end

(** Actual child-issued governance transport. Captures are same-domain finite
    observations, not permission or decoded evidence. Private source/ancestry IO
    stays in Factory between capture and bind. Bind rechecks the exact physical
    child observation; later source checks permit its own factual Book ACK. *)
module Causal_governance : sig
  type actor = t
  type captured
  type t

  (** Freeze SAME actual Caller owner and complete immutable source/work/origin.
      Errors remain negative. Registered no-moderator callers require the genuine
    Builder-issued classification and SAME current Actor Binding; an old
    Caller/Bound cannot refresh after configuration or worker replacement.
    Capturing grants no parent admission or effect. *)
  val capture
    :  actor
    -> request:Script_tool_calls.preparation
    -> (captured, Agent_protocol.Error.t) result

  (** Only a genuine controller-free unrelated child may use this branch. No
      server identity, missing-custody fallback or governance scope is invented.
      Bound currentness retains the exact original Run/Unbound, subject and
      recorded observer/full source, alongside SAME issuer/domain/Caller owner.
      An observer-only factual association cannot authorize a native boundary. *)
  val bind_standalone : actor -> captured -> (t, Agent_protocol.Error.t) result

  (** Requires SAME actual Factory issuer and checked ancestry; None ancestry
      only for a genuine no-delegation root. Unknown inherited origin rejects.
      Does not invoke the Store refresh closure or run any source callback. *)
  val bind_registered
    :  actor
    -> captured
    -> source_owner:Runtime_source_owner.t
    -> ancestry:Delegation_authority.Checked_ancestry.t option
    -> (t, Agent_protocol.Error.t) result

  (** Nonyielding same-domain negative check of SAME actual Caller owner,
      immutable source/work/origin and full forwarding. No Store refresh or
      permission is supplied by the Bound. Its own factual Book ACK is allowed. *)
  val check_current : t -> (unit, Agent_protocol.Error.t) result
end

(** Source-stage only. Calls authorize outside Actor, then authenticates SAME
    Factory issuer, actual request/governance and unloaded construction basis.
    Source stage must capture the returned value before cancellation and repeat
    authorize/currentness after each wait. No worker/Manager is installed. *)
val begin_policy_restoration
  :  t
  -> source_owner:Runtime_source_owner.t
  -> source:Runtime_builder.Source.t
  -> basis:Policy_restore_basis.t
  -> request:Script_tool_calls.preparation
  -> governance:Causal_governance.t
  -> authorize:(unit -> (unit, Agent_protocol.Error.t) result)
  -> (Policy_restoration.t, Agent_protocol.Error.t) result

(** Adjacent to actual Cell publication under protected source-stage custody.
    Consumes construction exclusion into a separate actual Published association;
    no durable Init/lifecycle ACK is invented. Owner must own the first Cell lease
    before this RPC and retain it on every Error/exn. *)
val publish_policy_restoration
  :  t
  -> restoration:Policy_restoration.t
  -> access:Runtime_builder.Restored_policy_access.t
  -> (Policy_restoration.Published.t, Agent_protocol.Error.t) result

(** Closes still-Constructing only. A promoted Published association belongs to
    the actual Owner anchor and survives lexical source-stage retirement. *)
val end_policy_restoration
  :  t
  -> restoration:Policy_restoration.t
  -> (unit, Agent_protocol.Error.t) result

(** Same canonical history reservation/ACK under this actual construction. *)
val reserve_policy_restoration_history_block
  :  t
  -> restoration:Policy_restoration.t
  -> count:int
  -> (History_id_source.reservation, Agent_protocol.Error.t) result

(** Actual registered graph owner. Later factual callbacks retain exact owner
    membership; this opener grants no inference allocation or provider access. *)
val open_policy_restoration_inference_owner
  :  t
  -> restoration:Policy_restoration.t
  -> source:Transcript.Source_id.t
  -> (Inference_owner.t, Agent_protocol.Error.t) result

(** Trusted host handoff for a parent's live delegated policy check. [governance]
    must bind SAME physical preparation request and actual Caller. It is required
    even for an unrelated child; absence never selects a weaker path. [authorize]
    must validate the private relation and owned child invocation candidate after entering
    the parent gate. The handoff rechecks before entering [f], before and after
    native admission, before decision commit and before disclosing native outcomes
    or saved decisions. A denied disclosure retains the already committed outcome
    and checkpoint. [authorize] must be repeatable and must not run the policy
    handler itself. Native implementations must still check after their own
    internal approval waits; this wrapper cannot intercept arbitrary host code.
    The actor persists the actual parent installation and receipt together with
    its exact Cause seed or standalone Event classification before executing [f].
    Replays create no seed or original transfer. These metadata facts grant no
    provider admission; [commit] saves parent snapshot,
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
  -> capture:Delegated_policy_claim.Capture.t
  -> request:Script_tool_calls.preparation
  -> governance:Causal_governance.t
  -> delegation:Agent_protocol.Moderator_execution.delegation
  -> event:Chat_response.Moderation.Event.t
  -> authorize:(unit -> (unit, Agent_protocol.Error.t) result)
  -> snapshot:
       (unit
        -> (Session.Moderator_state.Identity_snapshot.t, Agent_protocol.Error.t) result)
  -> (executing:Agent_protocol.Moderator_execution.t
      -> policy_caller:Delegated_policy_caller.t option
      -> model_jobs:Model_job_admission.t
      -> allocate_inference:Inference_allocation.Port.t
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

(** Direct non-yielding currentness check. Retains the exact physical borrow,
    Manager callback epoch and source installation. Unavailable captures grant
    no original proof. Call again after any awaited authorization and at final
    submission; this check itself does not grant effect authority. *)
val check_callback_current
  :  t
  -> Callback_scope.t
  -> (unit, Agent_protocol.Error.t) result

(** Borrow the original proof only from this exact admission. A fresh legacy
    admission without an original object returns None; historical unavailable
    custody rejects. An expired original object rejects rather than becoming None.
    Returned proofs also expire with this callback/Manager/source. Queue custody
    transfer is supplied by producer integration, never the unrelated drainer. *)
val callback_agent_actor
  :  t
  -> scope:Callback_scope.t
  -> (Operator_authorization.t option, Agent_protocol.Error.t) result

(** Native Tool executor for this exact callback and its already claimed Event.
    Constructing it grants no authority. Every invocation rechecks the physical
    Manager/callback, source and original admission before effect dispatch.
    Historical unavailable custody rejects; genuine admitted None remains distinct.
    Claims and truthful outcomes advance only their exact constructor Invocation
    ACK projection. Model child admission issued to this exact constructor's
    native invocation retains the same Init/preparation and selects the child
    only after its actual Job ACK, before publication. Final inference checks
    use nonyielding original/currentness predicates; host policy refresh belongs
    to the earlier admission/refresh phase. Returned outcomes do not renew
    expired effect authority. *)
val callback_tool_executor
  :  t
  -> scope:Callback_scope.t
  -> Native_tool_invocation.executor

(** Opaque Caller view of this SAME actual legacy callback. Its original is
    checked only when preparing/consuming a delegated policy call; creation does
    not authorize effects or veto unrelated factual callback completion. *)
val callback_policy_caller
  :  t
  -> scope:Callback_scope.t
  -> (Delegated_policy_caller.t, Agent_protocol.Error.t) result

(** Model Job admission for this exact installed callback only. Each admission
    rejects constructor scopes, rechecks the physical callback/Manager/source and
    original authority, then uses the actual Event parent. An escaped port cannot
    retarget to a later callback or fall back to initialization services. This
    grants neither synchronous constructor execution nor permission from IDs. *)
val callback_model_jobs : t -> scope:Callback_scope.t -> Model_job_admission.t

(** Immediate legacy Schedule services bound to this exact callback. Constructing
    the record grants no authority: each call rechecks the actual Manager/source,
    physical borrow and original admission at actor persistence. Unavailable or
    expired custody rejects; genuine admitted None retains legacy behavior.
    after_ms preserves nonnegative checked wall-clock delay and raw JSON payload,
    and atomically records creator evidence plus an independent live loan before
    publication. Failed ACK discards the speculative loan; owner exit retires it.
    cancel preserves cancellation of any retained Scheduled ID, including a
    historical target without provenance or custody. This is not a delivery or
    provider port. The Manager binding owns Snapshot/value conversion and must
    not fall back to constructor services when this scope is unavailable. *)
val callback_schedule_services
  :  t
  -> scope:Callback_scope.t
  -> Runtime_builder.schedule_services

(** Admit a genuine Boundary or Queue_head selection from the exact installed
    Manager. Boundaries use the real active operation, or the one acknowledged
    startup/resume continuation on an idle actor. Internal events require an exact
    Queue_head; callers cannot select a relaxed mode or supply proof constructors.
    Claim ACK installs the callback loan before publication. Failed/uncertain save
    discards speculative custody; post-ACK errors do not replay the callback.
    A committed fresh emitted suffix receives independent loans from this exact
    admitted callback. Historical or recovered rows remain unavailable; the
    drainer never supplies missing custody. Emitted loans install before publish
    and remain acknowledged if publication later fails. *)
val with_legacy_callback
  :  t
  -> operation_id:Agent_protocol.Id.Operation.t option
  -> legacy_callback_claim

(** Claim this actual constructor's one Session_start or Session_resume from
    its exact active Manager callback. The same Pending/Ready scope, immutable
    control basis and original admission fence every effect. Claim/source/metadata
    and receipt commit together; old queue rows remain unavailable. An initial
    Run retains its actual issued family: immutable Goal episode/link/Admission
    or reciprocal recurrence occurrence. The callback and native children keep
    that genuine Run attribution; metadata cannot supply an absent owner.
    A rejected claim ACK leaves the one-shot phase unchanged; executed failure
    is not replayed.
    True means checkpoint and cleanup completed; this owner never claims a raw
    Queue_head or lends permission from stored metadata. Actual host routing is a
    separate consumer of this CPS interface. *)
val with_initialization_callback
  :  t
  -> scope:Initialization_scope.t
  -> manager:Chat_response.Moderator_manager.t
  -> legacy_callback_claim

(** Claim the exact legacy Queue_head selected by the installed opaque Manager.
    True means the callback committed its checkpoint and cleanup completed; false
    means the actual physical actor was unavailable. Empty queues do not invoke
    this wrapper. The existing claim/failed/interrupted no-replay cleanup owns
    the whole CPS lifetime. First capture atomically installs only source facts,
    structural queue metadata and the real claim receipt, without creating a Run
    or inferred Unbound origin. Generic/v1 handoffs retain strict envelopes.
    Fresh emitted suffixes name their actual completed Event and retain its
    original nullable attribution. Only the process-local acknowledged row loan
    can lend continuation custody; stored captures alone cannot. Legacy queue
    rows never match payload-shaped Run wakes or borrow Run action services;
    unavailable custody rejects turn and compaction requests rather than
    downgrading to legacy authority.
    End-session and truthful failure/cleanup remain available. *)
val with_legacy_queued_callback
  :  t
  -> ?capture_cleanup:(Callback_cleanup.Completion.t -> unit)
  -> callback:Chat_response.Moderator_manager.Callback.t
  -> operation_id:Agent_protocol.Id.Operation.t option
  -> (scope:Callback_scope.t
      -> commit:
           (snapshot:Session.Moderator_state.Identity_snapshot.t
            -> requests:Agent_protocol.Invocation.follow_up
            -> notifications:string list
            -> (unit, Agent_protocol.Error.t) result)
      -> (unit, Agent_protocol.Error.t) result)
  -> (bool, Agent_protocol.Error.t) result

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

    Completed producers may lend their exact original admission through the
    actor's bounded process-local continuation inventory. Claim ACK transfers that
    loan once; it never borrows a later draining operation's admission. Reopened
    historical absence remains unavailable, genuine admitted None stays distinct,
    and expired original custody cannot authorize fresh Model or follow-up work.
    Pure observation outcome/cleanup still settles truthfully. No receipt recreates
    a loan, and failed claim persistence leaves the original loan unconsumed.

    The callback must prospectively commit the observation acknowledgement and
    moderator snapshot together. Use [retain_follow_up] in the manager so runtime
    requests survive until a host applies them durably after releasing ownership.
    Failure/cancellation records observation failure without altering native
    results; successful acknowledgements are never replayed. This includes
    cancellation arriving during claim persistence before callback entry; the
    original cancellation exception is preserved. The callback does
    not acquire foreground native-tool authority. This API does not schedule a
    turn, install a wakeup or apply retained follow-up requests. *)
val with_idle_moderator_observation
  :  t
  -> observer:Agent_protocol.Invocation.observer
  -> (observing:Agent_protocol.Invocation.t
      -> capabilities:Invocation_capabilities.t
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
      -> capabilities:Invocation_capabilities.t
      -> execute:Native_tool_invocation.executor
      -> commit:
           (resolved:Agent_protocol.Invocation.t
            -> snapshot:Session.Moderator_state.Identity_snapshot.t
            -> (unit, Agent_protocol.Error.t) result)
      -> (unit, Agent_protocol.Error.t) result)
  -> (bool, Agent_protocol.Error.t) result

(** Process-local owner for the aggregate legacy moderator callback. Create
    before entering the host's finite cancellation-protected claim/drain/capture
    block. A successful claim is retained by the Actor before the reply; this
    value grants no source, Run or child-creation permission. Other moderator
    families keep their existing borrow protocols. All operations belong to the
    actual Actor domain. Result-returning operations reject a foreign domain;
    [cancel] and [leave_capture] do nothing before touching the cell when called
    from a foreign domain: neither cancellation nor capture-exit proof is then
    installed. *)
module Idle_moderator_completion : sig
  type actor = t
  type t

  val create : actor -> t
  val claim : t -> (History_entry.t list option, Agent_protocol.Error.t) result

  (** Capture the actual callback drain exactly once. Ordinary completion uses
      the existing writer; a governed Turn retains its exact selected continuation
      for [finish], without waiting on ancestors in this finite capture block.
      Retains each real selected writer and State ACK, including no-Turn metadata,
      Goal Hold and compaction prefixes.
      An attempted write without an installed ACK remains uncertain; no callback
      replay or global failure rewrite is performed. *)
  val capture
    :  t
    -> Runtime_builder.moderator_drain
    -> (unit, Agent_protocol.Error.t) result

  (** Actual callback-returned error before capture. Failure metadata has its
      own real attempted/ACK outcome and cannot erase an uncertain completion. *)
  val fail_callback
    :  t
    -> failure:Agent_protocol.Error.t
    -> (unit, Agent_protocol.Error.t) result

  (** Non-yielding one-shot exit signal in the finite host block's finally.
      Does not release the claim or assert that a writer was unattempted. *)
  val leave_capture : t -> unit

  (** After leaving the finite block, return the cached ordinary result or drive
      the same governed Turn through reservation, canonical ACK, confirmation and
      its exact worker suffix. Call outside mailbox and Owner state locks.
      The first caller owns this drive; concurrent callers only join its result,
      and cancelling a joining caller does not cancel the driver. All later calls
      replay that exact result or exception (with its backtrace), including after
      successful handoff removes the Actor's continuation. Cancelling the driver
      retains exact factual cleanup custody; it does not replay the callback. *)
  val finish : t -> (unit, Agent_protocol.Error.t) result

  (** First-wins negative cancellation, followed by Actor-owned factual cleanup.
      No RPC is made synchronously. Cleanup never retries an uncertain writer.
      Fiber-setup failure is retained on the same owner and cannot replace the
      original callback error or cancellation; [join_retirement] reports it. *)
  val cancel : t -> failure:Agent_protocol.Error.t -> unit

  (** Join the same retirement outside mailbox and Owner state locks. A failed
      or unknown writer retains the exact owner and primary diagnostic; source
      close must propagate the failure and retain its resources. *)
  val join_retirement : t -> (unit, Agent_protocol.Error.t) result
end

(** [claim_idle_moderator] acquires the process-local exclusive moderator
    borrow only when the running session is idle and unblocked. A prospective
    governed admission leaves the borrow unavailable until its actual native ACK
    or retirement releases that occupancy. Existing callback completion and
    factual ACK/cleanup remain available. *)
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

(** Trusted host startup policy. An actor without its actual durable writer
    owner always reports disabled capacity. *)
val set_context_capacity_host
  :  t
  -> Session_context_capacity.Host.t
  -> (unit, Agent_protocol.Error.t) Result.t

(** Safe status bound to an exact running operation of this local owner.
    Reading after retirement fails. The native caller must separately admit
    the invoking tool and recheck any durable transition claim. *)
val context_capacity
  :  t
  -> operation_id:Agent_protocol.Id.Operation.t
  -> Session_context_capacity.Port.t

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
    Save_only invokes no worker/preparation/recovery port. No-Start paths stay
    local and perform no ancestor wait. A positive inherited Start retains its
    exact archive, prepared history and selected worker while reserving roots
    outside the Actor mailbox and ownership locks. The actual writer repeats
    attachment/lease, session/generation/revision and source checks.
    The native ACK installs the operation's unconfirmed guard before publication;
    complete ancestor confirmation admits that same guard before worker effects.
    History handoff rejects a changed selected worker after ACK; its accepted
    history and Included accounting remain factual. Publication precedes launch,
    within the protected ACK handoff, without masking external waits.
    Started means admitted host operation, not provider dispatch. A disconnected
    client cannot roll back acknowledged history; owner interruption uses the
    existing operation/receipt reconciliation. *)

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

(** Actual original operator command proof, current Send_messages and non-yielding
    host write policy, checked before receipt lookup and fresh commit. The actor
    computes exact input identity and commits catalog/receipt/source installation
    atomically through ordinary persistence. Safe Hold/Abandon need no moderator.
    No Run is enabled, cancelled or scheduled. Saturation fails fresh keys closed;
    exact retained evidence remains queryable after source/generation changes. *)
val goal_command
  :  t
  -> actor:Operator_authorization.t
  -> authorize:(Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> session:Agent_protocol.Session_ref.t
  -> request:Goal_command.t
  -> (Goal_command_receipt.t, Agent_protocol.Error.t) result

(** Current original View_session_transcript proof and host visibility. Stored
    creator/principal IDs cannot authorize reads. No runtime is constructed. *)
val read_goals
  :  t
  -> actor:Operator_authorization.t
  -> authorize:(Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> (Agent_protocol.Goal.t list, Agent_protocol.Error.t) result

(** Opt in with the original current writer and host visibility check. The
    actor records exactly the settled canonical view under its durable Store's
    actual host-qualified owner. Repeating enablement returns the current scope
    without a second journal delta. Legacy archive windows remain unavailable. *)
val enable_recovery_windows
  :  t
  -> actor:Operator_authorization.t
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> authorize:(Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> (Agent_protocol.Recovery_window.Scope.t, Agent_protocol.Error.t) result

(** The bound durable actor checks original owned-session authority in its mailbox.
    Exact note bytes travel only on this requested result. Listing pairs metadata
    with the current catalog revision needed by a subsequent create. *)
val read_recovery_note
  :  t
  -> access:Recovery_note_access.t
  -> Recovery_notes.Catalog.selection
  -> (Recovery_notes.Catalog.Content.t, Agent_protocol.Error.t) result

val list_recovery_notes
  :  t
  -> access:Recovery_note_access.t
  -> ( Recovery_notes.Catalog.Catalog_revision.t * Recovery_notes.Catalog.metadata list
       , Agent_protocol.Error.t )
       result

(** Process-local result from an actual protected model call. It pins the
    operation's physical custody and the exact note or catalog revision. The
    callbacks do not retain the callback scope, body or catalog. The Factory
    checks registry ownership and reserves the source for provider submission. *)
type recovery_note_delivery = private
  { source : Agent_protocol.Session_ref.t
  ; materialize : unit -> (Jsonaf.t, Agent_protocol.Error.t) result
  ; check_current : unit -> bool
  }

val prepare_recovery_note_query
  :  t
  -> access:Recovery_note_access.t
  -> Recovery_note_query.t
  -> (recovery_note_delivery, Agent_protocol.Error.t) result

(** A current writer and original host visibility are checked before receipt
    lookup and at the actual writer. The receipt is returned after the publish
    boundary. A replay performs no Store mutation. *)
val mutate_recovery_note_operator
  :  t
  -> access:Recovery_note_access.t
  -> key:Agent_protocol.Idempotency_key.t
  -> Recovery_notes.Catalog.mutation
  -> (Recovery_notes.Catalog.Receipt.t, Agent_protocol.Error.t) result

(** Uses the actual checked native invocation identity as the receipt key.
    A current token for the selected effect and the actor's physical executing
    invocation are required. *)
val mutate_recovery_note_native
  :  t
  -> access:Recovery_note_access.t
  -> Recovery_notes.Catalog.mutation
  -> (Recovery_notes.Catalog.Receipt.t, Agent_protocol.Error.t) result

(** Canonical two-session note sharing. These functions are for the trusted host.
    [check_owner] must be a nonyielding check of this exact physical Store and
    Registry association on its creating domain. It supplies no caller authority. *)
module Recovery_note_shares : sig
  type actor = t
  type check_owner = unit -> (unit, Agent_protocol.Error.t) result
  type reader
  type target
  type disclosure

  val management_revision
    :  actor
    -> access:Recovery_note_access.t
    -> check_owner:check_owner
    -> (Recovery_note_share.Revision.t, Agent_protocol.Error.t) result

  (** Idempotently bind this actual durable target before entering the source
      Actor. Establishes only an owner incarnation, never a local or incoming
      grant. For a trusted host after source management admission. *)
  val establish_target
    :  actor
    -> check_owner:check_owner
    -> (target, Agent_protocol.Error.t) result

  val create
    :  actor
    -> access:Recovery_note_access.t
    -> check_owner:check_owner
    -> target:target
    -> key:Agent_protocol.Idempotency_key.t
    -> Recovery_note_share.create
    -> (Recovery_note_share.Receipt.t, Agent_protocol.Error.t) result

  val revoke
    :  actor
    -> access:Recovery_note_access.t
    -> check_owner:check_owner
    -> key:Agent_protocol.Idempotency_key.t
    -> Recovery_note_share.revoke
    -> (Recovery_note_share.Receipt.t, Agent_protocol.Error.t) result

  (** Capture the exact recipient, durable incarnation and original current
      scoped authority without entering a second mailbox. *)
  val reader
    :  actor
    -> access:Recovery_note_access.t
    -> check_owner:check_owner
    -> (reader, Agent_protocol.Error.t) result

  (** Fresh protected provider-input admission for this actual running foreground
      operation. Checks its physical current operation custody and original
      authenticated transcript scope on each use, even after a native callback
      ended. A persisted operation coordinate alone grants nothing. *)
  val reader_for_delivery
    :  actor
    -> operation_id:Agent_protocol.Id.Operation.t
    -> check_owner:check_owner
    -> (reader, Agent_protocol.Error.t) result

  (** Explicit one-grant selection on the source actor. The result is inert
      until [materialize], and carries no plaintext. *)
  val select
    :  actor
    -> check_owner:check_owner
    -> reader:reader
    -> Recovery_note_share.Reference.t
    -> (disclosure, Agent_protocol.Error.t) result

  (** Trusted Factory only. Run outside the source mailbox with actual Registry
      read custody already held. A same-domain reservation excludes canonical
      writes that change protected source state and rejects a pending writer.
      The caller bounds the callback duration and releases Registry custody on
      return, exception or cancellation. The supplied check is synchronous. *)
  val with_source_custody
    :  actor
    -> check_owner:check_owner
    -> f:((unit -> bool) -> 'a)
    -> ('a, Agent_protocol.Error.t) result

  (** Synchronous exact note selection within source custody. Entry,
      recipient and grant admission are still checked on every use. *)
  val select_for_delivery
    :  actor
    -> check_owner:check_owner
    -> reader:reader
    -> Recovery_note_share.Reference.t
    -> (disclosure, Agent_protocol.Error.t) result

  (** Final synchronous two-owner check and exact note lookup. Invoke within
      the source's retained ownership lifetime immediately before publication
      with no intervening yield or additional untrusted callback. *)
  val materialize
    :  disclosure
    -> (Recovery_notes.Catalog.Content.t, Agent_protocol.Error.t) result
end

(** Actual admitted native effect and original execution epoch are checked in
    the mailbox before CAS and immediately before persistence. Replacement needs
    a genuine live source-use capture; current-root-only retries may return only
    an exact previously acknowledged receipt. No transcript row is appended. *)
val instruction_command
  :  t
  -> admission:Instruction_native_admission.t
  -> source:Instruction_command_admission.Source.t option
  -> request:Instruction_command.t
  -> (Instruction_command.Receipt.t, Agent_protocol.Error.t) result

(** Same sole instruction transaction owner for original-operator and native
    composition. Current original authority, target generation and real writer
    attachment (operator writes only) precede receipt lookup and persistence.
    Replacements require the same live source Use as the native route. *)
val instruction_command_admitted
  :  t
  -> admission:Instruction_admission.t
  -> source:Instruction_command_admission.Source.t option
  -> request:Instruction_command.t
  -> (Instruction_command.Receipt.t, Agent_protocol.Error.t) result

(** Nonyielding immutable selection/generation epoch comparison for a snapshot
    read from this actor. This grants no source or execution permission. The
    owner separately retains original physical execution and source Use checks. *)
val instruction_selection_current : t -> Session_state.t -> bool

(** Install the actual store/retained Handle owner once during host factory
    construction. This is metadata custody, never target execution authority. *)
val set_recurrence_controller
  :  t
  -> Recurrence_controller.t
  -> (unit, Agent_protocol.Error.t) result

(** Original operator proof and current host write policy precede receipt
    disclosure and persistence. Capture/enable also require genuine current
    Active+Automatic lifecycle and actual installed source. A fresh command
    reconciles stale births before CAS. A retained key is classified first
    without reconciliation. Every new write checks original authority at the
    canonical writer, including an unbound operator Pause or Retire.
    Safe Pause/Retire require no installed moderator. No target is launched. *)
val recurrence_command
  :  t
  -> actor:Operator_authorization.t
  -> authorize:(Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> request:Recurrence_command.t
  -> (Recurrence_command_receipt.t, Agent_protocol.Error.t) result

(** Borrowed metadata command. The caller provides the actual host refresh
    on the native caller fiber. New captured definitions compare the exact
    invocation and Run-owner attribution again at the Actor writer. *)
val recurrence_native_command
  :  t
  -> admission:Recurrence_native_admission.t
  -> authorize:(Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> refresh:(unit -> (unit, Agent_protocol.Error.t) result)
  -> request:Recurrence_command.t
  -> (Recurrence_command_receipt.t, Agent_protocol.Error.t) result

module Recurrence_target_invalidation : sig
  (** Local factual evidence. The retained Remove fact belongs to the host Store.
      A source observation belongs to the exact current target Actor. Neither
      authorizes or launches an execution. *)
  type actor = t

  type t

  val removed
    :  controller:actor
    -> Agent_store.Session_store.Command_receipts.Removal.t
    -> Recurrence_definition.t
    -> (t option, Agent_protocol.Error.t) result

  (** Check the exact Actor's current target identity and demonstrate a monotone
      captured-source change. Both constructors bind the actual controller
      Actor and require the same live Store. *)
  val changed_source
    :  controller:actor
    -> Resource_observation.t
    -> Recurrence_definition.t
    -> (t option, Agent_protocol.Error.t) result
end

(** Persist the existing safety pause on exactly this enabled revision. The
    original host policy and target evidence are checked again at the canonical
    writer edge. Unadmitted decisions are withdrawn, with attempt custody intact. *)
val pause_invalid_recurrence_target
  :  t
  -> actor:Operator_authorization.t
  -> authorize:
       (Session_state.t
        -> Recurrence_definition.t
        -> (unit, Agent_protocol.Error.t) result)
  -> definition:Recurrence_identity.Definition.t
  -> expected_control_revision:int64
  -> evidence:Recurrence_target_invalidation.t
  -> (unit, Agent_protocol.Error.t) result

module Recurrence_creation : sig
  (** Process-local permit issued only by this actor after actual reservation ACK.
      No constructor, decoder, ID lookup or creator equality can recreate it.
      Original caller, current policy, source, birth and control remain required;
      this is not permission to activate a child or admit a Run. *)
  type t

  (** Immutable evidence only. Reading it does not refresh permission. *)
  val reservation : t -> Recurrence_child_reservation.t

  module Checked : sig
    (** Process-local prepared effect view. No constructor or decoder. *)
    type t

    (** Immutable captured data only; this getter grants no authority. *)
    val definition : t -> Recurrence_definition.t

    (** Actual caller retained by the refreshed proof. This is immutable policy
        input only; identity alone cannot authorize an effect. *)
    val principal : t -> Agent_protocol.Principal.t

    (** Synchronous, nonyielding final check against the actual issuing actor,
        original proof and host policy, genuine lifecycle/source/control, exact
        protected row, runtime liveness and current grace. Call after all yielding
        preparation immediately before each owned mutation. A successful check
        grants nothing across a subsequent yield or fence; refresh through the
        actor before the next phase. Unexpected trusted guard exceptions propagate. *)
    val check_current : t -> (unit, Agent_protocol.Error.t) result
  end
end

(** Atomically move an exact Ready fresh-child nominal into protected custody, or
    reacquire its original reservation after fresh authorization. Requires real
    current Create_sessions and exact host target policy, genuine lifecycle,
    installed source, enabled original control and current grace. A different
    freshly authorized principal may reauthorize: the original principal remains
    immutable audit data. Failed/uncertain persistence publishes no permit and
    uses the existing whole-writer recovery fence, never replacement identities.
    The trusted [authorize] callback must not yield and must check current host
    target policy; it is retained and repeated at every final effect boundary.
    No child/artifact install, link, timer or Run is created here. *)
val reserve_recurrence_child
  :  t
  -> actor:Operator_authorization.t
  -> authorize:
       (Session_state.t
        -> Recurrence_definition.t
        -> (unit, Agent_protocol.Error.t) result)
  -> occurrence:Recurrence_identity.Occurrence.t
  -> expected_control_revision:int64
  -> (Recurrence_creation.t, Agent_protocol.Error.t) result

(** Re-enter the exact issuing actor before each next factory phase and after
    yielding preparation. Rechecks original proof/current host policy, fresh
    genuine Current, exact retained row/source/birth/control, runtime and grace.
    A foreign actor, decoded row, expired proof or stale frontier cannot refresh
    permission. Returning through this RPC is itself a scheduling boundary:
    invoke [Recurrence_creation.Checked.check_current] after all yielding
    preparation immediately before the actual owner's mutation. No authority is
    held across a later yield or fence. *)
val check_recurrence_creation
  :  t
  -> Recurrence_creation.t
  -> (Recurrence_creation.Checked.t, Agent_protocol.Error.t) result

(** Record the exact selected occurrence and original attempt zero through the
    actual controller transaction. Requires fresh Send_messages and current host
    policy for the exact retained definition in both branches. [authorize] must
    not yield. Only a new record additionally requires a running ready controller,
    current automatic birth/source, enabled control, current selection grace and
    the actual caller as attempt principal. Exact retained original data remains
    available after control/source changes under fresh policy, without a new
    transaction. The original principal/control revision remain audit facts.

    Returns immutable request data only after ACK and fresh reauthorization;
    neither target visibility/execution permission, retry proof nor activation
    authority is issued. Failed or unknown ACK never replaces request identities.
    No target runtime is loaded, source installed or Run admitted. *)
val record_recurrence_execution
  :  t
  -> actor:Operator_authorization.t
  -> authorize:
       (Session_state.t
        -> Recurrence_definition.t
        -> (unit, Agent_protocol.Error.t) result)
  -> intent:Recurrence_execution_intent.t
  -> attempt:Recurrence_execution_attempt.t
  -> (Recurrence_execution.t, Agent_protocol.Error.t) result

(** Self-target recording under the exact Selected source/preparation owner.
    Before callbacks, requires this actual actor and full controller and target
    Session_refs and the unchanged original attempt request/principal/CAS/key/digest. Current
    controller policy/source/grace still apply. Only its exact acknowledged
    record advances that preparation; duplicate DATA neither advances nor
    reattaches custody. Fresh grace clocks precede final original-authorization
    checks and callback-free custody bases. No target Run or execution authority
    is issued. *)
val record_prepared_recurrence_execution
  :  t
  -> binding:Source_binding.t
  -> actor:Operator_authorization.t
  -> authorize:
       (Session_state.t
        -> Recurrence_definition.t
        -> (unit, Agent_protocol.Error.t) result)
  -> intent:Recurrence_execution_intent.t
  -> attempt:Recurrence_execution_attempt.t
  -> (Recurrence_execution.t, Agent_protocol.Error.t) result

module Recurrence_run_observation : sig
  (** Actual target-owner observation of retained immutable Run evidence. No
      constructor or decoder; native/decoded origin/reference data cannot mint
      this value. The owning Store and exact target remain attached privately,
      together with validation of saved extended terminal references against the
      actual retained payload and any process-local completion clock sample.
      A present invalid live clock is distinct from recovery absence. *)
  type t

  val origin : t -> Recurrence_run_origin.t
  val terminal : t -> Recurrence_run_reference.t option

  (** Synchronous, nonyielding original-read-policy, actual owner/lifecycle and
      exact retained evidence check. Returning through the observation RPC is a
      scheduling boundary: invoke this after yielding preparation immediately
      before a separate owned effect. It grants nothing across a later yield. *)
  val check_current : t -> (unit, Agent_protocol.Error.t) result
end

module Admitted_activation : sig
  module Checked : sig
    (** Actor-issued association for one actual admitted Run and its original
        Admission. No constructor, decoder or caller-supplied validator. A view
        retains its concrete issuer's live custody and one consumption cell. *)
    type t

    (** Complete current policy, timing, original and final pure-basis check.
        Trusted callbacks are nonyielding; exceptions propagate. A successful
        check grants no permission across a yield. *)
    val check_current : t -> (unit, Agent_protocol.Error.t) result

    val check_policy : t -> (unit, Agent_protocol.Error.t) result
    val check_timing : t -> (unit, Agent_protocol.Error.t) result
    val check_authority : t -> (unit, Agent_protocol.Error.t) result

    (** Callback-free actual association/owner/Run/source and Store basis.
        Stale or consumed custody rejects before any stored host callback. *)
    val check_basis : t -> (unit, Agent_protocol.Error.t) result
  end

  (** Fresh effect checks require current issuer policy. An actual registered
      inference allocation uses its Actor-owned same-registration submission
      guard; automatic admission holds stop new work without re-admitting or
      cancelling that allocation. This distinction grants no caller bypass. *)
  module Continuation : sig
    (** One exact consumed Checked/Held link. The actual admitted association
        remains live through construction and acknowledged handoff; no returned
        DATA, source identifier or historical receipt reconstructs this owner. *)
    type t

    val target : t -> Held_run.Activation.t

    (** Refresh actual Store/Handle observations and concrete issuer policy.
        May perform Store I/O. Call outside Actor mailboxes and Owner mutexes. *)
    val refresh : t -> (unit, Agent_protocol.Error.t) result

    (** Trusted nonyielding policy callbacks with pure preflight/final bases.
        Repeat at the fresh effect boundary, then timing, originals and both
        final pure bases without another yield or supplied callback. *)
    val check_policy : t -> (unit, Agent_protocol.Error.t) result

    (** The actual association issuer's fresh timing obligations; not a lasting
        timing certificate. Original guards and final pure bases follow. *)
    val check_timing : t -> (unit, Agent_protocol.Error.t) result

    val check_authority : t -> (unit, Agent_protocol.Error.t) result
    val check_basis : t -> (unit, Agent_protocol.Error.t) result
  end
end

(** Actual Goal episode admission and neutral activation through the issuing
    actor, its selected source and original command custody. *)
module Goal_activation : sig
  (** Real ACK-only same-actor Goal episode/Run association and original command
      custody. No constructor/decoder or supplied validation callback. *)
  type t

  val held : t -> Held_run.t

  (** SAME live one-shot association; not a new issue on each call. Invalidation
      before pause/source/control writer attempts is monotone, even after resume. *)
  val checked : t -> Admitted_activation.Checked.t

  module Admission : sig
    type activation = t

    type t =
      | Rejected of Agent_protocol.Error.t
      | Uncertain of Agent_protocol.Error.t
      | Retained of Agent_protocol.Run_receipt.t
      | Activation_unavailable of
          { admission : Agent_protocol.Run_receipt.t
          ; error : Agent_protocol.Error.t
          }
      | Admitted of activation
  end
end

(** Authenticate actual selected Source_binding/preparation and its retained
    original command. Validate exact goal intent/current source/Goal creator,
    Workflow request, CAS, capacity and existing accounting. Atomically commit
    Enabled policy, original new Run/Admission and immutable episode/link. The
    request comes ONLY from preparation; no optional Goal on ordinary writers.
    Captured actual writer-attempt evidence governs Rejected vs Uncertain. ACK
    installs inert association before publication; no input/native/initializer.
    Activation_unavailable preserves that known admission and exact live-issuance
    error; Retained reports only an exact preexisting receipt lookup. *)
val admit_held_goal_run
  :  ?writer:Agent_protocol.Id.Attachment.t
  -> t
  -> binding:Source_binding.t
  -> intent:Goal_episode_admission.intent
  -> session:Agent_protocol.Session_ref.t
  -> (Goal_activation.Admission.t, Agent_protocol.Error.t) result

(** Actual command+read lookup before acquiring any source/runtime. Derive
    original principal/key/digest from authenticated command and exact request;
    require exact immutable Run_goal_link intent and episode association. Missing
    DATA does NOT prove no effects and grants no permission. Does not create custody. *)
val goal_run_receipt
  :  ?writer:Agent_protocol.Id.Attachment.t
  -> t
  -> actor:Operator_authorization.t
  -> authorize:(Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> request:Goal_activation_request.t
  -> session:Agent_protocol.Session_ref.t
  -> (Agent_protocol.Run_receipt.t option, Agent_protocol.Error.t) result

module Goal_continuation : sig
  module Preparation : sig
    (** Actual registered existing-episode request custody under this Actor's
        private preparation owner. No constructor, decoder or permission from
        Run/source IDs. The original Run binding remains independently required. *)
    type t

    (** Factual known command ACK in the issuing Eio domain, retained even after
        negative closure or a publication exception. This neither reauthorizes
        nor rebuilds custody. A foreign-domain call raises Invalid_argument. *)
    val receipt : t -> Goal_command_receipt.t option
  end

  module Source_binding : sig
    (** One actual source selection bound to the SAME preparation and old Run
        installation. This cannot replace initial Source_binding or Held_run. *)
    type t

    val source : t -> Runtime_source_preparation.t
    val check_basis : t -> (unit, Agent_protocol.Error.t) result

    (** Same-domain irreversible local fresh-source revocation before yielding
        resource joins. Does not erase an in-flight writer's known ACK or close
        factual preparation custody; the Actor release runs after joined cleanup.
        Idempotent; a foreign-domain call raises Invalid_argument. *)
    val close : t -> unit
  end

  (** Known command-ACK owner over the SAME existing Run binding, Admission,
      episode and account. Source/runtime execution still requires the actual
      construction consumer; receipt DATA alone cannot recreate this owner. *)
  type t

  val receipt : t -> Goal_command_receipt.t

  (** Same-domain actual physical source-pair validation. This grants no fresh
      execution and never infers association from matching source identifiers. *)
  val check_source_binding
    :  t
    -> binding:Source_binding.t
    -> (unit, Agent_protocol.Error.t) result

  module Resume_issuance : sig
    type t =
      | Retained of Goal_command_receipt.t
      | Prepare of Preparation.t
  end

  module Admission : sig
    type continuation = t

    type t =
      | Retained of Goal_command_receipt.t
      | Waiting of
          { receipt : Goal_command_receipt.t
          ; wake : Agent_protocol.Run_wake.t
          }
      | Ready of continuation
      | Rejected of Agent_protocol.Error.t
      | Uncertain of Agent_protocol.Error.t
      | Resume_failed of
          { receipt : Goal_command_receipt.t
          ; error : Agent_protocol.Error.t
          }
  end
end

module Goal_execution_runtime : sig
  module Handoff : sig
    (** Factual outcome selected by the actual Actor owner, never by a caller's
        callback return. These values grant no execution or resource authority. *)
    type t = private
      | Queued_event of Agent_protocol.Moderator_execution.t
      | Subscription_turn of Agent_protocol.Operation.t
      | Claimed_job_resource of Agent_protocol.Job.t
      | Lifecycle_wait of Agent_protocol.Run_wake.t
      | Lifecycle_settlement of Agent_protocol.Goal_episode.Settlement.t
  end

  module Preparation : sig
    type t

    (** Same-domain historical actual transfer. Known ACK survives closure or
        publication failure. None is not a no-effects or no-writer certificate. *)
    val handoff : t -> Handoff.t option
  end

  module Source_binding : sig
    type t

    val source : t -> Runtime_source_preparation.t
    val check_basis : t -> (unit, Agent_protocol.Error.t) result

    (** Pure irreversible revocation of fresh use; preserves custody, in-flight
        writers and known ACK. Factual Actor close follows all actual joins. *)
    val close : t -> unit
  end

  type t

  module Kind : sig
    type t =
      | Queued_wake
      | Subscription_wake
      | Claimed_job
  end

  val kind : t -> Kind.t

  (** Exact immutable wake DATA from this owner. Claimed Job/Resume rejects.
      Creating Eio domain only; no readiness or execution permission. *)
  val wake : t -> (Agent_protocol.Run_wake.t, Agent_protocol.Error.t) result

  (** Physical source pair, issuing Actor/preparation and owner identity, not IDs. *)
  val check_source_binding
    :  t
    -> binding:Source_binding.t
    -> (unit, Agent_protocol.Error.t) result

  module Wake_request : sig
    (** Read-only production routing DATA. Exact canonical record comparison is
        mandatory; a stale/foreign DTO is Error, never Ordinary fallback. *)
    type t =
      | Idle
      | Timer of Agent_protocol.Schedule.t
      | Job_delivery of Run_job_delivery.t
      | Job_completion of Agent_protocol.Job.t
  end

  module Wake_selection : sig
    type t =
      | Ordinary
      | No_work
      | Suspended_goal of Goal_execution_selection.Suspension.t
      | Wake of Agent_protocol.Run_wake.t
  end

  module Wake_issuance : sig
    type t =
      | No_work
      | Suspended_goal of Goal_execution_selection.Suspension.t
      | Prepare of Preparation.t
  end

  module Wake_result : sig
    (** Returned only for actual acknowledged ownership or a joined lifecycle
        outcome; ready-but-unclaimed is not Claimed. No new Administrative turn. *)
    type t = private
      | Queued of Agent_protocol.Moderator_execution.t
      | Started of Agent_protocol.Operation.t
      | Waiting of Agent_protocol.Run_wake.t
      | Settled of Agent_protocol.Goal_episode.Settlement.t
      | Controlled of Goal_callback_control.t
  end
end

(** Callback-free negative routing before any ordinary cold load. Reuses the
    real producer/Goal selected-work planner, does no queue/claim/write or issuing.
    Idle selects only actual next eligible canonical head/committed wake, not an
    arbitrary later head. Ordinary permits existing ordinary path only after an
    exact unbound/unrelated selection; it never means stale proof or read failure.
    Source Begin repeats every check after actual Owner reservation. *)
val select_goal_runtime_wake
  :  t
  -> request:Goal_execution_runtime.Wake_request.t
  -> (Goal_execution_runtime.Wake_selection.t, Agent_protocol.Error.t) result

(** After SAME Owner source-stage reservation, select exact pending Wait + equal
    Waiting wake, actual live binding/Admission/episode/link/full source and real
    producer readiness. Job must be canonical terminal/enqueued; timer actually
    due and owned/Delivering or enqueued; subscription actually committed and
    Pending_wake. validate(identity) alone is not readiness. Held is Suspended
    without slot/write; original/producer/source loss is normal error/retirement.
    Consumed/absent wake is No_work. No proof is reconstructed after recovery. *)
val begin_goal_wake_preparation
  :  t
  -> wake:Agent_protocol.Run_wake.t
  -> (Goal_execution_runtime.Wake_issuance.t, Agent_protocol.Error.t) result

(** Actual Goal-attributed Running scope only, including generation/attempt and
    SAME original binding. No second Job claim, Enabled mutation or new account.
    Hold does not suspend/interrupt/requeue an already claimed Job. Fresh Init
    effects remain canonical-policy gated; failure is a normal actual Job failure.
    A stale/foreign/ordinary scope rejects rather than falling back to a loader. *)
val begin_goal_job_runtime_preparation
  :  t
  -> owner:Job_runtime_owner.t
  -> (Goal_execution_runtime.Preparation.t, Agent_protocol.Error.t) result

val bind_goal_execution_runtime_source
  :  t
  -> preparation:Goal_execution_runtime.Preparation.t
  -> source:Runtime_source_preparation.t
  -> (Goal_execution_runtime.Source_binding.t, Agent_protocol.Error.t) result

(** Bind only SAME source preparation and original execution. No Goal Enable,
    Resume receipt, Held_run, Run, Admission, episode or Job claim is issued. *)
val prepare_goal_execution_runtime
  :  t
  -> binding:Goal_execution_runtime.Source_binding.t
  -> (Goal_execution_runtime.t, Agent_protocol.Error.t) result

val end_goal_execution_runtime_preparation
  :  t
  -> preparation:Goal_execution_runtime.Preparation.t
  -> (unit, Agent_protocol.Error.t) result

module Delegated_load : sig
  module Custody = Delegated_runtime_load.Custody

  module Preparation : sig
    type t

    (** Same-domain monotone local revocation, no callback/RPC. Factual end after
        owned joins remains mandatory, including cancellation before binding. *)
    val close : t -> unit
  end

  module Source_binding : sig
    type t

    val source : t -> Runtime_source_preparation.t
    val check_basis : t -> (unit, Agent_protocol.Error.t) result
    val close : t -> unit
  end

  type t

  (** Exact SAME preparation/source association; data does not establish Init,
      installed-callback permission, selected execution, or parent authority. *)
  val check_source_binding
    :  t
    -> binding:Source_binding.t
    -> (unit, Agent_protocol.Error.t) result
end

(** After actual Owner source-stage reservation only. The actual Factory source
    issuer must match this Actor; state must retain its genuine delegated-child
    reference and ordinary execution selection. No saved creator or Goal/Run is
    inferred. Prepared_run remains SAME actual caller-owned preparation. *)
val begin_delegated_load_preparation
  :  t
  -> source_owner:Runtime_source_owner.t
  -> custody:Delegated_load.Custody.t
  -> (Delegated_load.Preparation.t, Agent_protocol.Error.t) result

val bind_delegated_load_source
  :  t
  -> preparation:Delegated_load.Preparation.t
  -> source:Runtime_source_preparation.t
  -> (Delegated_load.Source_binding.t, Agent_protocol.Error.t) result

val prepare_delegated_load
  :  t
  -> binding:Delegated_load.Source_binding.t
  -> (Delegated_load.t, Agent_protocol.Error.t) result

(** After source/parent/resource joins. Revokes only this load view; never closes
    a borrowed initial preparation's caller-owned finalizer. No NoEffects proof. *)
val end_delegated_load_preparation
  :  t
  -> preparation:Delegated_load.Preparation.t
  -> (unit, Agent_protocol.Error.t) result

module Runtime_construction : sig
  (** Closed private selection of the SAME Initial admitted continuation or
      Existing_goal Ready owner or genuine ordinary delegated-load source.
      No new Run, binding, admission or permission. *)
  type t

  val of_admitted : Admitted_activation.Continuation.t -> t
  val of_goal : Goal_continuation.t -> t
  val of_existing_goal_execution : Goal_execution_runtime.t -> t
  val of_delegated_load : Delegated_load.t -> t

  module Initialization : sig
    type t =
      | Scoped
      | Installed_resume
  end

  (** Actual Actor-selected route DATA. An ordinary load never makes a Ready
      initializer eligible by merely supplying a context. *)
  val initialization : t -> Initialization.t

  val initial_preparation : t -> Run_preparation.t option
  val source : t -> Runtime_source_preparation.t

  (** Local irreversible fresh-use revocation, same domain, no RPC/policy/clock.
      Known command and final adoption ACK facts survive. Called before Owner
      resource joins. Does not erase a writer's subsequent factual known ACK. *)
  val close : t -> unit

  (** Pure actual Initial admission close obligation, not a canonical ACK or
      execution grant. Lexical cleanup retains the same source/runtime through
      uncertain persistence; lifecycle close still revokes effects. *)
  val admission_retained : t -> bool

  (** Historical final adoption ACK fact only: same physical runtime additionally
      required by Owner. Covers actual Started, Waiting and Settled outcomes.
      Ordinary delegated installation is always false and does not promote
      execution selection. *)
  val handoff_acknowledged : t -> bool

  (** Refresh Store outside mailbox/Owner mutex. Final pure stages are repeated
      after callbacks/yields; actual acknowledged inference registrations retain
      their existing private admitted-only guard, never this fresh admission. *)
  val refresh : t -> (unit, Agent_protocol.Error.t) result

  val check_policy : t -> (unit, Agent_protocol.Error.t) result
  val check_timing : t -> (unit, Agent_protocol.Error.t) result
  val check_authority : t -> (unit, Agent_protocol.Error.t) result
  val check_basis : t -> (unit, Agent_protocol.Error.t) result
end

module Failed_startup_disposal : sig
  (** Actual process-local custody release of one original failed Initial
      construction. The original End, Run, and cleanup snapshot keep their
      observed outcomes. Available after Actor death. Creating-domain only. *)
  type t

  (** Physical original construction association; foreign reads raise Invalid_argument. *)
  val same_construction : t -> construction:Runtime_construction.t -> bool
end

module Failed_startup_request : sig
  type actor = t
  type t

  (** Nonyielding naming DATA for already captured original objects, no authority.
      Creating-domain/Actor association checked. Owner retains SAME request before
      dispatch so lost reply can be observed without rerunning any retirement.
      Unsupported/unbound/dead/disposal-only Actor rejects before allocation. *)
  val create
    :  actor
    -> construction:Runtime_construction.t
    -> retirement:Retirement_cleanup.t
    -> (t, Agent_protocol.Error.t) result

  (** Domain-checked read of only THIS request's actual atomic receipt. None is
      unknown/pending; this never reads Actor latest or issues mailbox work. *)
  val released : t -> (Failed_startup_disposal.t option, Agent_protocol.Error.t) result
end

(** Exact nonyielding Actor release after permanent closing retirement Joined,
    original writer-entered failure/no ACK on SAME canonical backend, original
    failed Initial End, closed actual Init callbacks, and fresh no-live-work
    guards. Input requires its original producer exit and normal graph-close.
    Authored startup requires its original producer exit and either an uninstalled
    first lifecycle write or a confirmed lifecycle and joined callback before the
    failed final aggregate. The Runtime Owner checks its independent physical
    joins. Success atomically caches the receipt and installs a monotonic
    disposal-only gate before reply. Only State, Shutdown and SAME request
    repeat remain admitted. *)
val release_failed_startup_custody
  :  t
  -> request:Failed_startup_request.t
  -> (Failed_startup_disposal.t, Agent_protocol.Error.t) result

(** Negative-only creating-domain read. True only DENIES checkpoint dispatch;
    neither false nor true grants execution or custody authority. Foreign domain
    raises Invalid_argument. *)
val is_startup_disposal_only : t -> bool

val check_runtime_construction_admission
  :  t
  -> construction:Runtime_construction.t
  -> (unit, Agent_protocol.Error.t) result

(** Same synchronous physical installation body. Initial adapter preserves its
    None->Some law. Goal permits exact existing worker/Manager reuse or the exact
    cold constructor's empty slot; competing runtime/borrow rejects. Optional
    policy DATA uses the existing turn-budget writer with this exact final guard.
    Store refresh repeats after any policy ACK and before physical install.
    The optional factual retained-lifecycle witness is accepted only for the
    actual Existing_goal fresh binding. It preserves physical cold installation
    while joining its genuinely completed SAME lifecycle, without callback replay
    or inferred authority. Initial and ordinary delegated routes reject Some. *)
val install_runtime_construction
  :  ?unmoderated_source:Runtime_builder.Unmoderated_source.t
  -> ?retained_lifecycle:Runtime_builder.Retained_lifecycle.t
  -> ?automatic_turn_policy:Chat_response.Runtime_semantics.policy
  -> ?moderator_manager:Chat_response.Moderator_manager.t
  -> t
  -> construction:Runtime_construction.t
  -> worker:Operation_worker.t
  -> inference:Inference_client.Execution.t
  -> (unit, Agent_protocol.Error.t) result

(** Cold-only: actual bound Runtime_resume eligibility and installed worker/Manager
    checked before entering its SAME lifecycle. Inject exact Session_resume claim
    into the existing queued-event runner. Legacy invokes no callback again:
    its real restored constructor ACK already happened inside Factory build.
    Loaded reuse skips this API. Actor records joined completion before final
    continuation can proceed. Actual callback Run.wait/Goal.finish is retained. *)
val run_goal_runtime_resume
  :  t
  -> continuation:Goal_continuation.t
  -> resume:Runtime_builder.Runtime_resume.t
  -> (Goal_callback_control.t option, Agent_protocol.Error.t) result

module Goal_resume_result : sig
  module Continuation : sig
    type t =
      | Started of Agent_protocol.Operation.t
      | Waiting of Agent_protocol.Run_wake.t
      | Settled of Agent_protocol.Goal_episode.Settlement.t
      | Controlled of Goal_callback_control.t
  end

  type t =
    | Resumed of
        { receipt : Goal_command_receipt.t
        ; continuation : Continuation.t
        }
    | Retained of Goal_command_receipt.t
    | Resume_failed of
        { receipt : Goal_command_receipt.t
        ; error : Agent_protocol.Error.t
        }
    | Rejected of Agent_protocol.Error.t
    | Uncertain of Agent_protocol.Error.t
end

(** Final actual same-Run decision after loaded reuse or joined cold lifecycle.
    Release only the real constructor hold through existing aggregate settlement.
    Waiting returns the actual retained wake. Terminal returns Settlement.create
    from original episode + actual Run + exact terminal receipt. Only live
    Admitted/Active with no pending Finish attaches a new Administrative Operation
    from retained history. No parser, User input, Continue intent or second Run.
    The exact joined source, Run work and selected worker are captured before
    cancellable ancestor admission outside the Actor. The real Goal controller
    installs once at the same State ACK as inherited Included accounting; launch
    waits for actual ancestor confirmation and the SAME selected worker. A
    prepared or uncertain writer is not an ACK and retains its actual owner for
    factual cleanup/recovery. Waiting, Settled, Controlled and retained outcomes
    reserve no Turn; their genuine metadata writers keep their existing results.
    Known command ACK remains in every post-ACK failure. *)
val continue_goal_episode
  :  t
  -> continuation:Goal_continuation.t
  -> (Goal_resume_result.t, Agent_protocol.Error.t) result

module Runtime_construction_resource : sig
  (** Resource DATA borrow only; no effect permission. Exact current physical
      Event/Invocation/acknowledged child execution remains mandatory. *)
  type t

  val check_basis : t -> (unit, Agent_protocol.Error.t) result
end

val runtime_construction_resource
  :  t
  -> construction:Runtime_construction.t
  -> owner:Agent_protocol.Job.launch_owner
  -> (Runtime_construction_resource.t, Agent_protocol.Error.t) result

(** Factual cleanup of this exact construction inventory after local close and resource joins. Preserves the existing Run, unrelated work, passive Wait and known ACK facts. *)
val end_runtime_construction
  :  t
  -> construction:Runtime_construction.t
  -> (unit, Agent_protocol.Error.t) result

module Runtime_constructor_graph : sig
  (** Immutable association of the actual issuing Init scope and SAME registered
      Inference_owner. It owns no execution permission or second graph registry.
      The Actor authenticates factual handle/registration membership even after
      Init/Stop. Forwarded Custody must still be the exact current owner. *)
  type t

  val owner : t -> Inference_owner.t
end

module Runtime_constructor_mutations : sig
  val open_inference_owner
    :  t
    -> construction:Runtime_construction.t
    -> source:Transcript.Source_id.t
    -> (Runtime_constructor_graph.t, Agent_protocol.Error.t) result

  val acknowledge_inference
    :  t
    -> graph:Runtime_constructor_graph.t
    -> handle:Inference_ledger.Handle.t
    -> Inference_runtime.Attempt.t
    -> (unit, Agent_protocol.Error.t) result

  val observe_inference
    :  t
    -> graph:Runtime_constructor_graph.t
    -> handle:Inference_ledger.Handle.t
    -> Inference.Observation.t
    -> (unit, Agent_protocol.Error.t) result

  val observe_owned_inference
    :  t
    -> graph:Runtime_constructor_graph.t
    -> Inference.Observation.t
    -> (unit, Agent_protocol.Error.t) result

  val complete_inference
    :  t
    -> graph:Runtime_constructor_graph.t
    -> handle:Inference_ledger.Handle.t
    -> Inference_client.Completion.t
    -> (unit, Agent_protocol.Error.t) result

  val release_inference
    :  t
    -> graph:Runtime_constructor_graph.t
    -> handle:Inference_ledger.Handle.t
    -> (unit, Agent_protocol.Error.t) result

  val seal_inference_owner
    :  t
    -> graph:Runtime_constructor_graph.t
    -> (unit, Agent_protocol.Error.t) result

  val finish_inference_owner
    :  t
    -> graph:Runtime_constructor_graph.t
    -> (unit, Agent_protocol.Error.t) result

  val replace_shell_approval_grants
    :  t
    -> construction:Runtime_construction.t
    -> Session.Shell_state.Approval_grant.persisted list
    -> (unit, Agent_protocol.Error.t) result

  val replace_shell_extension_snapshots
    :  t
    -> construction:Runtime_construction.t
    -> Session.Shell_state.Extension_snapshot.t list
    -> (unit, Agent_protocol.Error.t) result

  val add_shell_manifest_grant
    :  t
    -> construction:Runtime_construction.t
    -> Session.Shell_state.Manifest_grant.persisted
    -> (unit, Agent_protocol.Error.t) result

  val add_job
    :  t
    -> construction:Runtime_construction.t
    -> Agent_protocol.Job.t
    -> (Agent_protocol.Job.t, Agent_protocol.Error.t) result

  val add_schedule
    :  t
    -> construction:Runtime_construction.t
    -> Agent_protocol.Schedule.t
    -> (Agent_protocol.Schedule.t, Agent_protocol.Error.t) result

  val cancel_schedule
    :  t
    -> construction:Runtime_construction.t
    -> schedule_id:Agent_protocol.Id.Schedule.t
    -> (Agent_protocol.Schedule.t, Agent_protocol.Error.t) result

  val capture_model_source
    :  t
    -> construction:Runtime_construction.t
    -> job_id:Agent_protocol.Id.Job.t
    -> generation:int
    -> target:Inference.Request.Target.t
    -> limits:Document_schema.Limits.t
    -> (unit, Agent_protocol.Error.t) result

  val capture_recipe_target
    :  t
    -> construction:Runtime_construction.t
    -> job_id:Agent_protocol.Id.Job.t
    -> generation:int
    -> attempt:int
    -> target:Inference.Request.Target.t
    -> limits:Document_schema.Limits.t
    -> (unit, Agent_protocol.Error.t) result

  val complete_model_job
    :  t
    -> construction:Runtime_construction.t
    -> job_id:Agent_protocol.Id.Job.t
    -> generation:int
    -> attempt:int
    -> Runtime_builder.model_job_outcome
    -> (Agent_protocol.Job.t, Agent_protocol.Error.t) result

  val interrupt_model_job
    :  t
    -> construction:Runtime_construction.t
    -> job_id:Agent_protocol.Id.Job.t
    -> generation:int
    -> attempt:int
    -> reason:string
    -> (Agent_protocol.Job.t, Agent_protocol.Error.t) result

  val checkpoint_moderator
    :  ?moderator_manager:Chat_response.Moderator_manager.t
    -> t
    -> construction:Runtime_construction.t
    -> Jsonaf.t option
    -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result
end

(** Called ONLY when Owner actually has no runtime. Requires the exact new Goal
    owner and no installed worker/borrow. Genuine lifecycle Starting then Idle
    ACKs retain that owner and both originals. Public wrapper refreshes outside
    Actor BETWEEN these two phase-specific requests, not under one mailbox call. No public ordinary Start, new Run
    or operation. A failed second ACK does not erase the first command receipt. *)
val begin_goal_runtime_construction
  :  t
  -> construction:Runtime_construction.t
  -> (unit, Agent_protocol.Error.t) result

(** One initial/existing-goal Init body. The old begin_held_initialization remains
    a used Initial adapter. Ordinary legacy Init with no context is unchanged. *)
val begin_runtime_initialization
  :  t
  -> construction:Runtime_construction.t
  -> purpose:Initialization_scope.Purpose.t
  -> expected:Session_state.t
  -> (Initialization_scope.t, Agent_protocol.Error.t) result

val reserve_runtime_history_block
  :  t
  -> construction:Runtime_construction.t
  -> count:int
  -> (History_id_source.reservation, Agent_protocol.Error.t) result

(** Current command/read-authorized exact principal/key/digest receipt lookup
    before runtime/source acquisition. Historical DATA remains reportable after
    later episode revisions and grants no resumed execution authority. *)
val goal_resume_receipt
  :  ?writer:Agent_protocol.Id.Attachment.t
  -> t
  -> actor:Operator_authorization.t
  -> authorize:(Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> session:Agent_protocol.Session_ref.t
  -> request:Goal_resume_request.t
  -> (Goal_command_receipt.t option, Agent_protocol.Error.t) result

(** Owner must first reserve its actual exclusive source stage. Select the
    existing physical Run binding and both originals, immutable episode/Admission,
    original Goal/catalog/Run CAS and full source epoch. Capture actual session
    revision separately; no new Run, Scope, account or initial preparation. *)
val begin_goal_resume_preparation
  :  t
  -> actor:Operator_authorization.t
  -> authorize:(Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> session:Agent_protocol.Session_ref.t
  -> request:Goal_resume_request.t
  -> (Goal_continuation.Resume_issuance.t, Agent_protocol.Error.t) result

(** Bind the actual Factory-restored source, without executing or adopting it.
    Exact existing Run/Goal/request installation and same live owner are required. *)
val bind_goal_continuation_source
  :  t
  -> preparation:Goal_continuation.Preparation.t
  -> source:Runtime_source_preparation.t
  -> (Goal_continuation.Source_binding.t, Agent_protocol.Error.t) result

(** Commit only Enabled+the exact command receipt. Unknown first writer installs
    no Ready owner; known ACK is retained before canonical publication and later
    issuance errors are Resume_failed. Waiting preserves its actual passive Wait
    and creates no Operation. Exceptions/cancellation preserve their primary. *)
val admit_goal_resume
  :  ?writer:Agent_protocol.Id.Attachment.t
  -> t
  -> binding:Goal_continuation.Source_binding.t
  -> (Goal_continuation.Admission.t, Agent_protocol.Error.t) result

(** Factual same-owner release after joined source resources. No fresh original
    authorization, new admission, passive-wake cancellation or Run interruption.
    Actual known receipt DATA remains retained even when construction is closed. *)
val end_goal_continuation_preparation
  :  t
  -> preparation:Goal_continuation.Preparation.t
  -> (unit, Agent_protocol.Error.t) result

module Recurrence_activation : sig
  (** Revocable process-local controller custody for one exact acknowledged
      occurrence/attempt/Run. No constructor or decoder. The actual target's
      held admission/initialization owner remains separately required. *)
  type t

  val origin : t -> Recurrence_run_origin.t

  module Checked : sig
    (** A fresh genuine Store/Current view of a still-live permit. Unrelated
        controller ACKs may stale this view without revoking the permit; refresh
        through the actor. A revoked cell is never refreshed or revived. *)
    type t

    (** Complete check: reject closed/stale custody, run all controller/target-read
        policies, repeat both originals, sample fresh grace timing, then check both
        pure bases. Callbacks are trusted nonyielding host code; exceptions
        propagate. No permission survives a yield. *)
    val check_current : t -> (unit, Agent_protocol.Error.t) result

    (** Controller and target-read policies, retaining their original-proof checks.
        First rejects closed/stale custody without invoking stored host code. *)
    val check_policy : t -> (unit, Agent_protocol.Error.t) result

    (** Both original authentication guards and immutable scopes, without host
        policy. Executes trusted nonyielding host code after a pure preflight. *)
    val check_authority : t -> (unit, Agent_protocol.Error.t) result

    (** Sample supplied wall/monotonic clocks and validate fresh nominal grace.
        Executes host callbacks after a pure preflight; this is not a durable
        timing certificate or a substitute for policy/original authentication. *)
    val check_timing : t -> (unit, Agent_protocol.Error.t) result

    (** Callback-free exact owner/source/control/attempt, actual target Admitted
        Run and Store Current. Invokes no original, policy or clock callback.
        The target consumer must compose ALL host policies, ALL originals, fresh
        timing, then BOTH pure target/controller bases immediately before the
        first effect. Both genuine held ownership and this view are required;
        this basis alone authenticates neither. Same Eio domain, with no yield
        or callback after the final bases. *)
    val check_basis : t -> (unit, Agent_protocol.Error.t) result

    (** The SAME checked owner and consumption cell, without refresh or issuance.
        Consuming this view also consumes the recurrence Checked. *)
    val admitted : t -> Admitted_activation.Checked.t
  end

  module Continuation : sig
    (** Consumes one exact Checked/held-target link. Controller custody remains
        live; only this actual target activation may acknowledge its own progress.
        Refresh uses the genuine Store/Handle after yields, including self-target
        ACKs. The old Checked is not weakened or reused as a fresh view. *)
    type t = Admitted_activation.Continuation.t

    val target : t -> Held_run.Activation.t

    (** Policy and genuine lifecycle observation refresh; may perform Store I/O.
        Call outside actor mailboxes and runtime-owner mutexes. *)
    val refresh : t -> (unit, Agent_protocol.Error.t) result

    (** Trusted nonyielding policy callbacks only, with pure preflight/final
        ownership. No Store I/O or mailbox call. Repeat at the actual fresh
        writer boundary before clock/original/final pure checks. *)
    val check_policy : t -> (unit, Agent_protocol.Error.t) result

    (** Trusted clock/grace callbacks, then original authentication guards must
        precede both final pure bases. Neither check grants a lasting permission. *)
    val check_timing : t -> (unit, Agent_protocol.Error.t) result

    val check_authority : t -> (unit, Agent_protocol.Error.t) result
    val check_basis : t -> (unit, Agent_protocol.Error.t) result
  end
end

(** Consume the exact Prepared binding/Held/actual admitted association once.
    No constructor, initializer, history or worker runs here. The host retains
    its real Prepared lease and, after this yielding RPC, refreshes and repeats
    all policies, fresh timing, originals and final joint pure bases. *)
val begin_held_activation
  :  t
  -> binding:Source_binding.t
  -> held:Held_run.t
  -> checked:Admitted_activation.Checked.t
  -> (Admitted_activation.Continuation.t, Agent_protocol.Error.t) result

(** Consume only the exact linked Admitted target and current Checked view.
    No native resources, history/content conversion, initializer or worker runs.
    The host separately retains the actual Runtime_owner.Prepared lease; after
    this yielding mailbox call it refreshes the continuation and repeats the
    final joint first-effect fence. *)
val begin_held_recurrence_activation
  :  t
  -> binding:Source_binding.t
  -> held:Held_run.t
  -> checked:Recurrence_activation.Checked.t
  -> (Recurrence_activation.Continuation.t, Agent_protocol.Error.t) result

(** Exact owned constructor entry for a consumed held activation. It reuses the
    existing Initialization scope and ACK rules; arbitrary live Runs or original
    proofs cannot bypass the ordinary unresolved-Run exclusion. *)
val begin_held_initialization
  :  t
  -> continuation:Admitted_activation.Continuation.t
  -> purpose:Initialization_scope.Purpose.t
  -> expected:Session_state.t
  -> (Initialization_scope.t, Agent_protocol.Error.t) result

(** Acquire only after actual target admission AND reciprocal controller ACK.
    Requires current Send_messages and trusted nonyielding execution-selection
    policy, genuine Active+Automatic controller lifecycle/birth, enabled current
    source/control/definition, current grace and exact opaque same-Store target
    observation of an Admitted Run. The stored intent's caller/control revision
    remain audit data. No runtime loads, target preparation, initialization or
    worker launch occurs. At most one live cell per actual retained current
    execution; reacquisition closes only that occurrence's old cell. *)
val acquire_recurrence_activation
  :  t
  -> actor:Operator_authorization.t
  -> authorize:
       (Session_state.t
        -> Recurrence_definition.t
        -> (unit, Agent_protocol.Error.t) result)
  -> observation:Recurrence_run_observation.t
  -> (Recurrence_activation.t, Agent_protocol.Error.t) result

(** Actual Store refresh may yield; repeat original policy and exact still-live
    custody afterwards. Fresh target observation required, including after a
    self-target's own ACK. Caller/principal cannot be replaced by this operation. *)
val check_recurrence_activation
  :  t
  -> Recurrence_activation.t
  -> observation:Recurrence_run_observation.t
  -> (Recurrence_activation.Checked.t, Agent_protocol.Error.t) result

(** Idempotent issuer-bound lexical release, including closed owner custody.
    An actual root ACK transfers the same registered controller cell to its
    pending Operation; releasing the consumed original permit does not retire
    that owner. Internal Stop/source/control invalidation still closes it.
    No external cancellation, join, durable erasure or no-effects/retry claim.
    Actual target single activation remains owned by its held Run capability. *)
val release_recurrence_activation
  :  t
  -> Recurrence_activation.t
  -> (unit, Agent_protocol.Error.t) result

module Recurrence_import : sig
  module Timing : sig
    type t =
      | No_successor
      | Live_phase
      | Recovery_adoption
      | Held of Agent_protocol.Error.t
  end

  (** Result DATA, not a target or activation capability. Held accompanies an
      acknowledged evidence-plus-review-hold candidate; its diagnostic does not
      claim the commit failed. Retained/unavailable-detail results never install
      a phase or rearm. Fresh controller visibility governs returned data after
      ACK, without requerying a target or reusing its stale precommit witness. *)
  type t =
    { settlement : Recurrence_settlement.t
    ; timing : Timing.t
    }
end

(** Observe the exact immutable attempt through this actual target's owned
    Store/Handle and retained Run index. Requires genuine current
    View_session_transcript and nonyielding current host read policy, repeated
    after the Store observation and after the callback. The reader may differ
    from the original request principal. No runtime is loaded or initialized;
    stopped and old-source terminal evidence remains readable under current
    visibility policy. No automatic/execution authority is granted.

    A missing retained request returns None, never a no-effects/retry proof.
    A receipt lacking its exact reciprocal origin or terminal evidence rejects.
    Positive observations validate the actual Admission and complete terminal
    payload/canonical digest; no copied result history is stored. *)
val observe_recurrence_run
  :  t
  -> actor:Operator_authorization.t
  -> authorize:(Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> intent:Recurrence_execution_intent.t
  -> attempt:Recurrence_execution_attempt.t
  -> (Recurrence_run_observation.t option, Agent_protocol.Error.t) result

(** Import an actual opaque target observation from the same owned Store into
    this controller's exact retained occurrence attempt. Requires fresh original
    Send_messages and current evidence visibility for the historical definition;
    the caller may differ from the stored audit principal. Both policy callbacks
    are trusted and must not yield. No runtime is loaded or initialized.

    Saved extended terminal references are authenticated against the target's
    actual Run/receipt, then retained as complete original JSON by the pure owner. Unchanged
    results require current visibility but write no transaction, increment no
    counter and install no phase. Detail_not_retained makes no prior completion
    or matching-receipt claim. Already acknowledged facts need no later target
    lookup to classify pending work or explicitly enable a saved basis.

    A new automatic successor additionally requires the current running,
    enabled, source/birth/lifecycle match and [automatic_policy]. This policy
    authorizes rearm metadata only, never target execution. Receipt wall and
    actual surviving monotonic timing prepare the phase before persistence.
    Missing process timing preserves wall reconciliation; invalid live timing
    commits one historical evidence-plus-review-hold candidate and returns Held
    alongside acknowledged DATA. Fact contradictions reject before persistence.

    All target/controller policy callbacks precede callback-free final checks
    of both original proofs, retained facts and actual owner/Current bases.
    Nonyielding policy callbacks may revoke another proof; callback ordering alone
    does not secure this joint fence. These checks precede the existing persistence
    owner. ACK installs state and the prepared phase before publication. Return
    visibility refreshes only the controller; a self-target's precommit witness
    is not reused after its own ACK. A postACK visibility error does not claim
    rollback. No retry, Run admission or activation authority is issued. *)
val import_recurrence_execution
  :  t
  -> actor:Operator_authorization.t
  -> authorize:
       (Session_state.t
        -> Recurrence_definition.t
        -> (unit, Agent_protocol.Error.t) result)
  -> automatic_policy:
       (Session_state.t
        -> Recurrence_definition.t
        -> (bool, Agent_protocol.Error.t) result)
  -> observation:Recurrence_run_observation.t
  -> (Recurrence_import.t, Agent_protocol.Error.t) result

(** A continuously owned self-target Held may import ONLY its exact nonterminal
    Admitted origin/Admission association through the same Store. Both actual
    target/controller proofs and current policies remain required. The single
    association ACK advances only that Held's original preparation before
    publication, leaving request/CAS/key/digest unchanged. Retained DATA writes
    nothing and cannot recreate or reattach Held custody. PostACK visibility
    refreshes only the controller, never the stale self-target observation.
    No activation, startup, terminal settlement or no-effects proof is issued. *)
val import_held_recurrence_execution
  :  t
  -> held:Held_run.t
  -> actor:Operator_authorization.t
  -> authorize:
       (Session_state.t
        -> Recurrence_definition.t
        -> (unit, Agent_protocol.Error.t) result)
  -> automatic_policy:
       (Session_state.t
        -> Recurrence_definition.t
        -> (bool, Agent_protocol.Error.t) result)
  -> observation:Recurrence_run_observation.t
  -> (Recurrence_import.t, Agent_protocol.Error.t) result

module Recurrence_child_link : sig
  (** Actual owned-child observation. No constructor, decoder or session-ID
      lookup can mint this proof. It retains a synchronous currentness check;
      immutable association data alone grants no authority. *)
  type t

  val association : t -> Recurrence_child_association.t

  (** Synchronous final proof check immediately before a separate owned effect.
      No authority or child-liveness guarantee is held across a later yield. *)
  val check_current : t -> (unit, Agent_protocol.Error.t) result
end

(** Observe this actual child's supported origin, exact reference and genuine
    same-store Child_installed record while it remains intentionally dormant.
    Does not initialize, load a runtime, admit a Run or publish parent linkage. *)
val verify_recurrence_child
  :  t
  -> reservation:Recurrence_child_reservation.t
  -> (Recurrence_child_link.t, Agent_protocol.Error.t) result

(** Reauthorize the original parent permit and actual child proof, then persist
    one immutable association before returning its acknowledgement. The child
    must share this actor's actual Store; final child/current-parent checks are
    synchronous and do not wait on another actor mailbox. No Run is admitted. *)
val link_recurrence_child
  :  t
  -> permit:Recurrence_creation.t
  -> child:Recurrence_child_link.t
  -> max_children:int
  -> max_encoded_bytes:int
  -> (Recurrence_child_association.t, Agent_protocol.Error.t) result

(** Current authorized read with effective lifecycle retirement. No native/script
    runtime is initialized; stored controls and acceptance receipts grant no work. *)
val read_recurrences
  :  t
  -> actor:Operator_authorization.t
  -> authorize:(Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> (Recurrence_view.t list, Agent_protocol.Error.t) result

(** Same original read/current lifecycle policy as read_recurrence_planning.
    Returns bounded controller DATA only. Target visibility IO/rendering happens
    outside the mailbox; re-read and compare the exact basis after that IO.
    No source loan/runtime/target observation, import or retry proof is issued. *)
val query_recurrences
  :  t
  -> actor:Operator_authorization.t
  -> authorize:(Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> request:Recurrence_query.request
  -> (Recurrence_query.t, Agent_protocol.Error.t) result

(** The same authorized query plus its immutable session state from the same
    mailbox turn, for registry read composition. *)
val query_recurrences_with_state
  :  t
  -> actor:Operator_authorization.t
  -> authorize:(Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> request:Recurrence_query.request
  -> (Session_state.t * Recurrence_query.t, Agent_protocol.Error.t) result

module Recurrence_planning_read : sig
  type t =
    { entries : Recurrence_view.t list
    ; planning : Recurrence_planning_status.t
    }
  [@@deriving sexp_of]
end

(** Same original transcript/visibility authorization as read_recurrences,
    repeated after actual lifecycle IO. Process-local availability is diagnostic,
    never a permission or persisted issue. Missing host config leaves metadata
    readable and reports Not_configured. No native/script runtime is initialized. *)
val read_recurrence_planning
  :  t
  -> actor:Operator_authorization.t
  -> authorize:(Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> (Recurrence_planning_read.t, Agent_protocol.Error.t) result

(** Trusted factory-only installation after the actual controller owner. No
    principal/creator lookup or target authority follows from this port. *)
val set_recurrence_planning_host
  :  t
  -> Recurrence_planning_host.t
  -> (unit, Agent_protocol.Error.t) result

(** One bounded sweep of retained IDs through the actor, independently of user
    runtime/callback ownership. Heavy named-zone reads/calendar planning occur
    outside the mailbox; final source/control/Running/Automatic/current original
    host proof and lifecycle checks precede the existing atomic persistence.
    Phase installs only after ACK and before yielding state publication. No
    target is launched and fixed-delay awaits the actual terminal owner.
    Cancellation joins preparation cleanup; stale cleanup cannot release a new
    token. Called by the existing sole daemon scheduler, never a new timer. *)
val sweep_recurrences : t -> (unit, Agent_protocol.Error.t) result

(** A scoped resource borrow selected from the actual executing authored
    lifecycle Event or one of its native Invocation descendants. IDs, source
    snapshots and Run membership cannot construct it. Its pure check is valid on
    the issuing Eio domain without an Actor request, host callback or Store read;
    stop and owner replacement revoke it. An Event-owned borrow ends with that
    callback; an actual admitted Job execution descendant retains only its own
    physical borrow until that execution ends or startup hands off. It grants no
    new execution permission. Runtime_owner still validates its exact registered
    source stage, physical runtime and ordinary lease admission. This token
    retains resource DATA only; canonical Job effect guards recheck deadlines
    after waits before new effects. It does not sample a clock under Owner locks. *)
module Held_authored_resource : sig
  type t

  val check_basis : t -> (unit, Agent_protocol.Error.t) result
end

val held_authored_resource
  :  t
  -> continuation:Admitted_activation.Continuation.t
  -> owner:Agent_protocol.Job.launch_owner
  -> (Held_authored_resource.t, Agent_protocol.Error.t) result

(** Start the original Authored_start of this same held Run. The runtime-bound
    selection is factual DATA: actual installed worker/Manager, source, current
    checkpoint and genuine pending lifecycle are revalidated under the issued
    owner. Acknowledged legacy startup is never replayed. A real v1 Session_start
    is joined through this exact owner and original custody, then the shared Run
    aggregate is settled without the startup hold. Only the actual final ACK
    installs immutable handoff proof and returns Started with the original
    Admission. A terminal callback returns Settled with real evidence; an exact
    native control returns Controlled without inventing
    startup handoff; it grants no further effect. Reached
    writer errors/exceptions consume the startup attempt; factual cleanup does
    not reauthorize expired effects or imply rollback. *)
val start_held_authored_run
  :  t
  -> continuation:Admitted_activation.Continuation.t
  -> startup:Runtime_builder.Authored_start.t
  -> (Held_authored_result.t, Agent_protocol.Error.t) result

(** First owned startup write: final policy refresh has occurred outside the
    mailbox; the real writer repeats clock/original and joint pure bases. Own ACK
    advances the same preparation and installs its genuine Run authority before
    constructor effects. Later reservations under this exact startup reuse its
    physical scope and caller binding; a different retained owner is rejected.
    No synthetic input/Operation is allocated. *)
val reserve_held_history_block
  :  t
  -> continuation:Admitted_activation.Continuation.t
  -> count:int
  -> (History_id_source.reservation, Agent_protocol.Error.t) result

(** Retire this actual scoped startup if it did not hand off. Selected factual
    Run interruption preserves completed child winners and marks unresolved
    custody Unconfirmed in its one canonical terminal ACK. No reauthorization,
    initializer replay, effect rollback or no-effects claim. Idempotent after
    successful cleanup; foreign physical owners reject. *)
val end_held_activation
  :  t
  -> activation:Held_run.Activation.t
  -> (unit, Agent_protocol.Error.t) result

(** Parse the original User_submission of this exact held continuation once.
    Reserves a real history ID before input effects. [prepare] runs outside the
    mailbox and must join/seal/finish its actual scope-bound input graph before
    returning DATA. Only then is the exact result recorded as Parsed_ready and
    passed to [f]; escaped execution services cannot perform further effects.
    Scope exit closes admission without rolling back input effects. Primary
    errors/cancellation retain their outcome; failed factual ACKs remain durable
    recovery evidence and never imply no effects. No Operation is created here. *)
val with_held_input
  :  t
  -> continuation:Admitted_activation.Continuation.t
  -> prepare:
       (scope:Held_input_scope.t
        -> Runtime_input_preparation.Execution.t
        -> (Runtime_input_preparation.Prepared_input.t, Agent_protocol.Error.t) result)
  -> (scope:Held_input_scope.t
      -> input:Runtime_input_preparation.Prepared_input.t
      -> ('a, Agent_protocol.Error.t) result)
  -> ('a, Agent_protocol.Error.t) result

(** Attach the exact recorded Parsed_ready input to its SAME held Run and launch
    the existing foreground worker. No parse, admission, receipt or root replay.
    Only the actual root ACK transfers Run/original/controller custody to the
    Operation, before publication. A reached writer Error/exception consumes the
    submission right without claiming whether it committed. Before-writer denial
    installs no owner. Returns the root ACK, not a first-effect or completion ACK.
    Input/startup/preparation and original controller-permit scopes may then exit;
    the transferred cell remains subject to controller Stop/source/control and
    original revocation until the actual winning worker branch's final fence.
    Factual worker termination follows the existing Run settlement law. Inherited
    Goal scopes reserve and confirm through the shared descendant Turn owner
    outside the Actor mailbox and Owner mutex. Only the initial finite handoff
    request is cancellation-protected; ancestor waits remain cancellable. Unknown
    canonical writers retain the exact parsed input, preparation exit, staged
    native owner and reservation set until factual settlement or recovery. *)
val submit_held_run_input
  :  t
  -> scope:Held_input_scope.t
  -> input:Runtime_input_preparation.Prepared_input.t
  -> (submission, Agent_protocol.Error.t) result

(** Install the first physical worker/inference pair of the SAME held startup.
    Repeats joint target/controller admission after mailbox entry and any Manager
    validation. Requires the actual registered live continuation/preparation and
    an empty worker slot; a loaded/replaced worker cannot be overwritten. Uses
    the ordinary runtime validation and custody installation without replacing
    the Run binding, admitting a Run, writing history or replaying startup.
    An optional automatic-turn policy uses the ordinary budget recipe under
    the same held owner's final writer fence before physical installation; an
    equal existing policy is not reset. A policy ACK is followed by fresh Store
    custody outside the mailbox. Successful installation preserves startup custody for the subsequent actual input handoff. Runtime resources
    and cleanup remain owned by the calling Runtime_owner; this returns no lease.
    Same Eio domain as the issuing startup. *)
val install_held_runtime
  :  ?unmoderated_source:Runtime_builder.Unmoderated_source.t
  -> ?automatic_turn_policy:Chat_response.Runtime_semantics.policy
  -> ?moderator_manager:Chat_response.Moderator_manager.t
  -> t
  -> continuation:Admitted_activation.Continuation.t
  -> worker:Operation_worker.t
  -> inference:Inference_client.Execution.t
  -> (unit, Agent_protocol.Error.t) result

(** Same actual retirement/lifecycle admission with the existing exact Held
    Continuation. Only its physical issuing actor and live checked preparation
    can enter construction before ordinary selection. No caller Boolean bypass. *)
val check_held_runtime_admission
  :  t
  -> continuation:Admitted_activation.Continuation.t
  -> (unit, Agent_protocol.Error.t) result

(** Check actual issuing Actor and current scope before the ordinary background
    adapter enters its runtime lease. No saved result grants rights across yield. *)
val check_job_runtime_owner
  :  t
  -> owner:Job_runtime_owner.t
  -> (unit, Agent_protocol.Error.t) result

(** Same Builder.Runtime_resume lifecycle and scoped native services as command
    resume, under exact old Run original/account. Loaded reuse skips this call.
    Session_resume Wait/Finish facts are retained, not translated into Continue. *)
val run_goal_execution_runtime_resume
  :  t
  -> execution:Goal_execution_runtime.t
  -> resume:Runtime_builder.Runtime_resume.t
  -> (Goal_callback_control.t option, Agent_protocol.Error.t) result

(** After real resource install and joined cold lifecycle (or loaded reuse), bind
    the SAME still-active acknowledged Job scope to that SAME installed worker/
    Manager and original Run. Protected actual Actor acknowledgement marks the
    resource handoff before caller cancellation; no new durable claim/attempt.
    Hold alone does not reject this resource retention or truthful completion.
    Fresh later effects retain canonical policy/expiry checks. *)
val acknowledge_goal_job_runtime
  :  t
  -> execution:Goal_execution_runtime.t
  -> (unit, Agent_protocol.Error.t) result

(** Pure projection of existing services after exact still-active physical scope,
    actor and request occurrence check; creates no new scope or Job claim. The
    returned canonical ports retain all original lifetime/effect guards. *)
val goal_job_runtime_services
  :  t
  -> execution:Goal_execution_runtime.t
  -> (job_execution, Agent_protocol.Error.t) result

(** Already-admitted Job RESOURCE retention only. Exact active physical scope,
    retained Run/original and actual installed worker/inference/Manager; no new
    Enabled or foreground-quiescence requirement. Current/deadline checks occur
    here outside Owner locks. The returned same-domain proof has a pure physical
    basis for final lease publication; effects retain their independent gates. *)
val job_runtime_resource
  :  t
  -> owner:Job_runtime_owner.t
  -> worker:Operation_worker.t
  -> inference:Inference_client.Execution.t
  -> moderator_manager:Chat_response.Moderator_manager.t option
  -> (Runtime_construction_resource.t, Agent_protocol.Error.t) result

(** Reuse exact enqueue writer/ACK and current Manager checkpoint. Only the
    genuine matching Run Job delivery can be appended; not callback admission. *)
val enqueue_goal_wake_job_delivery
  :  t
  -> execution:Goal_execution_runtime.t
  -> delivery:Run_job_delivery.t
  -> before:Session.Moderator_state.Identity_snapshot.t
  -> after:Jsonaf.t
  -> (unit, Agent_protocol.Error.t) result

(** Reuse actual complete_schedule writer and immutable claimed schedule. No new
    raw legacy loan from equal observer; missing old physical custody rejects. *)
val claim_goal_wake_schedule
  :  t
  -> execution:Goal_execution_runtime.t
  -> schedule:Agent_protocol.Schedule.t
  -> (Agent_protocol.Schedule.t option, Agent_protocol.Error.t) result

val complete_goal_wake_schedule
  :  t
  -> execution:Goal_execution_runtime.t
  -> expected:Session.Moderator_state.Identity_snapshot.t
  -> expected_schedule:Agent_protocol.Schedule.t
  -> moderator_manager:Chat_response.Moderator_manager.t
  -> moderator_snapshot:Jsonaf.t
  -> (Agent_protocol.Schedule.t, Agent_protocol.Error.t) result

(** Exact installed v1 Manager queue ABI. Only matching actual queue head may
    claim this wake; existing retirement/expiry rules remain first. SAME real
    claim_wake + Moderator_execution ACK binds scope/inventory and marks immutable
    handoff BEFORE publication. Retained/failed callback never replays. *)
val with_current_goal_wake_queued_event_tools
  :  t
  -> execution:Goal_execution_runtime.t
  -> Moderator_event.claim

(** Subscription uses existing notification proposal + start_idle_turn/
    Run_transition.admit_turn body. Select ONLY the exact matching Pending_wake;
    unrelated pending inputs/follow-ups are not silently adopted by this consumer.
    Actual Operation/Run_work + Wait consumption ACK marks handoff before publish.
    In an associated child, the SAME canonical Turn ACK installs the actual Goal
    owner and every inherited ancestor receipt; launch follows ancestor
    confirmation on the same selected worker without fresh quota admission.
    A prepared or uncertain writer is not a positive handoff. The bool may report
    notification metadata only; native ACK facts govern actual runtime transfer.
    Never manufacture a queued subscription frame or call queued claim_wake. *)
val deliver_goal_wake_notifications
  :  t
  -> execution:Goal_execution_runtime.t
  -> Notification_delivery.idle
  -> (bool, Agent_protocol.Error.t) result

(** Factual final selection after joined lifecycle and actual queue/turn adapter.
    Returns SAME acknowledged transfer, or new lifecycle Wait/Settlement with its
    actual ACK. An unchanged old Wait without transfer is not success. Original
    loss/uncertainty/stale producer is Error; never infer transfer from unit/bool.
    No automatic Administrative Operation is created here. *)
val finish_goal_wake_runtime
  :  t
  -> execution:Goal_execution_runtime.t
  -> (Goal_execution_runtime.Wake_result.t, Agent_protocol.Error.t) result

val deliver_goal_wake_job
  :  t
  -> execution:Goal_execution_runtime.t
  -> job:Agent_protocol.Job.t
  -> before:Session.Moderator_state.Identity_snapshot.t
  -> after:Jsonaf.t
  -> (unit, Agent_protocol.Error.t) result

(** Negative-only maintenance before scheduling suspension. Validate exact current
    producer DATA, then retire actual Waiting Goal Runs whose original custody is
    lost using the existing factual interruption writer. Never claims, constructs,
    selects, Enables or executes; no-work leaves the session unchanged. *)
val reconcile_goal_wake_authority
  :  t
  -> request:Goal_execution_runtime.Wake_request.t
  -> (unit, Agent_protocol.Error.t) result

(** Native Goal mutation staging. The exact selected native borrow is checked in
    Actor. Callback invocations stage only into their actual lexical transaction;
    they cannot fall back to an immediate writer when the route is absent. The
    returned [unit] is speculative: only the native result and selected parent
    checkpoint ACK can make it durable. No Run permission is returned.

    Before initial input handoff, a stopped session permits native Goal metadata
    only through the exact current event borrow and joined Init callback of its
    admitted initial Goal constructor. Original/source/effect currentness and
    explicit Stop revocation still apply. Ordinary, unbound and delegated stopped
    callbacks remain denied. *)
val stage_native_goal_create
  :  t
  -> admission:Goal_native_admission.t
  -> authorize:
       (Operator_authorization.t
        -> Session_state.t
        -> (unit, Agent_protocol.Error.t) result)
  -> request:Agent_protocol.Goal_native_create_request.t
  -> (unit, Agent_protocol.Error.t) result

val stage_native_goal_control
  :  t
  -> admission:Goal_native_admission.t
  -> authorize:
       (Operator_authorization.t
        -> Session_state.t
        -> (unit, Agent_protocol.Error.t) result)
  -> request:Agent_protocol.Goal_native_control_request.t
  -> (unit, Agent_protocol.Error.t) result

val stage_native_goal_edit
  :  t
  -> admission:Goal_native_admission.t
  -> authorize:
       (Operator_authorization.t
        -> Session_state.t
        -> (unit, Agent_protocol.Error.t) result)
  -> edit:Agent_protocol.Goal_edit.t
  -> (unit, Agent_protocol.Error.t) result

val stage_native_goal_resume
  :  t
  -> admission:Goal_native_admission.t
  -> authorize:
       (Operator_authorization.t
        -> Session_state.t
        -> (unit, Agent_protocol.Error.t) result)
  -> request:Goal_resume_request.t
  -> (unit, Agent_protocol.Error.t) result

(** The projection includes selected earlier local metadata edits only. It does
    not replace actual source/Run/custody checks or issue execution authority. *)
val native_goal_state
  :  t
  -> admission:Goal_native_admission.t
  -> authorize:
       (Operator_authorization.t
        -> Session_state.t
        -> (unit, Agent_protocol.Error.t) result)
  -> (Session_state.t, Agent_protocol.Error.t) result

module Goal_native_resume : sig
  module Detail : sig
    type t =
      | Actor_outcome of Goal_resume_result.Continuation.t
      | Unavailable
  end

  module Status : sig
    type t =
      | Staged of Agent_protocol.Goal_native_resume_intent.t
      | Accepted of Agent_protocol.Goal_native_resume_intent.t
      | Interrupted_before_dispatch of Agent_protocol.Goal_native_resume_intent.t
      | Dispatching of Agent_protocol.Goal_native_resume_intent.t
      | Unknown of Agent_protocol.Goal_native_resume_intent.t
      | Discarded of Agent_protocol.Goal_native_resume_intent.t
      | Rejected of Agent_protocol.Goal_native_resume_intent.t
      | Acknowledged of
          { intent : Agent_protocol.Goal_native_resume_intent.t
          ; receipt : Goal_command_receipt.t
          ; detail : Detail.t
          ; failure : Agent_protocol.Error.t option
          }
  end

  module Selection : sig
    (** Physical scheduling reference to the actual Invocation's accepted command
        loan. No decoder or ID constructor; this is not a source reservation or
        execution claim. The Owner must reserve its real stage before Begin. *)
    type t

    val request : t -> Goal_resume_request.t
    val session : t -> Agent_protocol.Session_ref.t
    val invocation_id : t -> Agent_protocol.Id.Invocation.t
  end
end

(** Actual retained native initial-episode selection and factual status. No
    decoder restores a Selection, original loan or source reservation. *)
module Goal_native_start : sig
  module Selection : sig
    type t

    val request : t -> Agent_protocol.Goal_native_start_request.t
    val session : t -> Agent_protocol.Session_ref.t
    val invocation_id : t -> Agent_protocol.Id.Invocation.t
  end

  module Status : sig
    type t =
      | Staged of Agent_protocol.Goal_native_start_intent.t
      | Accepted of Agent_protocol.Goal_native_start_intent.t
      | Interrupted_before_dispatch of Agent_protocol.Goal_native_start_intent.t
      | Dispatching of Agent_protocol.Goal_native_start_intent.t
      | Unknown of Agent_protocol.Goal_native_start_intent.t
      | Discarded of Agent_protocol.Goal_native_start_intent.t
      | Rejected of Agent_protocol.Goal_native_start_intent.t
      | Admitted of
          { intent : Agent_protocol.Goal_native_start_intent.t
          ; admission : Agent_protocol.Run_receipt.t
          ; failure : Agent_protocol.Error.t option
          }
  end
end

module Goal_native_action : sig
  module Selection : sig
    type t =
      | Start of Goal_native_start.Selection.t
      | Resume of Goal_native_resume.Selection.t
  end
end

(** The single retained action slot; selection does not claim dispatch or reserve
    a source. Missing host leaves accepted DATA pending. *)
val select_native_goal_action
  :  t
  -> (Goal_native_action.Selection.t option, Agent_protocol.Error.t) result

(** Actual Owner source reservation must precede this call. Claim ACK precedes
    consuming the same Entry original exactly once into private initial custody.
    Native command CAS/digest remain frozen; session preparation CAS is pinned once. *)
val begin_native_goal_start_preparation
  :  t
  -> selection:Goal_native_start.Selection.t
  -> (Run_preparation.Deferred.Decision.t, Agent_protocol.Error.t) result

(** Historical factual lookup, even after the original episode changes. This
    neither requires fresh execution custody nor installs any Run authority. *)
val native_goal_start_receipt
  :  t
  -> selection:Goal_native_start.Selection.t
  -> (Agent_protocol.Run_receipt.t option, Agent_protocol.Error.t) result

val native_goal_start_status
  :  t
  -> admission:Goal_native_admission.t
  -> authorize:
       (Operator_authorization.t
        -> Session_state.t
        -> (unit, Agent_protocol.Error.t) result)
  -> invocation_id:Agent_protocol.Id.Invocation.t
  -> (Goal_native_start.Status.t, Agent_protocol.Error.t) result

(** Actual Factory calls this after successful target/parent source joins and
    factual Deferred closure, including exceptional/cancelled primary outcomes.
    Validates the exact private owner/physical preparation. No phase rewrite,
    eligibility or no-effects grant. Retires terminal/unadmitted original only;
    a real admitted nonterminal Run retains the same original independently of
    closed preparation. Failure never replaces an existing primary failure. *)
val finish_native_goal_start_dispatch
  :  t
  -> selection:Goal_native_start.Selection.t
  -> completion:Run_preparation.Closed_attempt.t
  -> (unit, Agent_protocol.Error.t) result

(** Observe joined normal dispatch result. An actual Admitted association is
    factual even after terminal/source cleanup removed its live custody slot;
    this validates its immutable historical Run/link and never restores a loan.
    Fresh/claimed mutations still require the exact live slot. Actual admission
    is never rewritten by activation failure; unknown cannot become Rejected. *)
val observe_native_goal_start_dispatch
  :  t
  -> selection:Goal_native_start.Selection.t
  -> failure:Agent_protocol.Error.t option
  -> (unit, Agent_protocol.Error.t) result

val stage_native_goal_start
  :  t
  -> admission:Goal_native_admission.t
  -> authorize:
       (Operator_authorization.t
        -> Session_state.t
        -> (unit, Agent_protocol.Error.t) result)
  -> request:Agent_protocol.Goal_native_start_request.t
  -> (unit, Agent_protocol.Error.t) result

val select_native_goal_resume
  :  t
  -> (Goal_native_resume.Selection.t option, Agent_protocol.Error.t) result

val begin_native_goal_resume_preparation
  :  t
  -> selection:Goal_native_resume.Selection.t
  -> (Goal_continuation.Resume_issuance.t, Agent_protocol.Error.t) result

val observe_native_goal_dispatch
  :  t
  -> selection:Goal_native_resume.Selection.t
  -> failure:Agent_protocol.Error.t option
  -> (unit, Agent_protocol.Error.t) result

val native_goal_resume_receipt
  :  t
  -> selection:Goal_native_resume.Selection.t
  -> (Goal_command_receipt.t option, Agent_protocol.Error.t) result

(** Current authenticated native reader may inspect historical command facts.
    A receipt is checked against its stored original principal/request and actual
    immutable episode/admission link; it need not be the current Goal episode.
    Accepted/Claimed are durable facts; positive Actor_outcome comes only from the
    same preparation's actual acknowledged phase, never caller success DATA.
    Joined host detail is not certified here. Negative host detail is process-local.
    Recovery never restores custody from this projection. *)
val native_goal_resume_status
  :  t
  -> admission:Goal_native_admission.t
  -> authorize:
       (Operator_authorization.t
        -> Session_state.t
        -> (unit, Agent_protocol.Error.t) result)
  -> invocation_id:Agent_protocol.Id.Invocation.t
  -> (Goal_native_resume.Status.t, Agent_protocol.Error.t) result

module Child_origin : sig
  module Preparation : sig
    (** Same-domain birth custody captured by the actual parent Actor. It retains
        the original Factory/Store and native execution selection, not a public
        Goal ID or an ambient current Goal. No decoder; no effect permission. *)
    type t
  end

  module Bound : sig
    (** One actual lower-key/child/transaction allocation. Exact retry returns
        the same binding; a different allocation cannot reuse its preparation. *)
    type t

    (** Frozen DATA for this exact lower admission only. It cannot reconstruct
        preparation custody, issue a root route or authorize initialization. *)
    val origin : t -> Agent_store.Delegation_goal_origin.t
  end

  module Reservation : sig
    (** Same-domain private lower ACK, retaining actual Factory/Store and exact
        frozen origin. No decoder and no initializer permission by itself. *)
    type t
  end
end

(** Capture in the actual admitted native callback BEFORE yielding compilation
    or source preparation. Checks exact Admission/binding and registered native
    execution, actual original Run/Goal basis and original Factory/controller.
    [ancestry=None] is accepted only for this Actor's actual Spec with no
    delegation. A delegated parent needs complete known origin from the same
    Factory's checked private chain, re-resolved in this Actor's controller Store.
    Historical Unknown is never reconstructed or downgraded to Unrelated.
    Associated private origin additionally requires actual scoped accounting with
    the same session and positive origin. Known Unrelated permits historical
    missing/Unknown own accounting, but any positive stored origin must agree.

    This token classifies birth; it is not a current external root policy grant.
    Original native permission and all future owner-route/effect guards remain
    required. Currentness is repeated at binding, not extended by retention.
    Private Store reads and original authorization run outside the Actor mailbox;
    finite capture/consume checks retain the exact immutable parent basis. *)
val prepare_native_child_origin
  :  t
  -> source_owner:Runtime_source_owner.t
  -> admission:Native_tool_invocation.Admission.t
  -> binding:Chat_response.Tool_capability.binding
  -> ancestry:Delegation_authority.Checked_ancestry.t option
  -> (Child_origin.Preparation.t, Agent_protocol.Error.t) result

(** Separate trusted host-management path under its original operator admission.
    Classifies a new child explicitly Unrelated and never selects ambient Goal
    state. It is not a fallback after native preparation fails and does not grant
    management permission. Requires the original Factory/Actor/controller.
    Store observation is outside the mailbox against its exact captured basis. *)
val prepare_unrelated_child_origin
  :  t
  -> source_owner:Runtime_source_owner.t
  -> (Child_origin.Preparation.t, Agent_protocol.Error.t) result

(** Bind the exact protected recurrence row issued by this Actor. Its immutable
    principal/key/child/transaction come from the opaque permit and retained row.
    A bound definition also requires the actual delegated ancestry of its
    controller, when delegated, and uses its frozen creator Workflow association.
    An ordinary definition uses the original Unrelated management route. *)
val bind_recurrence_child_origin
  :  t
  -> source_owner:Runtime_source_owner.t
  -> permit:Recurrence_creation.t
  -> ancestry:Delegation_authority.Checked_ancestry.t option
  -> (Child_origin.Bound.t, Agent_protocol.Error.t) result

(** Bind before private lower reserve and child initialization. Rechecks original
    parent/native/Store selection and complete retained ancestry. Key principal
    matches the parent's factual creating_principal, as existing Factory creation
    requires; this does not turn that identity into execution permission.
    Inherited first-child anchors remain exact; only actual local Goal attribution
    adds this new edge. No reservation/frontier or runtime effect is performed.
    This prepares NEW birth only. An existing lower key must separately reconcile
    its retained original admission/origin; a fresh invocation must not reclassify
    that child under a later Goal episode or retrofit historical omission.
    Refresh invalidates its prior cached guard before any wait; failed/cancelled
    or superseded attempts cannot renew it. Only the finite final mailbox check
    consumes the exact externally observed capture and binds this allocation. *)
val bind_child_origin
  :  t
  -> preparation:Child_origin.Preparation.t
  -> key:Agent_store.Delegation_store.Key.t
  -> child_session_id:Agent_protocol.Id.Session.t
  -> transaction_id:Agent_protocol.Id.Transaction.t
  -> (Child_origin.Bound.t, Agent_protocol.Error.t) result

(** New reservation: exact Bound key/child/transaction/frozen origin; resolution
    is through actual Actor controller Store, never the supplied locator itself.
    Accepts live Reserved or later stage. No permission/initializer ACK minted. *)
val observe_child_origin_reservation
  :  t
  -> source_owner:Runtime_source_owner.t
  -> bound:Child_origin.Bound.t
  -> reference:Agent_store.Delegation_store.Reference.t
  -> (Child_origin.Reservation.t, Agent_protocol.Error.t) result

(** Existing lower key: derives original origin only from actual private record.
    Exact positive parent slot/frontier replays without a new write. A retained
    Unrelated admission with no child slot may recover through the same finite
    registration as new birth. An old Unknown slot or changed reference/origin
    cannot be relabelled. No new Bound or current Goal/episode classification.
    Historical lower omission stays Unknown and is not upgraded. A retained
    Associated admission may recover a missing frontier only against existing
    local roots through GD.register_retained_branch; all its local scopes and
    the exact child slot register atomically without canonical/account changes. *)
val observe_retained_child_origin_reservation
  :  t
  -> source_owner:Runtime_source_owner.t
  -> reference:Agent_store.Delegation_store.Reference.t
  -> (Child_origin.Reservation.t, Agent_protocol.Error.t) result

(** Parent GD/C ACK BEFORE child initializer/runtime construction. New local-root
    branch uses GD.register_branch; inherited-only or Unrelated GD.register_child.
    Historical absent GD accepts only proposed finite first-Unrelated reason,
    never an Associated partial origin. Existing frontier exact replay no-ops;
    a retained private Unrelated admission can recover a genuinely absent slot.
    Retained Associated uses GD.register_retained_branch for missing local
    frontiers and preserves original source/creator attribution. This factual
    recovery never authorizes execution after source adoption.
    Actual native original is retained from Bound on new path; retained replay
    takes genuine current borrowed callback independently of creator episode.
    Existing operator/parent policy remains check_parent, no child Actor needed. *)
val register_child_origin_reservation
  :  t
  -> reservation:Child_origin.Reservation.t
  -> native_callback:Native_tool_invocation.borrowed option
  -> check_parent:(Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> (unit, Agent_protocol.Error.t) result

(* Same creating Eio domain only. These values are factual
   association/read DATA, not child/Goal execution or source authority. *)
module Child_installation : sig
  type t
end

module Child_usage_observation : sig
  type t
end

module Child_usage_import : sig
  type t =
    | Imported of Child_contributions.disposition
    | Historical_absence
end

(** Factual snapshot of the actual child matching the exact private birth
    Reservation. Factory invokes outside mailbox/Owner locks while retaining its
    actual creation resource lease. A private finite child capture proves exact
    Spec reference/session/source-owner/controller; actual lower resolution and
    Store observation run outside the mailbox; finite consume rechecks same
    immutable State/issuer/controller/currentness before returning the existing
    opaque observation. Child_accounting.snapshot uses that actual captured State.

    Neither transcript-read authority nor parent fresh Goal policy is required
    to observe this factual accounting projection. No origin is inferred from
    snapshot fields; exact positive lower origin must match child GD. Nonzero
    retained snapshots remain factual DATA; absence never becomes zero. *)
val observe_child_origin_usage
  :  t
  -> reservation:Child_origin.Reservation.t
  -> (Child_usage_observation.t, Agent_protocol.Error.t) result

(** Import through the SAME private finite factual kernel as operator usage
    import, with closed birth-reservation provenance instead of a public generic
    authorize callback. The parent requires its exact acknowledged slot/frontier;
    the observation must retain that same reference/controller/source owner.
    Store/child refresh happens outside the mailbox; finite consume validates
    exact original State and final negative guards before the existing writer.

    [native_callback] preserves the original creation callback's Init/Goal/Held
    acknowledgement custody independently of transcript-read permission. It does
    not select a new parent Goal or grant child activation. Existing management
    check_parent remains nonyielding and cannot be replaced by decoded IDs.
    Duplicate/stale/absence disposition remains the existing DATA result; no
    canonical admission, replacement stream or new root confirmation is minted. *)
val import_child_origin_usage
  :  t
  -> reservation:Child_origin.Reservation.t
  -> observation:Child_usage_observation.t
  -> native_callback:Native_tool_invocation.borrowed option
  -> check_parent:(Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> (Child_usage_import.t, Agent_protocol.Error.t) result

(** Only the actual Factory privately supplies its existing issuer, after
    verifying its real child allocation/Actor/Store tuple. The proof exposes no
    usage. Exact Spec reference, same issuer and actual Store Current are required;
    decoded references, copied readonly identities and foreign Actors cannot
    mint a proof acceptable to this parent Factory. Creating another issuer does
    not make it equal to an existing Factory's private issuer. *)
val observe_child_installation
  :  t
  -> source_owner:Runtime_source_owner.t
  -> reference:Agent_store.Delegation_store.Reference.t
  -> (Child_installation.t, Agent_protocol.Error.t) result

(** Factual bounded slot ACK under the existing native/captured creation guard.
    [check_parent] must not yield or call another mailbox. It is the same current
    creation policy; this API creates no policy/Goal grant. It resolves the actual
    delegation and repeats pure association checks before the writer. Same slot is
    idempotent. Unknown ACK prohibits Linked/start; acknowledged Unobserved slots
    are retained even if later publication fails. *)
val register_child_installation
  :  ?native_callback:Native_tool_invocation.borrowed
  -> t
  -> source_owner:Runtime_source_owner.t
  -> child:Child_installation.t
  -> check_parent:(Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> max_children:int
  -> max_encoded_bytes:int
  -> (unit, Agent_protocol.Error.t) result

(** Independently authorized child transcript visibility before and after actual
    Store observation. The Factory additionally checks the actual owner tuple.
    An older snapshot remains immutable while refresh checks current visibility
    and the same live physical child/Store; a newer revision is not new authority.
    Historical missing accounting remains an opaque observation with no snapshot,
    never invented zero. No runtime or delegation scan is loaded into its version.
    The callback is nonyielding and may not call a mailbox. *)
val observe_child_usage
  :  t
  -> source_owner:Runtime_source_owner.t
  -> actor:Operator_authorization.t
  -> authorize:(Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> reference:Agent_store.Delegation_store.Reference.t
  -> (Child_usage_observation.t, Agent_protocol.Error.t) result

(** Parent read authority is checked independently. A factual first import may
    register one bounded Unobserved slot, retaining historical inventory uncertainty.
    Only exact actual child associations accept snapshots. Duplicate
    and stale imports do not write when their slot already exists. The final writer guard calls no mailbox or
    policy and repeats original/physical/snapshot currentness. Known acknowledged
    facts are not undone by a post-ACK visibility failure. *)
val import_child_usage
  :  t
  -> source_owner:Runtime_source_owner.t
  -> actor:Operator_authorization.t
  -> authorize:(Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> observation:Child_usage_observation.t
  -> max_children:int
  -> max_encoded_bytes:int
  -> (Child_usage_import.t, Agent_protocol.Error.t) result

(** Runtime descendant admission owners. Constructors retain native custody;
    decoded DATA grants none of these capabilities. All operations use their
    creating Eio domain. *)
module Descendant_admission : sig
  module Preparation : sig
    type t

    val occurrence : t -> Goal_descendant_occurrence.t
    val scopes : t -> Agent_store.Delegation_goal_origin.Scope.t list

    (** SAME captured own Spec reference, for routing only. No authority. *)
    val delegation : t -> Agent_store.Delegation_store.Reference.t option
  end

  module Root_route : sig
    type t

    (** Frozen locator DATA only, never authority for another Actor/Store. *)
    val scope : t -> Agent_store.Delegation_goal_origin.Scope.t
  end

  module Reservation : sig
    type t
  end

  module Reservation_attempt : sig
    (** Root-issued no-write plan with SAME original basis/preparation/policy
        cell and physical writer-attempt/ACK cells. Host retains BEFORE commit. *)
    type t

    val scope : t -> Agent_store.Delegation_goal_origin.Scope.t

    (** Existing actual positive ACK, including after publication failure.
        None is uncertainty/unattempted, never permission or a no-writer seal. *)
    val acknowledged : t -> Reservation.t option

    (** Readonly native writer/ACK outcome. No reservation constructor or refund
        authority. Host cleanup checks its issuing domain and exact child seal. *)
    type outcome =
      | Not_attempted
      | Acknowledged of Reservation.t
      | Attempt_unconfirmed

    val outcome : t -> outcome
  end

  module Reservation_set : sig
    (** Claimed exactly once by its physical Preparation in the actual child
        Slot, BEFORE first host await. No constructor, caller retain, reset,
        decoder or snapshot replacement. Claim survives factual cleanup. *)
    type t

    val attempts : t -> Reservation_attempt.t list

    (** SAME opaque preparation retained before the first host await. *)
    val preparation : t -> Preparation.t
  end

  module Reserve_result : sig
    type t =
      | Reserved of Reservation.t
      | Blocked of Goal_descendants.Availability.t
      | Attempt_unconfirmed of Reservation_attempt.t
  end

  module Root_before_writer : sig
    type t
  end

  module Before_writer : sig
    type t

    val check_current : t -> (unit, Agent_protocol.Error.t) result
  end

  module Closed_before_writer : sig
    type t
  end

  module Acknowledgement : sig
    (** Genuine actual child installed ACK: SAME Prep/identity + real canonical
        Handle/Turn_handle/tracking + installed State + complete Included vector
        + SAME driver-owned Reservation_set. No constructor or decoder. *)
    type t

    val reservations : t -> Reservation_set.t
  end

  module Import_acknowledgement : sig
    (** Positive actual immediate-parent import owner over SAME existing opaque
        Child_usage_observation carrier, not snapshot/disposition DTO alone. *)
    type t

    (** Importing parent's OWN installed Spec reference. The incoming child
        observation reference is not the next ancestor edge. Routing DATA only. *)
    val delegation : t -> Agent_store.Delegation_store.Reference.t option
  end

  module Confirmation : sig
    type t
  end

  module Root_admitted : sig
    type t
  end

  module Admitted_views : sig
    type t

    val check_current : t -> (unit, Agent_protocol.Error.t) result
  end

  (** Factual usage of the actual child under its genuine installed canonical
      ACK. Same Factory/Store/full source session and own Spec reference/origin
      are mandatory; actual Actor reconstruction is allowed. Capture and consume
      are finite; Store resolution/observation runs outside the Actor mailbox and
      ownership-state/registry locks, with exact
      post-await State/controller fences before interpreting errors.

      Reuses the existing opaque snapshot carrier; current usage may be newer
      than the ACK. No transcript grant, fresh Goal/source eligibility, old
      execution-owner liveness or initialized-runtime load is required. An
      initialized actual accounting snapshot is required; absence is not zero.
      Refresh retains immutable snapshot contents and returns only a current
      negative source guard. *)
  val observe_acknowledged_usage
    :  t
    -> source_owner:Runtime_source_owner.t
    -> acknowledgement:Acknowledgement.t
    -> (Child_usage_observation.t, Agent_protocol.Error.t) result

  (** Same factual source observation for the importing parent's next edge,
      under its genuine positive immediate-parent import ACK. Authenticates its
      OWN retained Spec reference, never the incoming child's reference. *)
  val observe_imported_usage
    :  t
    -> source_owner:Runtime_source_owner.t
    -> acknowledgement:Import_acknowledgement.t
    -> (Child_usage_observation.t, Agent_protocol.Error.t) result

  (** Outer capture, actual Store observation outside the Actor mailbox and
      ownership-state/registry locks, then finite consume. Every local-owner
      scope is selected from the actual complete vector. *)
  val bind_root_routes
    :  t
    -> source_owner:Runtime_source_owner.t
    -> ancestry:Delegation_authority.Checked_ancestry.t
    -> preparation:Preparation.t
    -> (Root_route.t list, Agent_protocol.Error.t) result

  (** Begin atomically creates and retains the next canonical root attempt in
      SAME uniquely claimed driver Set before replying. Duplicate scope rejects;
      retry the original retained attempt. No capacity is reserved by Begin.
      Commit recomputes current policy/count/nonce and may defer without writer.
      Physically attempted/unknown attempts NEVER re-enter the writer. *)
  val begin_reservation
    :  t
    -> route:Root_route.t
    -> reservations:Reservation_set.t
    -> (Reservation_attempt.t, Agent_protocol.Error.t) result

  val commit_reservation
    :  t
    -> attempt:Reservation_attempt.t
    -> (Reserve_result.t, Agent_protocol.Error.t) result

  val reconcile_reservation
    :  t
    -> attempt:Reservation_attempt.t
    -> (Reserve_result.t, Agent_protocol.Error.t) result

  val refresh_reserved
    :  t
    -> reservation:Reservation.t
    -> (Root_before_writer.t, Agent_protocol.Error.t) result

  val prepare_before_writer
    :  preparation:Preparation.t
    -> reservations:Reservation_set.t
    -> views:Root_before_writer.t list
    -> (Before_writer.t, Agent_protocol.Error.t) result

  (** Factual exact pending retirement under SAME actual physical child seal.
      Does not require live policy cell/current execution/Goal eligibility. *)
  val close_before_writer
    :  t
    -> reservation:Reservation.t
    -> closed:Closed_before_writer.t
    -> (unit, Agent_protocol.Error.t) result

  (** Same pure import planner/kernel as existing child usage; actual opaque
      source observation and physical Factory/Store association, no transcript
      or execution grant. Facts survive Held/closed/revocation/source changes. *)
  val import_usage
    :  t
    -> source_owner:Runtime_source_owner.t
    -> observation:Child_usage_observation.t
    -> (Import_acknowledgement.t, Agent_protocol.Error.t) result

  val confirmation
    :  Acknowledgement.t
    -> imports:Import_acknowledgement.t list
    -> (Confirmation.t, Agent_protocol.Error.t) result

  val refresh_admitted_root
    :  t
    -> reservation:Reservation.t
    -> acknowledgement:Acknowledgement.t
    -> confirmation:Confirmation.t
    -> (Root_admitted.t, Agent_protocol.Error.t) result

  val admitted_views
    :  acknowledgement:Acknowledgement.t
    -> confirmation:Confirmation.t
    -> views:Root_admitted.t list
    -> (Admitted_views.t, Agent_protocol.Error.t) result
end

(** Finite causal route association beneath per-H admission. These values are
    issued only from the SAME actual occupied causal preparation and frozen
    native selection. They grant no provider, count or token admission. All Store
    I/O runs outside the mailbox; finite consume repeats exact Actor/currentness. *)
module Causal_admission : sig
  module Preparation : sig
    type t

    val session : t -> Agent_protocol.Session_ref.t
  end

  module Reservation : sig
    type t

    val owner : t -> Agent_protocol.Session_ref.t
  end

  module Reservation_attempt : sig
    type t

    val owner : t -> Agent_protocol.Session_ref.t
    val acknowledged : t -> Reservation.t option

    type outcome =
      | Not_attempted
      | Known_not_reserved
      | Acknowledged of Reservation.t
      | Attempt_unconfirmed

    (** [Known_not_reserved] is a settled admission outcome, not evidence that no
        storage write occurred: a policy Hold may have been acknowledged. The
        detailed original result remains available through commit/reconciliation. *)
    val outcome : t -> outcome
  end

  module Reservation_set : sig
    type t

    val preparation : t -> Preparation.t
    val attempts : t -> Reservation_attempt.t list

    val requests
      :  t
      -> (Goal_policy_contributions.Request.t list, Agent_protocol.Error.t) result
  end

  module Acknowledgement : sig
    (** Only the actual canonical State-ACK suffix issues this custody. It keeps
        SAME native ACK, actual callee and Plan-owned actual reservation Set. *)
    type t

    val reservations : t -> Reservation_set.t
  end

  module Usage_observation : sig
    (** Complete preserved actual source projections. Neither decoded DATA nor
        this factual observation grants execution or initial confirmation. *)
    type t
  end

  module Import_acknowledgement : sig
    (** Initial-confirmation State ACK. Factual correction has separate custody
        and cannot enter the initial confirmation API. *)
    type t

    val reservation : t -> Reservation.t
  end

  module Root_before_writer : sig
    type t
  end

  module Before_writer : sig
    type t
  end

  (** Assemble only SAME complete ordered actual root views, with the consumed
      full-reference observation and complete actual owner bindings, including
      inline/covered owners when the remote vector is empty. No reservation or
      ACK is issued; original/inline policy remain Actor-owned. *)
  val prepare_before_writer
    :  preparation:Preparation.t
    -> reservations:Reservation_set.t
    -> views:Root_before_writer.t list
    -> (Before_writer.t, Agent_protocol.Error.t) result

  module Reserve_result : sig
    type t =
      | Reserved of Reservation.t
      | Blocked of Goal_admission_policy.Count_availability.t
      | Attempt_unconfirmed of Reservation_attempt.t
  end

  module Route_observation : sig
    type t

    (** Immutable complete target-owner locators, preserving captured order and
        typed full-Session_ref deduplication, including inline/covered owners.
        A locator does not authenticate or select a runtime. *)
    val owners : t -> Agent_protocol.Session_ref.t list
  end

  module Root_route : sig
    type t

    val owner : t -> Agent_protocol.Session_ref.t
    val target : t -> Policy_cause_data.Goal_target.t
  end

  (** Factual source capture/current Controller observation/finite consume.
      No old worker, Caller, live work row, current source epoch, Goal eligibility
      or transcript permission is needed. Actual Factory/Store/session and SAME
      immutable streams/frontier remain strict; old stream generations may
      survive a later State reset. Absence cannot become empty/zero. *)
  val observe_acknowledged_usage
    :  t
    -> source_owner:Runtime_source_owner.t
    -> acknowledgement:Acknowledgement.t
    -> (Usage_observation.t, Agent_protocol.Error.t) result

  (** Observe the original callee/stream under genuine initial import custody;
      this never exports the importing root's physical usage or renews an original. *)
  val observe_imported_usage
    :  t
    -> source_owner:Runtime_source_owner.t
    -> acknowledgement:Import_acknowledgement.t
    -> (Usage_observation.t, Agent_protocol.Error.t) result

  (** Same exact callee State/preparation/Set is retained before the first yield.
      Optional ancestry is genuine SAME issuer evidence for actual Spec.delegation;
      None is valid only for a real root. Every full Ref in every original Frame,
      current Cause and actual suffix is checked, including zero/covered targets.
      Metadata/source changes cause Conflict, never route relabeling or retry. *)
  val observe_routes
    :  t
    -> source_owner:Runtime_source_owner.t
    -> preparation:Preparation.t
    -> reservations:Reservation_set.t
    -> callee_ancestry:Delegation_authority.Checked_ancestry.t option
    -> (Route_observation.t, Agent_protocol.Error.t) result

  (** Actual root repeats SAME Store/issuer and all-record observations before
      validating exact Goal/source/GD/Scope. Only remote causal Overlay choices
      issue routes; covered/inline targets are validated and never reserved here.
      Historical missing physical root metadata remains unavailable. *)
  val bind_root_routes
    :  t
    -> source_owner:Runtime_source_owner.t
    -> observation:Route_observation.t
    -> (Root_route.t list, Agent_protocol.Error.t) result

  (** Retain SAME next native attempt and actual Actor route in SAME Plan-owned
      Set before reply. Both vectors are preallocated/validated before the two
      nonyielding assignments; Begin itself reserves no count. *)
  val begin_reservation
    :  t
    -> route:Root_route.t
    -> reservations:Reservation_set.t
    -> (Reservation_attempt.t, Agent_protocol.Error.t) result

  (** Fresh count/token/control/deadline check only before reserve. Exact actual
      writer kernel and ACK survive reply/publication failure. Unknown retains
      the original root fence and cannot invoke a second writer or refund. *)
  val commit_reservation
    :  t
    -> attempt:Reservation_attempt.t
    -> (Reserve_result.t, Agent_protocol.Error.t) result

  (** Factual native outcome only. Never captures fresh policy, starts a writer,
      repeats an old invocation or substitutes current State for original basis. *)
  val reconcile_reservation
    :  t
    -> attempt:Reservation_attempt.t
    -> (Reserve_result.t, Agent_protocol.Error.t) result

  (** Current exact pending plus count-free activation check for that owned
      reservation. Actual original/effect guard remains mandatory at callee.
      This view cannot stand in for the later canonical ACK/confirmation. *)
  val refresh_reserved
    :  t
    -> reservation:Reservation.t
    -> (Root_before_writer.t, Agent_protocol.Error.t) result

  module Closed_before_writer : sig
    (** Actual same-slot monotone seal; no constructor/decoder. Issued only by
        the existing governed callee's finite no-future-writer settlement. *)
    type t
  end

  module Confirmation : sig
    type t

    (** Immutable exact initial receipt vector; not a new confirmation issuer. *)
    val imports : t -> Import_acknowledgement.t list
  end

  module Root_admitted : sig
    type t
  end

  module Admitted_views : sig
    type t

    val check_current : t -> (unit, Agent_protocol.Error.t) result
  end

  (** Initial Pending -> Added only: SAME stream, pending.next_prefix and exact
      head/work/cause/origin witness. Actual ACK installed before publication.
      Duplicate/Stale alone cannot issue this ACK or clear Pending. *)
  val import_usage
    :  t
    -> reservation:Reservation.t
    -> observation:Usage_observation.t
    -> (Import_acknowledgement.t, Agent_protocol.Error.t) result

  module Correction_attempt : sig
    (** Exact actual root-issued immutable operation receipt; retained in SAME
        root fence before prepare replies and retained by host before commit. *)
    type t
  end

  module Correction_acknowledgement : sig
    (** Factual-only actual State ACK. Cannot be passed to [confirmation]. *)
    type t
  end

  module No_write : sig
    type t =
      | Exact_duplicate
      | Superseded
  end

  module Correction_preparation : sig
    (** [Awaiting_initial_confirmation] retains all custody without a write or
        settlement: another actual initial import must advance first. It is not
        a provider failure or permission to consume that attempt's pending. *)
    type t =
      | Prepared of Correction_attempt.t
      | No_write of No_write.t
      | Awaiting_initial_confirmation
  end

  (** Explicit factual correction. Actual prior import custody authenticates
      immutable same stream, not a later H admission. Current Book must return
      Replaced at CURRENT confirmed_prefix/unchanged head, or raw-changing
      Duplicate. Exact complete-byte Duplicate/Stale returns factual No_write
      without a writer/ACK; Superseded does not mean input equals current root. *)
  val prepare_correction
    :  t
    -> acknowledgement:Import_acknowledgement.t
    -> observation:Usage_observation.t
    -> (Correction_preparation.t, Agent_protocol.Error.t) result

  val commit_correction
    :  t
    -> attempt:Correction_attempt.t
    -> (Correction_acknowledgement.t, Agent_protocol.Error.t) result

  (** Exact receipt only; old ACK remains historical after a newer operation,
      never overwrites current phase. No re-invocation or fresh policy. *)
  val reconcile_correction
    :  t
    -> attempt:Correction_attempt.t
    -> (Correction_acknowledgement.t, Agent_protocol.Error.t) result

  (** Factual lost-prepare-reply lookup: SAME imported stream custody and SAME
      immutable observation identity, not old reservation alone. Returns only
      the exact already retained operation; never prepares/invokes one. *)
  val retained_correction
    :  t
    -> acknowledgement:Import_acknowledgement.t
    -> observation:Usage_observation.t
    -> (Correction_attempt.t option, Agent_protocol.Error.t) result

  (** Explicit finite refusal of SAME prepared operation. Before any writer it
      records actual rejection through its kernel then seals/restores. Unknown
      or ACK cannot be abandoned; no general timestamp/refund/cancel proof. *)
  val abandon_correction
    :  t
    -> attempt:Correction_attempt.t
    -> (unit, Agent_protocol.Error.t) result

  (** Initial-import reconciliation only; SAME retained initial receipt and
      immutable observation, even after a later correction. The receipt is
      installed at actual State ACK and never replaced by later usage. No new
      writer, policy check or disposition ACK. *)
  val reconcile_usage
    :  t
    -> reservation:Reservation.t
    -> observation:Usage_observation.t
    -> (Import_acknowledgement.t, Agent_protocol.Error.t) result

  (** No fresh route/current permission check. SAME actual slot seal + exact
      Pending; no unknown refund and no close after canonical ACK. *)
  val close_before_writer
    :  t
    -> reservation:Reservation.t
    -> closed:Closed_before_writer.t
    -> (unit, Agent_protocol.Error.t) result

  (** Pure exact ordered native vector check, including empty vector. Inline and
      physically covered choices have no remote ACK. Real canonical ACK + each
      actual initial import, never Set.complete/DTO/disposition alone. *)
  val confirmation
    :  Acknowledgement.t
    -> imports:Import_acknowledgement.t list
    -> (Confirmation.t, Agent_protocol.Error.t) result

  (** No prospective count. Current admitted Goal/source/deadline and actual root
      retained-owner view still gate the final callee check. Factual cleanup does
      not call this function or depend on its success. *)
  val refresh_admitted_root
    :  t
    -> reservation:Reservation.t
    -> acknowledgement:Acknowledgement.t
    -> confirmation:Confirmation.t
    -> (Root_admitted.t, Agent_protocol.Error.t) result

  val admitted_views
    :  acknowledgement:Acknowledgement.t
    -> confirmation:Confirmation.t
    -> views:Root_admitted.t list
    -> (Admitted_views.t, Agent_protocol.Error.t) result

  (** SAME Plan-owned factual transport, single elected driver owned by its
      existing614 Factual_usage cell. Retains exact observations/operations and
      successful initial prefix across Error/cancellation; no additional election
      or provider authority. Cached reads recheck actual source/Store/ACK custody. *)
  val initial_usage
    :  t
    -> source_owner:Runtime_source_owner.t
    -> acknowledgement:Acknowledgement.t
    -> (Usage_observation.t, Agent_protocol.Error.t) result

  val initial_imports
    :  Acknowledgement.t
    -> (Import_acknowledgement.t list, Agent_protocol.Error.t) result

  val initial_confirmation
    :  Acknowledgement.t
    -> (Confirmation.t option, Agent_protocol.Error.t) result

  val retain_initial_import
    :  Acknowledgement.t
    -> observation:Usage_observation.t
    -> imported:Import_acknowledgement.t
    -> (unit, Agent_protocol.Error.t) result

  val retain_initial_confirmation
    :  Acknowledgement.t
    -> Confirmation.t
    -> (unit, Agent_protocol.Error.t) result

  val correct_imported_usage
    :  t
    -> source_owner:Runtime_source_owner.t
    -> acknowledgement:Acknowledgement.t
    -> imported:Import_acknowledgement.t
    -> ( Inference_graph_owner.Factual_usage.Correction_outcome.t
         , Agent_protocol.Error.t )
         result
end

module Descendant_host : sig
  (** Actual Factory adapter captured under original Runtime_source_owner.
      Calls occur ONLY in existing child outer driver, never the Actor mailbox or ownership-state/Registry
      lock. Driver already owns exact set before first callback await. Child
      producer/writer/suffix and truthful post-ACK release remain Actor-owned. *)
  type t =
    { prepare :
        Descendant_admission.Preparation.t
        -> Descendant_admission.Reservation_set.t
        -> (Descendant_admission.Before_writer.t, Agent_protocol.Error.t) result
    ; close :
        Descendant_admission.Reservation_set.t
        -> Descendant_admission.Closed_before_writer.t
        -> (unit, Agent_protocol.Error.t) result
    ; confirm :
        Descendant_admission.Acknowledgement.t
        -> (Descendant_admission.Confirmation.t, Agent_protocol.Error.t) result
    ; refresh_admitted :
        Descendant_admission.Acknowledgement.t
        -> Descendant_admission.Confirmation.t
        -> (Descendant_admission.Admitted_views.t, Agent_protocol.Error.t) result
    }

  module Causal_driver : sig
    type t =
      { prepare :
          Causal_admission.Preparation.t
          -> Causal_admission.Reservation_set.t
          -> (Causal_admission.Before_writer.t, Agent_protocol.Error.t) result
      ; close :
          Causal_admission.Reservation_set.t
          -> Causal_admission.Closed_before_writer.t
          -> (unit, Agent_protocol.Error.t) result
      ; confirm :
          Causal_admission.Acknowledgement.t
          -> (Causal_admission.Confirmation.t, Agent_protocol.Error.t) result
      ; refresh_admitted :
          Causal_admission.Acknowledgement.t
          -> Causal_admission.Confirmation.t
          -> (Causal_admission.Admitted_views.t, Agent_protocol.Error.t) result
      ; correct_usage :
          Causal_admission.Acknowledgement.t
          -> Causal_admission.Confirmation.t
          -> ( Inference_graph_owner.Factual_usage.Correction_outcome.t
               , Agent_protocol.Error.t )
               result
      }
  end

  module Binding : sig
    (** Actual Factory source lifetime, with no execution permission or decoder. *)
    type t

    (** Pure same-domain retirement of this exact physical source. Idempotent;
        cannot clear a newer binding. Original native Plan retains its SAME host
        for factual ACK confirmation/close after retirement. No seal or refund. *)
    val close : t -> unit
  end
end

(** Finite actual Factory install before build/Init, including ordinary Ready
    reload without a Runtime_construction. Requires SAME issuer/domain/controller
    and physical selected State. Replaces only fresh host use; no Store/RPC or
    host callback runs in the Actor mailbox. Producer retains SAME binding/host
    before any root wait. Native original source/permission and policy checks
    remain independent; canonical/episode progress alone is not retirement.
    Retains the SAME actual delegated ancestry guard for historical own-origin
    classification outside the mailbox; omission alone never means Unrelated.

    During policy-only construction, [policy_restoration] must be the SAME
    target Actor's current construction association. This repeats its actual
    source/Caller admission before the unchanged Factory-owner, selected State
    and ancestry checks. Omission does not bypass construction exclusion. *)
val install_descendant_host
  :  ?policy_restoration:Policy_restoration.t
  -> ?causal_driver:Descendant_host.Causal_driver.t
  -> t
  -> source_owner:Runtime_source_owner.t
  -> expected:Session_state.t
  -> check_ancestry:
       (unit -> (Delegation_authority.Checked_ancestry.t, Agent_protocol.Error.t) result)
         option
  -> host:Descendant_host.t
  -> (Descendant_host.Binding.t, Agent_protocol.Error.t) result

(** Finite Factory install for a cold ordinary authored root reload. A Ready,
    stopped selection compares the exact construction inputs. Schedules remain
    live on the Actor, and their updates can advance the commit stamps while the
    Factory builds. An authenticated reload must supply its exact current Ready
    construction scope and retain its original admission. An unscoped reload
    can accept schedule progress only in the absence of an initialization scope.
    Both require current runtime and ordinary execution admission and the same
    Factory ownership and lifecycle. *)
val install_ordinary_ready_descendant_host
  :  ?initialization_scope:Initialization_scope.t
  -> ?causal_driver:Descendant_host.Causal_driver.t
  -> t
  -> source_owner:Runtime_source_owner.t
  -> expected:Session_state.t
  -> host:Descendant_host.t
  -> (Descendant_host.Binding.t, Agent_protocol.Error.t) result
