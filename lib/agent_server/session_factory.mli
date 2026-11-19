open! Core

(** Durable daemon session creation and recovery dependencies. *)

type limits =
  { max_journal_payload : int
  ; max_segment_bytes : int64
  ; max_segment_frames : int
  ; commit_queue_capacity : int
  ; mailbox_capacity : int
  ; snapshot_payload_limit : int
  ; snapshot_every_events : int
  ; snapshot_every_ms : int
  ; moderator_reservation_size : int
  ; history_block_size : int
  ; owner_lease_duration_ms : int
  ; event_replay_capacity : int
  ; max_attachments_per_session : int
  ; subscriber_queue_capacity : int
  ; job_result_inline_bytes : int
  ; job_result_max_bytes : int
  ; job_result_recovery_max_count : int
  ; job_result_recovery_max_bytes : int
  ; delegation_recovery_max_count : int
  ; delegation_recovery_max_bytes : int
  ; delegation_artifact_max_entries : int
  ; delegation_artifact_max_bytes : int
  ; delegation_max_depth : int
  ; managed_submission_max_count : int option
  ; managed_stop_max_count : int option
  ; managed_message_max_bytes : int option
  ; managed_output_page_max_bytes : int
  ; job_result_collection : Agent_store.Job_result_store.Publisher.collection_limits
  ; subscriptions : Agent_session.Staged_subscriptions.limits
  ; schedules : Agent_session.Staged_schedules.limits
  ; notifications : Agent_session.Staged_notifications.limits
  ; ingress : Agent_session.Staged_ingress.limits
  }

(** Host-owned, acknowledged inference tracking for this actual actor.
    Identity allocation and attempt callbacks preserve durable ownership; they
    are never invented from a presentation callback or ambient backend. *)
type runtime_inference_ports = Graph_tracking.Upstream.t

(** Composition-root selection policy. Capture and migration are distinct host
    decisions: reading targetless storage never invokes normal source capture.
    A missing migration callback leaves historical data inspectable but refuses
    model execution. Resolve checks the exact stored selection against current
    adapter eligibility and credentials policy without changing that selection.
    Capture, migration, approval and resolution perform no login, network or
    model effects. Model/settings updates retain target-owned future fields;
    independently admitted child/job selections remain unchanged. *)

type inference_policy =
  { capture_inference_target :
      prompt_revision_id:Agent_protocol.Id.Prompt_revision.t
      -> config:Chat_response.Config.t
      -> (Inference.Request.Target.t, Inference_runtime.Preparation_error.t) Result.t
  ; capture_developer_default_target :
      unit -> (Inference.Request.Target.t, Inference_runtime.Preparation_error.t) Result.t
    (** Trusted startup capture only for an explicitly configured Developer
        Provider_default. The composition root closes this over the SAME host
        as capture/profile/resolve below; unlike Prompt capture it has no
        prompt revision and cannot accept a fabricated one. Never invoked for
        Disabled or for an embedding-supplied Developer configuration. Startup
        resolves its result through this policy's exact resolver and fails on
        capture/resolve error. It is not a client model-selection grant. *)
  ; select_inference_profile :
      current:Inference.Request.Target.t
      -> profile:string
      -> (Inference.Request.Target.t, Inference_runtime.Preparation_error.t) Result.t
  ; recapture_inference_target :
      current:Inference.Request.Target.t
      -> prompt_revision_id:Agent_protocol.Id.Prompt_revision.t
      -> config:Chat_response.Config.t
      -> (Inference.Request.Target.t, Inference_runtime.Preparation_error.t) Result.t
  ; migrate_inference_target :
      (Agent_session.Session_state.Spec.t
       -> (Inference.Request.Target.t, Inference_runtime.Preparation_error.t) Result.t)
        option
  ; migrate_model_job_target :
      (Agent_session.Session_state.Spec.t
       -> Agent_protocol.Job.t
       -> (Inference.Request.Target.t, Inference_runtime.Preparation_error.t) Result.t)
        option
  ; approve_inference_target_change :
      current:Inference.Request.Target.t
      -> proposed:Inference.Request.Target.t
      -> (unit, Inference_runtime.Preparation_error.t) Result.t
  ; resolve_inference_context : Inference_runtime.resolver
  ; runtime_inference_ports :
      Agent_session.Session_actor.t
      -> (runtime_inference_ports, Inference_runtime.Preparation_error.t) Result.t
  }

type model_job_inference =
  { context : Inference_runtime.Context.t
  ; capture_recipe_target :
      Inference.Request.Target.t -> (unit, Agent_protocol.Error.t) Result.t
  }

module Child_source_policy : sig
  (** Host acquisition policy; retains no original directory. Its callback borrows
      actual original Scope and current dependency history for each new File child.
      Runtime semantic/shell/content policies remain the actual Factory policies. *)
  type t

  val create
    :  source:Agent_session.Captured_agent_runner.Source.t
    -> runtime_policy_revision:Chatml_source.Digest.t
    -> t

  (** Bind the configured source acquisition policy to one authentic retained
      Prompt revision's original source lease and selected compiler contract.
      This creates no new source grant or child execution authority. *)
  val bind
    :  t
    -> tool_contract:Chatml_compilation.Tool_contract.t
    -> active_agent_calls:Agent_session.Selected_agent_depth.Limits.t
    -> content_limits:Chatmd_prompt_content.Limits.t option
    -> artifact_store:Agent_store.Prompt_artifact_store.t
    -> source_lease:Agent_session.Prompt_source_lease.t
    -> Agent_session.Captured_agent_runner.Source_binding.t
end

type t

module Developer_catalog : sig
  type t

  module Admission : sig
    module Error : sig
      type t =
        | Busy
        | Publication_uncertain
        | Foreign_domain
      [@@deriving equal, sexp_of]
    end
  end

  val workspace_definitions : t -> Agent_session.Workspace_definition.t list

  val find_workspace
    :  t
    -> Agent_protocol.Id.Workspace_definition.t
    -> Agent_session.Workspace_definition.t option

  (** Read the current compiled profile during the caller's catalog admission.
      The ID is only selection data; this projection grants no run, source or
      native permission and cannot replace the publication owner. *)
  val find_profile : t -> string -> Agent_session.Permission_policy.t option

  val manifest_grants : t -> Operator_manifest_grant.t list
  val with_admission : t -> f:(unit -> 'a) -> ('a, Admission.Error.t) result
end

(** Read/admission projection of the factory's exact publication owner.
    It exposes no replacement/seal and cannot be minted from copied lists. *)
val developer_catalog : t -> Developer_catalog.t

(** Verify the actual target's pinned publication, complete stored tree and
    program/content custody before staging its source under the existing runtime
    owner. The entry must retain this Factory's actual live Store Handle, including
    a newly created entry not yet registered. Cached authored revisions use the Catalog's exact retained publication;
    captured recurrence children use their real private admission. No original
    source lookup, history/input conversion, Manager/initializer, native/MCP setup,
    provider call or Run admission occurs.

    Generated and authored-tool children borrow their actual already loaded
    parent's already-realized bindings for static restore/selection. The no-load
    parent lease is acquired before parent policy lookup and retained through [f];
    a cold parent or unrealized bindings reject without constructing resources.
    Ordinary eager/cold loading paths remain separate. The callback cannot retain
    Prepared or borrowed resource custody after return. The caller ends its actual
    Run preparation and still owns current command/delegation authorization,
    reciprocal ACK and final effect admission. Verification grants none of those.
    Cancellation and unexpected checkpoint failures propagate unchanged. The real owner also carries its deferred builder: only a consumed exact Held
    continuation can enter cold construction. It reuses this selected source and
    parent lease and repeats the joint fence after yielded preparation, before
    native/Agent/Manager/startup effects. Loaded reuse never initializes again.
    Scoped construction is not an input/Operation handoff receipt. *)
val with_prepared_run_source
  :  t
  -> Session_registry.entry
  -> preparation:Agent_session.Run_preparation.t
  -> (Runtime_owner.Prepared.t -> ('a, Agent_protocol.Error.t) result)
  -> ('a, Agent_protocol.Error.t) result

module Deferred_run_attempt : sig
  (** Result of one newly issued attempt after its actual source resources have
      joined. An error is not evidence that no effects occurred. *)
  type 'a t

  val result : 'a t -> ('a, Agent_protocol.Error.t) result

  (** Present only after successful cleanup, an eligible Actor-owned factual
      close and fresh live Store observation. This grants no retry, rotation or
      execution authority. An absent observation does not change [result]. *)
  val no_effects : 'a t -> Agent_session.Session_actor.No_effects_observation.t option

  (** The same opaque retry, available only after genuine joined target/parent
      cleanup and Actor acceptance of its pre-controller-write closure. Initial
      attempts always return [None]. This is never reconstructed from DATA.
      When present, [no_effects] is [None]; only this frozen retry can resume. *)
  val retry : 'a t -> Agent_session.Session_actor.Recurrence_retry.t option

  module Outcome : sig
    type 'a attempt = 'a t

    type 'a t =
      | Retained of Agent_protocol.Run_receipt.t
      | Attempted of 'a attempt
  end
end

(** Reserve the actual target's exclusive source lane before issuing a new
    Actor-authenticated preparation. This excludes already active ordinary
    runtime borrowers and prevents them from entering during source selection.
    The callback receives the same preparation and Prepared lifetime; it cannot
    retain either after return. A retained receipt bypasses the callback.

    Selection uses the same captured-source verification as
    [with_prepared_run_source]. Every acquired target and generated-parent lease
    must finish successfully before the Factory can complete the Actor's closed
    attempt and request a no-effects observation. Actual admission attempts or
    execution irreversibly disqualify it. Primary errors take precedence over
    cleanup failures; exceptions and cancellation propagate with their original
    backtrace. This does not rotate an attempt or automatically retry work. *)
val with_effect_deferred_run_source
  :  t
  -> Session_registry.entry
  -> actor:Operator_authorization.t
  -> authorize:(Agent_session.Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> request:Agent_session.Run_admission_request.Headless.t
  -> (Agent_session.Run_preparation.Deferred.t
      -> Runtime_owner.Prepared.t
      -> ('a, Agent_protocol.Error.t) result)
  -> ('a Deferred_run_attempt.Outcome.t, Agent_protocol.Error.t) result

(** Consume a selected native initial Goal command through the same source,
    preparation and joined cleanup as headless admission. The Actor owns its
    original; no caller-supplied operator or mutable revision is accepted.
    Native attempts do not issue a NEXT/retry observation. *)
val with_native_goal_start_source
  :  t
  -> entry:Session_registry.entry
  -> selection:Agent_session.Session_actor.Goal_native_start.Selection.t
  -> (Agent_session.Run_preparation.Deferred.t
      -> Runtime_owner.Prepared.t
      -> ('a, Agent_protocol.Error.t) result)
  -> ('a Deferred_run_attempt.Outcome.t, Agent_protocol.Error.t) result

(** Consume the exact old observation into one retry under the same authenticated
    target source reservation and retained-source verification as initial
    issuance. The callback receives its actual retry and Prepared; target
    admission remains blocked until the exact controller rotation ACK.

    A selected source failure can return the same retry through
    [Deferred_run_attempt.retry] only after successful full cleanup and actual
    Actor completion. Pending retry closure cannot issue an independent absence
    observation. A known rotation ACK may instead permit the ordinary NEXT
    observation after closure, subject to the Actor's factual phase/Store checks.
    Primary errors, exceptions and cancellation retain the initial API's rules;
    an exception propagates after factual close and does not fabricate a result. *)
val with_recurrence_retry_run_source
  :  t
  -> Session_registry.entry
  -> observation:Agent_session.Session_actor.No_effects_observation.t
  -> actor:Operator_authorization.t
  -> authorize:(Agent_session.Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> intent:Agent_session.Recurrence_execution_intent.t
  -> previous:Agent_session.Recurrence_execution_attempt.t
  -> request:Agent_session.Run_admission_request.Headless.t
  -> (Agent_session.Session_actor.Recurrence_retry.t
      -> Runtime_owner.Prepared.t
      -> ('a, Agent_protocol.Error.t) result)
  -> ('a Deferred_run_attempt.t, Agent_protocol.Error.t) result

(** Resume only the SAME frozen unpublished request from its actual joined
    pre-controller-write closure. No new request/intent/ordinal is accepted.
    Resource selection, cleanup, callback/error and admission rules are identical
    to [with_recurrence_retry_run_source]. *)
val resume_recurrence_retry_run_source
  :  t
  -> Session_registry.entry
  -> retry:Agent_session.Session_actor.Recurrence_retry.t
  -> actor:Operator_authorization.t
  -> authorize:(Agent_session.Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> (Agent_session.Session_actor.Recurrence_retry.t
      -> Runtime_owner.Prepared.t
      -> ('a, Agent_protocol.Error.t) result)
  -> ('a Deferred_run_attempt.t, Agent_protocol.Error.t) result

(** Parse the genuine held input with its own lexical inference graph. Call only
    from [Session_actor.with_held_input]'s preparation callback, while retaining
    the selected runtime and its source/parent leases. The supplied runtime is
    the one selected by that live runtime owner, not a substitute parser.
    The exact issued execution services are obtained from [scope]; the entry's
    actual actor validates scope and graph ownership. No future Operation or
    expired constructor tracking is borrowed.

    The parser joins its child calls before returning; this function seals and
    finishes their graph before the actor records the parsed result. Fresh
    effects require current authorization; exact completed outcomes and cleanup
    remain factual. Primary errors and cancellation survive cleanup, while a
    cleanup failure after successful parsing is reported. Parsed data alone
    neither starts the Run nor grants a successful activation handoff. *)
val prepare_held_input
  :  t
  -> entry:Session_registry.entry
  -> runtime:Agent_session.Runtime_builder.t
  -> scope:Agent_session.Session_actor.Held_input_scope.t
  -> ( Agent_session.Runtime_input_preparation.Prepared_input.t
       , Agent_protocol.Error.t )
       result

(** Startup-only once binding of the SAME retained Authoring_installation source
    port, before scheduler/client execution exposure. Repeated installation
    rejects. Existing lazy-runtime services consult it at use, so recovery does
    not capture stale absence. Factory owns no duplicate source owner; the
    installation must outlive all session consumers. Closed/absent roots deny. *)
val install_instruction_sources
  :  t
  -> Agent_session.Instruction_source_port.t
  -> (unit, Agent_protocol.Error.t) result

type generated_lifetime =
  | Owned
  | Independent
[@@deriving equal, sexp_of]

val create
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> store:Agent_store.Session_store.t
  -> registry:Session_registry.t
  -> idempotency_store:Agent_store.Idempotency_store.t
  -> blob_store:Agent_store.Blob_store.t
  -> prompts:Agent_session.Prompt_catalog.t
  -> workspaces:Agent_session.Workspace_catalog.t
  -> permission_profiles:Agent_session.Permission_policy.t list
  -> manifest_grants:Operator_manifest_grant.t list
  -> quota_manager:Agent_session.Quota_manager.t
  -> job_capacity:Job_capacity.t
  -> tool_dir:string
  -> home:string option
  -> inference_policy:inference_policy
  -> qualify_chatml_extensions:bool
  -> session_helpers:Agent_session.Session_management_channel.grant list
  -> independent_lifetime_policy:string option
  -> chatml_runtime_policy:Chat_response.Runtime_semantics.policy
  -> authoring_validation_host:Chat_response.Authoring_validation.host option
  -> script_preparation:Agent_session.Runtime_script_preparation.t option
  -> content_limits:Chatmd_prompt_content.Limits.t option
  -> child_sources:Child_source_policy.t option
  -> recurrence_planning_host:Agent_session.Recurrence_planning_host.t option
  -> durability:Agent_store.Journal_segment.durability
  -> limits:limits
  -> t

(** Resolve this exact job's durable source selection, explicitly migrating and
    acknowledging its capture if authorized. Returned recipe capture is bound to
    this job generation/attempt; no current-parent derivation or ambient fallback.
    The caller has claimed the actual running job before recipe execution. *)
val model_job_inference
  :  t
  -> Session_registry.entry
  -> Agent_protocol.Job.t
  -> (model_job_inference, Agent_protocol.Error.t) Result.t

module Catalog_seal : sig
  module Error : sig
    type t =
      | Busy
      | Workspaces_busy
      | Publication_uncertain
      | Foreign_domain
      | Stale_preparation
      | Aborted
      | Committed
    [@@deriving equal, sexp_of]
  end

  (** Creating-domain reservation for precomputed immutable catalog state. None of
      these operations acquires locks, polls, invokes callbacks, allocates or yields.
      This token neither acquires native source authority nor admits authorization. *)
  type t

  (** Before acknowledged durability only. Releases both reservations, preserving
      published state; repeated abort succeeds. Committed/uncertain tokens reject. *)
  val abort : t -> (unit, Error.t) Result.t

  (** Preserves old pointers and permanently blocks replacements on this owner.
      Repeated marking succeeds; committed/aborted tokens reject. *)
  val mark_uncertain : t -> (unit, Error.t) Result.t

  (** After acknowledged durability only. Installs precomputed pointers; repeated
      commit succeeds without reinstalling old state. Aborted/uncertain reject. *)
  val commit : t -> (unit, Error.t) Result.t
end

(** Preallocate and reserve the actual workspace/profile/history/grant replacement
    before external durable publication. This token owns no Prompt_catalog or
    baseline transaction. The host seals those separately and commits ALL pointer
    swaps in one same-domain protected nonyielding phase only after durable success.
    Current sessions' pinned profile revisions remain available. Original checkpoint
    exceptions/cancellation propagate; legacy installation cannot invalidate a seal. *)
val seal_catalogs
  :  t
  -> workspaces:Agent_session.Workspace_catalog.t
  -> permission_profiles:Agent_session.Permission_policy.t list
  -> manifest_grants:Operator_manifest_grant.t list
  -> checkpoint:(unit -> unit)
  -> (Catalog_seal.t, Catalog_seal.Error.t) Result.t

(** Compatibility publication for future sessions, retaining pinned permission
    revisions. Uses the same reservation as [seal_catalogs] and rejects pending,
    uncertain or foreign-domain installation before either catalog changes.
    Does not replace native source ownership or authorization admission. *)
val install_catalogs
  :  t
  -> workspaces:Agent_session.Workspace_catalog.t
  -> permission_profiles:Agent_session.Permission_policy.t list
  -> manifest_grants:Operator_manifest_grant.t list
  -> unit

(** Fresh catalog-dependent creation borrows one coordinated publication generation
    through source/workspace/profile selection, layout and joined runtime preflight.
    Factory or Prompt_catalog sealed/uncertain state rejects before source selection.
    Coordinated publishers must reserve [seal_catalogs] before durable publication.
    Existing pinned restoration and inherited child creation keep their own captured
    admission paths; they do not select a new prompt definition here. *)
val create_session
  :  t
  -> command_audit:Document_schema.Document.t option
  -> principal:Agent_protocol.Principal.t
  -> Agent_protocol.Session.Create_request.t
  -> (Session_registry.entry, Agent_protocol.Error.t) result

(** Same actual creation owner, retaining the original authentication through
    actor-owned runtime initialization. An expired proof rejects; this never
    derives authority from the saved creating principal. *)
val create_session_authenticated
  :  t
  -> command_audit:Document_schema.Document.t option
  -> actor:Operator_authorization.t
  -> Agent_protocol.Session.Create_request.t
  -> (Session_registry.entry, Agent_protocol.Error.t) result

(** Validate pinned source/profile/workspace and explicitly approved target
    recapture without evaluating initializers. Return Pending selected state.
    The caller must fence/retire old resources and atomically compare-and-set this
    candidate before constructing any new runtime. Failed initialization retains
    the selected configuration as durable notactivated evidence. *)
val prepare_administration
  :  t
  -> Session_registry.entry
  -> previous:Agent_session.Session_state.t
  -> Agent_session.Session_state.t
  -> fresh_history:bool
  -> (Agent_session.Session_state.t, Agent_protocol.Error.t) result

(** [import_legacy t ~principal ~source_id ~source_path ~legacy request]
    creates one stopped durable session from a validated legacy snapshot. It
    records immutable source provenance and never mutates [source_path]. Uses the
    SAME scoped publication admission as [create_session] through joined layout and
    durable creation; pending/uncertain publication rejects before source selection. *)
val import_legacy
  :  t
  -> principal:Agent_protocol.Principal.t
  -> source_id:string
  -> source_path:string
  -> legacy:Session.t
  -> Agent_protocol.Session.Create_request.t
  -> (Session_registry.entry, Agent_protocol.Error.t) result

(** Loads and registers indexed durable sessions required at startup, in private
    parent-before-child dependency order. Includes needed ancestors, rejects cycles
    and excessive depth, and rolls back loaded entries on failure. Stopped generated
    inspection does not require loading a deleted/revoked parent. *)
val recover_sessions : t -> (Session_registry.entry list, Agent_protocol.Error.t) result

(** Startup, before accepting commands or starting schedulers. Validate unfinished
    creation artifacts and complete linking for installed stopped children against
    current parent authority. Intents without a child await a keyed retry; missing
    or stopped parents revoke their incomplete admissions. No generated initializer
    or model call runs here. Corrupt installed data fails closed. *)
val reconcile_generated_creations : t -> (unit, Agent_protocol.Error.t) result

(** Qualified internal host creation of a generated child.
    Revalidates the prepared definition against the loaded parent's exact native
    bindings, persists a protected retry mapping and complete initial journal/
    snapshot before publication, then links under the parent's actor checkpoint.
    Defaults to stopped. [start_immediately] persists an initial activation intent
    before linking, then loads and starts the child. Retries and startup recovery
    resume this intent, but never restart a subsequently stopped child. Permanent
    activation failure is retained on the inspectable child. Does not start a model
    turn or grant caller access.
    External adapters must authenticate their invoking parent before calling.
    Uses the parent's durable principal, workspace and permission profile.
    [lifetime] defaults to [Owned]. [Independent] requires the configured trusted
    [independent_lifetime_policy]; its digest is recorded and checked on every
    restoration/invocation. For stateful moderation, each ancestor must supply
    eligible source evidence for its original policy owner. Subsequent policy work
    requires fresh admission. Creation needs an active parent through the durable
    link checkpoint.
    Failed/ambiguous installs retain their private reservation for reconciliation. *)
val create_generated_session
  :  ?start_immediately:bool
  -> ?lifetime:generated_lifetime
  -> t
  -> parent_session_id:Agent_protocol.Id.Session.t
  -> idempotency_key:Agent_protocol.Idempotency_key.t
  -> display_name:string option
  -> Agent_session.Generated_definition.t
  -> (Session_registry.entry, Agent_protocol.Error.t) result

module Recurrence_child : sig
  (** Stopped durable child data, never Run or initialization permission. *)
  type t

  val session : t -> Agent_protocol.Session_ref.t
  val reference : t -> Agent_store.Delegation_store.Reference.t
end

(** Consume the actual controller permit to create or recover its one immutable
    captured child through the existing delegation/artifact/session owners.
    Current caller and host policies are rechecked; original stored principal is
    audit data only. Parent association ACK precedes lower Linked publication
    and registry installation. All retries retain the same IDs and exact source
    publication, including reuse of the controller's artifact. No initializer,
    runtime activation or Run occurs. Ordinary execution remains fenced pending
    genuine reciprocal occurrence Run admission. *)
val create_recurrence_child
  :  t
  -> parent:Session_registry.entry
  -> permit:Agent_session.Session_actor.Recurrence_creation.t
  -> (Recurrence_child.t, Agent_protocol.Error.t) result

(** Resume indexed pending generated starts under the daemon scheduler. Temporary
    ancestor unavailability leaves the intent pending; terminal activation errors
    are committed to the child. Persistence failures remain pending for retry. *)
val resume_generated_initial_starts : t -> unit

(** Reconcile a generated child's acknowledged parent stop and join old work
    before an authorized explicit start. Must run outside runtime-owner locks.
    Current parent policy and linkage are checked before changing child state. *)
val prepare_session_start
  :  t
  -> Session_registry.entry
  -> (unit, Agent_protocol.Error.t) result

(** Conservatively retain an inherited workspace while a privately linked
    Independent child is running or has a durable initial-start intent, including
    the interval before native resource borrowing. Uses bounded private records
    and published index hints; never enters another actor or activates a runtime.
    Call under the workspace owner's maintenance lock and propagate read errors.
    This supplies retention, not execution authority. *)
val workspace_retained
  :  t
  -> Agent_session.Session_state.t
  -> (bool, Agent_protocol.Error.t) result

(** [complete_index_recovery t entries] checkpoints reconciled actor metadata
    and accurate scheduling hints before clearing the durable rebuild marker.
    Call with the entire successful [recover_sessions] result, only after job
    and schedule reconciliation succeeds. Any checkpoint failure retains the
    marker and prevents readiness. *)
val complete_index_recovery
  :  t
  -> Session_registry.entry list
  -> (unit, Agent_protocol.Error.t) result

(** Restore immutable durable state without activating source/runtime/target,
    acquiring a writer, changing recovery metadata, or repairing a torn tail.
    Original retained frames, fallbacks and bounded archives are validated under
    the same admission policy. The temporary session lock is always released. *)
val read_session
  :  t
  -> Agent_store.Session_index.Entry.t
  -> (Agent_session.Session_state.t, Agent_protocol.Error.t) Result.t

(** Same immutable replay and temporary Handle lifetime as [read_session],
    with a checked recurrence projection captured before closing the Handle. *)
val read_recurrence_snapshot
  :  t
  -> actor:Operator_authorization.t
  -> authorize:(Agent_session.Session_state.t -> (unit, Agent_protocol.Error.t) Result.t)
  -> request:Agent_session.Recurrence_query.request
  -> Agent_store.Session_index.Entry.t
  -> ( Agent_session.Session_state.t * Agent_session.Recurrence_query.snapshot
       , Agent_protocol.Error.t )
       Result.t

(** Startup-only fold before any loader, recovered actor or scheduler exists.
    The actual indexed projection is checked under Registry's read lease. One
    same-store Handle owns the nonactivating immutable current state read and all
    archive reads, then closes before return. Current snapshot/journal decoding
    uses the existing recovery/state validators; archive reads are deferred until
    their complete reference list is admitted by [admit_archive], before retaining
    its lookup or reading an archive. Ordinary [read_session] validation stays
    unchanged.

    Fold the current state and each referenced pre-change snapshot once through
    the existing checksum/session/revision/disposition archive decoder. Nested
    historical references must name the same archive file identity present in the
    complete current list; no recursive replay or second decoder. [checkpoint]
    covers reference traversal/comparison. The caller owns aggregate read-count,
    job and logical traversal-work admission; existing storage byte ceilings remain
    per state/archive, not a newly claimed aggregate I/O budget. Any error aborts
    the complete protection attempt; no actor, source, target or runtime activates.
    Caller holds exclusive startup ownership and must skip all pruning on failure.
    Callback exceptions retain identity through existing Handle/read cleanup. *)
val fold_indexed_retained_states
  :  t
  -> Agent_store.Session_index.Entry.t
  -> authorize:(Agent_protocol.Session.t -> (unit, Agent_protocol.Error.t) Result.t)
  -> checkpoint:(unit -> unit)
  -> admit_archive:(unit -> (unit, Agent_protocol.Error.t) Result.t)
  -> init:'a
  -> f:('a -> Agent_session.Session_state.t -> ('a, Agent_protocol.Error.t) Result.t)
  -> ('a, Agent_protocol.Error.t) Result.t

(** Automatic host recovery, retaining the actual recovery selection through
    startup or lazy registry publication. Validated captured recurrence children
    undergo normal durable/Run recovery without runtime construction and remain
    unselected until a genuine new activation handoff. Caller serializes the ID
    and must publish with [Session_registry.add_recovered] or the recovery loader. *)
val recover_registered_session
  :  t
  -> Agent_store.Session_index.Entry.t
  -> (Session_registry.Recovered.t, Agent_protocol.Error.t) result

(** Reconstructs one genuinely ordinary indexed session on demand. The caller must
    serialize loads for a session ID and register the returned entry exactly once.
    Captured recurrence children require [recover_registered_session]; this entry-
    only compatibility path refuses them rather than discarding selection. *)
val recover_session
  :  t
  -> Agent_store.Session_index.Entry.t
  -> (Session_registry.entry, Agent_protocol.Error.t) result

(** Immutable Recovery.read using the caller's existing checked Handle/actor lock.
    Retains no resource and performs no repair, source/credential/runtime creation
    or checkpoint writes. Generated and authored delegated source identity/contract
    and captured inference target are checked against their retained admission,
    without requiring a live parent or native workspace. Caller closes Handle and
    excludes lifecycle mutations. *)
val read_owned_session
  :  t
  -> Agent_store.Session_store.Handle.t
  -> (Agent_session.Session_state.t, Agent_protocol.Error.t) Result.t

(** Canonical durable metadata projected from an immutable state. This pure
    operation grants no storage or lifecycle authority; callers retain the
    existing Store admission and validation requirements. *)
val metadata : Agent_session.Session_state.t -> Agent_store.Session_store.Metadata.t

(** Complete canonical scheduling projection from an immutable state. Lifecycle
    authority is applied separately by Session_store.prepare_lifecycle. *)
val index_entry : Agent_session.Session_state.t -> Agent_store.Session_index.Entry.t

(** Recheck current bounded private linkage under the generated-creation
    coordinator. Live parent/child obligations reject removal; revoked terminal
    historical links with completed cleanup may remain. Caller retains lifecycle
    exclusion through subsequent publication and physical cleanup. *)
val validate_session_removal
  :  t
  -> Agent_session.Session_state.t
  -> (unit, Agent_protocol.Error.t) Result.t

(** Borrow no second Handle: validated same-Store actual Handle transfers to the
    concrete recovery owner before any fallible retained-parent preparation or
    yielding construction/activation. Invalid host depth limits therefore clean or
    retain this same owned Handle rather than reject before adoption. Success returns
    actual entry; failure cleans or retains that same partial owner in Registry.
    A foreign Handle is rejected without transfer or mutation. Caller holds issuing
    per-ID reservation and has consumed exact current lifecycle/anchor witness.
    Current retained ancestors are read without activation under their existing
    owned leases. Missing loaded ancestry alone preserves a pending initial start
    only when immutable generation/source/epoch/linkage remains valid. Known
    retirement consumes it; unavailable/corrupt observations remain errors.
    No nested reservation for this same session is acquired. The supplied actual
    original selection actor is retained through eager runtime reconstruction;
    Ready legacy Session_resume acquires its genuine Runtime_construction scope
    with that same current actor and CAS. No saved creator or absent-original
    fallback is inferred. Automatic recovery remains a separate host path. *)
val recover_owned_session
  :  t
  -> Agent_store.Session_store.Handle.t
  -> actor:Operator_authorization.t
  -> (Session_registry.entry, Agent_protocol.Error.t) Result.t

(** Pure same-host actual Handle ownership check before reconstruction transfer. *)
val owns_handle : t -> Agent_store.Session_store.Handle.t -> bool

(** Negative-only current owner association for retained execution policy. Checks
    this Factory's exact registered Actor/Owner/Handle, Store identity, closing
    and retained failed cleanup. A transient read/lifecycle reservation alone
    does not revoke this association. No admission, selection, runtime or source
    permission is granted; fresh entrypoints still use [check_registered_entry]
    and actual Actor lifecycle and execution guards remain mandatory. Same
    creating Eio domain only. *)
val check_registered_owner
  :  t
  -> Session_registry.entry
  -> session:Agent_protocol.Session_ref.t
  -> (unit, Agent_protocol.Error.t) result

(** [check_registered_owner] plus the existing canonical execution-selection
    observation. For retained original policy only; fresh admission still uses
    [check_execution_selected_entry]. Does not load or select an owner. Same
    creating Eio domain only. *)
val check_execution_selected_owner
  :  t
  -> Session_registry.entry
  -> session:Agent_protocol.Session_ref.t
  -> (unit, Agent_protocol.Error.t) result

(** Pure negative association check against this Factory's actual registry,
    Actor/Owner pairing and owned Handle. [session] is expected-reference DATA,
    compared with the actual Store server and Handle session. Closing, failed
    cleanup, foreign or displaced owners, and concurrent lifecycle reservations
    return nonretryable [Conflict]. No runtime is loaded. Stopped/recovered entries remain
    valid for factual reconciliation. This grants no permission: actual source
    CPS still requires its private Factory key, and all final authority checks
    remain in the existing Actor/Runtime_owner ports. *)
val check_registered_entry
  :  t
  -> Session_registry.entry
  -> session:Agent_protocol.Session_ref.t
  -> (unit, Agent_protocol.Error.t) result

(** Rebuild missing-index projections through immutable owned reads without
    actors/providers. Retain the eager-recovery marker for actual Execute recovery. *)
val rebuild_index_projections : t -> (unit, Agent_protocol.Error.t) Result.t

(** Retry durable initial-start intents only on actual loaded selected owners;
    never discover/load indexed sessions. Current linkage still governs each start. *)
val resume_selected_initial_starts : t -> unit

module Seed_recovery_limits : sig
  (** One complete seed reconciliation invocation. Session rows/first complete
      transactions are counted before retention; bytes conservatively cover all
      charged frames/read buffers, including rechecks. Work counts each borrowed
      checkpoint through enumeration, decoding, comparisons and repair; bounded
      admitted codec/comparator stages may be nonpreemptible between polls.
      These are independent positive bounds, not a heap sandbox or transport
      allowance. A byte-admitted document may still exceed the work bound. *)
  type t

  val create
    :  max_sessions:int
    -> max_transactions:int
    -> max_input_bytes:int
    -> max_work_steps:int
    -> (t, Agent_protocol.Error.t) Result.t

  (** Configurable host default: 4096 rows/first transactions, 256 MiB conservative
      aggregate, 2M cooperative steps. Startup and receipt service borrow the same
      configured profile; each reconciliation is one fresh invocation. *)
  val default : t

  val max_sessions : t -> int
  val max_transactions : t -> int
  val max_input_bytes : t -> int
  val max_work_steps : t -> int
end

module Seed_outcome : sig
  type t =
    | Created of Session_registry.entry
    | Rejected of Agent_protocol.Error.t
    | Uncertain of Agent_protocol.Error.t

  (** Rejected proves no layout attempt occurred. Uncertain must retain the
      original protected Pending and must not execute creation again. Created
      owns an unloaded stopped entry after journal/snapshot/index admission.
      Exceptions retain their identity/backtrace after joined cleanup. *)
end

(** Admit a new stopped identity under actual current catalog/workspace/profile
    policy. Retained selection borrows the exact authorized lifecycle owner and
    accepts authored roots only; current profile digest/runtime policy must still
    authorize its pinned policy. Prelayout complete Created bounds precede Store
    allocation. No Converter, initializer, provider request, attachment or runtime
    load occurs. Caller holds the real original protected command reservation. *)
val seed_session
  :  t
  -> principal:Agent_protocol.Principal.t
  -> request:Agent_protocol.Session_seed.Request.t
  -> command_audit:Document_schema.Document.t
  -> checkpoint:(unit -> unit)
  -> Seed_outcome.t

(** Before request service in Execute and On_demand modes, scan the actual owned
    store under finite positive aggregate limits, validating real Created audit
    evidence. Recover the original identity/result into existing protected
    receipts and repair incomplete initial snapshot/index through existing
    storage owners. No actor/runtime/provider/source reconstruction occurs.
    Absent/ambiguous outcomes remain Pending; conflicting identities fail closed.
    General snapshot-less replay and legacy import rules remain unchanged. *)
val reconcile_seed_creations
  :  t
  -> limits:Seed_recovery_limits.t
  -> checkpoint:(unit -> unit)
  -> (unit, Agent_protocol.Error.t) Result.t

module Recurrence_source_preview : sig
  (** Joined immutable proposal DATA; no preparation, source loan, NoEffects,
      runtime or execution permission. The later actual service restages it. *)
  type t

  val source : t -> Agent_protocol.Run_source.t
  val expected_revision : t -> int64
end

(** Acquire the exact local target's canonical metadata owner without runtime
    construction, inference migration, Session_resume or autonomous start. Reuse
    an existing actual owner physically; reconstruct an indexed target once under
    the actual registry lifecycle reservation, owned Handle and recovery owner.
    Active/Automatic lifecycle, actual provenance/profile/workspace/ancestry and
    NONQUEUING capacity remain required. Repeat the original Send_messages/read
    proof and supplied nonyielding host state policy outside internal mutexes.
    Failure preserves the primary error/exception and owns unfinished cleanup.
    Success stays unselected until actual checked handoff or explicit selection;
    full source failure can retain this SAME unloaded owner for genuine retry. *)
val acquire_recurrence_target
  :  t
  -> session:Agent_protocol.Session_ref.t
  -> actor:Operator_authorization.t
  -> authorize:(Agent_session.Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> (Session_registry.entry, Agent_protocol.Error.t) result

(** Immutable factual read through this Factory's actual Registry. Checks local
    server, current original Send/read scopes and supplied nonyielding summary
    visibility before IO and again after the joined read. Never loads/selects an
    owner or constructs a runtime. Errors and cancellation remain unchanged. *)
val read_recurrence_state
  :  t
  -> session:Agent_protocol.Session_ref.t
  -> actor:Operator_authorization.t
  -> authorize:(Agent_protocol.Session.t -> (unit, Agent_protocol.Error.t) result)
  -> (Agent_session.Session_state.t, Agent_protocol.Error.t) result

(** Recover only an actual retained reciprocal captured child. Current parent
    original visibility and host target policy are required, but no new creation
    permit, Enabled control, grace, native installation or initialization occurs.
    Verify the real stored delegation/link/Spec/artifact through the canonical
    metadata acquisition owner. Missing facts cannot create a replacement child.
    Returned opaque Child DATA grants no execution permission. *)
val acquire_recurrence_child
  :  ?fresh_goal:bool
  -> t
  -> parent:Session_registry.entry
  -> actor:Operator_authorization.t
  -> authorize:
       (Agent_session.Session_state.t
        -> Agent_session.Recurrence_definition.t
        -> (unit, Agent_protocol.Error.t) result)
  -> occurrence:Agent_session.Recurrence_identity.Occurrence.t
  -> (Recurrence_child.t * Session_registry.entry, Agent_protocol.Error.t) result

(** Select/verify the actual source through the existing static source path and
    installation rules. Join every temporary source/parent loan before returning
    source+CAS DATA. No native/MCP, Manager, initializer, input/history, runtime or
    provider work. The original proof and actual source/revision repeat after
    yields and cleanup. Cold/unrealized Generated parents are not warmed. *)
val preview_recurrence_source
  :  t
  -> Session_registry.entry
  -> actor:Operator_authorization.t
  -> authorize:(Agent_session.Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> (Recurrence_source_preview.t, Agent_protocol.Error.t) result

(** Pure exact registered-owner selection check. Metadata-only targets are valid
    registered source/activation owners but cannot serve as selected controllers.
    Ordinary genuine pre-registration creation remains a separate private path. *)
val check_execution_selected_entry
  :  t
  -> Session_registry.entry
  -> session:Agent_protocol.Session_ref.t
  -> (unit, Agent_protocol.Error.t) result

(** Initial recurrence controller selection, before the scheduler has issued a proof
    or attempted work. A temporary lifecycle reservation on the exact, selected
    owner returns retryable [Conflict]. Ordinary entry and execution admission
    retain their nonretryable reservation checks. *)
val check_recurrence_controller_selection
  :  t
  -> Session_registry.entry
  -> session:Agent_protocol.Session_ref.t
  -> (unit, Agent_protocol.Error.t) result

module Goal_resume_source : sig
  module Decision = Runtime_owner.Goal_resume_source.Decision

  (** Completed staging report: target and every acquired generated-parent
      source loan have joined, then the actual Actor preparation was factually
      closed. Cleanup failures remain explicit; this proves no absence of effects. *)
  type 'a t

  (** Preserve exact primary Error/exception/cancellation and raw backtrace.
      Cleanup/close failures replace only a successful callback. Fresh is callback
      DATA, not a resumed episode or a usable escaped continuation. *)
  val result : 'a t -> ('a Decision.t, Agent_protocol.Error.t) result

  (** Historical actual command ACK, including Retained and ACK-before-publication
      failure. Read from the actual captured preparation/decision after all joins;
      no reauthorization, decoding, receipt search or inferred writer success.
      None means no known ACK, not a confirmed negative/NoEffects. *)
  val receipt : 'a t -> Agent_session.Goal_command_receipt.t option

  (** Both target and parent joins, plus factual preparation close. A successful
      callback cannot hide a failed or missing required cleanup report. *)
  val cleanup : 'a t -> Runtime_owner.Scope_outcome.Cleanup.t
end

(** Fixed source-only composition over with_selected_source: actual registered
    entry/Actor/Owner/Store association; hidden issuer; exclusive Owner stage;
    actual Actor Begin; actual captured source restore/parent loan; one bind.
    The callback can pass Prepared.binding to Actor.admit_goal_resume and then
    the genuine Owner continuation consumer. Ready alone is staging DATA and is
    never described as resumed execution. All acquired target/parent finalizers
    finish before factual end_goal_continuation_preparation and report creation.
    Primary failures remain primary; actual known receipt is retained separately. *)
val with_goal_resume_source_reported
  :  t
  -> Session_registry.entry
  -> actor:Operator_authorization.t
  -> authorize:(Agent_session.Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> session:Agent_protocol.Session_ref.t
  -> request:Agent_session.Goal_resume_request.t
  -> (Runtime_owner.Prepared_goal_continuation.t -> ('a, Agent_protocol.Error.t) result)
  -> ('a Goal_resume_source.t, Agent_protocol.Error.t) result

(** Caller remains within Actor.with_job_execution; opaque SAME scope, not Job
    DTO, selects Goal construction. Uses SAME selected-source report core and
    Owner.with_goal_job_runtime after all real association checks. Callback is a
    resource borrower, not an issuer/adoption certificate. Its actual Actor job
    services remain required for inference/native effects. No completion/requeue.
    Ordinary-attributed owners reject (scheduler uses old ordinary path); current
    original/expiry/policy/physical checks are repeated at actual effects. *)
val with_goal_job_runtime
  :  t
  -> Session_registry.entry
  -> owner:Agent_session.Session_actor.Job_runtime_owner.t
  -> (Agent_session.Runtime_builder.t -> ('a, Agent_protocol.Error.t) result)
  -> ('a Runtime_owner.Construction_result.t, Agent_protocol.Error.t) result

(** Discover one persistence-enabled specialist from this Factory's genuine
    registered entry and its ALREADY loaded/realized authored resources.
    No public capability registry, source path, replacement artifact or current
    principal reconstructed from DATA is accepted.

    [actor] is the original authenticated reader. [authorize] is the existing
    trusted nonyielding host visibility policy; View_session_transcript and the
    same original proof are checked before/after it and after capture. This does
    not authorize a recurrence command, native effect or execution. Create/edit
    keep their actual command admission and final Send_messages checks.

    Holds the existing Factory-key source-capability lease only. It never loads
    or initializes a runtime, realizes lazy native resources, creates an artifact
    on disk, admits a recurrence/child/Run, or acquires another lease from inside
    a native handler. Actor/policy callbacks run outside the Owner mutex.

    The capture helper can also be reused by an already-admitted native callback
    under its existing resource loan, without acquiring a second lease. Target
    DATA includes full captured source and private pins and is for trusted host
    composition; it is not a public status projection. Parent/source/configuration
    changes during capture reject. Cancellation/exceptions preserve identity and
    joined cleanup; a normal Error remains Error throughout the actual source
    lease CPS. *)
val preview_recurrence_authored_target
  :  t
  -> Session_registry.entry
  -> actor:Operator_authorization.t
  -> authorize:(Agent_session.Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> name:string
  -> mode:Agent_protocol.Run.Mode.t
  -> (Agent_session.Recurrence_target.t, Agent_protocol.Error.t) result

(** Prepare exact immutable request/policy/deadline before actual Job scope and
    any cold construction. Uses the same Factory policy as installed executor.
    Goal and ordinary routing share the actual Owner executor/result body; this
    never claims/completes/requeues the already Running Job. *)
val execute_background_job
  :  t
  -> Session_registry.entry
  -> Agent_protocol.Job.t
  -> (Runtime_owner.background_result, Agent_protocol.Error.t) result

module Goal_wake_result : sig
  type t =
    | No_work
    | Suspended_goal of Agent_session.Goal_execution_selection.Suspension.t
    | Handed_off of Agent_session.Session_actor.Goal_execution_runtime.Wake_result.t
    | Controlled of Agent_session.Session_actor.Goal_callback_control.t
end

(** Actual registered same Factory/Store/Actor/Owner entry. Uses SAME authenticated
    source-owner reservation, selected-source/generated-parent CPS and full source
    ABI; no new original is supplied or inferred from creator. The actual live Run
    original supplies rights. Source joins then factual close finish before success.
    Errors/exceptions/cancellation stay primary through actual target+parent cleanup;
    known claim/install/Wait/Settlement ACK survives independently in Actor state.
    This never grants NoEffects or reconstructs execution after physical recovery.
    Reconciliation of already-consumed wake is No_work, not another callback.
    Suspended is a genuine pre-effect selection, never a converted source error. *)
val deliver_goal_wake
  :  t
  -> Session_registry.entry
  -> wake:Agent_protocol.Run_wake.t
  -> (Goal_wake_result.t, Agent_protocol.Error.t) result

(** Fixed actual idle routing before any ordinary runtime load. Queued Goal
    callbacks and subscription wakes use the exact source consumer above;
    ordinary work keeps the existing adapter. A suspended wake remains factual
    Pending data; no failed selection is converted into ordinary authority. *)
val drain_idle_moderator
  :  t
  -> Session_registry.entry
  -> (bool, Agent_protocol.Error.t) result

(** Finite post-callback native Resume uses the same actual registered target,
    hidden Factory source owner, exclusive source stage, selected source and
    joined cleanup as operator Resume. The opaque selection is not a claim:
    only Actor Begin after that reservation may consume its same live loan. *)
val with_native_goal_resume_source_reported
  :  t
  -> Session_registry.entry
  -> selection:Agent_session.Session_actor.Goal_native_resume.Selection.t
  -> (Runtime_owner.Prepared_goal_continuation.t -> ('a, Agent_protocol.Error.t) result)
  -> ('a Goal_resume_source.t, Agent_protocol.Error.t) result

(** Same creating Eio domain only. Import one immutable acknowledged usage snapshot from the actual immediate
    child into its actual parent's bounded contribution slot. Both supplied
    entries must be this Factory's canonical registered Actor/Owner/Handle tuple;
    a decoded reference, copied entry or read-only issuer identity is insufficient.
    This does not discover, select, initialize or execute a child. The caller uses
    existing metadata ownership to obtain entries and independently supplies the
    current transcript-read policy for each. No source/Goal rights are granted.

    Reads and parent write are sequential; no registry/Owner lock or parent
    mailbox is held while calling the child Actor. Nonyielding policy callbacks
    may not call another mailbox. Original expiry/revocation and actual ownership
    are checked again after the reads. Stored child revision decides replacement;
    an older authenticated snapshot never becomes a new authority or delta charge.
    Historical absence preserves an Unobserved slot and explicit unknown coverage.
    Writer/cancellation errors propagate; an unknown parent ACK never fabricates
    zero, removal, refund or a successful import. *)
val import_child_usage
  :  t
  -> parent:Session_registry.entry
  -> child:Session_registry.entry
  -> actor:Operator_authorization.t
  -> authorize_parent:
       (Agent_session.Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> authorize_child:
       (Agent_session.Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> (Agent_session.Session_actor.Child_usage_import.t, Agent_protocol.Error.t) result

(** Narrow trusted-host invariant boundary; absent from production handlers and
    protocol transport. Implementation owners remain private. *)
module For_testing : sig
  open! Core
  module A = Agent_session
  module P = Agent_protocol
  module B = A.Runtime_builder
  module Actor = A.Session_actor
  module Owner = Runtime_owner
  module CO = A.Close_observation

  type 'a result = ('a, P.Error.t) Result.t

  (** Intentional trusted-host testing boundary, absent from protocol transport
      and production handlers. Owns one actual private Cell with the supplied
      Actor/Handle/source/profile. No permission, source or Joined receipt is issued.
      Existing current Caller/restoration/Owner admission checks remain necessary.

      Domain and original-open checks precede fresh selection, disclosure, pin,
      publication and resource use. Exiting [with_probe] expires only those uses:
      it neither marks the Cell closed nor reports cleanup success. Existing
      callback/finalizer order remains the caller's responsibility. Retained
      cleanup and pure target facts remain available on the same creating domain;
      expiration cannot strand an acquired loan or suppress its real failure. *)
  type t

  type probe = t

  val with_probe
    :  actor:Actor.t
    -> handle:Agent_store.Session_store.Handle.t
    -> source:B.Source.t
    -> profile:A.Permission_policy.t
    -> (t -> 'a)
    -> 'a

  (** Pure factual pending query and exact same-Cell cleanup. Valid after expiry;
      no fresh selection or cleanup success is inferred from an absent borrower. *)
  val pending : t -> bool

  val close_pending : t -> unit result
  val close_pending_reported : t -> unit result CO.t

  (** Pure same-Cell cleanup adapter, available on the creating domain before or
      after expiry. Its callbacks retain no fresh selection/use authority and
      forward the actual Cell cleanup results unchanged. *)
  val cleanup_port : t -> Owner.Policy_resource_cleanup.t

  module Materialization : sig
    (** Actual successful public Builder objects from this invocation. *)
    type t =
      { product : B.Product.t
      ; binding : B.Product.Binding.t
      ; physical : B.t
      }
  end

  module Target : sig
    (** Actual Cell-elected immutable identity, never permission or completion. *)
    type t

    module Status : sig
      type t =
        | Held of
            { native : B.Native_identity.t option
            ; product : B.Product_identity.t option
            }
        | Released
    end

    val equal : t -> t -> bool

    (** Forward only this genuine issued target to the Owner's fixed Cell check.
        No target, cleanup authority or new physical owner is issued. *)
    val capture_consumer : t -> owner:Owner.t -> Owner.Resource_consumer.t result

    (** Domain and exact Cell association, then pure factual current view. Allowed
        after scope expiry; old identity is never rebound to a replacement unit. *)
    val status : t -> cell:probe -> Status.t result

    (** Execute the existing exact foreign-Cell check, returning its actual Error.
        The internal distinct empty Cell supplies no execution/cleanup authority. *)
    val check_foreign : t -> unit result
  end

  module Loan : sig
    (** SAME already-public abstract loan identity; no Cell MLI is required. *)
    type t = Owner.Policy_resource_access.loan

    (** Cleanup is domain-checked and remains available after root scope expiry. *)
    val close : t -> unit result

    val close_reported : t -> unit result CO.t
    val target : t -> Target.t

    (** Disclosure requires the root probe open and the SAME actual loan/Cell. *)
    val product : t -> cell:probe -> B.Product.t result

    val runtime : t -> cell:probe -> B.t result
  end

  module Native_loan : sig
    type t

    val target : t -> Target.t
    val close : t -> unit result
    val close_reported : t -> unit result CO.t
    val check_current : t -> cell:probe -> expected:A.Session_state.t -> unit result

    val check_retained_source
      :  t
      -> source:B.Source.t
      -> expected:A.Session_state.t
      -> unit result

    val with_resources : t -> f:(B.resources -> 'a result) -> 'a result

    val with_source_resources
      :  t
      -> f:(source:B.Source.t -> B.resources -> 'a result)
      -> 'a result
  end

  module Ordinary : sig
    type t

    (** Selects actual Cell construction after genuine restoration/current checks.
        [build] captures each actual Product before its next yield. Returned
        physical runtime is recorded before [f]; old capture/finally order remains.
        No private Key/Build or arbitrary closed-success setter escapes. Actual
        caller/restoration/source admission remains the host's responsibility;
        this adapter does not validate an extra restoration token. Original
        Key/select protocol failures keep their raised projection before callback
        entry; the new expired-scope refusal is returned as Error. *)
    val with_probe
      :  probe
      -> basis:A.Policy_restore_basis.t
      -> check:(unit -> unit result)
      -> expected:A.Session_state.t
      -> build:(t -> capture_tracking:(Graph_tracking.t -> unit) -> B.t result)
      -> f:(t -> 'a result)
      -> 'a result

    val capture_product
      :  t
      -> product:B.Product.t
      -> binding:B.Product.Binding.t
      -> unit result

    val expected : t -> A.Session_state.t
    val access : t -> B.Restored_policy_access.t result
    val publish : t -> Loan.t result
    val target : t -> Target.t

    (** Cleanup uses THIS actual original physical closer, followed by the existing
        Build acknowledgement only on success. Checks exact current ownership
        before invoking effects. Repeated successful calls re-observe the actual
        completion; a released old target never closes a replacement. Valid after
        expiry, domain-first; actual failure and retry behavior remain unchanged. *)
    val close_original : t -> unit result
  end

  module Native : sig
    type t

    (** Attach actual Cell scope before Driver fork. Actual host/graph callback,
        Child capture, input callback and Prepared_native.prepare keep their old
        order; SAME Lifetime owns local/outer cleanup. Genuine restoration and
        current source validation remain host-owned and run in their original
        order; no extra restoration token is accepted or claimed validated here.
        Original Key/select protocol failures may raise before callback entry.
        No Scope/Lifetime handle escapes. *)
    val with_probe
      :  probe
      -> sw:Eio.Switch.t
      -> expected:A.Session_state.t
      -> host:(capture_tracking:(Graph_tracking.t -> unit) -> B.Native_host.t)
      -> input:(unit -> B.Native_input.t result)
      -> on_physical_release:(unit -> unit)
      -> f:(t -> 'a result)
      -> 'a result

    val expected : t -> A.Session_state.t
    val basis : t -> A.Policy_restore_basis.t
    val prepared : t -> B.Prepared_native.t result
    val loan : t -> Native_loan.t
    val target : t -> Target.t
  end

  module Attachment : sig
    type t

    (** Capture the actual Product_attachment before [build] can yield; attach
        actual returned Product/binding/physical runtime before [f]. The actual
        original reservation preserves its raised protocol-failure projection. *)
    val with_probe
      :  Native.t
      -> build:
           (B.Prepared_native.t
            -> attachment:B.Product_attachment.t
            -> Materialization.t result)
      -> f:(t -> B.Product.t -> 'a result)
      -> 'a result

    val prepared : t -> B.Prepared_native.t result
    val binding : t -> B.Product.Binding.t result
    val target : t -> Target.t

    val publish
      :  t
      -> basis:A.Policy_restore_basis.t
      -> expected:A.Session_state.t
      -> (Loan.t * B.Restored_policy_access.t) result

    (** Delegates to THIS actual Cell promotion close, which already performs the
        physical close and acknowledgement together. Never separately pre-closes
        its runtime. Exact attachment ownership is checked before effects; repeated
        success re-observes that completion without retiring a later attachment.
        Domain-first and available after expiry; no free success setter. *)
    val close : t -> unit result
  end

  module Empty_partial : sig
    (** Actual unallocated ordinary selection, scoped to its original callback.
        A callback escape records that SAME actual Raised failure on its selection
        before preserving the exception/trace; it never leaves a Building unit. *)
    type t

    val with_ordinary
      :  probe
      -> expected:A.Session_state.t
      -> failure:P.Error.t
      -> during:(t -> unit)
      -> after_failure:(Target.t -> unit)
      -> unit result

    (** Native has no positive-acknowledgement operation; observe its real target
        before/after actual failure, then caller explicitly closes pending work. *)
    val with_native
      :  probe
      -> expected:A.Session_state.t
      -> failure:P.Error.t
      -> during:(Target.t -> unit)
      -> after_failure:(Target.t -> unit)
      -> unit result

    val target : t -> Target.t

    (** Try the existing unallocated acknowledgement only for THIS actual empty
        ordinary unit. Cannot acknowledge another resource or an expired callback. *)
    val try_unallocated_acknowledgement : t -> unit result
  end

  module Parent_cleanup : sig
    (** Cleanup-only opaque probe. No permission, producer or Completion issuer. *)
    type t

    val create : sw:Eio.Switch.t -> t
    val capture_release : t -> Owner.Native_release.t -> unit

    (** Exact actual provider Owner accompanies the original report. Duplicate
        record preserves the existing Invalid_argument invariant. *)
    val record : t -> owner:Owner.t -> 'a Owner.Native_borrow_outcome.t -> unit

    val close : t -> unit result CO.t
  end

  (** Fixed adapters use SAME Cell/Actor/Handle/profile and existing original Owner
      admission checks. Pin/disclosure requires root-open; retained cleanup does
      not. No private Native_lease type leaks into these public signatures. *)
  val pin_native
    :  t
    -> expected_native:B.Native_identity.t
    -> expected:A.Session_state.t
    -> Native_loan.t result

  val native_access
    :  t
    -> prepare:
         (admission:Owner.Native_resource_admission.t
          -> stage:Owner.Native_source_stage.t
          -> unit result)
    -> Owner.Native_resource_access.t

  val policy_access
    :  t
    -> restore:
         (stage:Owner.Policy_restoration_source.t
          -> request:A.Script_tool_calls.preparation
          -> governance:Actor.Causal_governance.t
          -> authorize:(unit -> unit result)
          -> unit result)
    -> Owner.Policy_resource_access.t
end
