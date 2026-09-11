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

type t

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
  -> home:string
  -> model_post_stream:Agent_session.Runtime_builder.model_post_stream option
  -> qualify_chatml_extensions:bool
  -> independent_lifetime_policy:string option
  -> chatml_runtime_policy:Chat_response.Runtime_semantics.policy
  -> authoring_validation_host:Chat_response.Authoring_validation.host option
  -> durability:Agent_store.Journal_segment.durability
  -> limits:limits
  -> t

(** [install_catalogs] atomically publishes catalogs used for future session
    creation while retaining permission revisions pinned by existing sessions. *)
val install_catalogs
  :  t
  -> workspaces:Agent_session.Workspace_catalog.t
  -> permission_profiles:Agent_session.Permission_policy.t list
  -> manifest_grants:Operator_manifest_grant.t list
  -> unit

val create_session
  :  t
  -> command_audit:string option
  -> principal:Agent_protocol.Principal.t
  -> Agent_protocol.Session.Create_request.t
  -> (Session_registry.entry, Agent_protocol.Error.t) result

(** [prepare_administration t entry candidate ~fresh_history] validates and
    initializes detached runtime state without actor callbacks or reservations.
    Close all preparation resources before returning immutable candidate state.
    The caller must compare-and-set the captured revision at actor commit. *)
val prepare_administration
  :  t
  -> Session_registry.entry
  -> Agent_session.Session_state.t
  -> fresh_history:bool
  -> (Agent_session.Session_state.t, Agent_protocol.Error.t) result

(** [import_legacy t ~principal ~source_id ~source_path ~legacy request]
    creates one stopped durable session from a validated legacy snapshot. It
    records immutable source provenance and never mutates [source_path]. *)
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
    restoration/invocation. Independent resource ancestry currently rejects
    stateful parent moderation rather than omitting that parent's rules. Creation
    itself still needs an active parent through the durable link checkpoint.
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

(** Reconstructs one indexed session on demand. The caller must serialize
    loads for a session ID and register the returned entry exactly once. *)
val recover_session
  :  t
  -> Agent_store.Session_index.Entry.t
  -> (Session_registry.entry, Agent_protocol.Error.t) result
