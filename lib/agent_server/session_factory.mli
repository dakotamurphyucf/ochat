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
  ; delegation_max_depth : int
  ; job_result_collection : Agent_store.Job_result_store.Publisher.collection_limits
  ; subscriptions : Agent_session.Staged_subscriptions.limits
  ; schedules : Agent_session.Staged_schedules.limits
  ; notifications : Agent_session.Staged_notifications.limits
  ; ingress : Agent_session.Staged_ingress.limits
  }

type t

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

(** Qualified internal host creation of an initially stopped generated child.
    Revalidates the prepared definition against the loaded parent's exact native
    bindings, persists a protected retry mapping and complete initial journal/
    snapshot before publication, then links under the parent's actor checkpoint.
    Does not initialize scripts, start a model turn or grant caller access.
    External adapters must authenticate their invoking parent before calling.
    Uses the parent's durable principal, workspace and permission profile.
    Failed/ambiguous installs retain their private reservation for reconciliation. *)
val create_generated_session
  :  t
  -> parent_session_id:Agent_protocol.Id.Session.t
  -> idempotency_key:Agent_protocol.Idempotency_key.t
  -> display_name:string option
  -> Agent_session.Generated_definition.t
  -> (Session_registry.entry, Agent_protocol.Error.t) result

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
