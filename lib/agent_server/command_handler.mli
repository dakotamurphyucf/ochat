open! Core

type create_session =
  command_audit:Document_schema.Document.t option
  -> principal:Agent_protocol.Principal.t
  -> Agent_protocol.Session.Create_request.t
  -> (Session_registry.entry, Agent_protocol.Error.t) result

type t

val create
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> registry:Session_registry.t
  -> prompts:Agent_session.Prompt_catalog.t
  -> workspaces:Agent_session.Workspace_catalog.t
  -> start_queue:Agent_session.Start_queue.t
  -> idempotency_store:Agent_store.Idempotency_store.t
  -> audit_store:Agent_store.Audit_store.t
  -> blob_store:Agent_store.Blob_store.t
  -> session_store:Agent_store.Session_store.t
  -> authoring:Authoring_installation.t option
  -> lifecycle_service:Session_lifecycle_service.t
  -> retained:Retained_session.t
  -> archive_payload_limit:int
  -> initialize:
       (principal:Agent_protocol.Principal.t
        -> Agent_protocol.Initialize.Request.t
        -> (Agent_protocol.Initialize.Response.t, Agent_protocol.Error.t) result)
  -> ping:(Agent_protocol.Ping.Request.t -> Agent_protocol.Ping.Response.t)
  -> server_info:(unit -> Agent_protocol.Method_result.Server_info.t)
  -> server_health:(Agent_protocol.Health.Request.t -> Agent_protocol.Health.Response.t)
  -> cancel_job:(actor:Agent_session.Session_actor.t -> job:Agent_protocol.Job.t -> unit)
  -> create_session:create_session
  -> seed_factory:Session_factory.t option
  -> seed_recovery_limits:Session_factory.Seed_recovery_limits.t
       (** [Some factory] borrows the actual same Store/Registry factory used by this
      host; it is not another source owner. [None] returns an unsupported capability
      error for session.seed and must not advertise sessions.seed. Daemon (including
      Embedded and its stdio dispatcher) supplies its actual existing Factory.
      Pending receipt lookup borrows the same bounded recovery profile as startup. *)
  -> prepare_session_start:
       (Session_registry.entry -> (unit, Agent_protocol.Error.t) result)
  -> workspace_retained:
       (Agent_session.Session_state.t -> (bool, Agent_protocol.Error.t) result)
  -> provider_operator:Provider_operator_port.t option
  -> dependency_administration:Dependency_administration.t option
  -> prepare_administration:
       (Session_registry.entry
        -> previous:Agent_session.Session_state.t
        -> Agent_session.Session_state.t
        -> fresh_history:bool
        -> (Agent_session.Session_state.t, Agent_protocol.Error.t) result)
  -> t

val handle
  :  t
  -> ?actor:Operator_authorization.t
  -> context:Connection_context.t
  -> inference_budget:Inference_query_budget.t
  -> Agent_protocol.Command.t
  -> (Agent_protocol.Public.Result.t, Agent_protocol.Error.t) result

(** Join authoring's exact physical connection BEFORE detaching any session
    attachment. On failure retain the installation's cleanup custody and do
    not report successful detach. Daemon owns the only unit transport facade:
    it records a finite failure and retries at shutdown before releasing
    borrowed source/upload owners. Unexpected exceptions preserve identity. *)
val close_connection : t -> Connection_context.t -> (unit, Agent_protocol.Error.t) result
