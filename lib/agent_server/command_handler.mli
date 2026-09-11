open! Core

type create_session =
  command_audit:string option
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
  -> initialize:
       (principal:Agent_protocol.Principal.t
        -> Agent_protocol.Initialize.Request.t
        -> (Agent_protocol.Initialize.Response.t, Agent_protocol.Error.t) result)
  -> ping:(Agent_protocol.Ping.Request.t -> Agent_protocol.Ping.Response.t)
  -> server_info:(unit -> Agent_protocol.Method_result.Server_info.t)
  -> server_health:(Agent_protocol.Health.Request.t -> Agent_protocol.Health.Response.t)
  -> cancel_job:(Agent_protocol.Id.Job.t -> unit)
  -> create_session:create_session
  -> prepare_session_start:
       (Session_registry.entry -> (unit, Agent_protocol.Error.t) result)
  -> prepare_administration:
       (Session_registry.entry
        -> Agent_session.Session_state.t
        -> fresh_history:bool
        -> (Agent_session.Session_state.t, Agent_protocol.Error.t) result)
  -> t

val handle
  :  t
  -> context:Connection_context.t
  -> Agent_protocol.Command.t
  -> (Agent_protocol.Method_result.t, Agent_protocol.Error.t) result

(** Detaches every attachment owned by the connection. Detached sessions keep
    running according to their configured liveness. *)
val close_connection : t -> Connection_context.t -> unit
