open! Core

(** Shared bounded Developer and semantic-authoring setup for real daemon E2E
    scenarios. The source policy selects only the authored document script; it
    does not grant native, provider, or transport authority. *)

val developer_configuration
  :  ?capture_policy:Agent_server.Authoring_context_service.Developer.Target_policy.t
  -> ?max_runs:int
  -> ?authorize_native:
       (Agent_protocol.Host_invocation.t
        -> Chat_response.Tool_capability.binding
        -> (unit, Agent_protocol.Error.t) result)
  -> unit
  -> Agent_server.Daemon.Developer_configuration.t

(** Existing bounded embedding policies shared by the trace and Unix control fixtures. *)
val repl_wire_limits : unit -> Agent_protocol.Developer_repl_wire.Limits.t

val repl_configuration : unit -> Agent_server.Daemon.Developer_configuration.Repl.t

val semantic_authoring_policy
  :  Eio_unix.Stdenv.base
  -> Agent_server.Authoring_installation.Host_policy.t
