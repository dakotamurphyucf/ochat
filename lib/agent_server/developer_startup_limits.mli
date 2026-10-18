open! Core

(** One finite lower-owner preset for the declarative Developer startup path.
    The validated operator capacity controls its corresponding Store, Root,
    Agent, REPL and watch ceilings. Every component is created with its
    existing lower constructor; failure rejects startup before a Developer
    owner opens. This module owns no resource, policy or grant. *)
type t

val create
  :  capacity:Developer_startup_configuration.Capacity.t
  -> approvals:Developer_startup_configuration.approvals
  -> repl:Developer_startup_configuration.repl
  -> portable:bool
  -> initialize_limits:Agent_protocol.Initialize.Limits.t
  -> (t, Agent_protocol.Error.t) result

val capture : t -> Authoring_context_service.Developer.Limits.t
val host : t -> Developer_daemon_configuration.Limits.t
val publisher : t -> Agent_store.Developer_result_publisher.Limits.t
val approvals : t -> Developer_approval_owner.Limits.t option
val repl : t -> Developer_daemon_configuration.Repl.t option
val portable : t -> Developer_daemon_configuration.Portable.t option
