(** Private Builder classification and immutable Actor installation affinity.
    These facts grant no execution, permission, source lease or admission. *)
open! Core

type t

module Binding : sig
  type source = t
  type t

  (** Pure domain-first physical worker and complete accepted basis check. *)
  val check
    :  t
    -> state:Session_state.t
    -> worker:Operation_worker.t
    -> manager:Chat_response.Moderator_manager.t option
    -> (Policy_cause_data.Caller_source.t, Agent_protocol.Error.t) result

  (** Read-only warm no-op validation of the SAME original Builder fact.
      Never replaces or reissues the current binding. *)
  val check_source
    :  t
    -> source
    -> state:Session_state.t
    -> worker:Operation_worker.t
    -> manager:Chat_response.Moderator_manager.t option
    -> (unit, Agent_protocol.Error.t) result

  (** Prepare only the exact checked configuration transition's next binding.
      No mutation, callback, ACK or fresh authority. *)
  val reconfigure
    :  t
    -> previous:Session_state.t
    -> candidate:Session_state.t
    -> worker:Operation_worker.t
    -> manager:Chat_response.Moderator_manager.t option
    -> (t, Agent_protocol.Error.t) result
end

val bind
  :  t
  -> state:Session_state.t
  -> worker:Operation_worker.t
  -> manager:Chat_response.Moderator_manager.t option
  -> (Binding.t, Agent_protocol.Error.t) result

module Host : sig
  (** Only Runtime_builder's verified actual no-artifact moderator selection
      issues this fact, for its final physical worker and exact Artifact. The
      public Runtime_builder facade deliberately omits this issuer. *)
  val create
    :  session_id:Agent_protocol.Id.Session.t
    -> artifact:Agent_store.Prompt_artifact_store.Artifact.t
    -> worker:Operation_worker.t
    -> t
end
