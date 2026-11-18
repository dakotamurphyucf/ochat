open! Core

(** One generation of successful wire-field decodes. No native caller values
    enter this evidence. Exact document limits bind all captured decoders. *)
type t

val ledger_of_jsonaf
  :  limits:Document_schema.Limits.t
  -> Jsonaf.t
  -> (Inference_ledger.t, Agent_protocol.Error.t) Result.t

val policy_canonical_usage_of_jsonaf
  :  Jsonaf.t
  -> (Policy_canonical_usage.t, Agent_protocol.Error.t) Result.t

val goal_policy_contributions_of_jsonaf
  :  Jsonaf.t
  -> (Goal_policy_contributions.t, Agent_protocol.Error.t) Result.t

module Capture : sig
  type evidence := t
  type t

  (** A capture belongs to one synchronous decoder invocation. Different limits
      discard all previous evidence. Do not share captures between invocations. *)
  val create : ?previous:evidence -> limits:Document_schema.Limits.t -> unit -> t

  val canonical_history
    :  t
    -> Jsonaf.t
    -> (Agent_protocol.History.entry list, Agent_protocol.Error.t) Result.t

  val invocations
    :  t
    -> Jsonaf.t
    -> (Agent_protocol.Invocation.t list, Agent_protocol.Error.t) Result.t

  val moderator_executions
    :  t
    -> Jsonaf.t
    -> (Agent_protocol.Moderator_execution.t list, Agent_protocol.Error.t) Result.t

  val inference_ledger
    :  t
    -> Jsonaf.t
    -> (Inference_ledger.t, Agent_protocol.Error.t) Result.t

  (** Actual successful component wire decodes at their existing fixed
      profiles. Reuse additionally requires this capture's unchanged State
      limits and exact complete child JSON. Unvisited optional fields are
      absent from the finished generation. Native context validation is separate. *)
  val policy_canonical_usage
    :  t
    -> Jsonaf.t
    -> (Policy_canonical_usage.t, Agent_protocol.Error.t) Result.t

  val goal_policy_contributions
    :  t
    -> Jsonaf.t
    -> (Goal_policy_contributions.t, Agent_protocol.Error.t) Result.t

  (** Freezes the current generation. The immutable result has no link to earlier
      generations or to this capture's mutable slots. *)
  val finish : t -> evidence
end
