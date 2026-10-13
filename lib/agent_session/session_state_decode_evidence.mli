open! Core

(** One generation of successful wire-field decodes. No native caller values
    enter this evidence. Exact document limits bind all captured decoders. *)
type t

val ledger_of_jsonaf
  :  limits:Document_schema.Limits.t
  -> Jsonaf.t
  -> (Inference_ledger.t, Agent_protocol.Error.t) Result.t

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

  (** Freezes the current generation. The immutable result has no link to earlier
      generations or to this capture's mutable slots. *)
  val finish : t -> evidence
end
