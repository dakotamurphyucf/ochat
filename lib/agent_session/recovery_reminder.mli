open! Core

module Claim : sig
  type t

  val create
    :  scope:Agent_protocol.Recovery_window.Scope.t
    -> policy:Chatml_source.Digest.t
    -> operation:Agent_protocol.Id.Operation.t
    -> preparation:string
    -> (t, Agent_protocol.Error.t) Result.t

  val preparation : t -> string
  val equal : t -> t -> bool
end

module Change : sig
  type t =
    | Claim of Claim.t
    | Accept of Claim.t * Inference.Observation.Observation_id.t

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) Result.t
  val shape : Document_schema.Shape.t
end

type t

(** A pending claim can be superseded by a later exact preparation for the same
    key. Once accepted, that window and policy cannot claim a second attempt.
    A full ledger leaves other provider work available. *)
val can_claim
  :  t option
  -> scope:Agent_protocol.Recovery_window.Scope.t
  -> policy:Chatml_source.Digest.t
  -> bool

val accepted_by : t option -> Claim.t -> Inference.Observation.Observation_id.t -> bool

val apply
  :  t option
  -> Change.t
  -> scope:Agent_protocol.Recovery_window.Scope.t
  -> operation:Agent_protocol.Id.Operation.t
  -> (t, Agent_protocol.Error.t) Result.t

val validate_state
  :  t
  -> session:Agent_protocol.Id.Session.t
  -> generation:int
  -> owner:Agent_protocol.Session_ref.t option
  -> (unit, Agent_protocol.Error.t) Result.t

val check_unchanged
  :  t option
  -> incoming:t option
  -> (unit, Agent_protocol.Error.t) Result.t

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) Result.t
val shape : Document_schema.Shape.t
