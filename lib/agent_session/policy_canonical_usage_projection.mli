(** Private bounded root projection over the one preserved carrier. *)
type t = Policy_canonical_usage_carrier.t

module Stream = Policy_canonical_usage_data.Stream

module Head : sig
  module Witness : sig
    type t

    val forwarding : t -> Policy_cause_data.Forwarding.Witness.t option
    val cause : t -> Policy_cause_data.Key.t
    val caller_origin : t -> Agent_protocol.Inference_origin.t option
    val child_invocation : t -> Agent_protocol.Id.Invocation.t
    val admission_sha256 : t -> string
  end

  type t

  val witness : t -> Witness.t
  val canonical : t -> Goal_descendants.Canonical.Fact.t
  val cause : t -> Policy_cause_data.Key.t
  val work : t -> Policy_cause_data.Allocation_work.t
end

module Projection : sig
  type t

  val session_revision : t -> int64
  val stream : t -> Stream.t
  val admitted_attempts : t -> int64
  val usage : t -> Goal_usage.t
  val head : t -> Head.t option
  val shape : Document_schema.Shape.t
  val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result

  (** Known fields only, for native codec composition. Use [to_json] to retain
      unknown fields when storing or forwarding the projection. *)
  val known_json : t -> Jsonaf.t

  (** Complete preserved 256KiB/depth80 projection. Count is the selected subset
      prefix, never global ordinal. Head contains its root-relevant immutable
      witness after live work/row prune, not provider payload or runtime loans. *)
  val to_json : t -> Jsonaf.t
end

val projections
  :  t
  -> ledger:Inference_ledger.t
  -> session_revision:int64
  -> (Projection.t list, Agent_protocol.Error.t) result
