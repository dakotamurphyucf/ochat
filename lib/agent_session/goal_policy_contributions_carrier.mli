(** Private complete carrier and static Goal/context checks. No transition or
    original authority can be inferred by decoding a carrier. *)
module Data = Goal_policy_contributions_data

type t = Data.t Document_schema.Extension_carrier.t

val value : t -> Data.t
val limits : Document_schema.Limits.t
val shape : Document_schema.Shape.t
val admit : t -> (t, Agent_protocol.Error.t) result
val with_value : t -> Data.t -> (t, Agent_protocol.Error.t) result
val adopt : t -> incoming:t -> (t, Agent_protocol.Error.t) result
val of_document : Document_schema.Document.t -> (t, Agent_protocol.Error.t) result
val to_document : t -> (Document_schema.Document.t, Agent_protocol.Error.t) result

(** Complete existing capacity capture for one immutable carrier. Preparation
    measures current/future growth, depth, forwarded profiles and work reserves
    before returning. Raw validation still checks the actual supplied complete
    child, followed by intrinsic depth. No result is shared across independent
    raw admission or State checks, and no current-document override is exposed. *)
module Prepared_capacity : sig
  type carrier = t
  type t

  val prepare : carrier -> (t, Agent_protocol.Error.t) result
  val required_depth : t -> int
  val reserved_growth : t -> Capacity_growth.t
  val validate : t -> Jsonaf.t -> (unit, Agent_protocol.Error.t) result
end

val reserved_growth : t -> (Capacity_growth.t, Agent_protocol.Error.t) result
val required_depth : t -> (int, Agent_protocol.Error.t) result
val validate_encoded_capacity : t -> Jsonaf.t -> (unit, Agent_protocol.Error.t) result

val validate_context
  :  t
  -> owner:Agent_protocol.Session_ref.t
  -> catalog:Goal_catalog.t option
  -> accounts:Goal_accounts.t option
  -> canonical:Policy_canonical_usage.t option
  -> ledger:Inference_ledger.t
  -> session_revision:int64
  -> (unit, Agent_protocol.Error.t) result

(** At the canonical ledger leaf, accounts still belong to [before] and inline
    streams use [after]. The completed transaction also calls [validate_context]
    against its final ledger and accounts. *)
val validate_canonical_edge
  :  t
  -> owner:Agent_protocol.Session_ref.t
  -> catalog:Goal_catalog.t option
  -> accounts:Goal_accounts.t option
  -> canonical:Policy_canonical_usage.t option
  -> before:Inference_ledger.t
  -> after:Inference_ledger.t
  -> session_revision:int64
  -> (unit, Agent_protocol.Error.t) result

val inline
  :  owner:Agent_protocol.Session_ref.t
  -> canonical:Policy_canonical_usage.t option
  -> ledger:Inference_ledger.t
  -> session_revision:int64
  -> (Policy_canonical_usage.Projection.t list, Agent_protocol.Error.t) result

val check_stream_capacity
  :  t option
  -> inline:Policy_canonical_usage.Projection.t list
  -> (unit, Agent_protocol.Error.t) result

(** Copy complete Stream/Projection children through decode/adopt, preserving
    exact known candidate identity, protected extensions and full capacity. *)
val retain_slot
  :  t
  -> stream:Policy_canonical_usage.Stream.t
  -> projection:Policy_canonical_usage.Projection.t option
  -> (t, Agent_protocol.Error.t) result
