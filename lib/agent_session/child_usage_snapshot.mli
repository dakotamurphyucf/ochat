(** Bounded safe replacement DATA from one actual delegated session. The codec
    conveys no read, import or execution authority and contains no foreign
    inference Handles, configuration or history. *)
module Partition : sig
  type t

  val create
    :  usage:Goal_usage.t
    -> marks:Canonical_usage_fold.Marks.t
    -> (t, Agent_protocol.Error.t) result

  val empty : t
  val usage : t -> Goal_usage.t
  val marks : t -> Canonical_usage_fold.Marks.t
  val add_disjoint : t -> t -> (t, Agent_protocol.Error.t) result
end

module Coverage : sig
  type t

  val create
    :  historical_children_unknown:bool
    -> unobserved_children:int64
    -> (t, Agent_protocol.Error.t) result

  val complete : t
  val historical_children_unknown : t -> bool
  val unobserved_children : t -> int64
  val add_disjoint : t -> t -> (t, Agent_protocol.Error.t) result
end

module Descendants : sig
  (** Known cumulative subtotal and its uncertainty travel together. An absent
      child observation is not an exact zero partition. *)
  type t

  val create : partition:Partition.t -> coverage:Coverage.t -> t
  val partition : t -> Partition.t
  val coverage : t -> Coverage.t
end

type t

val create
  :  reference:Agent_store.Delegation_store.Reference.t
  -> revision:int64
  -> own:Partition.t
  -> descendants:Descendants.t
  -> canonical_coverage:Agent_protocol.Inference_query.Coverage.t
  -> (t, Agent_protocol.Error.t) result

val reference : t -> Agent_store.Delegation_store.Reference.t

(** Actual child session revision of the ACKed projection. A mutable external
    delegation scan is not part of this value's version or digest. *)
val revision : t -> int64

val digest : t -> string
val own : t -> Partition.t
val descendants : t -> Descendants.t
val coverage : t -> Coverage.t
val total : t -> (Partition.t, Agent_protocol.Error.t) result
val validate_update : t -> incoming:t -> (unit, Agent_protocol.Error.t) result
val shape : Document_schema.Shape.t
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result

(** Not a valid snapshot: carrier/complete-capacity probing only. *)
val capacity_projection : reference:Agent_store.Delegation_store.Reference.t -> Jsonaf.t
