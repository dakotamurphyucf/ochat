(** Bounded safe replacement DATA from one actual delegated session. The codec
    conveys no read, import or execution authority and contains no foreign
    inference Handles, configuration or history. *)
module Partition : sig
  type t

  val create
    :  usage:Goal_usage.t
    -> marks:Canonical_usage_fold.Marks.t
    -> (t, Agent_protocol.Error.t) result

  val equal : t -> t -> bool
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

module Goal_scope : sig
  (** One frozen inherited root, projected through this same immediate-child
      snapshot. [own] is the actual whole-child own partition, never a separate
      scoped ledger. [included] is DATA and need not represent a root-wide total
      equal to this intermediate subtotal. *)
  type t

  val create
    :  scope:Agent_store.Delegation_goal_origin.Scope.t
    -> own:Partition.t
    -> descendants:Descendants.t
    -> included:Goal_descendant_included.t option
    -> (t, Agent_protocol.Error.t) result

  val scope : t -> Agent_store.Delegation_goal_origin.Scope.t
  val own : t -> Partition.t
  val descendants : t -> Descendants.t
  val total : t -> (Partition.t, Agent_protocol.Error.t) result
  val coverage : t -> Coverage.t
  val included : t -> Goal_descendant_included.t option
  val validate_update : t -> incoming:t -> (unit, Agent_protocol.Error.t) result
  val shape : Document_schema.Shape.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result

  (** Exact shared profile for the complete scoped vector, also used for local
      capacity payload measurements. Enclosing complete-document limits remain
      stricter when smaller; this does not enlarge an admitted profile. *)
  val capacity_limits : Document_schema.Limits.t

  (** Invalid raw capacity witness consumed by [Child_usage_capacity]. Fixed
      scope and an existing Included's full branch Reference remain unchanged.
      With [maximum_attempt=false], preserve the Included occurrence's kind;
      absent heads use maximum Attempt. With [maximum_attempt=true], probe
      maximum Attempt even for an existing Host_turn, so the actual carrier can
      reject an incompatible protected replacement. Never decode as evidence. *)
  val capacity_projection
    :  scope:Agent_store.Delegation_goal_origin.Scope.t
    -> included:Goal_descendant_included.t option
    -> maximum_attempt:bool
    -> Jsonaf.t
end

type t

val create
  :  reference:Agent_store.Delegation_store.Reference.t
  -> revision:int64
  -> own:Partition.t
  -> descendants:Descendants.t
  -> canonical_coverage:Agent_protocol.Inference_query.Coverage.t
  -> (t, Agent_protocol.Error.t) result

(** Exact frozen origin vector in its admitted order, with one matching entry
    per root and every entry's [own] equal to the actual [own] above. Bounded by
    the admitted component structural profile; actual enclosing depth/profile bounds
    remain stricter. *)
val create_with_scopes
  :  reference:Agent_store.Delegation_store.Reference.t
  -> revision:int64
  -> own:Partition.t
  -> descendants:Descendants.t
  -> canonical_coverage:Agent_protocol.Inference_query.Coverage.t
  -> origin:Agent_store.Delegation_goal_origin.t
  -> goal_scopes:Goal_scope.t list
  -> (t, Agent_protocol.Error.t) result

(** None is historical Unknown and remains omitted on encoding, preserving old
    logical snapshot digests. Some [] is positively Unrelated. A known scoped
    vector cannot appear, disappear or change its frozen roots in an update. *)
val goal_scopes : t -> Goal_scope.t list option

(** The owning child slot must validate decoded DATA against its actual frozen
    origin before replacement/adoption. This function grants no authority. *)
val validate_origin
  :  t
  -> origin:Agent_store.Delegation_goal_origin.t
  -> (unit, Agent_protocol.Error.t) result

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
