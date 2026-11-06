(** Bounded ACKed projections of immediate children only. No slot or decoded
    snapshot grants Goal ownership or child execution permission. *)
module Inventory : sig
  type t =
    | Complete_inventory
    | Historical_children_unknown
  [@@deriving equal, sexp_of]
end

type t

type disposition =
  | Added
  | Replaced
  | Duplicate
  | Stale
[@@deriving equal, sexp_of]

(** Bounds come from the actual validated Factory delegation profile. Only a
    genuine new parent State may start Complete_inventory; observing old child
    records is never proof that a historical inventory is complete. *)
val create
  :  inventory:Inventory.t
  -> max_children:int
  -> max_encoded_bytes:int
  -> (t, Agent_protocol.Error.t) result

val inventory : t -> Inventory.t
val references : t -> Agent_store.Delegation_store.Reference.t list

(** Exact identity is idempotent. Same child/key with a different admission
    conflicts. A new slot is Unobserved and reserves its complete known growth.
    Failure after this ACK never removes it or fabricates child zero. *)
val register
  :  t
  -> reference:Agent_store.Delegation_store.Reference.t
  -> (t, Agent_protocol.Error.t) result

val replace
  :  t
  -> snapshot:Child_usage_snapshot.t
  -> (t * disposition, Agent_protocol.Error.t) result

val aggregate : t -> (Child_usage_snapshot.Descendants.t, Agent_protocol.Error.t) result
val of_jsonaf : Jsonaf.t -> (t, Agent_protocol.Error.t) result
val to_jsonaf : t -> (Jsonaf.t, Agent_protocol.Error.t) result
val adopt : t -> incoming:t -> (t, Agent_protocol.Error.t) result
val reserved_growth : t -> (Capacity_growth.t, Agent_protocol.Error.t) result
val limits : Document_schema.Limits.t
