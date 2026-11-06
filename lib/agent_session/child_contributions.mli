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

(** Immutable admitted bounds; later host configuration does not rewrite them. *)
val max_children : t -> int

val max_encoded_bytes : t -> int
val references : t -> Agent_store.Delegation_store.Reference.t list

(** Historical absence stays unknown. A retained slot's origin cannot be
    inferred or changed by a later snapshot. *)
val origin
  :  t
  -> reference:Agent_store.Delegation_store.Reference.t
  -> (Agent_store.Delegation_goal_origin.t option, Agent_protocol.Error.t) result

(** The current observation for the exact registered admission. [None] is an
    unobserved slot; a missing or conflicting reference is an error. *)
val snapshot
  :  t
  -> reference:Agent_store.Delegation_store.Reference.t
  -> (Child_usage_snapshot.t option, Agent_protocol.Error.t) result

(** Includes explicit Unrelated origin: both positive alternatives require the
    scoped accounting storage semantics. *)
val has_goal_origins : t -> bool

(** Register historical unknown origin. Exact identity with the same unknown
    origin is idempotent; an already known origin cannot be erased. Same child/key with a different admission
    conflicts. A new slot is Unobserved and reserves its complete known growth.
    Failure after this ACK never removes it or fabricates child zero. *)
val register
  :  t
  -> reference:Agent_store.Delegation_store.Reference.t
  -> (t, Agent_protocol.Error.t) result

(** Register a new slot with the exact frozen origin from its lower admission.
    Re-registering requires identical reference and origin, including presence. *)
val register_with_origin
  :  t
  -> reference:Agent_store.Delegation_store.Reference.t
  -> origin:Agent_store.Delegation_goal_origin.t
  -> (t, Agent_protocol.Error.t) result

val replace
  :  t
  -> snapshot:Child_usage_snapshot.t
  -> (t * disposition, Agent_protocol.Error.t) result

val aggregate : t -> (Child_usage_snapshot.Descendants.t, Agent_protocol.Error.t) result

(** Sum only immediate child subtrees carrying this root. Positively unrelated
    children are excluded before considering their observation coverage. Unknown
    historical associations remain labelled uncertainty, never known zero. A
    matching root with different frozen attribution is a conflict. *)
val aggregate_scope
  :  t
  -> scope:Agent_store.Delegation_goal_origin.Scope.t
  -> (Child_usage_snapshot.Descendants.t, Agent_protocol.Error.t) result

(** At the owning session, sum the disjoint first-child branches of one Goal.
    Branch attribution can differ between children; it remains frozen within
    each slot. Intermediate sessions use [aggregate_scope] instead. *)
val aggregate_root
  :  t
  -> owner:Agent_protocol.Session_ref.t
  -> goal_id:Agent_protocol.Id.Goal.t
  -> (Child_usage_snapshot.Descendants.t, Agent_protocol.Error.t) result

val of_jsonaf : Jsonaf.t -> (t, Agent_protocol.Error.t) result
val to_jsonaf : t -> (Jsonaf.t, Agent_protocol.Error.t) result
val adopt : t -> incoming:t -> (t, Agent_protocol.Error.t) result
val reserved_growth : t -> (Capacity_growth.t, Agent_protocol.Error.t) result
val limits : Document_schema.Limits.t

(** Ordinary session mutations preserve every positive-origin slot and its
    snapshot. New known attribution or scoped observations require the paired
    descendant Update. Historical unknown slots retain normal factual updates;
    all existing profile and unknown-field preservation checks still apply. *)
val validate_ordinary_update
  :  t option
  -> incoming:t option
  -> (unit, Agent_protocol.Error.t) result

(** Maximum full component-document depth of current data and existing-slot
    future snapshots, including compatible scoped receipt alternatives and
    preserved unknowns. Root is depth1 and scalar leaves count. The enclosing
    State adds its actual JSON path length and checks its actual max_depth;
    additive growth remains reserved exactly once by [reserved_growth]. *)
val required_depth : t -> (int, Agent_protocol.Error.t) result
