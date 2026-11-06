(** DATA reservation relation. Positive prefix <= nonce: every confirmation
    consumes a nonce, while no-writer closure consumes one without a confirmation.
    These counters are never an admission ACK.
    The scope anchor must match the exact branch reference key and child; only
    runtime authentication can verify its transaction against the lower record. *)
type t [@@deriving equal, sexp_of]

(** Scope anchor matches the complete validated Reference key and child. The
    transaction still requires the actual lower record at runtime. *)
val validate_branch
  :  scope:Agent_store.Delegation_goal_origin.Scope.t
  -> branch_reference:Agent_store.Delegation_store.Reference.t
  -> (unit, Agent_protocol.Error.t) result

val create
  :  scope:Agent_store.Delegation_goal_origin.Scope.t
  -> branch_reference:Agent_store.Delegation_store.Reference.t
  -> nonce:int64
  -> next_prefix:int64
  -> occurrence:Goal_descendant_occurrence.t
  -> (t, Agent_protocol.Error.t) result

val scope : t -> Agent_store.Delegation_goal_origin.Scope.t
val branch_reference : t -> Agent_store.Delegation_store.Reference.t
val nonce : t -> int64
val next_prefix : t -> int64
val occurrence : t -> Goal_descendant_occurrence.t
val shape : Document_schema.Shape.t
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result

(** Invalid capacity witness with this exact immutable scope/reference and the
    largest future occurrence/counters. Never decoded as reservation evidence. *)
val capacity_projection
  :  scope:Agent_store.Delegation_goal_origin.Scope.t
  -> branch_reference:Agent_store.Delegation_store.Reference.t
  -> Jsonaf.t
