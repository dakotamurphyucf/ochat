(** Pure bounded DATA relation for one frozen root/first-child branch. No method
    authenticates ancestry, observes a writer ACK or proves no writer exists. *)
module Last : sig
  type t =
    | Included of Goal_descendant_included.t
    | No_writer_closed of Goal_descendant_pending.t
  [@@deriving equal, sexp_of]
end

type t [@@deriving equal, sexp_of]

val create
  :  scope:Agent_store.Delegation_goal_origin.Scope.t
  -> reference:Agent_store.Delegation_store.Reference.t
  -> (t, Agent_protocol.Error.t) result

val scope : t -> Agent_store.Delegation_goal_origin.Scope.t
val reference : t -> Agent_store.Delegation_store.Reference.t
val confirmed_prefix : t -> int64
val last_nonce : t -> int64
val pending : t -> Goal_descendant_pending.t option
val last : t -> Last.t option

module Reserve_result : sig
  type branch = t

  type t =
    | Reserved of branch * Goal_descendant_pending.t
    | Already_pending of Goal_descendant_pending.t
    | Busy of Goal_descendant_pending.t
end

(** Exact live occurrence replay returns Already_pending unchanged. A different
    occurrence returns Busy. Fresh allocation checks both counters before +1. *)
val reserve
  :  t
  -> occurrence:Goal_descendant_occurrence.t
  -> (Reserve_result.t, Agent_protocol.Error.t) result

(** Only the exact current pending receipt can advance the prefix. Exact repeat
    of the latest Included terminal outcome is idempotent. *)
val confirm
  :  t
  -> included:Goal_descendant_included.t
  -> (t, Agent_protocol.Error.t) result

(** Clears the exact pending without changing prefix or reusing nonce. Exact
    repeat of the latest No_writer_closed terminal outcome is idempotent. *)
val close_before_writer
  :  t
  -> pending:Goal_descendant_pending.t
  -> (t, Agent_protocol.Error.t) result

module Action : sig
  type t =
    | Reserve of Goal_descendant_occurrence.t
    | Confirm of Goal_descendant_included.t
    | Close_before_writer of Goal_descendant_pending.t
end

(** Recompute precisely this finite action from the prior value and require the
    entire incoming value to equal its result. Busy/Already_pending are no-op
    Reserve steps. This never authorizes arbitrary endpoint adoption. *)
val validate_update
  :  t
  -> incoming:t
  -> action:Action.t
  -> (unit, Agent_protocol.Error.t) result

(** Bare DATA codec validates immutable identity and static frontier relations;
    raw preflight precedes decoding. Native candidates share the same hard
    256 KiB/depth64/fields16384/nodes32768 bound. Enclosing component owns complete
    document bounds/unknown preservation and its tighter remaining capacity. *)
val limits : Document_schema.Limits.t

val shape : Document_schema.Shape.t
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
