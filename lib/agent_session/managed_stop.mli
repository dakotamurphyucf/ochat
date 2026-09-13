open Core

(** Immutable admission receipt, persisted in the same transaction as stop intent.
    A receipt proves admission, not that resource cleanup has finished. Its key is
    scoped to the private relationship and survives target restart/replacement. *)
type t = private
  { id : Agent_protocol.Id.Transaction.t
  ; reference : Agent_store.Delegation_store.Reference.t
  ; key : Agent_protocol.Idempotency_key.t
  ; mode : Agent_protocol.Session.stop_mode
  ; generation : int
  ; stop_epoch : int64
  ; accepted_at : Agent_protocol.Timestamp.t
  }
[@@deriving equal, sexp]

val create
  :  reference:Agent_store.Delegation_store.Reference.t
  -> key:Agent_protocol.Idempotency_key.t
  -> mode:Agent_protocol.Session.stop_mode
  -> generation:int
  -> stop_epoch:int64
  -> now:Agent_protocol.Timestamp.t
  -> (t, Agent_protocol.Error.t) result

val same_key : t -> t -> bool
val validate : t -> (unit, Agent_protocol.Error.t) result

(** Bounded metadata, excluding private delegation and policy details. *)
val to_json : t -> Jsonaf.t
