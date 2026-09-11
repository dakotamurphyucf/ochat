open Core

type outcome =
  | Completed
  | Failed
  | Cancelled
  | Interrupted
  | Invalidated
[@@deriving equal, sexp]

type status =
  | Deferred
  | Ready
  | Assigned of Agent_protocol.Id.Operation.t
  | Terminal of Agent_protocol.Id.Operation.t option * outcome
[@@deriving equal, sexp]

(** The creation reference scopes retries to the actual parent/generation/principal
    and target. Target generation is deliberately not part of the retry key: an
    old send must not become a new message following target reset. *)
type t = private
  { reference : Agent_store.Delegation_store.Reference.t
  ; key : Agent_protocol.Idempotency_key.t
  ; request_sha256 : string
  ; generation : int
  ; history_id : Agent_protocol.History.Id.t
  ; created_at : Agent_protocol.Timestamp.t
  ; updated_at : Agent_protocol.Timestamp.t
  ; status : status
  ; output_ids : Agent_protocol.History.Id.t list
  }
[@@deriving equal, sexp]

val create
  :  reference:Agent_store.Delegation_store.Reference.t
  -> key:Agent_protocol.Idempotency_key.t
  -> request_sha256:string
  -> generation:int
  -> history_id:Agent_protocol.History.Id.t
  -> now:Agent_protocol.Timestamp.t
  -> (t, Agent_protocol.Error.t) result

val same_key : t -> t -> bool
val validate : t -> (unit, Agent_protocol.Error.t) result
val validate_transition : previous:t -> t -> (unit, Agent_protocol.Error.t) result

(** Receipt metadata without private delegation fields or output content. A
    terminal status identifies the processing outcome, not an automatic retry. *)
val to_json : t -> Jsonaf.t

(** Pure reconciliation from actual accepted history and typed operation terminal
    events. Never infers completion from Idle or assistant text. The actor persists
    returned changes in the same transaction as adoption/termination. *)
val reconcile
  :  generation:int
  -> reference:Agent_store.Delegation_store.Reference.t option
  -> discarded:bool
  -> adopted:bool
  -> appended:Agent_protocol.History.entry list
  -> operation:Agent_protocol.Operation.t option
  -> terminals:(Agent_protocol.Id.Operation.t * outcome) list
  -> now:Agent_protocol.Timestamp.t
  -> t
  -> t
