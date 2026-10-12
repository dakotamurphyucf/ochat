open! Core
module P = Agent_protocol
module Store = Agent_store
module S = Agent_session

(** Private stateless evidence for this command's actual first Created journal
    transaction. General creation/recovery and legacy import remain unchanged. *)
type t

(** Ordinary transaction validation precedes extraction. Require the first
    transaction, actual Created state and Session_created event to agree on the
    identity/counters, plus an authentic protected session.seed Command_audit
    whose principal is the actual creating principal.
    No snapshot-less bootstrap for other methods or other transitions. Returned
    state keeps complete admitted unknown-field preservation through the existing
    state/delta document owner. [limits] bounds complete input; [checkpoint]
    borrows the caller's finite whole recovery work allowance, including neutral
    relation validation. Original checkpoint exceptions propagate. *)
val of_created
  :  Store.Transaction.t
  -> limits:Document_schema.Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Store.Store_error.t) Result.t

(** Decode one already-checksummed initial journal frame through the existing
    document/transaction owners. A different original command (or no audit) is
    [None]; an actual session.seed audit must satisfy all [of_created] invariants.
    Unknown flags or malformed complete transactions reject. Used by immutable
    startup reconciliation and the indexed mutable-owner admission gate. *)
val of_initial_frame
  :  Store.Frame.t
  -> limits:Document_schema.Limits.t
  -> checkpoint:(unit -> unit)
  -> (t option, Store.Store_error.t) Result.t

val audit : t -> Store.Idempotency_store.Command_audit.t
val session_id : t -> P.Id.Session.t
val transaction_sequence : t -> int64
val transaction_hash : t -> string
val restored : t -> S.Session_persistence.Restored.t
val result : t -> P.Session_seed.Result.t

(** Pure matching of one existing protected Pending/terminal receipt against the
    complete actual audit key/digest/retention and created identity. Conflicting
    terminal results reject. Success requires the exact accepted transaction
    sequence; Pending may omit it or name that same sequence. Absence never
    creates a substitute record. Factory
    also rejects two actual Created identities for one original command before
    completing either. Snapshot/index repair precedes final Success completion.
    This module performs no IO, activates no runtime and owns no retry table. *)
val validate_receipt
  :  t
  -> Store.Idempotency_store.record
  -> (unit, Store.Store_error.t) Result.t
