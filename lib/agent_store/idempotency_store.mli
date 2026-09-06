(** Durable command idempotency records. Session actors must commit equivalent
    records in the same transaction as their state mutation. *)

module Key : sig
  type t =
    { principal_id : Agent_protocol.Id.Principal.t
    ; session_id : Agent_protocol.Id.Session.t option
    ; method_name : string
    ; idempotency_key : Agent_protocol.Idempotency_key.t
    }
  [@@deriving compare, sexp]
end

module Command_audit : sig
  type t =
    { key : Key.t
    ; request_digest : string
    ; protected_record : bool
    }
  [@@deriving sexp]

  val encode : t -> string
  val decode : string -> (t, Store_error.t) result
end

type outcome =
  | Pending
  | Success of Jsonaf.t
  | Failure of Agent_protocol.Error.t

type retention =
  | Standard
  | Protected
[@@deriving compare, equal, sexp]

type record =
  { key : Key.t
  ; request_digest : string
  ; accepted_transaction_sequence : int64 option
  ; outcome : outcome
  ; created_at : Agent_protocol.Timestamp.t
  ; expires_at : Agent_protocol.Timestamp.t option
  ; retention : retention
  }

type lookup =
  | Missing
  | Replay of record
  | Conflict of record

(** Keep successful cached outcomes encoded until replay. The public records
    and persisted schema are unchanged; writes do not repeatedly traverse every
    cached JSON response tree. *)
type t

val open_or_create : env:Eio_unix.Stdenv.base -> path:string -> (t, Store_error.t) result
val lookup : t -> key:Key.t -> request_digest:string -> lookup

(** [record] durably inserts a record. An existing key with another request
    digest returns a typed conflict without replacing the original. *)
val record : t -> record -> (record, Store_error.t) result

(** [complete] durably replaces a matching pending record with its terminal
    outcome. Completed records are returned unchanged, making completion
    idempotent. *)
val complete
  :  t
  -> key:Key.t
  -> request_digest:string
  -> accepted_transaction_sequence:int64 option
  -> outcome:outcome
  -> (record, Store_error.t) result

(** [mark_accepted] records the session-journal transaction that accepted a
    matching request without changing its pending or terminal outcome. *)
val mark_accepted
  :  t
  -> key:Key.t
  -> request_digest:string
  -> transaction_sequence:int64
  -> (record, Store_error.t) result

(** [reconcile_accepted] applies journal-backed acceptance sequences in one
    durable index replacement. Missing standard receipts are treated as
    expired, while missing protected receipts and conflicts fail closed. *)
val reconcile_accepted
  :  t
  -> (Command_audit.t * int64) list
  -> (int, Store_error.t) result

(** [prune_expired] removes expired standard records and retains protected ones. *)
val prune_expired : t -> now:Agent_protocol.Timestamp.t -> (int, Store_error.t) result
