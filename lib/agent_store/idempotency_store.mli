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

  include Core.Comparator.S with type t := t
end

module Command_audit : sig
  type t =
    { key : Key.t
    ; request_digest : string
    ; protected_record : bool
    }
  [@@deriving sexp]

  (** New authored receipt. Use encode_carrier for edits of restored receipts. *)
  val encode : t -> (Document_schema.Document.t, Store_error.t) result

  val restore
    :  Document_schema.Document.t
    -> (t Document_schema.Extension_carrier.t, Store_error.t) result

  val encode_carrier
    :  t Document_schema.Extension_carrier.t
    -> (Document_schema.Document.t, Store_error.t) result

  (** Read-only projection used by receipt reconciliation. *)
  val decode : Document_schema.Document.t -> (t, Store_error.t) result
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

(** Complete named-field cache envelope. Restored unknown fields remain attached
    to stable record identities through updates. Expiration explicitly retires
    the expired records and retains all other unknown fields. The complete cache
    admits fresh reserved metadata under the original 16 MiB / 1M field / 2M node profile.
    Existing receipts use derived compatibility headroom (38,877,216 bytes,
    1.6M fields, 2.7M nodes) so old near-full Pending receipts remain completable.
    Whole terminal outcomes have a 16 MiB base artifact bound. Separate original
    Pending custody can compose under a 32 MiB artifact bound;
    lookup uses validated cached outcomes without filesystem IO. Receipt expiration
    remains unchanged. Disk space is not guaranteed by metadata admission.
    Reference scans apply their caller's aggregate disk-and-memory budgets,
    including referenced artifacts. Bounded orphan retirement occurs during prune,
    under this owner, after disk and memory references have both been validated.

    The caller holds exclusive ownership of the index directory for this store's
    entire lifetime. The per-instance mutex does not serialize independently
    opened stores on the same path. Publication retains an opened directory
    capability while the owner mutex protects metadata/artifact ordering. *)
type t

val open_or_create : env:Eio_unix.Stdenv.base -> path:string -> (t, Store_error.t) result
val lookup : t -> key:Key.t -> request_digest:string -> lookup

(** Validate and scan both the durable file and current memory map, preserving
    references across failed write acknowledgements. The record/byte budgets cover
    both views. Decode successful JSON before scanning so escaped IDs remain visible.
    Any pending response defers collection (Ok None) without calling f; corruption,
    missing/linked files, duplicate durable keys or budget excess return an error.

    On success, call f with retained candidate IDs while holding the store mutex.
    Acquire the owning actor checkpoint before entering. The callback must not
    reenter this store or await an actor: actor commits can need this mutex.
    The callback must establish all other roots before deleting anything. *)
val with_retained_references
  :  t
  -> candidates:Agent_protocol.Id.Blob.t list
  -> max_records:int
  -> max_bytes:int
  -> f:(Agent_protocol.Id.Blob.t list -> ('a, Store_error.t) result)
  -> ('a option, Store_error.t) result

(** [record] durably inserts a record. An existing key with another request
    digest returns a typed conflict without replacing the original. Fresh capacity
    exhaustion returns Admission_capacity before durable Pending or publication;
    existing-record mutation failures retain their ordinary IO/document semantics. *)
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
