(** Pure session lifecycle authority. No actor, filesystem or authorization
    decisions occur here. Callers reauthorize current ownership before replay. *)
open! Core

module P = Agent_protocol
module Revision = P.Session_lifecycle.Revision
module Admission = P.Session_lifecycle.Result.Admission
module Status = P.Session_lifecycle.Result.Status

module Anchor : sig
  type t = private
    { generation : int
    ; session_revision : int64
    ; latest_event_sequence : int64
    }
  [@@deriving equal, sexp_of]

  val create
    :  generation:int
    -> session_revision:int64
    -> latest_event_sequence:int64
    -> (t, P.Error.t) Result.t
end

module Outcome : sig
  type action =
    | Archive
    | Restore
    | Resume
    | Remove
  [@@deriving equal, sexp]

  type disposition =
    | Applied
    | Already_current
  [@@deriving equal, sexp]

  type t = private
    { session_id : P.Id.Session.t
    ; anchor : Anchor.t
    ; lifecycle_revision : Revision.t
    ; status : Status.t
    ; admission : Admission.t
    ; action : action
    ; disposition : disposition
    ; completed_at : P.Timestamp.t
    }

  val create
    :  session_id:P.Id.Session.t
    -> anchor:Anchor.t
    -> lifecycle_revision:Revision.t
    -> status:Status.t
    -> admission:Admission.t
    -> action:action
    -> disposition:disposition
    -> completed_at:P.Timestamp.t
    -> (t, P.Error.t) Result.t

  val equal : t -> t -> bool
  val method_name : action -> string

  (** Existing session.delete is also permitted for Archive/Remove. The service
      must derive action from the validated ORIGINAL delete policy and retain its
      original key/params digest; this function is not authorization. *)
  val accepts_method : action -> method_name:string -> bool
end

module Receipt : sig
  type t = private
    { key : Idempotency_store.Key.t
    ; request_digest : string
    ; outcome : Outcome.t
    ; created_at : P.Timestamp.t
    ; expires_at : P.Timestamp.t
    ; completion_acknowledged : bool
    }

  val create
    :  key:Idempotency_store.Key.t
    -> request_digest:string
    -> outcome:Outcome.t
    -> created_at:P.Timestamp.t
    -> expires_at:P.Timestamp.t
    -> completion_acknowledged:bool
    -> (t, P.Error.t) Result.t

  val equal : t -> t -> bool
  val same_proof : t -> t -> bool
end

type t

val initial : session_id:P.Id.Session.t -> t

(** Explicit bootstrap for original archive markers, not a mutation API. *)
val of_original_archive : session_id:P.Id.Session.t -> t

val restore
  :  session_id:P.Id.Session.t
  -> status:Status.t
  -> admission:Admission.t
  -> revision:Revision.t
  -> receipts:Receipt.t list
  -> (t, P.Error.t) Result.t

val session_id : t -> P.Id.Session.t
val status : t -> Status.t
val admission : t -> Admission.t
val revision : t -> Revision.t
val receipts : t -> Receipt.t list
val equal : t -> t -> bool
val max_receipts : int

(** Retention in milliseconds: 24 hours. Unacknowledged proof never expires.
    Terminal Applied Remove proof stays pinned while Removed even after generic
    completion acknowledgement; only physical document cleanup retires it. *)
val receipt_retention_ms : int

val lookup
  :  t
  -> key:Idempotency_store.Key.t
  -> request_digest:string
  -> now:P.Timestamp.t
  -> (Receipt.t option, P.Error.t) Result.t

type record = t

module Prepared : sig
  type t

  val previous : t -> record
  val next : t -> record
  val outcome : t -> Outcome.t
end

(** Exact-key replay precedes expected-revision checks. Restore of Active is a
    no-op preserving its admission. Removed rejects all fresh non-Remove actions.
    Expired acknowledged receipts alone may retire; no early capacity eviction. *)
val prepare
  :  t
  -> expected:Revision.t
  -> anchor:Anchor.t
  -> action:Outcome.action
  -> key:Idempotency_store.Key.t
  -> request_digest:string
  -> now:P.Timestamp.t
  -> (Prepared.t, P.Error.t) Result.t

(** Caller must first prove exact generic durable completion. No outcome changes. *)
val acknowledge
  :  t
  -> key:Idempotency_store.Key.t
  -> request_digest:string
  -> (t, P.Error.t) Result.t

(** Preservation boundary: permits only lawful transition, immutable receipts,
    false→true acknowledgement and acknowledged expiry retirement. *)
val validate_successor : t -> next:t -> now:P.Timestamp.t -> (unit, P.Error.t) Result.t
