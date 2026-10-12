open! Core

(** Private actual first-Created/receipt recovery owner. All native services are
    borrowed from one live Factory/Store; no actor or retry table is allocated. *)
module Limits : sig
  type t

  val create
    :  max_sessions:int
    -> max_transactions:int
    -> max_input_bytes:int
    -> max_work_steps:int
    -> (t, Agent_protocol.Error.t) Result.t

  val default : t
  val max_sessions : t -> int
  val max_transactions : t -> int
  val max_input_bytes : t -> int
  val max_work_steps : t -> int
end

type storage_limits =
  { max_journal_payload : int
  ; max_segment_bytes : int64
  ; max_segment_frames : int
  ; snapshot_payload_limit : int
  }

type t

val create
  :  env:Eio_unix.Stdenv.base
  -> store:Agent_store.Session_store.t
  -> registry:Session_registry.t
  -> idempotency_store:Agent_store.Idempotency_store.t
  -> journal_document_limits:Document_schema.Limits.t
  -> storage_limits:storage_limits
  -> max_work_steps:int
  -> t

(** Dedicated invocation-affine seed work accounting shared by admission and
    recovery; every poll checks cancellation before and after the borrowed
    callback. Only this invocation's own exhausted bound becomes a typed error. *)
val with_work
  :  max_work_steps:int
  -> checkpoint:(unit -> unit)
  -> ((unit -> unit) -> ('a, Agent_protocol.Error.t) Result.t)
  -> ('a, Agent_protocol.Error.t) Result.t

(** Mutable-owner/close/prune admission: ordinary records are unchanged; actual
    authenticated seed Created requires its exact protected durable Success. *)
val require_success
  :  t
  -> Agent_store.Session_store.Handle.t
  -> (unit, Agent_protocol.Error.t) Result.t

(** Bounded indexed first-frame evidence, Pending-only no-later-transaction proof
    and existing journal/snapshot repair. Exact durable Success precedes the
    nonactivating Registry index after lifecycle release. Unknown/contradictory
    proof fails closed; no runtime/source/provider reconstruction or creation retry.
    Original callbacks/cancellation/backtraces propagate after owned cleanup. *)
val reconcile
  :  t
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (unit, Agent_protocol.Error.t) Result.t
