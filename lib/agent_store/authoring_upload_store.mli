(* OCH-281 owns the wire DTOs and canonical manifest validation. This module
   owns private durable staging only; no transport, compiler or publisher. *)

module Bundle = Agent_protocol.Authoring_bundle.Bundle

type t
type sealed

module Claim_result : sig
  type t =
    | Ready of sealed
    | Rejected of Bundle.Rejection.t
end

module Status : sig
  type t =
    | Open of Bundle.Status.Progress.t list
    | Ready
    | Rejected of Bundle.Rejection.t
    | Expired

  (** Offsets are ordered by canonical manifest member order and bounded by
        max_members. No published state is manufactured in OCH-263. The later
        proof-gated publisher adds publication state and wire projection. *)
end

module Limits : sig
  type t

  val create
    :  max_active_uploads:int
    -> max_active_per_principal:int
    -> max_members:int
    -> max_manifest_bytes:int
    -> max_member_bytes:int
    -> max_bundle_bytes:int
    -> max_chunk_bytes:int
    -> max_reserved_bytes:int
    -> max_reserved_per_principal:int
    -> max_work_steps:int
    -> max_journal_records:int
    -> max_journal_bytes:int
    -> max_idempotency_records:int
    -> max_tombstones:int
    -> max_tombstone_bytes:int
    -> max_recovery_entries:int
    -> max_recovery_metadata_bytes:int
    -> max_recovery_source_bytes:int
    -> staging_ttl:Core.Time_ns.Span.t
    -> receipt_ttl:Core.Time_ns.Span.t
    -> expired_tombstone_ttl:Core.Time_ns.Span.t
    -> t Core.Or_error.t
end

module Error : sig
  type t =
    | Invalid_manifest
    | Limit_exceeded
    | Idempotency_conflict
    | Invalid_offset
    | Incomplete
    | Sealed
    | Expired
    | Missing_or_foreign
    | Busy
    | Closed
    | Storage of Store_error.t
  [@@deriving sexp_of]
end

(** [daemon_lock] must be live, owned by [host_id], and at
      [Data_root.daemon_lock_path data_root]; all three facts are checked on
      admission and operations. The caller retains that OS lock through
      [close_and_wait]. A race-safe process-local live-root registry prevents
      accidental duplicate store facades; it is not an OS ownership claim and
      releases its reservation on failed/cancelled open, close and switch
      cleanup. [sw] joins owned Eio resources at close. A dedicated private sibling under
      Data_root.path is created and recovered with Durable_file/Document_schema
      primitives. It does not use
      Blob_store's session/message metadata or expose a native path. Startup
      scans have finite entry/metadata/byte/work ceilings and reject links,
      unknown authoritative records, conflicting keys and corrupt manifests.
      Current phase/progress record, key and tombstone counts/bytes have explicit finite ceilings in
      addition to staged source bytes. Published records belong to OCH-264's
      proof-gated owner. At a retention ceiling
      the store rejects new work; it never evicts live or protected receipts to
      make room. No second independently opened store may own the same path.
      The store serializes mutations, recovery and status reads through its
      own operation gate, independent of any host gate. *)
val open_or_create
  :  env:Eio_unix.Stdenv.base
  -> sw:Eio.Switch.t
  -> data_root:Data_root.t
  -> daemon_lock:Lock.t
  -> host_id:Agent_protocol.Id.Server.t
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

(** For each serialized operation below, [checkpoint] runs immediately after
    acquiring the store operation gate, before uncertain-state recovery or
    private upload access. A host may wrap it with a non-yielding current-grant
    check; callbacks retain their exception/cancellation identity and do not
    poison the gate. [now] is a trusted, non-yielding wall-clock callback,
    sampled only under that gate after any recovery and the admission
    checkpoint. Production callers must pass a callback that reads their
    current host clock; fixed callbacks are suitable only for deterministic
    fixtures. The sampled value governs admission, not a second check after
    all I/O completes. *)

(** The transport-neutral DTO is already syntactically validated. The store
      revalidates member count, canonical IDs/collisions, byte lengths/digests
      and aggregate ceilings before reserving disk and writing a durable begin
      record. The idempotency identity is (host, principal, begin key), bound to
      canonical manifest + optional parent provenance. Same key/same payload
      replays one upload ID without double reservation; different payload
      conflicts. A lost begin reply is recoverable after process restart while
      its bounded idempotency record is retained. *)
val begin_upload
  :  t
  -> principal:Agent_protocol.Id.Principal.t
  -> request:Bundle.Begin.request
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> checkpoint:(unit -> unit)
  -> (Bundle.Begin.response, Error.t) Result.t

(** Append only at the member's next accepted offset. An entirely accepted
      retry is a no-op only when the exact bounded bytes match the durable file;
      a changed or partially overlapping retry conflicts. Before reporting the
      next offset, fsync the bytes and atomically replace/sync the offset record.
      Recovery truncates an unacknowledged suffix or fails closed, never treats
      it as an accepted chunk. Cancellation before offset-record fsync cannot
      acknowledge the chunk; cancellation after fsync may yield an uncertain
      reply, reconciled by exact-byte retry/status. Source IDs are looked up in
      the private manifest, not used as filesystem paths. Chunk count, bytes,
      journal records and work are bounded. *)
val chunk
  :  t
  -> principal:Agent_protocol.Id.Principal.t
  -> request:Bundle.Chunk.request
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> checkpoint:(unit -> unit)
  -> (Bundle.Chunk.response, Error.t) Result.t

(** Seal only a complete stage after independently rechecking every exact
      length/digest. An incomplete call records no commit key, so it can be
      retried after more chunks. A complete call durably binds one commit key
      to principal, upload, manifest and complete payload digest; same key
      returns Ready while awaiting publication, or Rejected for a previously
      verified invalid payload. Another key conflicts. Sealed members are
      immutable. This is NOT
      closure validation or bundle publication. The returned `sealed` is a
      store-owned, instance-and-epoch-bound handle. Snapshot rejects one from
      another store or a closed/reopened instance; a recovered claim obtains a
      fresh handle by exact-key retry. *)
val claim_commit
  :  t
  -> principal:Agent_protocol.Id.Principal.t
  -> request:Bundle.Commit.request
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> checkpoint:(unit -> unit)
  -> (Claim_result.t, Error.t) Result.t

(** A checked, immutable in-memory snapshot under the stage's aggregate byte
      ceiling. This does not parse ChatMD, restore or compose a graph, or grant
      execution. OCH-264 consumes it with existing captured-only validation.
      [now] is the host-owned clock callback. A sealed claim has one fixed receipt
      deadline set at its first complete commit; retry/status does not renew it.
      An expired claim or a handle from a closed/reopened store cannot snapshot.
      Snapshot creation rechecks durable files after restart; cancellation
      publishes no bundle. Full materialization is bounded by aggregate source
      bytes and charged to the same work ledger. *)
val snapshot
  :  t
  -> sealed
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> checkpoint:(unit -> unit)
  -> (Bundle.Manifest.t * Chatml_source.t list, Error.t) Result.t

(** OCH-264 extends this store with a publisher accepting its abstract
      validated-closure proof, never a caller-created Receipt. Publication is
      durably reconciled against the exact sealed upload/payload and installed
      immutable bundle before status can expose Published. OCH-263 stores the
      sealed claim and bounded metadata needed for that extension; it cannot
      issue Published or mint a proof. *)

val status
  :  t
  -> principal:Agent_protocol.Id.Principal.t
  -> upload_id:Agent_protocol.Id.Authoring_upload.t
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> checkpoint:(unit -> unit)
  -> (Status.t, Error.t) Result.t

(** Principal-owned cancellation durably tombstones the stage before joined
      cleanup. Existing published bundles are never undone. Returns Expired to
      the same authorized principal through the documented tombstone interval;
      foreign/unknown IDs use one indistinguishable error. *)
val cancel
  :  t
  -> principal:Agent_protocol.Id.Principal.t
  -> upload_id:Agent_protocol.Id.Authoring_upload.t
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> checkpoint:(unit -> unit)
  -> (unit, Error.t) Result.t

(** Bounded maintenance: do not delete sealed claims named by
      [protected_uploads], supplied by the trusted closure-validation owner.
      This accepts upload IDs rather than future bundle IDs because OCH-263
      does not mint a bundle. Excessive inputs fail the work limit.
      Expire stages, receipts and tombstones according to original absolute
      deadlines, not a renewed connection lease. After a tombstone/idempotency
      record is pruned, its old request key is no longer reserved; the finite
      replay guarantee ends at that documented retention horizon. Uncertain or
      corrupt references defer deletion. Every mutation is joined with
      close/cancel and synced before success. *)
val prune
  :  t
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> protected_uploads:Agent_protocol.Id.Authoring_upload.t list
  -> checkpoint:(unit -> unit)
  -> (int, Error.t) Result.t

(** Close is idempotent and joins the operation gate. Expected sync/cleanup
      failure is returned; cancellation propagates after required cleanup. *)
val close_and_wait : t -> (unit, Error.t) Result.t
