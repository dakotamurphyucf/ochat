(* The protocol owns canonical wire DTOs. This module owns one private durable
   stage and proof-gated publication state, without transport or execution. *)

module Bundle = Agent_protocol.Authoring_bundle.Bundle

type t
type sealed

(** The parent recorded in the persisted manifest of this exact Store claim.
    This is a comparison coordinate, not a Core context or read grant. *)
val sealed_parent_context : sealed -> Agent_protocol.Authoring.Context.Expected.t option

(** Comparison coordinate of the daemon that opened this store. This does not
    grant access to any upload. *)
val host_id : t -> Agent_protocol.Id.Server.t

module Proof : sig
  type t =
    | Project of Chatml_authoring_bundle_project.Validated.t
    | Chatmd_document of Chatml_authoring_bundle_document.Validated.t
end

module Snapshot : sig
  type t

  val manifest : t -> Bundle.Manifest.t
  val sources : t -> Chatml_source.t list
end

module Published_pin : sig
  type t

  (** Comparison evidence minted only while this Store verifies a published
      capture. It is bound to the Store instance, principal, upload, bundle,
      original deadline and durable publication; it grants no read itself. *)
end

module Published_capture : sig
  type t

  module Graph : sig
    type t

    val selector : t -> Bundle.Manifest.Script.Selector.t option
    val lock_source : t -> Chatml_source.t
    val sources : t -> Chatml_source.t list
  end

  val receipt : t -> Bundle.Receipt.t
  val manifest : t -> Bundle.Manifest.t
  val uploaded_sources : t -> Chatml_source.t list
  val graphs : t -> Graph.t list
  val pin : t -> Published_pin.t

  (** Pure, bounded comparison with a newly restored genuine closure. Reuse the
      publisher's one receipt-basis projection and compare every saved graph's
      selector, lock and source bytes; no saved receipt or captured source
      grants read or execution authority. *)
  val agrees_with_proof : t -> Proof.t -> checkpoint:(unit -> unit) -> bool
end

module Claim_result : sig
  type t =
    | Ready of sealed
    | Published of
        { receipt : Bundle.Receipt.t
        ; parent_context : Agent_protocol.Authoring.Context.Expected.t option
        }
    | Rejected of
        { reason : Bundle.Rejection.t
        ; parent_context : Agent_protocol.Authoring.Context.Expected.t option
        }

  (** Terminal exact-key retries retain the persisted parent. A caller must
      recheck it before returning either outcome; a wire Expected alone is not
      physical parent authority. *)
end

module Status : sig
  type t =
    | Open of Bundle.Status.Progress.t list
    | Ready
    | Validating
    | Published of Bundle.Receipt.t
    | Rejected of Bundle.Rejection.t
    | Expired

  (** Offsets are ordered by canonical manifest member order and bounded by
      max_members. Validating means a durable intent without an authoritative
      marker; Published requires a verified marker and exact installed bytes. *)
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
    -> max_published_records:int
    -> max_installed_sources:int
    -> max_installed_bytes_per_bundle:int
    -> max_installed_bytes:int
    -> max_installed_per_principal:int
    -> max_publication_metadata_bytes:int
    -> max_intent_bytes:int
    -> max_marker_bytes:int
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

(** Check an existing verified capture's publication under this Store's
    operation gate. Fresh trusted time, live daemon lock, original deadline,
    current authoritative intent/marker and exact receipt/manifest identity
    must agree. This checks bounded durable publication metadata, not installed
    source bytes; an initial read still requires [with_published]. Recovery may
    itself perform bounded integrity I/O. The host separately reauthorizes its
    bundle-read grant and selected compiler contracts after this yielding call.
    A pin from another Store instance or a pruned publication fails closed. *)
val check_published_pin
  :  t
  -> principal:Agent_protocol.Id.Principal.t
  -> Published_pin.t
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> checkpoint:(unit -> unit)
  -> (unit, Error.t) Result.t

(** Nonyielding comparison for a previously verified published capture. Try
    the existing Store gate; Busy means another operation owns it. Under that
    gate, verify the live daemon lock and compare this physical Store,
    principal, current in-memory publication marker/receipt/manifest and the
    original deadline using fresh trusted [now]. No wait, durable read or
    installed-source verification occurs here;
    [check_published_pin] remains the full yielding integrity check. Both use
    the same pure pin-identity comparison. *)
val check_published_pin_now
  :  t
  -> principal:Agent_protocol.Id.Principal.t
  -> Published_pin.t
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> (unit, Error.t) Result.t

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
      addition to staged source bytes. At a retention ceiling
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
      execution. The publisher consumes it with captured-only validation.
      [now] is the host-owned clock callback. A sealed claim has one fixed receipt
      deadline set at its first complete commit; retry/status does not renew it.
      An expired claim or a handle from a closed/reopened store cannot snapshot.
      Snapshot creation rechecks durable files after restart; cancellation
      publishes no bundle. Full materialization is bounded by aggregate source
      bytes and charged to the same work ledger. *)
val snapshot
  :  t
  -> principal:Agent_protocol.Id.Principal.t
  -> sealed
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> checkpoint:(unit -> unit)
  -> (Snapshot.t, Error.t) Result.t

(** Publish only from the exact live principal-bound sealed claim, checked
    snapshot and genuine opaque captured-closure proof. The existing Store gate
    rechecks exact bytes/manifest, commit key and fixed deadline. Durable intent
    chooses a stable bundle ID before installed captures; only a final atomic
    synced marker can expose Published. [final_admission] must be a trusted
    non-yielding current-actor/selection check. The Store samples fresh trusted
    time and calls it immediately before that marker rename. Incomplete intent
    remains Validating and
    same-key retry keeps its original ID. Cancellation may leave uncertain
    durable work; authorized status/commit retry reconciles it. *)
val publish
  :  t
  -> principal:Agent_protocol.Id.Principal.t
  -> sealed
  -> snapshot:Snapshot.t
  -> proof:Proof.t
  -> parent_pin:Published_pin.t option
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> checkpoint:(unit -> unit)
  -> final_admission:(unit -> unit)
  -> (Bundle.Receipt.t, Error.t) Result.t

(** [parent_pin] is required even for parentless publication (where it is
    [None]), so the publisher must make the parent decision explicitly. A
    parent pin is minted only by the actual uploaded parent capture. After all
    yielding publication work and ordinary metadata verification, the existing
    already-held Store gate compares the pin through the same pure identity
    rule as [check_published_pin_now], immediately before marker rename. It
    checks the live daemon lock and Store/principal/marker/deadline without
    recursively borrowing the Store or performing extra I/O. A mismatch leaves pending intent custody
    for an authorized exact-key retry. [final_admission] independently checks
    the current physical Core parent/actor and selected publication policy. *)

(** An authorized validator's coarse rejection before any durable intent.
    An existing intent cannot lose its selected ID through rejection. *)
val reject
  :  t
  -> principal:Agent_protocol.Id.Principal.t
  -> sealed
  -> reason:Bundle.Rejection.t
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> checkpoint:(unit -> unit)
  -> (unit, Error.t) Result.t

(** An authenticated, deadline-bound borrow of one verified published capture.
    [admit_metadata] is a trusted bounded non-yielding host callback after the
    principal/deadline lookup, before this request's captured bytes are verified
    or materialized. It sees comparison metadata only. Internal recovery before
    this point may itself perform bounded integrity reads; this hook does not
    claim to precede all store I/O. The caller must independently authorize the
    read before entering Store and recheck current selection after waits.
    Admission checks the durable marker and exact installed bytes under the
    Store gate. [f] runs outside that gate while prune protects its
    source files; its immutable byte values may escape as data, but the borrow
    is no longer authority after return. One Store borrow may be active at a
    time; a second returns Busy without waiting or materializing another
    capture. The active input bytes and graph metadata obey the configured
    bundle, installed-source and intent caps; this is not a heap-byte claim.
    Close joins active borrowers and a
    borrower calling close on this store receives Busy. *)
val with_published
  :  t
  -> principal:Agent_protocol.Id.Principal.t
  -> bundle_id:Agent_protocol.Id.Authoring_bundle.t
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> checkpoint:(unit -> unit)
  -> admit_metadata:(Bundle.Receipt.t -> Bundle.Manifest.t -> unit)
  -> f:(Published_capture.t -> 'a)
  -> ('a, Error.t) Result.t

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
      [protected_uploads], supplied by the trusted closure-validation owner,
      or captures with an active Store read lease. Excessive inputs fail the
      work limit.
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

(** True only after [close_and_wait] has joined every read lease and released
    this Store's process-local live-root reservation. Beginning close is not
    enough. The issuing Session_store uses this comparison before releasing
    the exclusive daemon lock borrowed by this child. *)
val fully_closed : t -> bool
