(** Eio streaming storage for temporary uploads and session-owned blobs. *)

module Metadata = Blob_metadata

module Handle : sig
  type t

  (** Last validated observation for diagnostics only. Production authority
      decisions use [metadata_checked]; reads/mutations enforce it internally. *)
  val metadata : t -> Metadata.t

  (** Reject if uncertain adoption could not prove the original or installed
      exact document/data pair. A fresh checked reopen may recover authority. *)
  val metadata_checked : t -> (Metadata.t, Store_error.t) result
end

module Upload : sig
  type t
end

type t

(** Physical storage root for a result. The developer constructor retains an
    opaque, live-Store-issued witness; IDs and native paths cannot construct it.
    This only identifies custody, not a job/attempt or read disclosure. *)
module Result_root : sig
  type t

  val conversation : Session_store.Handle.t -> t
  val developer : Developer_run_store.Result_root.t -> t

  (** Comparison data for constructing a stage; [target] is never a grant. *)
  val target : t -> Metadata.Target.t
end

(** Bind the one daemon-owned developer Store to this store's shared coordinator.
    The real Session_store must physically issue [runs] and validate this
    Blob_store's exact Eio filesystem capability and Data_root directories;
    equal native path strings cannot prove affinity. Every facade made by
    [with_max_upload_bytes] shares the binding. Binding the same Store again is
    idempotent. After an orderly Developer_run_store close, a new Store under
    the SAME still-live Session_store may replace it while the coordinator is
    quiescent; a different Session_store requires a new Blob_store. A nonempty
    developer namespace without a live matching binding fails retention proof.
    This is host wiring, not a run, job or result grant. *)
val bind_developer_runs
  :  t
  -> session_store:Session_store.t
  -> Developer_run_store.t
  -> (unit, Store_error.t) result

(** Physical comparison for a live developer result publisher: verifies that
    this facade shares the coordinator bound to [runs], whose Session_store
    also owns this store's filesystem roots. No run/job/read authority follows
    from a true result; each result operation still rechecks its Result_root. *)
val developer_binding_matches : t -> Developer_run_store.t -> bool

(** A result-open observation bound to its original storage root. Every later
    result operation reborrows and revalidates that root; callers cannot pass
    this token to the unscoped [load_verified]/[read_range] functions. *)
module Result_read : sig
  type t

  (** Last validated observation for display only, never read authority. *)
  val metadata : t -> Metadata.t
end

val open_result
  :  t
  -> Result_root.t
  -> Agent_protocol.Id.Blob.t
  -> (Result_read.t, Store_error.t) result

val load_result_verified
  :  t
  -> sw:Eio.Switch.t
  -> Result_read.t
  -> max_bytes:int
  -> (string, Store_error.t) result

val read_result_range
  :  t
  -> sw:Eio.Switch.t
  -> Result_read.t
  -> offset:int64
  -> max_bytes:int
  -> (string, Store_error.t) result

(** The same selected stage and bytes use the existing staged-write engine and
    coordinator for both custody forms. The stage target must equal the root;
    the caller owns a durable private preparation intent before a write. A
    developer stage is kept in its bounded typed temporary namespace, separate
    from ordinary session uploads. No root or stage alone proves a job result. *)
val ensure_result_staged_content
  :  t
  -> sw:Eio.Switch.t
  -> Result_root.t
  -> stage:Blob_stage_documents.t
  -> string
  -> (Result_read.t, Store_error.t) result

(** Recover exact staged bytes without repair or publication. Missing or short
    partial data returns None; mismatched metadata, root or content fails. *)
val load_result_staged_content
  :  t
  -> sw:Eio.Switch.t
  -> Result_root.t
  -> metadata:Metadata.t
  -> max_bytes:int
  -> (string option, Store_error.t) result

val create
  :  env:Eio_unix.Stdenv.base
  -> temporary_directory:string
  -> durable_directory:string
  -> max_upload_bytes:int64
  -> (t, Store_error.t) result

(** Create a host-selected size policy over the same storage and coordinator.
    All live facades over these directories must derive from one created store;
    independent create calls do not coordinate. This is a host API, not a grant
    for a tool or descendant to raise its captured limits. *)
val with_max_upload_bytes : t -> max_upload_bytes:int64 -> (t, Store_error.t) result

type retention

(** Historical conservative presence projection. This is a bounded diagnostic
    and comparison API, not the collector's current reference or deletion
    proof; cross-owner collection uses [with_developer_retention] and the
    complete shared inventory below. *)
type developer_retention_presence =
  | No_developer_evidence
  | Has_developer_evidence
[@@deriving sexp_of]

(** With a live bound Store, validate both recognized developer Blob namespaces
    and conservatively detect the separate developer result-intent namespace,
    against every existing, bounded, authentic run root, charging run-document
    enumeration and all blob-file work to the same reader allowance as the
    enclosing conversation collection. Without one, verify the original live
    Session_store Handle and exact Data_root, then detect any developer
    authority entry conservatively. Unknown directories,
    linked/corrupt files, foreign metadata, incomplete unprovable stages and
    exhausted budgets reject the whole proof. Even a run document with no blob
    files returns [Has_developer_evidence], because its contents may reference
    conversation blobs. Any result-intent entry returns
    [Has_developer_evidence] even if a run document is absent; unknown/linked
    namespace structure fails closed. Complete developer blobs remain
    conservatively retained:
    this operation does not authorize deletion or infer publication/disclosure.
    A closed prior binding uses the issuing Handle's exact live Data_root to
    detect unbound evidence rather than borrowing a stale run issuer. It runs
    inside [with_retention] and must not call another public Blob_store
    operation. *)
val validate_developer_retention
  :  retention
  -> session:Session_store.Handle.t
  -> reader:Retention_reader.t
  -> (developer_retention_presence, Store_error.t) result

(** Borrow an exact result root during one retention scan without acquiring the
    Blob_store coordinator again. For developer roots the daemon-root lease
    remains held throughout [f], including bounded reader I/O; callbacks may
    not reenter Blob_store or Developer_run_store. The paths are comparison
    locations under the borrowed root, never standalone authority. *)
val with_result_retention_root
  :  retention
  -> Result_root.t
  -> reader:Retention_reader.t
  -> f:
       (durable_directory:string
        -> temporary_directory:string
        -> ('a, Store_error.t) result)
  -> ('a, Store_error.t) result

(** Hold the shared storage coordinator through f, deferring with Ok None when
    any upload or read is active. New uploads, reads, adoption, staged writes and
    expiry wait until f returns. Ordinary readers run concurrently without holding
    the mutex across network backpressure. Uploads release their activity on
    finish, abort or switch cleanup; reads release it on every return/cancellation.
    Acquire runtime owner, actor, publisher and response-cache locks first, in
    that order when the host supplies all four. A developer result operation
    acquires this coordinator before Developer_run_store's daemon-root lease;
    no current Developer_run_store path enters Blob_store while leased.
    The callback must not reenter public store operations, await actors or use the
    token concurrently. Establish all retained roots before deletion. The token
    expires on callback exit, including exceptions; this supplies serialization,
    not evidence that any particular artifact is unreferenced. *)
val with_retention
  :  t
  -> f:(retention -> ('a, Store_error.t) result)
  -> ('a option, Store_error.t) result

(** Collection-only borrow of the SAME coordinator and retention token as
    [with_retention], excluding active uploads/reads without masking caller
    cancellation during a potentially large reference inventory. The callback
    must complete proof before [protect_collection_mutation] is called. Neither
    phase may reenter this Blob_store or retain the token. The coordinator is
    never released between proof and deletion. Other [with_retention] callers
    keep their existing cancellation policy. *)
val with_collection_proof
  :  t
  -> f:(retention -> ('a, Store_error.t) result)
  -> ('a option, Store_error.t) result

(** The physically configured Blob upload ceiling for the active retention
    scope. This is only a bound, not authority to address a path. *)
val retention_max_upload_bytes : retention -> (int64, Store_error.t) result

(** Shield only the exact, retryable deletion/marker cleanup after a complete
    proof and final caller cancellation checkpoint, while the same retention
    token, Blob coordinator and developer-root gate remain live. A typed error
    leaves the remaining records for retry. *)
val protect_collection_mutation
  :  retention
  -> f:(unit -> ('a, Store_error.t) result)
  -> ('a, Store_error.t) result

(** Scoped, comparison-only proof of the exact developer source BINDING for
    this collector. It is minted only while BOTH the Blob coordinator and the
    selected developer-root gate are held. It is NOT a complete Blob or
    reference proof: [Blob_retention.scan] must still inventory and validate
    all developer documents, intents and Blob pairs before any deletion. Its
    paths are owner-derived and do not authorize arbitrary native filesystem
    access. A retained token is invalid as soon as either callback exits. *)
module Developer_retention : sig
  type t

  val inventory : t -> Developer_run_store.Retention.t option
  val intent_directory : t -> string
  val temporary_namespace : t -> string
  val durable_namespace : t -> string
  val check_current : t -> retention -> (unit, Store_error.t) result
end

(** Under a live retention token, select ONE developer-root exclusion path.
    With the physically bound live Store, enter its long [Retention.with_inventory]
    lease: this is the same Session_store developer-root gate, so the Handle's
    unbound collection gate must NOT be nested around it. For an unbound Store,
    hold the Handle's existing gate and prove the authority directory, sibling
    intent namespace and both developer Blob namespaces empty under [reader].
    A closed/stale binding or unbound nonempty evidence fails closed; an active
    but not yet bound root owner may defer with [Ok None]. The callback runs
    while the chosen gate is held, through graph traversal and deletion.
    The scoped [Developer_retention] supplies paths derived from the
    authenticated Data_root and this Blob_store's exact filesystem capability,
    not caller input. [checkpoint]
    charges bounded scan work; exception/cancellation identity is preserved. *)
val with_developer_retention
  :  retention
  -> session:Session_store.Handle.t
  -> reader:Retention_reader.t
  -> checkpoint:(unit -> unit)
  -> f:(Developer_retention.t -> ('a, Store_error.t) result)
  -> ('a option, Store_error.t) result

(** A cancellable read-only startup use of the SAME shared Blob coordinator and one
    Retention_reader allowance. The bound [runs] Store must physically issue
    [root]; recheck that fact and [sw] AFTER waiting for the coordinator and
    before exposing [f]. The reader is rooted at that Store's exact Data_root
    with this Blob_store's filesystem capability. [checkpoint] charges the
    enclosing work allowance before root/reader setup and after waits; its
    original exception and cancellation propagate. [f] may reborrow Store roots and
    inspect stages, but must not reenter public Blob_store operations or retain
    the reader/token afterward. [intent_directory] is the bound developer
    Store Data_root's authentic sibling result-intent directory, not a caller path.
    The two developer Blob namespace paths are likewise coordinator-owned;
    the callback inspects their presence through the same reader before scanning.
    [Ok None] defers when an upload/read/stage is
    active, as [with_retention] does. Unlike deletion, this read-only borrow
    does not protect cancellation through the callback: the coordinator releases
    safely on the caller's original exception. This serializes the inventory, not
    result publication or cross-owner deletion. *)
val with_developer_result_inventory
  :  t
  -> sw:Eio.Switch.t
  -> runs:Developer_run_store.t
  -> root:Developer_run_store.Result_root.t
  -> max_entries:int
  -> max_bytes:int
  -> checkpoint:(unit -> unit)
  -> f:
       (reader:Retention_reader.t
        -> intent_directory:string
        -> temporary_namespace:string
        -> durable_namespace:string
        -> ('a, Store_error.t) result)
  -> ('a option, Store_error.t) result

(** A separate finite owner-bound reader scope for ONE later recovery action.
    It verifies the same physical Blob binding/Result_root and charges all
    authoritative Store reads to one reader and [checkpoint] allowance.
    Unlike the global inventory it holds NO Blob coordinator, so [f] may call
    the existing public Blob staging engine. Each action has a fresh finite
    allowance, never a reset between its pre/post checks. Backend stage writes
    retain their own upload cap; a reader cannot claim to meter bytes it did
    not read. This callback must not reenter itself or retain the reader. *)
val with_developer_result_reader
  :  t
  -> sw:Eio.Switch.t
  -> runs:Developer_run_store.t
  -> root:Developer_run_store.Result_root.t
  -> max_entries:int
  -> max_bytes:int
  -> checkpoint:(unit -> unit)
  -> f:(Retention_reader.t -> ('a, Store_error.t) result)
  -> ('a, Store_error.t) result

(** The exact session root and shared temporary-blob root under a live retention
    token, for bounded reference readers. This does not authorize another session
    or any deletion. The caller must retain the scope throughout the scan. *)
val retention_directories
  :  retention
  -> Session_store.Handle.t
  -> (string * string, Store_error.t) result

(** The global durable root used only by the recognized developer namespace.
    This path alone proves no ownership; the collector needs a scoped
    [Developer_retention] and complete inventory before deletion. *)
val retention_reserved_directory : retention -> (string, Store_error.t) result

(** Remove an unreferenced host-owned stage under a live retention scope.
    The caller must first verify its durable private preparation intent and prove
    absence of all references/active owners. Preflights every temporary/final
    metadata, data, partial and matching atomic metadata temporary before mutation.
    Removes data before metadata, then syncs both directories. Missing files allow
    retries after partial cleanup. Never removes the private intent: its owner
    must keep that record until this operation has durably succeeded.
    The session-rooted reader supplies the shared scan/delete budgets. *)
val discard_staged_unreferenced
  :  retention
  -> reader:Retention_reader.t
  -> Session_store.Handle.t
  -> stage:Blob_stage_documents.t
  -> (unit, Store_error.t) result

(** The existing exact-handle discard under a live retention scope, without
    reentering the coordinator. The caller must prove absence of every reference.
    Rejects use after the callback has returned. *)
val discard_retained_unreferenced
  :  retention
  -> Session_store.Handle.t
  -> Handle.t
  -> (unit, Store_error.t) result

(** [begin_upload] creates an exclusive server-owned partial file. *)
val begin_upload
  :  t
  -> sw:Eio.Switch.t
  -> id:Agent_protocol.Id.Blob.t
  -> creating_principal:Agent_protocol.Id.Principal.t
  -> target_session:Agent_protocol.Id.Session.t option
  -> kind:Agent_protocol.Blob.kind
  -> media_type:string
  -> display_name:string option
  -> allowed_use:string
  -> created_at:Agent_protocol.Timestamp.t
  -> expires_at:Agent_protocol.Timestamp.t option
  -> (Upload.t, Store_error.t) result

(** [write_string] streams one chunk while enforcing the configured limit. *)
val write_string : Upload.t -> string -> (unit, Store_error.t) result

(** [finish] fsyncs and installs blob metadata. If [expected_digest] is
    supplied it must equal the computed lowercase SHA-256 digest. *)
val finish : Upload.t -> expected_digest:string option -> (Handle.t, Store_error.t) result

(** [abort] closes and removes an unfinished upload. It is idempotent. *)
val abort : Upload.t -> unit

val open_temporary : t -> Agent_protocol.Id.Blob.t -> (Handle.t, Store_error.t) result

(** [open_session] resolves an adopted blob beneath the exact typed session
    handle without accepting a client-controlled native path. *)
val open_session
  :  t
  -> Session_store.Handle.t
  -> Agent_protocol.Id.Blob.t
  -> (Handle.t, Store_error.t) result

val load : t -> Handle.t -> (string, Store_error.t) result
val max_upload_bytes : t -> int64

(** Bounded streaming load, checking actual length and SHA-256 against metadata
    before returning bytes. Fails on growth, truncation or changed contents. *)
val load_verified
  :  t
  -> sw:Eio.Switch.t
  -> Handle.t
  -> max_bytes:int
  -> (string, Store_error.t) result

(** [read_range] reads at most [max_bytes] starting at [offset] without
    loading the complete blob. The offset may equal the blob length to obtain
    an empty terminal chunk, but may not exceed it. *)
val read_range
  :  t
  -> sw:Eio.Switch.t
  -> Handle.t
  -> offset:int64
  -> max_bytes:int
  -> (string, Store_error.t) result

(** [iter_chunks] reads a blob through Eio and invokes [f] with bounded
    chunks. The callback must not retain or mutate internal storage state or
    reenter this store (including another facade sharing its coordinator). *)
val iter_chunks
  :  t
  -> sw:Eio.Switch.t
  -> Handle.t
  -> chunk_size:int
  -> f:(string -> unit)
  -> (unit, Store_error.t) result

(** [adopt] moves a temporary blob below its allowed typed session handle without
    overwriting an existing blob. A failed metadata save restores temporary data;
    an already durable handle can be reused only by its original session. *)
val adopt : t -> Session_store.Handle.t -> Handle.t -> (Handle.t, Store_error.t) result

(** Read complete staged bytes using metadata from a validated private intent.
    Checks session, regular paths, length, digest and read limit, even when blob
    metadata was not installed. Missing data or a shorter partial returns None.
    Does not repair, publish or delete anything. Serialize with the stage owner;
    the caller must validate the intent and subsequent adoption checks all files. *)
val load_staged_content
  :  t
  -> sw:Eio.Switch.t
  -> Session_store.Handle.t
  -> metadata:Metadata.t
  -> max_bytes:int
  -> (string option, Store_error.t) result

(** Idempotently complete a host-owned staged write using the same ID and bytes.
    Requires a durable private preparation intent and exclusive ownership from the
    caller; allowed_use is not ownership proof. Existing files must match the
    exact selected metadata publication/content bytes (partials must be prefixes). Refuses links
    and conflicting files before mutation. Repairs unpaired data/metadata left by
    interrupted writes and adoption, without rerunning the originating tool. *)
val ensure_staged_content
  :  t
  -> sw:Eio.Switch.t
  -> Session_store.Handle.t
  -> stage:Blob_stage_documents.t
  -> string
  -> (Handle.t, Store_error.t) result

(** Discard a host-owned artifact known not to be referenced by a committed
    transaction. Checks the exact session and unchanged metadata before removal;
    callers must establish absence of durable references. *)
val discard_unreferenced
  :  t
  -> Session_store.Handle.t
  -> Handle.t
  -> (unit, Store_error.t) result

(** [cleanup_expired] removes only expired metadata/data pairs below the
    configured temporary blob directory. A protected blob or failed protection
    check is retained; errors are returned to the maintenance coordinator. *)
val cleanup_expired
  :  ?protect:(Metadata.t -> (bool, Store_error.t) result)
  -> t
  -> now:Agent_protocol.Timestamp.t
  -> (int, Store_error.t) result
