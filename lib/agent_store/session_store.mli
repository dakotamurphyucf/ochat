(** High-level ownership of the daemon data root and durable session paths. *)

module Metadata = Session_metadata

module Initial_projection : sig
  (** Validated coherent metadata and complete scheduling projection selected by
      the initializer from the same state. No implicit zero hints for journals. *)
  type t

  val create
    :  metadata:Metadata.t
    -> entry:Session_index.Entry.t
    -> (t, Store_error.t) result

  val metadata : t -> Metadata.t
  val entry : t -> Session_index.Entry.t
end

module Handle : sig
  type t

  (** Last validated observation. Use [metadata_checked] for current authority. *)
  val metadata : t -> Metadata.t

  (** Reject when an uncertain write could not be refreshed from disk. *)
  val metadata_checked : t -> (Metadata.t, Store_error.t) result

  val session_id : t -> Agent_protocol.Id.Session.t
  val directory : t -> string
  val snapshot_directory : t -> string
  val journal_directory : t -> string
  val cache_directory : t -> string
  val workspace_directory : t -> string
  val responses_directory : t -> string
  val audit_directory : t -> string
  val exports_directory : t -> string
  val archive_directory : t -> string
  val idempotency_directory : t -> string
end

type t

(** Root envelope version; independent of session side metadata and state versions. *)
val current_schema_version : int

val current_metadata_schema_version : int
val data_root : t -> Data_root.t
val server_id : t -> Agent_protocol.Id.Server.t
val session_index : t -> Session_index.t

(** Shared private child-creation ledger under this store's exclusive ownership. *)
val delegations : t -> Delegation_store.t

(** Host organization under this root's exclusive ownership; closed with store. *)
val organizations : t -> Organization_store.t

(** [index_was_rebuilt t] reports that a missing index was reconstructed and
    eager recovery remains required. Scheduling hints are unknown: the daemon
    must load every non-archived entry and recover its journal before scheduling
    work. A durable marker preserves this requirement across interrupted
    recovery attempts, even after the rebuilt index was installed. *)
val index_was_rebuilt : t -> bool

(** [complete_index_recovery t] durably clears the eager-recovery requirement.
    Call only after all non-archived sessions have been hydrated and their
    job/schedule/owner recovery transitions and accurate index hints persisted.
    A failed or interrupted daemon startup must leave the marker intact. *)
val complete_index_recovery : t -> (unit, Store_error.t) result

(** [is_closed t] reports whether [close] released the daemon-store lock. *)
val is_closed : t -> bool

(** [check_writable t] creates and removes an exclusive Eio-owned probe file
    beneath the data root. *)
val check_writable : t -> (unit, Store_error.t) result

(** [create] initializes a new store and holds its daemon lock until [close]. *)
val create
  :  env:Eio_unix.Stdenv.base
  -> sw:Eio.Switch.t
  -> root:string
  -> server_id:Agent_protocol.Id.Server.t
  -> process_start_identity:string option
  -> lock_nonce:string
  -> (t, Store_error.t) result

(** [open_existing] validates schema/server identity and acquires ownership.
    Rebuild a missing index atomically from valid [sessions/ses_*] layouts.
    Ignore staging directories and deleted tombstones outside that namespace;
    never follow session/layout symlinks. Invalid active metadata/layout and
    existing corrupt indexes fail closed without publishing an empty index.
    Existing archive markers own lifecycle authority. A cached archived/newer
    lifecycle entry without its marker fails closed; it cannot recreate authority. *)
val open_existing
  :  env:Eio_unix.Stdenv.base
  -> sw:Eio.Switch.t
  -> root:string
  -> process_start_identity:string option
  -> lock_nonce:string
  -> (t, Store_error.t) result

val close : t -> (unit, Store_error.t) result

(** [create_session] atomically installs a complete session directory, then
    acquires its actor lock. *)
val create_session
  :  t
  -> sw:Eio.Switch.t
  -> transaction_id:Agent_protocol.Id.Transaction.t
  -> actor_lock_nonce:string
  -> Metadata.t
  -> (Handle.t, Store_error.t) result

(** [create_session_initialized] creates the private staging layout first and
    lets [initialize] derive metadata from paths inside that layout before the
    directory is atomically installed. Its full validated scheduling projection
    is published in that same recoverable bracket after sessions-parent sync.
    Private staged journals remain unreachable until this installation. *)
val create_session_initialized
  :  t
  -> sw:Eio.Switch.t
  -> transaction_id:Agent_protocol.Id.Transaction.t
  -> actor_lock_nonce:string
  -> initialize:(staging_directory:string -> (Initial_projection.t, Store_error.t) result)
  -> (Handle.t, Store_error.t) result

(** [open_session] validates metadata identity and acquires the actor lock. *)
val open_session
  :  t
  -> sw:Eio.Switch.t
  -> actor_lock_nonce:string
  -> Agent_protocol.Id.Session.t
  -> (Handle.t, Store_error.t) result

(** Prevalidate the full metadata/index carriers under the existing index locks
    without authoritative effects, then durably mark the
    existing projection owner before authoritative archive/journal I/O. Handle's
    serial actor owns one token across commits; newer targets advance it. A failed
    or uncertain authority acknowledgement retains recovery. Only matching full
    metadata/index publication retires it; stale summaries/hints cannot do so. *)
val prepare_canonical_projection
  :  t
  -> Handle.t
  -> metadata:Metadata.t
  -> entry:Session_index.Entry.t
  -> (unit, Store_error.t) result

(** Publish metadata and the supplied full scheduling projection under one
    recoverable intent. Summary must match metadata; existing archive authority
    survives. Without [entry], previously validated scheduling hints are retained. *)
val write_metadata
  :  ?entry:Session_index.Entry.t
  -> t
  -> Handle.t
  -> Metadata.t
  -> (unit, Store_error.t) result

val close_session : t -> Handle.t -> (unit, Store_error.t) result

(** Read the durable identity-bearing archive marker of an opened session,
    independently of the reconstructable index. *)
val is_archived : t -> Handle.t -> (bool, Store_error.t) result

(** [archive_session] hides a closed session from normal recovery while
    retaining its complete durable directory. Persist an identity-bearing
    [ARCHIVED] marker before updating the reconstructable index, so subsequent
    index loss cannot resurrect an archived session. *)
val archive_session : t -> Agent_protocol.Id.Session.t -> (unit, Store_error.t) result

(** [remove_session] atomically moves a closed session out of the active
    namespace, removes its index entry, and then deletes the tombstone using
    Eio filesystem operations. *)
val remove_session : t -> Agent_protocol.Id.Session.t -> (unit, Store_error.t) result

(** [prune_response_artifacts] recursively removes regular response-artifact
    files whose modification time is no newer than [older_than]. It never
    follows or removes symbolic links and leaves directory structure intact. *)
val prune_response_artifacts
  :  t
  -> protected:Agent_protocol.Id.Session.t list
  -> older_than:Agent_protocol.Timestamp.t
  -> (int, Store_error.t) result

(** Last validated projection observation. Authoritative recovery, retention
    and lifecycle consumers use [list_sessions_checked]. *)
val list_sessions : t -> Session_index.Entry.t list

val list_sessions_checked : t -> (Session_index.Entry.t list, Store_error.t) result

module Lifecycle : sig
  module R = Session_archive_record

  module Observation : sig
    (** Admitted carrier/absence bound to retained Handle ownership and exact
        document observation. Equal lifecycle revision alone is not a CAS stamp. *)
    type t

    val value : t -> R.t
  end

  module Prepared : sig
    (** Exact preserved next bytes + full validated index target, derived from
        one actor-fenced canonical summary/complete scheduling projection. *)
    type t

    val outcome : t -> R.Outcome.t
  end

  module Installed : sig
    (** Acknowledged authority AND checked exact full index projection. Replays
        whose outcome is older than current state do not construct this witness.
        Caller may promote an OCH161 reservation without yielding under the same
        target fence only if current generation/revision still match. *)
    type t

    val outcome : t -> R.Outcome.t
    val entry : t -> Session_index.Entry.t
    val is_current : t -> Handle.t -> bool
  end

  module Removal : sig
    (** Logical absence owns terminal proof while runtime retirement and physical
        cleanup remain pending. This capability is bound to one store/Handle. *)
    type t

    val outcome : t -> R.Outcome.t option
    val handle : t -> Handle.t option
  end
end

(** Pure capability identity, including retired/closed Handles. Does not assert
    availability or grant IO permission; retained cleanup uses it to bind owner. *)
val owns_handle : t -> Handle.t -> bool

val read_lifecycle : t -> Handle.t -> (Lifecycle.Observation.t, Store_error.t) Result.t

val prepare_lifecycle
  :  t
  -> Handle.t
  -> Lifecycle.Observation.t
  -> current_entry:Session_index.Entry.t
  -> transition:Session_archive_record.Prepared.t
  -> now:Agent_protocol.Timestamp.t
  -> (Lifecycle.Prepared.t, Store_error.t) Result.t

val publish_lifecycle
  :  t
  -> Handle.t
  -> Lifecycle.Prepared.t
  -> (Lifecycle.Installed.t, Store_error.t) Result.t

(** Reconcile the exact original key/digest outcome into the owner's generic
    durable result store before acknowledging its proof. [complete] runs outside
    projection/index mutexes while the caller retains its Handle and target fence.
    A callback failure leaves proof unacknowledged. Terminal Remove proof remains
    pinned even after acknowledgement, until physical cleanup. *)
val complete_lifecycle_outcome
  :  t
  -> Handle.t
  -> key:Idempotency_store.Key.t
  -> request_digest:string
  -> complete:(Session_archive_record.Outcome.t -> (unit, Store_error.t) Result.t)
  -> (unit, Store_error.t) Result.t

(** Publish terminal lifecycle proof and exact catalog absence. The caller owns
    the target fence and independent retention authorization throughout. *)
val begin_removal
  :  t
  -> Handle.t
  -> Lifecycle.Prepared.t
  -> (Lifecycle.Removal.t, Store_error.t) Result.t

(** Reconcile exact original receipts in the existing retained host result owner,
    retire the owned runtime/Handle, then transfer the one terminal marker to a
    stable tombstone root before payload deletion. Callbacks run outside index/
    projection locks. The caller retains its per-ID reservation throughout.
    Empty startup containers have neither Handle nor lifecycle outcome. *)
val finish_removal
  :  t
  -> Lifecycle.Removal.t
  -> complete:(Session_archive_record.Receipt.t -> (unit, Store_error.t) Result.t)
  -> retire:(Handle.t -> (unit, Store_error.t) Result.t)
  -> (unit, Store_error.t) Result.t

(** Terminal sources and recognized cleanup containers. No runtime activation. *)
val pending_removal_ids : t -> (Agent_protocol.Id.Session.t list, Store_error.t) Result.t

(** Acquire terminal source ownership, or admit a retired cleanup container.
    Never manufacture a Handle from a partially removed payload. *)
val open_removal
  :  t
  -> sw:Eio.Switch.t
  -> actor_lock_nonce:string
  -> Agent_protocol.Id.Session.t
  -> (Lifecycle.Removal.t, Store_error.t) Result.t
