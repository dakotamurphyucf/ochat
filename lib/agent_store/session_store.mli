(** High-level ownership of the daemon data root and durable session paths. *)

module Metadata : sig
  type t =
    { schema_version : int
    ; session : Agent_protocol.Session.t
    ; prompt_artifact : string
    ; workspace_identity : string
    ; data_schema_version : int
    }
  [@@deriving sexp]

  (** [data_schema_version] belongs to the session-state codec, independently
      of the store/metadata schema. The store requires a positive value;
      eager session hydration validates compatibility using that codec. *)
end

module Handle : sig
  type t

  val metadata : t -> Metadata.t
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

val current_schema_version : int
val data_root : t -> Data_root.t
val server_id : t -> Agent_protocol.Id.Server.t
val session_index : t -> Session_index.t

(** Shared private child-creation ledger under this store's exclusive ownership. *)
val delegations : t -> Delegation_store.t

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
    Backfill durable archive markers from a valid older index. Archive flags
    already lost with a pre-marker index cannot be reconstructed. *)
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
    directory is atomically installed. *)
val create_session_initialized
  :  t
  -> sw:Eio.Switch.t
  -> transaction_id:Agent_protocol.Id.Transaction.t
  -> actor_lock_nonce:string
  -> initialize:(staging_directory:string -> (Metadata.t, Store_error.t) result)
  -> (Handle.t, Store_error.t) result

(** [open_session] validates metadata identity and acquires the actor lock. *)
val open_session
  :  t
  -> sw:Eio.Switch.t
  -> actor_lock_nonce:string
  -> Agent_protocol.Id.Session.t
  -> (Handle.t, Store_error.t) result

val write_metadata : t -> Handle.t -> Metadata.t -> (unit, Store_error.t) result
val close_session : t -> Handle.t -> (unit, Store_error.t) result

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

val list_sessions : t -> Session_index.Entry.t list
