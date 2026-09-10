(** Eio streaming storage for temporary uploads and session-owned blobs. *)

module Metadata : sig
  type t =
    { blob : Agent_protocol.Blob.Metadata.t
    ; creating_principal : Agent_protocol.Id.Principal.t
    ; target_session : Agent_protocol.Id.Session.t option
    ; allowed_use : string
    ; created_at : Agent_protocol.Timestamp.t
    ; expires_at : Agent_protocol.Timestamp.t option
    ; durable : bool
    }
  [@@deriving sexp]
end

module Handle : sig
  type t

  val metadata : t -> Metadata.t
end

module Upload : sig
  type t
end

type t

val create
  :  env:Eio_unix.Stdenv.base
  -> temporary_directory:string
  -> durable_directory:string
  -> max_upload_bytes:int64
  -> (t, Store_error.t) result

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
    chunks. The callback must not retain or mutate internal storage state. *)
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
    expected canonical metadata/content (partials must be prefixes). Refuses links
    and conflicting files before mutation. Repairs unpaired data/metadata left by
    interrupted writes and adoption, without rerunning the originating tool. *)
val ensure_staged_content
  :  t
  -> sw:Eio.Switch.t
  -> Session_store.Handle.t
  -> metadata:Metadata.t
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
