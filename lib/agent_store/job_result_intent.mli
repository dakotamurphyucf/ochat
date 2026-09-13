(** Checksummed, session-owned preparation intent written before any artifact
    bytes. Only this private store namespace establishes managed preparation
    ownership; an upload's caller-supplied allowed_use string is not such proof. *)
type t

val reference : t -> Agent_protocol.Job_artifact.t
val metadata : t -> Blob_store.Metadata.t

(** Protect a temporary blob from generic expiry only when its exact metadata
    matches a validated private intent in the owned data root. Labels alone do not
    protect uploads. Corrupt records, identity mismatches or linked directories
    return an error so the caller retains the files. No session lock is acquired:
    the intent precedes upload and outlives its stage, allowing maintenance to
    inspect it without reentering an actor or racing the publication lock. *)
val protects_temporary
  :  env:Eio_unix.Stdenv.base
  -> data_root:Data_root.t
  -> Blob_store.Metadata.t
  -> (bool, Store_error.t) result

(** Allocate a validated identity without IO, so a caller can retain it before
    the first possibly ambiguous filesystem operation. *)
val make
  :  session:Session_store.Handle.t
  -> reference:Agent_protocol.Job_artifact.t
  -> metadata:Blob_store.Metadata.t
  -> (t, Store_error.t) result

(** Durably save the same intent on retries. Refuses different/corrupt existing
    records and links; reestablishes durability after an ambiguous acknowledgement. *)
val save
  :  env:Eio_unix.Stdenv.base
  -> session:Session_store.Handle.t
  -> t
  -> (unit, Store_error.t) result

val create
  :  env:Eio_unix.Stdenv.base
  -> session:Session_store.Handle.t
  -> reference:Agent_protocol.Job_artifact.t
  -> metadata:Blob_store.Metadata.t
  -> (t, Store_error.t) result

(** Validate every intent, rejecting corruption, symlinks, identity mismatches and
    excessive counts before returning any candidate. Missing directories are empty. *)
val list
  :  env:Eio_unix.Stdenv.base
  -> session:Session_store.Handle.t
  -> max_count:int
  -> (t list, Store_error.t) result

(** The same validated scan using an existing reader rooted at the exact session,
    sharing the collection attempt's entry/byte budgets. Unknown entries and
    linked paths fail; recognized atomic-write temporary files are not published
    intents and still consume enumeration budget. Missing intent directories are
    empty. The caller must serialize with the publisher. *)
val list_with_reader
  :  reader:Retention_reader.t
  -> session:Session_store.Handle.t
  -> max_count:int
  -> (t list, Store_error.t) result

(** Idempotent intent removal after acknowledged publication or completed
    unreferenced cleanup. Serialize with the owning actor/preparation. Never call
    merely because an acknowledgement failed. *)
val remove
  :  env:Eio_unix.Stdenv.base
  -> session:Session_store.Handle.t
  -> t
  -> (unit, Store_error.t) result

(** Verify this exact durable intent and matching atomic-write temporaries,
    remove/sync its unreferenced staged files under the blob retention scope,
    then reverify and remove/sync intent temporaries and the intent last.
    Failed staged-file removal or sync leaves the ownership record for retry.
    An error acknowledging the final intent removal may occur after cleanup has
    completed; fresh enumeration determines what remains.
    A caller must establish all retained roots and exclude active owners
    before invoking this operation; it does not calculate reference absence.
    Uses the shared session-rooted reader for all preflight/reverification reads. *)
val discard_unreferenced
  :  env:Eio_unix.Stdenv.base
  -> scope:Blob_store.retention
  -> reader:Retention_reader.t
  -> session:Session_store.Handle.t
  -> t
  -> (unit, Store_error.t) result

(** Remove the exact private marker and matching atomic residue without touching
    artifact data. Caller must prove the current durable job has this terminal
    reference and its final data/metadata are verified under storage coordination.
    Uses the attempt's shared reader and syncs the marker directory. *)
val retire_published
  :  env:Eio_unix.Stdenv.base
  -> reader:Retention_reader.t
  -> session:Session_store.Handle.t
  -> t
  -> (unit, Store_error.t) result
