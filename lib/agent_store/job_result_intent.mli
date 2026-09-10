(** Checksummed, session-owned preparation intent written before any artifact
    bytes. Only this private store namespace establishes managed preparation
    ownership; an upload's caller-supplied allowed_use string is not such proof. *)
type t

val reference : t -> Agent_protocol.Job_artifact.t
val metadata : t -> Blob_store.Metadata.t

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

(** Idempotent intent removal after acknowledged publication or completed
    unreferenced cleanup. Serialize with the owning actor/preparation. Never call
    merely because an acknowledgement failed. *)
val remove
  :  env:Eio_unix.Stdenv.base
  -> session:Session_store.Handle.t
  -> t
  -> (unit, Store_error.t) result
