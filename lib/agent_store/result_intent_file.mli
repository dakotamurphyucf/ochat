(** Private framed result-selection file custody shared by conversation and
    developer publishers. This module owns the no-follow, bounded, exact-byte
    save/read/remove algorithm, not either document schema or Blob writer. A
    caller must decode the returned frame with its domain codec and verify
    embedded owner/job/reference fields before using it as comparison evidence. *)
open! Core

type t

(** The existing conversation path remains [session/result-preparations/<blob>.frame].
    The developer path is beneath the issuing Data_root's dedicated result-intent
    namespace as
    [developer-result-preparations/run_<run>/epoch_<epoch>/job_<job>-<attempt>.frame],
    keyed by typed run/epoch/job/attempt. The epoch is part of the
    canonical key even if a job ID happens to repeat in another historical
    epoch. Neither filename is accepted
    from an arbitrary string. A developer borrow rechecks the real Result_root
    and uses the issuing Store's filesystem capability. These values are storage
    locations, not job claims, terminal writers or result-read grants. *)
val conversation
  :  env:Eio_unix.Stdenv.base
  -> session:Session_store.Handle.t
  -> blob:Agent_protocol.Id.Blob.t
  -> t

val developer
  :  root:Developer_run_store.Result_root.t
  -> owner:Agent_protocol.Execution_owner.developer
  -> job:Agent_protocol.Id.Job.t
  -> attempt:int
  -> (t, Store_error.t) result

(** The one checksummed zero-flags Document_record framing path. Domain codecs
    choose their own versioned document and fixed schema limits; conversation
    continues to emit its existing v1 bytes. Exact owner/key field validation
    remains with each codec after decoding. *)
val encode
  :  Document_schema.Document.t
  -> limits:Document_schema.Limits.t
  -> (string, Store_error.t) result

val decode
  :  string
  -> limits:Document_schema.Limits.t
  -> max_frame_bytes:int
  -> (Document_schema.Document.t, Store_error.t) result

(** Validate one complete frame, size and zero flags before any write. Existing
    regular files must contain exactly the same bytes; a mismatch, linked path
    or partial frame fails closed. Save creates/syncs only the selected private
    namespace and uses the existing Durable_file atomic replacement. A failed
    acknowledgement cannot prove the file absent. [max_frame_bytes] must be
    positive and at most the fixed internal 128 KiB ceiling; domain codecs
    impose their smaller limits independently. *)
val save_exact : t -> max_frame_bytes:int -> string -> (unit, Store_error.t) result

(** [None] means the exact canonical filename is absent after an owner borrow.
    Invalid names, linked directories/files, oversized/incomplete frames and
    uncertain reads are errors, never absence. This operation does not enumerate
    the directory or assert that other intent files do not exist; full bounded
    startup inventory belongs to the recovery owner. *)
val read_exact : t -> max_frame_bytes:int -> (string option, Store_error.t) result

(** Remove/sync only an exact matching canonical framed file. Missing is
    idempotent, but differing bytes and uncertain writes retain custody. The
    publisher calls this only after actual Store installation or proved
    never-attempted discard; this function cannot make that proof itself. *)
val remove_exact
  :  t
  -> expected:string
  -> max_frame_bytes:int
  -> (unit, Store_error.t) result
