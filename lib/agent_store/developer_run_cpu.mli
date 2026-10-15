(** Private CPU boundary for the existing developer-run document codec.
    Store owns this adapter and supplies the actual Domain_manager from its
    opened Eio environment. It grants no storage, execution or decoding
    authority to a caller; the canonical Document_schema and
    Developer_run_document remain the sole format and typed validators. *)
open! Core

type t

val create : domain_mgr:Eio.Domain_manager.ty Eio.Resource.t -> t

(** Decode the one canonical, bounded document. Check byte limits before any
    off-domain copy. Only an immutable copy of [bytes] and pure limits enter
    the Domain_manager; Store/Root/Eio handles, mutable physical state, and
    [poll] stay on the calling domain. The returning admitted envelope runs
    Developer_run_document.of_decoded_document under the same [poll].
    [poll] charges the Store's one work allowance and yields at its finite
    cadence. The small-input path uses the identical decoder and validator.
    Cancellation/exception never returns a partial document. *)
val decode
  :  t
  -> string
  -> poll:(unit -> unit)
  -> limits:Document_schema.Limits.t
  -> max_epochs:int
  -> max_jobs:int
  -> max_invocations:int
  -> max_job_payload_bytes:int
  -> max_inference_attempts:int
  -> max_inference_bytes:int
  -> commands:Developer_command_limits.t
  -> (Developer_run_document.t, Store_error.t) result

(** Same decode, also retaining the original admitted envelope for existing
    cross-owner retention scanning without reparse or reconstruction. *)
val decode_with_document
  :  t
  -> string
  -> poll:(unit -> unit)
  -> limits:Document_schema.Limits.t
  -> max_epochs:int
  -> max_jobs:int
  -> max_invocations:int
  -> max_job_payload_bytes:int
  -> max_inference_attempts:int
  -> max_inference_bytes:int
  -> commands:Developer_command_limits.t
  -> (Developer_run_document.t * Document_schema.Document.t, Store_error.t) result

(** Existing typed validation/payload work uses the caller's charged [poll]
    on the Store domain. Only its opaque immutable Encode_input then crosses
    into the Domain_manager for the sole canonical create+stringify path.
    A failed or cancelled encode returns no bytes or Prepared. No callback
    runs in the other domain and no extra yield occurs after a Store's final
    before-rename admission check. *)
val encode
  :  t
  -> Developer_run_document.t
  -> poll:(unit -> unit)
  -> limits:Document_schema.Limits.t
  -> max_epochs:int
  -> max_jobs:int
  -> max_invocations:int
  -> max_job_payload_bytes:int
  -> max_inference_attempts:int
  -> max_inference_bytes:int
  -> commands:Developer_command_limits.t
  -> (string, Store_error.t) result
