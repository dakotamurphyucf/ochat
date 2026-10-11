open Core

(** One local package capture inside the host's already-admitted attempt.
    The descriptor owner remains borrowed until return. This function does not
    resolve dependency locators, follow dependencies or publish a lock. *)
val capture
  :  Project_prepare_attempt.t
  -> owner:Chatml_local_source.t
  -> project:Source.Portable_path.t
  -> namespace_override:string option
  -> selection:Chatml_project.Selection.request
  -> (Chatml_local_capture.t, Project_prepare_error.t) Result.t

(** Source/entry capacity is reserved before reading the declaration. Bootstrap
    validates the authored namespace even when an override is supplied, then
    captures its exact bytes once under the effective identity. Full declaration
    decoding and catalog capture reuse that source; no reread or provisional
    source identity is used. The final inventory counts the declaration once.

    Bootstrap work charges the native worst-case read allowance and two bounded
    JSON byte passes plus capture hashing/index passes. Catalog work retains the
    existing local backend's reservation model and literal policy ceiling. These
    are documented admission units, not CPU/syscall measurements. Outstanding
    reservations make a failed or cancelled capture ineligible for completion.
    Exceptions from cancellation/checkpoints propagate unchanged. *)
