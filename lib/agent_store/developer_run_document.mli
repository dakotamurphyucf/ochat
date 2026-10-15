(** The single versioned developer-run authority document. Version one stores
    bounded typed invocation and job arrays, including exact owner/epoch/capture
    membership and structural parent relationships. Its records alone grant no
    live execution or result-read authority. *)
open! Core

type epoch
type t

module Invocation_entry : sig
  type t

  val revision : t -> int
  val value : t -> Agent_protocol.Host_invocation.t
end

val create
  :  owner:Agent_protocol.Execution_owner.developer
  -> capture_fingerprint:Chatml_source.Digest.t
  -> t

val host_id : t -> Agent_protocol.Id.Server.t
val run_id : t -> Agent_protocol.Id.Developer_run.t
val principal_id : t -> Agent_protocol.Id.Principal.t
val revision : t -> int64
val epochs : t -> epoch list
val epoch_number : epoch -> int
val epoch_capture : epoch -> Chatml_source.Digest.t
val epoch_phase : epoch -> [ `Active | `Interrupted | `Closed ]
val epoch_revision : epoch -> int64
val find_invocation : t -> Agent_protocol.Id.Invocation.t -> Invocation_entry.t option
val invocation_count : t -> int
val find_job : t -> Agent_protocol.Id.Job.t -> Agent_protocol.Developer_job.t option
val jobs : t -> Agent_protocol.Developer_job.t list
val job_count : t -> int

(** A Model_call from the physical run root has no job ancestor and depth zero.
    Invocation descendants carry comparison coordinates; their actual claimed
    attempt and depth are proved by the host, not by this document predicate. *)
val valid_external_model_launch_shape : Agent_protocol.Developer_job.Launch.t -> bool

(** Pure validated mutations. Each call advances the document revision; callers
    may compose these before one atomic file replacement. Exact transition
    authority remains in Developer_run_store. *)
val set_invocation
  :  t
  -> poll:(unit -> unit)
  -> id:Agent_protocol.Id.Invocation.t
  -> expected_revision:int option
  -> after:Agent_protocol.Host_invocation.t
  -> max_invocations:int
  -> (t, Store_error.t) result

val append_selected
  :  t
  -> poll:(unit -> unit)
  -> limits:Document_schema.Limits.t
  -> selected:Agent_protocol.Developer_job.t list
  -> max_jobs:int
  -> max_selected:int
  -> max_job_payload_bytes:int
  -> (t, Store_error.t) result

(** Replace only the state of an existing exact job/attempt, preserving every
    immutable field and advancing the document revision. The owning Store
    decides whether the lifecycle transition is authorized. *)
val set_job_state
  :  t
  -> poll:(unit -> unit)
  -> id:Agent_protocol.Id.Job.t
  -> expected_attempt:int
  -> after:Agent_protocol.Developer_job.State.t
  -> (t, Store_error.t) result

val append_epoch
  :  t
  -> owner:Agent_protocol.Execution_owner.developer
  -> capture_fingerprint:Chatml_source.Digest.t
  -> max_epochs:int
  -> (t, Store_error.t) result

val set_phase
  :  t
  -> epoch:int
  -> phase:[ `Active | `Interrupted | `Closed ]
  -> (t, Store_error.t) result

(** Immutable, bounded canonical-serialization input. It is pure comparison
    data, not a Store Prepared, document observation or write authority. *)
module Encode_input : sig
  type t
end

(** Validate the typed document and build its complete JSON payload under the
    existing work and document limits. [poll] is called during traversal by
    the owner of the operation. No canonical bytes are written or published. *)
val prepare_encode
  :  t
  -> poll:(unit -> unit)
  -> limits:Document_schema.Limits.t
  -> max_epochs:int
  -> max_jobs:int
  -> max_invocations:int
  -> max_job_payload_bytes:int
  -> (Encode_input.t, Store_error.t) result

(** The original Document_schema.Document.create and to_string on the opaque
    prepared input. Pure and callback-free; exactly the same envelope, size,
    field and duplicate-key admission as [to_bytes]. No alternate encoder. *)
val finish_encode : Encode_input.t -> (string, Store_error.t) result

(** The direct pure wrapper over [prepare_encode] and [finish_encode]. Existing
    callers outside the Store can use it without an Eio runtime. *)
val to_bytes
  :  t
  -> poll:(unit -> unit)
  -> limits:Document_schema.Limits.t
  -> max_epochs:int
  -> max_jobs:int
  -> max_invocations:int
  -> max_job_payload_bytes:int
  -> (string, Store_error.t) result

(** Typed/version/relationship admission of an already canonically decoded
    Document_schema.Document.t. Revalidate the envelope against these exact
    [limits], then perform the sole typed validator also used by the direct
    wrappers below; no caller-supplied JSON or unchecked DTO is admitted.
    A decoder result admitted under these same limits proves that comparison
    with four scalar bounds, without another JSON traversal. Stricter limits
    retain the canonical full revalidation behavior.
    Pure except for its supplied [poll]. *)
val of_decoded_document
  :  Document_schema.Document.t
  -> poll:(unit -> unit)
  -> limits:Document_schema.Limits.t
  -> max_epochs:int
  -> max_jobs:int
  -> max_invocations:int
  -> max_job_payload_bytes:int
  -> (t, Store_error.t) result

(** Direct pure wrapper over canonical Document_schema.Document.decode and
    [of_decoded_document]; unknown fields/versions and limits fail closed. *)
val of_bytes
  :  string
  -> poll:(unit -> unit)
  -> limits:Document_schema.Limits.t
  -> max_epochs:int
  -> max_jobs:int
  -> max_invocations:int
  -> max_job_payload_bytes:int
  -> (t, Store_error.t) result

(** The same bounded decoder also returning its original admitted document
    tree. Cross-owner retention scans decoded strings, including escaped
    envelope/payload values, without a second parse or lossy reconstruction
    from known typed fields. [of_bytes] keeps its existing behavior by
    projecting this result's [t]. *)
val of_bytes_with_document
  :  string
  -> poll:(unit -> unit)
  -> limits:Document_schema.Limits.t
  -> max_epochs:int
  -> max_jobs:int
  -> max_invocations:int
  -> max_job_payload_bytes:int
  -> (t * Document_schema.Document.t, Store_error.t) result
