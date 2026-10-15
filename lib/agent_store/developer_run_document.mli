(** The single versioned developer-run authority document. Version two stores
    bounded typed invocation, job, inference-attempt and command arrays, including exact
    owner/epoch/capture membership and structural parent relationships. Its
    records alone grant no live execution or result-read authority. *)
open! Core

type epoch
type t

module Invocation_entry : sig
  type t

  val revision : t -> int
  val value : t -> Agent_protocol.Host_invocation.t
end

(** Ordinals are positive, allocated once per run and never recycled. These
    coordinates are persisted comparison data; a pure document row cannot
    admit work without the Store's entered physical Claim. *)
module Inference_entry = Developer_inference_row

module Command_row = Developer_command_row

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
val inference_entries : t -> Inference_entry.t list
val inference_count : t -> int

(** Sum of complete canonical inference-entry JSON bytes, including ordinal,
    owner/job/context wrappers; not only Attempt_record payload bytes. *)
val inference_bytes : t -> int

(** Retained commands from every epoch. Keys and command IDs are unique across
    the entire stable principal/run; rows keep their original owner/capture. *)
val command_rows : t -> Command_row.t list

val find_command : t -> Agent_protocol.Idempotency_key.t -> Command_row.t option
val command_count : t -> int
val command_bytes : t -> int

(** Pure structural admission. Append requires an absent key/ID, exact stable
    owner and an existing matching epoch/capture. Replacement requires the
    current exact row and immutable identity; the canonical Row phase validator
    checks [change]. Physical live authority and historical settlement policy
    belong to Store, never to these comparison functions. *)
val append_command
  :  t
  -> Command_row.t
  -> commands:Developer_command_limits.t
  -> poll:(unit -> unit)
  -> (t, Store_error.t) result

val change_command
  :  t
  -> expected:Command_row.t
  -> change:Command_row.Change.t
  -> limits:Document_schema.Limits.t
  -> commands:Developer_command_limits.t
  -> poll:(unit -> unit)
  -> (t, Store_error.t) result

val last_inference_ordinal : t -> int64

(** The next positive ordinal, or None on ordinary capacity exhaustion; the
    Store maps that case to Limit_exceeded. A row may be retained after terminal
    and ordinals are never recycled. *)
val next_inference_ordinal : t -> int64 option

(** One canonical source qualification, independent of any Session ID. The
    validated host/run/epoch/job coordinates plus ordinal determine the exact
    transcript scope; the resulting IDs grant no Claim or dispatch authority. *)
val inference_scope
  :  t
  -> owner:Agent_protocol.Execution_owner.developer
  -> job_id:Agent_protocol.Id.Job.t
  -> ordinal:int64
  -> relation:Transcript.Scope.relation
  -> (Transcript.Scope.t, Store_error.t) result

(** These pure mutations validate complete Attempt_record admission and exact
    structural ancestry under the same bounded traversal as [to_bytes]. Every
    row references a positive historical attempt no greater than that
    job's latest recorded attempt; a later retry never invalidates prior
    inference evidence. A nested row references a preceding retained ordinal
    for the same immutable Model_call job and exact historical attempt. Parent
    inference may already be Terminal when a
    delayed admitted tool call starts a nested inference; the physical Store
    manager requires a genuinely issued retained parent registration and the
    current entered worker Claim for NEW effect admission, independently of
    the parent's provider-attempt state. Only the record changes on
    replacement: ordinal, owner, capture, job, attempt and context ID remain
    immutable. Every change advances the document revision. *)
val append_inference
  :  t
  -> poll:(unit -> unit)
  -> limits:Document_schema.Limits.t
  -> ordinal:int64
  -> owner:Agent_protocol.Execution_owner.developer
  -> capture_fingerprint:Chatml_source.Digest.t
  -> job_id:Agent_protocol.Id.Job.t
  -> job_attempt:int
  -> context_id:Inference.Observation.Observation_id.t
  -> record:Inference.Observation.Attempt_record.t
  -> max_inference_attempts:int
  -> max_inference_bytes:int
  -> (t, Store_error.t) result

val set_inference_record
  :  t
  -> poll:(unit -> unit)
  -> limits:Document_schema.Limits.t
  -> ordinal:int64
  -> expected:Inference_entry.t
  -> after:Inference.Observation.Attempt_record.t
  -> max_inference_bytes:int
  -> (t, Store_error.t) result

(** Model_call launch shapes accepted by canonical persistence: a top-level
    Run_root with no parent/depth, a claimed Model_call child with a Run_root
    parent-job and positive depth, or an admitted Invocation parent. The
    validator separately checks the claimed child's historical parent kind,
    exact attempt/depth and owner/capture/epoch relation. This predicate alone
    grants no admission authority. *)
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
    existing work and document limits. Every encoded document also reserves
    exact aggregate and whole-envelope capacity for ALL still Prepared/Running
    inference rows to become Host_interrupted together, including their
    document revision increments. This bounds every intermediate subset:
    each such interruption expands the fixed state object, never shrinks it.
    The same reserve includes interruption of ALL nonterminal command rows,
    with their original execution_started flag and combined revision increments.
    Command and inference headroom is checked together in one complete envelope,
    not as independent alternatives. Later caller-chosen job/command terminal
    results retain their own admission limits. [poll] is called during traversal by
    the owner of the operation. No canonical bytes are written or published. *)
val prepare_encode
  :  t
  -> poll:(unit -> unit)
  -> limits:Document_schema.Limits.t
  -> max_epochs:int
  -> max_jobs:int
  -> max_invocations:int
  -> max_job_payload_bytes:int
  -> max_inference_attempts:int
  -> max_inference_bytes:int
  -> commands:Developer_command_limits.t
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
  -> max_inference_attempts:int
  -> max_inference_bytes:int
  -> commands:Developer_command_limits.t
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
  -> max_inference_attempts:int
  -> max_inference_bytes:int
  -> commands:Developer_command_limits.t
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
  -> max_inference_attempts:int
  -> max_inference_bytes:int
  -> commands:Developer_command_limits.t
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
  -> max_inference_attempts:int
  -> max_inference_bytes:int
  -> commands:Developer_command_limits.t
  -> (t * Document_schema.Document.t, Store_error.t) result
