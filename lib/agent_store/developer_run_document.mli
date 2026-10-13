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
val job_count : t -> int

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

val to_bytes
  :  t
  -> poll:(unit -> unit)
  -> limits:Document_schema.Limits.t
  -> max_epochs:int
  -> max_jobs:int
  -> max_invocations:int
  -> max_job_payload_bytes:int
  -> (string, Store_error.t) result

val of_bytes
  :  string
  -> poll:(unit -> unit)
  -> limits:Document_schema.Limits.t
  -> max_epochs:int
  -> max_jobs:int
  -> max_invocations:int
  -> max_job_payload_bytes:int
  -> (t, Store_error.t) result
