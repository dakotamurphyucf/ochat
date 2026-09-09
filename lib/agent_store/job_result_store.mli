(** Session-owned storage for already validated terminal completions. This service
    does not authorize a job, change its outcome, run tools, or advertise a feature.
    The host must retain exact job-attempt authority and apply result schemas and
    disclosure before preparation. *)
type prepared

val prepare
  :  Blob_store.t
  -> env:Eio_unix.Stdenv.base
  -> sw:Eio.Switch.t
  -> session:Session_store.Handle.t
  -> job:Agent_protocol.Job.t
  -> creating_principal:Agent_protocol.Id.Principal.t
  -> now:Agent_protocol.Timestamp.t
  -> max_bytes:int
  -> Agent_protocol.Completion.t
  -> (prepared, Store_error.t) result

val reference : prepared -> Agent_protocol.Job_artifact.t

(** Serialize commit/discard and protect the persistence acknowledgement from
    cancellation. A failed save leaves the same artifact prepared for retry;
    success retains it and prevents discard. A checksummed intent precedes artifact
    bytes and remains until acknowledged publication or completed discard; failed
    intent cleanup cannot turn an acknowledged publication into failure.
    Once any save is attempted, discard
    requires separate durable reference reconciliation, even when the callback
    reports failure: a failed acknowledgement is not proof nothing persisted.
    The callback must not reenter this
    preparation and must persist the supplied reference under its owning job. *)
val commit
  :  prepared
  -> persist:(Agent_protocol.Job_artifact.t -> ('a, Agent_protocol.Error.t) result)
  -> ('a, Agent_protocol.Error.t) result

(** Delete only a preparation for which persistence was never attempted. Idempotent
    after successful discard; attempted/retained artifacts cannot be discarded
    through this token. Recovery may separately remove proved-unreferenced blobs. *)
val discard : prepared -> (unit, Store_error.t) result

(** Bounded read of the exact session/job/attempt reference, verifying metadata,
    actual byte length and digest before decoding any completion. *)
val load
  :  Blob_store.t
  -> sw:Eio.Switch.t
  -> session:Session_store.Handle.t
  -> max_bytes:int
  -> Agent_protocol.Job_artifact.t
  -> (Agent_protocol.Completion.t, Store_error.t) result

(** Per-session publication service. The actor must validate the live attempt and
    completion contract before calling it. Large completions are written once;
    failed persistence retries reuse their exact prepared reference. *)
module Publisher : sig
  type t

  val create
    :  env:Eio_unix.Stdenv.base
    -> blobs:Blob_store.t
    -> sw:Eio.Switch.t
    -> session:Session_store.Handle.t
    -> principal:Agent_protocol.Id.Principal.t
    -> inline_bytes:int
    -> max_bytes:int
    -> (t, Agent_protocol.Error.t) result

  (** [jobs] is the current authoritative session state. Stale preparations are
      evicted from memory, retaining any potentially referenced artifact for
      separate durable orphan reconciliation. The persistence callback must not
      reenter this publisher. *)
  val publish
    :  t
    -> jobs:Agent_protocol.Job.t list
    -> job:Agent_protocol.Job.t
    -> now:Agent_protocol.Timestamp.t
    -> Agent_protocol.Completion.t
    -> persist:(Agent_protocol.Stored_completion.t -> ('a, Agent_protocol.Error.t) result)
    -> ('a, Agent_protocol.Error.t) result

  val load
    :  t
    -> Agent_protocol.Job_artifact.t
    -> (Agent_protocol.Completion.t, Agent_protocol.Error.t) result

  (** Check the result ceiling before publication. A host can record a small
      control failure directly if the business completion cannot be stored. *)
  val check_completion
    :  t
    -> Agent_protocol.Completion.t
    -> (unit, Agent_protocol.Error.t) result
end
