(** Session-owned storage for already validated terminal completions. This service
    does not authorize a job, change its outcome, run tools, or advertise a feature.
    The host must retain exact job-attempt authority and apply result schemas and
    disclosure before preparation. *)
type prepared

val prepare
  :  Blob_store.t
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
    success retains it and prevents discard. Once any save is attempted, discard
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
