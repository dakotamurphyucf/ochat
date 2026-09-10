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
    completion contract before calling it. Selection retains the completion and
    reference before the first filesystem operation. Failed intent, upload,
    metadata or persistence writes retry the same stage. Matching partial data can
    be rebuilt; complete data is reused and conflicting files are rejected.
    This cache handles retries within this process, not startup recovery. *)
module Publisher : sig
  type t

  type collection_limits =
    { max_intents : int
    ; max_entries : int
    ; max_bytes : int
    ; max_file_bytes : int
    }

  type collection_stats =
    { discarded : int
    ; retired : int
    ; retained : int
    }
  [@@deriving sexp]

  (** Collect under an owning actor's quiescent checkpoint. The host callback
      validates all historical, replay, cache and other roots using the shared
      reader, then calls [f] while keeping those roots stable. [None] defers the
      attempt. Lock order is actor, publisher, response cache, blob storage.
      Pending completion dependencies and exact nonterminal attempts are roots.
      Only validated private intents grant ownership. All root proofs precede
      deletion; bounded reads and IO failures retain remaining records for retry.
      Successfully published final artifacts can retire their private markers. *)
  val collect
    :  t
    -> jobs:Agent_protocol.Job.t list
    -> generation:int
    -> limits:collection_limits
    -> with_roots:
         (reader:Retention_reader.t
          -> candidates:Agent_protocol.Id.Blob.t list
          -> f:
               (Agent_protocol.Id.Blob.t list
                -> (collection_stats option, Store_error.t) result)
          -> (collection_stats option, Store_error.t) result)
    -> (collection_stats option, Store_error.t) result

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

  (** The completion already selected for this still-current attempt, including
      failed intent/upload writes. Retrying a waiting parent must not recalculate
      a different outcome while the original publication is pending. *)
  val pending_completion
    :  t
    -> job:Agent_protocol.Job.t
    -> Agent_protocol.Completion.t option

  (** Reconstruct selections from complete, verified private preparations for
      current async job attempts. Missing/partial data is left for interruption;
      terminal or superseded attempts are ignored. Conflicting completions and
      count/aggregate byte excess fail before changing the cache. Does no writes
      or tool execution. Call before startup interruption, serialized with the
      actor; publish through its normal completion transition. The timestamp is
      when the completion was originally selected. *)
  val restore
    :  t
    -> jobs:Agent_protocol.Job.t list
    -> generation:int
    -> max_count:int
    -> max_total_bytes:int
    -> ( (Agent_protocol.Job.t * Agent_protocol.Completion.t * Agent_protocol.Timestamp.t)
           list
         , Agent_protocol.Error.t )
         result
end
