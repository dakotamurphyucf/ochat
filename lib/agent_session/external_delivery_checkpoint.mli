(** Pure facts for an actual external delivery. The finite Actor owns the exact
    retained producer, Manager/source, original custody and SAME transaction ACK.
    No value here grants admission or reconstructs a process-local loan. *)
module Producer : sig
  type t =
    | Job_completion of Agent_protocol.Job.t
    | Run_job_completion of Run_job_delivery.t
    | Owned_schedule of Agent_protocol.Schedule.t
    | Ingress of
        { registration : External_ingress.t
        ; receipt : External_ingress.receipt
        }
end

module Snapshot : sig
  type t =
    | Known of Session.Moderator_state.Identity_snapshot.t
    | Encoded of Jsonaf.t
end

type t

(** Validate the complete original carrier and compare all known checkpoint
    fields with [before]. Derive exactly one appended payload from [producer],
    preserving the entire known prefix, source, state, requests and halt state.
    Advance Keep once under the actual full installation and finite bounds.
    Historical prefix custody remains unavailable.

    [Encoded] accepts the exact bare encoding supplied by Manager APIs, or an
    explicitly complete incoming carrier. The latter must already preserve raw
    extensions and contain exactly the planned metadata; it is never repaired
    from the previous carrier. Missing Schedule provenance/ingress inference
    originals are not replaced with admitted None or a principal-derived Run.
    Captured ingress provenance instead yields its exact receipt producer and
    original inference attribution, with full installation equality.

    [model_completion_format] selects only the exact Model payload projection.
    The Actor supplies it from its actual installed Manager or verified Goal
    runtime scope. The compatibility default is Legacy; formatting DATA cannot
    authorize delivery or replace any producer/source/attempt/custody check. *)
val prepare
  :  ?model_completion_format:Model_job_event.Format.t
  -> Session_state.t
  -> producer:Producer.t
  -> before:Session.Moderator_state.Identity_snapshot.t
  -> snapshot:Snapshot.t
  -> (t, Agent_protocol.Error.t) result

val checkpoint : t -> Jsonaf.t
val source : t -> Agent_protocol.Run_source.t
val metadata : t -> Moderator_queue.t
val entry : t -> Moderator_queue.Entry.t

(** Only an actually absent initial source installation needs this projection.
    Matching installation returns None, avoiding replacement of current Run/job
    changes by a whole earlier index. This is DATA, not source authorization. *)
val initial_source_index : t -> Run_state.t option
