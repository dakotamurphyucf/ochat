(** Pure, bounded transformations of the one developer-run authority document.
    These validate durable record shape and exact observed revisions. They never
    grant a live principal, parent, capacity reservation or execution effect. *)
open! Core

module Error : sig
  type t =
    | Conflict
    | Limit_exceeded
    | Store of Store_error.t
end

type t

val create
  :  owner:Agent_protocol.Execution_owner.developer
  -> capture_fingerprint:Chatml_source.Digest.t
  -> max_jobs:int
  -> max_invocations:int
  -> max_selected:int
  -> max_job_payload_bytes:int
  -> limits:Document_schema.Limits.t
  -> poll:(unit -> unit)
  -> t

val dispatch
  :  t
  -> Developer_run_document.t
  -> observed:(int * Agent_protocol.Host_invocation.t) option
  -> dispatched:Agent_protocol.Host_invocation.t
  -> (Developer_run_document.t, Error.t) result

val root_selected
  :  t
  -> Developer_run_document.t
  -> phase:[ `Active | `Interrupted | `Closed ]
  -> selected:Agent_protocol.Developer_job.t list
  -> (Developer_run_document.t, Error.t) result

val terminal_selected
  :  t
  -> Developer_run_document.t
  -> observed:(int * Agent_protocol.Host_invocation.t) option
  -> terminal:Agent_protocol.Host_invocation.t
  -> selected:Agent_protocol.Developer_job.t list
  -> (Developer_run_document.t, Error.t) result

(** Exact queued attempt -> Running, due at trusted [now]. The caller already
    bound [observed] to the current document and acquired shared capacity. *)
val claim
  :  t
  -> Developer_run_document.t
  -> observed:Agent_protocol.Developer_job.t
  -> now:Agent_protocol.Timestamp.t
  -> (Developer_run_document.t, Error.t) result

(** Cancel an exact unclaimed Queued job, including a queued retry, or an
    already-ended Waiting_completion job. This is state-only CAS policy for
    the current Active epoch. Running and Waiting_permission require their
    physical worker Claim; terminal jobs are never rewritten. Preserve the
    observed attempt and its started_at convention, clear pending retry/wait
    and result data, and record a completed Cancelled state at [now]. The
    caller must bind [observed] to one Store Observation; this pure function
    does not prove that no worker is still active. *)
val cancel_inactive
  :  t
  -> Developer_run_document.t
  -> observed:Agent_protocol.Developer_job.t
  -> now:Agent_protocol.Timestamp.t
  -> (Developer_run_document.t, Error.t) result

(** State-only transition for an exact physically claimed job. The caller
    proves its live Claim and a fresh document observation. [current_epoch]
    selects ordinary active-scope transitions or old-epoch terminal-only
    bookkeeping. No result custody or native effect is granted. *)
val claimed_transition
  :  t
  -> Developer_run_document.t
  -> claimed:Agent_protocol.Developer_job.t
  -> after:Agent_protocol.Developer_job.State.t
  -> current_epoch:bool
  -> (Developer_run_document.t, Error.t) result
