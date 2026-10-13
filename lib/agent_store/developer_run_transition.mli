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
