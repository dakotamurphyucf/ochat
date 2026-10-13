(** Pure, claim-bound terminal projection retained by the worker. This private
    module does not stage bytes or transition the Store. *)
open! Core

type t

val create
  :  claim:Agent_store.Developer_run_store.Claim.t
  -> proposed:Agent_protocol.Developer_job.State.t
  -> after:Agent_protocol.Developer_job.State.t
  -> ensure_attempted:(unit -> (unit, Agent_protocol.Error.t) result)
  -> acknowledge_installed:
       (Agent_store.Developer_run_store.Installed.t
        -> (unit, Agent_protocol.Error.t) result)
  -> (t, Agent_protocol.Error.t) result

val claim : t -> Agent_store.Developer_run_store.Claim.t
val proposed : t -> Agent_protocol.Developer_job.State.t
val after : t -> Agent_protocol.Developer_job.State.t
val ensure_attempted : t -> (unit, Agent_protocol.Error.t) result

val acknowledge_installed
  :  t
  -> Agent_store.Developer_run_store.Installed.t
  -> (unit, Agent_protocol.Error.t) result

val same_state
  :  Agent_protocol.Developer_job.State.t
  -> Agent_protocol.Developer_job.State.t
  -> bool
