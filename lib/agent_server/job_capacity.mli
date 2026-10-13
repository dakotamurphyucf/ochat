open! Core

(** Nonblocking hierarchical capacity accounting for daemon-owned jobs. *)

module Key : sig
  type t

  val create
    :  principal_id:Agent_protocol.Id.Principal.t option
    -> prompt:string
    -> workspace_conflict_domain:string
    -> session_id:Agent_protocol.Id.Session.t
    -> kind:Agent_protocol.Job.kind
    -> nested_depth:int
    -> t

  (** Developer workload is a validated selected project root. This separate
      tagged bucket uses the existing per-prompt limit as per-workload and the
      existing per-session limit as per-owner. Global/principal/workspace/kind
      dimensions remain shared with conversations. The actor must still prove
      the supplied project/capture and current selection; a key is no grant. *)
  val developer
    :  owner:Agent_protocol.Execution_owner.developer
    -> project_source:Chatml_source.Id.t
    -> workspace_conflict_domain:string
    -> kind:Agent_protocol.Job.kind
    -> nested_depth:int
    -> (t, Agent_protocol.Error.t) result
end

type t
type lease
type reservation

val create : limits:Config.Server.job_limits -> t

(** [try_acquire t key] returns [Ok None] when one or more concurrency
    dimensions are saturated. Nesting-depth violations are permanent request
    errors. A returned lease must be released exactly once. *)
val try_acquire : t -> Key.t -> (lease option, Agent_protocol.Error.t) result

(** [release lease] is idempotent. *)
val release : lease -> unit

(** Reserve capacity for a new job before its owning transaction commits. Staged
    reservations consume the same hierarchical capacity as active workers, but
    cannot be acquired by the scheduler. This grants capacity only, not authority
    to execute a payload; the host must validate ownership and tool policy. *)
val reserve_job
  :  t
  -> Key.t
  -> job:Agent_protocol.Job.t
  -> (reservation option, Agent_protocol.Error.t) result

(** Owner-tagged path over the same ledger as [reserve_job]. The job and key
    must match in owner, kind, depth and initial queued attempt. Reservation
    identity is owner plus Job ID, never bare Job ID. An existing reservation
    for that identity with a different attempt or immutable bucket rejects.
    Conversation jobs retain their existing one-session/generation-per-ID
    conflict rule while developer owners may use the same bare Job ID. *)
val reserve_host_job
  :  t
  -> Key.t
  -> job:Agent_protocol.Host_job.t
  -> (reservation option, Agent_protocol.Error.t) result

(** Publish only after durable job/owner commit. Idempotent; an already retired
    reservation cannot be revived. This operation performs no effects itself. *)
val publish : reservation -> unit

(** Release an unpublished reservation on abort. Published/claimed reservations
    belong to the scheduler and are unaffected by caller cleanup. *)
val abort : reservation -> unit

(** Transfer matching published capacity to a worker without charging twice.
    Staged/claimed reservations return None; unreserved legacy/recovered jobs
    acquire ordinary capacity. Mismatched owner keys/generations fail closed. *)
val try_acquire_job
  :  t
  -> Key.t
  -> job:Agent_protocol.Job.t
  -> (lease option, Agent_protocol.Error.t) result

(** Transfers only a matching published reservation for this exact queued
    attempt; staged and already claimed work cannot run. A recovered/unreserved
    job acquires ordinary capacity from this same ledger. The lease remains
    charged through claimed-worker cleanup and [release]. A newer queued retry
    with the same immutable key/owner sees [Ok None] while the old attempt is
    claimed, then may acquire after cleanup. Stale attempts or mismatches against
    staged/ready reservations reject. *)
val try_acquire_host_job
  :  t
  -> Key.t
  -> job:Agent_protocol.Host_job.t
  -> (lease option, Agent_protocol.Error.t) result

(** Retire unclaimed capacity when authoritative actor state makes a job terminal.
    Claimed leases stay with their worker until its actual cleanup releases them. *)
val retire_job : t -> Agent_protocol.Job.t -> unit

val retire_host_job : t -> Agent_protocol.Host_job.t -> unit

(** Release unclaimed reservations invalidated by an authoritative generation
    advance, including jobs removed by reset. Current/future generations remain. *)
val retire_previous_generations
  :  t
  -> session_id:Agent_protocol.Id.Session.t
  -> generation:int
  -> unit

(** Retires only unclaimed reservations for earlier epochs of the exact
    developer host/run/principal identity; an active worker keeps its lease.
    Per-owner capacity uses that stable identity without epoch, so old claimed
    workers remain charged against the new epoch until cleanup. *)
val retire_previous_epochs
  :  t
  -> current_owner:Agent_protocol.Execution_owner.developer
  -> unit

(** Release unclaimed reservations after the session runtime is closed. Active
    workers retain their lease until their cleanup completes. *)
val close_session : t -> session_id:Agent_protocol.Id.Session.t -> unit

val close_developer_run : t -> owner:Agent_protocol.Execution_owner.developer -> unit
