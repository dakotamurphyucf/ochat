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
end

type t
type lease

val create : limits:Config.Server.job_limits -> t

(** [try_acquire t key] returns [Ok None] when one or more concurrency
    dimensions are saturated. Nesting-depth violations are permanent request
    errors. A returned lease must be released exactly once. *)
val try_acquire : t -> Key.t -> (lease option, Agent_protocol.Error.t) result

(** [release lease] is idempotent. *)
val release : lease -> unit
