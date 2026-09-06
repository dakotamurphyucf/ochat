open! Core

(** Owns one session's quota and workspace-lease acquisition. *)

type acquire_result =
  | Acquired
  | Already_acquired
  | Queue_required of Agent_session.Quota_manager.blocking_scope
  | Rejected of Agent_protocol.Error.t

type t

val create
  :  manager:Agent_session.Quota_manager.t
  -> session_id:Agent_protocol.Id.Session.t
  -> principal_id:Agent_protocol.Id.Principal.t
  -> quota_key:Agent_session.Quota_key.t
  -> prompt_limit:int
  -> configured_overflow:Agent_session.Quota_manager.overflow
  -> workspace_lease_mode:Agent_session.Workspace_lease.mode option
  -> t

val try_acquire : t -> queue_if_limited:bool -> acquire_result
val runtime_ready : t -> unit
val release : t -> unit
val is_acquired : t -> bool
val blocked_by : t -> Agent_session.Quota_manager.blocking_scope -> bool

(** [replace_workspace t ~conflict_domain ~workspace_lease_mode] updates the
    next acquisition after a stopped session rotates its temporary workspace. *)
val replace_workspace
  :  t
  -> conflict_domain:string
  -> workspace_lease_mode:Agent_session.Workspace_lease.mode option
  -> (unit, string) result
