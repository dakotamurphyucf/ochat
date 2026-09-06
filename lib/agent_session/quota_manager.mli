(** Ordered acquisition of root-session, workspace, prompt, principal, and
    runtime-construction capacity. *)

type limits =
  { global_running_sessions : int
  ; per_principal_running_sessions : int
  ; runtime_construction : int
  }
[@@deriving compare, equal, sexp]

type overflow =
  | Reject
  | Queue
[@@deriving compare, equal, sexp]

type request =
  { session_id : Agent_protocol.Id.Session.t
  ; principal_id : Agent_protocol.Id.Principal.t
  ; quota_key : Quota_key.t
  ; prompt_limit : int
  ; overflow : overflow
  ; workspace_lease_mode : Workspace_lease.mode option
  }

type acquisition

type blocking_scope =
  | Global
  | Workspace of string
  | Prompt of Quota_key.t
  | Principal of Agent_protocol.Id.Principal.t
  | Runtime_construction
[@@deriving sexp]

type acquire_result =
  | Acquired of acquisition
  | Queue_required of blocking_scope
  | Rejected of Agent_protocol.Error.t

type t

val create
  :  limits:limits
  -> workspace_leases:Workspace_lease.t
  -> (t, Agent_protocol.Error.t) Result.t

val try_acquire : t -> request -> acquire_result

(** [runtime_ready] releases only construction capacity while retaining the
    running-session, workspace, prompt, and principal acquisitions. *)
val runtime_ready : t -> acquisition -> unit

(** [release] releases all remaining acquisitions in reverse order. *)
val release : t -> acquisition -> unit

val running_sessions : t -> int
