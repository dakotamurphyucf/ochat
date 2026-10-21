open Core

(** Correlation for installed Agent source preparation. This value is never
    persisted or used as a source/read/parent grant. The native registration,
    live source owner and caller's physical scope must independently authorize
    preparation before and after a yield. In particular, a developer launch is
    not a forged conversation [Job.launch_owner]. *)
type t

module Developer_parent : sig
  type t =
    | Root_action
    | Native_invocation of Agent_protocol.Id.Invocation.t
    | Worker_claim of
        { job_id : Agent_protocol.Id.Job.t
        ; attempt : int
        }
  [@@deriving equal, sexp_of]
end

val conversation : Agent_protocol.Job.launch_owner -> t

(** The caller derives [owner] and [parent] from its live Root/Native/Worker
    scope. A nonnegative attempt and typed IDs are validated; the result remains
    comparison data and cannot authenticate that scope. *)
val developer
  :  owner:Agent_protocol.Execution_owner.developer
  -> parent:Developer_parent.t
  -> (t, Agent_protocol.Error.t) result

val conversation_owner : t -> Agent_protocol.Job.launch_owner option
val developer_owner : t -> Agent_protocol.Execution_owner.developer option
val developer_parent : t -> Developer_parent.t option
