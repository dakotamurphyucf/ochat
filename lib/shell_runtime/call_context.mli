open Core

(** Actual caller services for an inherited shell binding. Immutable executor
    policy/resources remain owned by the registered shell runtime. *)
type t =
  { session_id : string
  ; approval_store : Shell_access.Approval.store
  ; approval_provider : Approval_broker.provider
  ; check : unit -> (unit, string) result
  }

(** Install trusted host services for one native dispatch. The getter must verify
    the actual admitted invocation before returning its services. Nested scopes
    shadow their parent; inherited fibers cannot use a scope after it returns. *)
val with_services : (unit -> (t, string) result) -> (unit -> 'a) -> 'a

(** Explicitly clear ambient services for a host using its own original runtime.
    This is not suitable for delegated callers requiring another session identity. *)
val without_services : (unit -> 'a) -> 'a

(** [Ok None] means a legacy unbound caller. An expired or rejected scope returns
    an error and must never fall back to registration-owner approvals. *)
val current : unit -> (t option, string) result
