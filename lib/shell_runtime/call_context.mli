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
val with_services
  :  ?prepare_executor:
       (Shell_access.Executor.config -> (Shell_access.Executor.config, string) result)
  -> (unit -> (t, string) result)
  -> (unit -> 'a)
  -> 'a

(** Narrow the actual selected moderator [Process.run] caller. A parent
    [with_services] keeps its exact session, Store, provider, and executor
    adapter. Without parent services, the binding adds only a selected-child
    execution check to the original Shell config; it cannot supply approval
    services. A nested check-only binding retains its parent's check and
    adapter. Every execution check refreshes child admission, repeats any
    parent check after that possibly yielding refresh, and ends with the
    returned nonyielding child check. An expired parent always denies. Both
    bindings expire at their respective lexical returns. *)
val with_narrowed_check
  :  refresh:(unit -> (unit -> (unit, string) result, string) result)
  -> (unit -> 'a)
  -> ('a, string) result

(** Revalidate the active caller and apply its trusted executor adapter after
    binding the caller's execution scope. Nested scopes never inherit an outer
    adapter implicitly. Expired bindings fail without restoring owner services. *)
val prepare_executor
  :  Shell_access.Executor.config
  -> (Shell_access.Executor.config, string) result

(** Bind the actual caller's session, approval store and admission check, then
    apply [prepare_executor]'s trusted adapter. Unbound calls retain the original
    owner config. Expired or rejected caller services fail without falling back
    to registration-owner services. This is the shared dispatch boundary for
    runtimes and their retained executable hook workers. *)
val prepare_executor_for_call
  :  Shell_access.Executor.config
  -> (Shell_access.Executor.config, string) result

(** Explicitly clear ambient services for a host using its own original runtime.
    This is not suitable for delegated callers requiring another session identity. *)
val without_services : (unit -> 'a) -> 'a

(** [Ok None] means either an unbound caller or a live check-only binding with
    no approval services. The latter still checks its selected source and
    installs its execution adapter through [prepare_executor_for_call]. An
    expired or rejected binding returns an error. *)
val current : unit -> (t option, string) result
