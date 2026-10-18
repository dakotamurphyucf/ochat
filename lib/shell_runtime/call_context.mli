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

(** Narrow an already active caller solely for a nested selected moderator
    [Process.run]. There is no unbound fallback: a real parent [with_services]
    must be live. Each inherited execution check uses the parent's exact
    services and executor adapter, then obtains a fresh child admission with
    [refresh], repeats the parent check after that possibly yielding refresh,
    and ends with the returned nonyielding child check. A cached child
    admission or a copied parent approval provider does not grant authority.
    Both bindings expire at their respective lexical returns. *)
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

(** [Ok None] means a legacy unbound caller. An expired or rejected scope returns
    an error and must never fall back to registration-owner approvals. *)
val current : unit -> (t option, string) result
