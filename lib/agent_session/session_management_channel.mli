open Core

(** Trusted host opt-in for a named shell tool. Neither the operation grant nor
    the final-context authorizer comes from helper request data. The authorizer
    must exclude ambient host credentials/control endpoints, including implicit
    sandbox roots, and validate the exact executable and environment. *)
type grant

val grant
  :  tool_name:string
  -> policy_revision:string
  -> allowed:Session_management.operation list
  -> limits:Shell_access.Request_channel.limits
  -> authorize:(Shell_access.Context.t -> (unit, string) result)
  -> (grant, string) result

(** Deterministic host-policy identity incorporated into authored tool resource
    fingerprints, including resource-only reconstruction. Empty grants preserve
    legacy identity. Hosts must change [policy_revision] when authorizer behavior
    changes; operation/limit/name changes are included automatically. *)
val policy_fingerprint : grant list -> string option

(** Called inside the actual native shell dispatch, after binding its execution
    identity/approval store. Matches the invoking tool, borrows its verified
    ceiling and lends the shared session services for this process lifetime.
    No matching grant preserves ordinary shell behavior; ambiguous grants fail.
    There is no dependency on native lifecycle-tool registration. *)
val prepare_executor
  :  grants:grant list
  -> creation:Generated_session_request.service option
  -> sessions:Managed_session_service.t option
  -> Shell_access.Executor.config
  -> (Shell_access.Executor.config, string) result
