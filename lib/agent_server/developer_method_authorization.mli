(** Shared method permission for independent developer Check, Fixture, Live
    and explicit REPL Task commands. This is one required principal scope, not a source, workspace,
    native, provider, receipt-disclosure or cancellation grant. Host and wire
    adapters must use the actual persisted/original command kind; a separate
    client-supplied kind is not authorization evidence. Repl_task_run maps to
    effectful Developer_live_run here; the hosted Task action also requires
    Developer_repl on both original and current actors through
    [check_repl_task_now]. Neither scope alone grants an effect. *)

val required_scope
  :  Agent_store.Developer_command_receipt.Kind.t
  -> Agent_protocol.Scope.t

(** Pure early check of the authenticated dispatch principal. Call before
    source/workspace acquisition or a Store key lookup, including an identical
    historical retry. Missing permission returns [Permission_denied]. *)
val authorize_principal
  :  Agent_store.Developer_command_receipt.Kind.t
  -> principal:Agent_protocol.Principal.t
  -> (unit, Agent_protocol.Error.t) result

(** Nonyielding host check of the original authenticated actor and current
    request actor. Both must still be current, have the same principal ID and
    authentication kind, and carry this exact action scope. Failure is a typed
    [Permission_denied]; unexpected guard exceptions propagate unchanged.

    Repeat after yielding work and at the final nonyielding effect/publication
    admission. This method permission never substitutes for current source,
    prompt/workspace pair, native/manifest/provider or owner checks. A
    historical retry has no source recapture but still needs this permission
    before disclosing its key; observation and cancellation separately use
    the installed disclosure/control policies. *)
val check_now
  :  Agent_store.Developer_command_receipt.Kind.t
  -> original:Operator_authorization.t
  -> current:Operator_authorization.t
  -> (unit, Agent_protocol.Error.t) result

(** A durable Task action requires both the original/current REPL permission
    and effectful Live permission. Repeat for historical disclosure, fresh
    Start and post-wait execution. Distinct receipt-read and Task/Control
    policies still apply. *)
val check_repl_task_now
  :  original:Operator_authorization.t
  -> current:Operator_authorization.t
  -> (unit, Agent_protocol.Error.t) result
