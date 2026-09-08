open Core

(** Scoped host admission/outcome persistence, without unrelated foreground
    capabilities. The host controls its lifetime and cancellation ownership. *)
type executor =
  invocation:Agent_protocol.Invocation.t
  -> (dispatched:Agent_protocol.Invocation.t
      -> (Agent_protocol.Invocation.outcome, Agent_protocol.Error.t) result)
  -> (Agent_protocol.Invocation.t, Agent_protocol.Error.t) result

(** Common native dispatch for a host-owned scope. Performs the same current
    capability, policy, input and output checks as [run]. In particular, [execute]
    must be a real actor-backed scope, not a direct call to the supplied callback.
    This grants no foreground/history authority. *)
val run_scoped
  :  execute:executor
  -> registry:(unit -> Chat_response.Tool_capability.t)
  -> reference:Chat_response.Tool_capability.reference
  -> invocation:Agent_protocol.Invocation.t
  -> is_halted:(unit -> bool)
  -> authorize:
       (Agent_protocol.Invocation.t
        -> Chat_response.Tool_capability.binding
        -> (unit, Agent_protocol.Error.t) result)
  -> prepare_output:
       (Openai.Responses.Tool_output.Output.t
        -> (Jsonaf.t, Agent_protocol.Error.t) result)
  -> (Agent_protocol.Invocation.t, Agent_protocol.Error.t) result

(** Internal common native invocation path for model and synchronous script
    origins. The registered implementation retains its native shell/file policy
    wrappers. No raw runner is exposed to the caller. [registry] supplies the
    current selected capabilities and is rechecked after authorizing waits.

    [authorize] must enforce current invocation policy and any required pre-tool
    moderator decision before returning. An active moderator's authorizing hook
    must fail before effects rather than being deferred. [prepare_output] must
    apply output disclosure/redaction and return a bounded structured value.
    Neither callback's diagnostics nor runner exceptions are copied to outcomes.
    [is_halted] is checked before admission policy and after authorizing waits;
    it must read current host lifecycle state. Recorded pre-tool rejection takes
    precedence and executes no authorization or implementation callback.

    Admission/outcome persistence uses the operation's [with_invocation] service;
    no moderator snapshot is borrowed or provider history manufactured. The
    caller publishes a real model call using [publish_invocation_output] and runs
    post-observation once. A script caller consumes the returned recorded outcome
    directly. Background ownership and standalone execution are separate services.
    This internal path does not enable model tools or install Tool.call by itself. *)
val run
  :  capabilities:Operation_worker.Capabilities.t
  -> registry:(unit -> Chat_response.Tool_capability.t)
  -> reference:Chat_response.Tool_capability.reference
  -> invocation:Agent_protocol.Invocation.t
  -> is_halted:(unit -> bool)
  -> authorize:
       (Agent_protocol.Invocation.t
        -> Chat_response.Tool_capability.binding
        -> (unit, Agent_protocol.Error.t) result)
  -> prepare_output:
       (Openai.Responses.Tool_output.Output.t
        -> (Jsonaf.t, Agent_protocol.Error.t) result)
  -> (Agent_protocol.Invocation.t, Agent_protocol.Error.t) result
