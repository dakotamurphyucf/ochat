open Core

(** Internal common native invocation path for model and synchronous script
    origins. The registered implementation retains its native shell/file policy
    wrappers. No raw runner is exposed to the caller. [registry] supplies the
    current selected capabilities and is rechecked after authorizing waits.

    [authorize] must enforce current invocation policy and any required pre-tool
    moderator decision before returning. An active moderator's authorizing hook
    must fail before effects rather than being deferred. [prepare_output] must
    apply output disclosure/redaction and return a bounded structured value.
    Neither callback's diagnostics nor runner exceptions are copied to outcomes.

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
  -> authorize:
       (Agent_protocol.Invocation.t
        -> Chat_response.Tool_capability.binding
        -> (unit, Agent_protocol.Error.t) result)
  -> prepare_output:
       (Openai.Responses.Tool_output.Output.t
        -> (Jsonaf.t, Agent_protocol.Error.t) result)
  -> (Agent_protocol.Invocation.t, Agent_protocol.Error.t) result
