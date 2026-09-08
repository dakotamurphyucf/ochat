open Core

exception Dispatch_error of Agent_protocol.Error.t

(** Stream adapter for the native capabilities selected when [create] is called.
    Known names remain claimed if the live registry is later narrowed/replaced;
    they cannot fall through to a legacy runner. Unknown names return [None].
    The registry callback must be a pure read of the current selection.
    Input validation runs before pre moderation and again for the final target.
    Rejected calls persist a failure without executing authorization or tools.

    Actual execution uses [Native_tool_invocation], including host admission,
    post-wait identity checks and disclosure. The stream supplies the existing
    final-target authorizer and post-tool observation. Results are published with
    an atomic actor receipt. Transient fork calls require their own persisted
    owner and are rejected here. This internal service does not enable features. *)
val create
  :  input:Operation_worker.Input.t
  -> capabilities:Operation_worker.Capabilities.t
  -> registry:(unit -> Chat_response.Tool_capability.t)
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> is_halted:(unit -> bool)
  -> admit:
       (Agent_protocol.Invocation.t
        -> Chat_response.Tool_capability.binding
        -> (unit, Agent_protocol.Error.t) result)
  -> prepare_output:
       (Openai.Responses.Tool_output.Output.t
        -> (Jsonaf.t, Agent_protocol.Error.t) result)
  -> Chat_response.In_memory_stream.Tool_dispatch.t
