open! Core

(** Host-only adapter from the one physically bound saved-Agent source callback
    to prepared Driver entries and the same Root native/Job/managed kernel.
    The returned runner contains no source selector, provider factory or
    independent capability registry. It is installed exactly once in the
    original SourceRuntime Bound state before the Host exposes the executor. *)
val create
  :  live:Developer_live_resources.live
  -> root:Developer_root_owner.run
  -> native:Developer_native_executor.t
  -> authorize:
       (Agent_protocol.Host_invocation.t
        -> Chat_response.Tool_capability.binding
        -> (unit, Agent_protocol.Error.t) result)
  -> prepare_output:
       (Openai.Responses.Tool_output.Output.t
        -> (Jsonaf.t, Agent_protocol.Error.t) result)
  -> ( Developer_agent_source_runtime.Selected_target.runner
       , Agent_protocol.Error.t )
       result
