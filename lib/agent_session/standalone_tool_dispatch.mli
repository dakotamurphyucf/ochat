(** Internal stream adapter for prepared standalone ChatML tools. Uses actual
    actor invocation ownership and atomic output publication. Runs a fresh two-
    argument entrypoint with shared context/schema/outcome validation; creates no
    moderator event, persistent session or model request.

    The host supplies execution budgets, lifecycle checks, final-target admission
    and post-wait revalidation, disclosure, and the scoped native policy service.
    [moderate_tool] supplies owned pre-tool outcomes for nested native calls;
    their runtime requests are retained in the dispatch result even when execution
    fails. End-session outcomes prevent the native effect. [observer] binds durable
    nested observation intent to the conversation moderator. The host remains
    responsible for draining observations and public installation. Pending is rejected
    until an owned completion service is installed. Known standalone names stay
    claimed on rejection; other kinds return None for the next dispatcher. *)
exception Dispatch_error of Agent_protocol.Error.t

(** Use the captured script's declared budgets and the execution service's default
    allocation budget. Hosts may supply their own policy via [execution_limits]. *)
val declared_execution_limits
  :  Chat_response.Extension_compiler.t
  -> Chatml_execution.limits

val create
  :  ?observer:Agent_protocol.Invocation.observer
  -> env:Eio_unix.Stdenv.base
  -> definition:Chat_response.Extension_compiler.definition
  -> input:Operation_worker.Input.t
  -> capabilities:Operation_worker.Capabilities.t
  -> script_tools:Script_tool_calls.t
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> is_halted:(unit -> bool)
  -> execution_limits:(Chat_response.Extension_compiler.t -> Chatml_execution.limits)
  -> admit:(Chat_response.In_memory_stream.Tool_dispatch.request -> (unit, string) result)
  -> revalidate:
       (Chat_response.In_memory_stream.Tool_dispatch.request -> (unit, string) result)
  -> prepare_outcome:(Agent_protocol.Invocation.outcome -> (unit, string) result)
  -> moderate_tool:
       (Agent_protocol.Invocation.t
        -> Chat_response.Moderation.Tool_call.t
        -> (Chat_response.Moderation.Outcome.t option, string) result)
  -> unit
  -> Chat_response.In_memory_stream.Tool_dispatch.t
