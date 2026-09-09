(** Internal stream adapter for prepared standalone ChatML tools. Uses actual
    actor invocation ownership and atomic output publication. Runs a fresh two-
    argument entrypoint with shared context/schema/outcome validation; creates no
    moderator event, persistent session or model request.

    The host supplies execution budgets, lifecycle checks, final-target admission
    and post-wait revalidation, disclosure, and the scoped native policy service.
    [moderate_tool] supplies owned pre-tool decisions for nested native calls;
    the host remains responsible for post-tool observations and public installation.
    Pending is rejected
    until an owned completion service is installed. Known standalone names stay
    claimed on rejection; other kinds return None for the next dispatcher. *)
exception Dispatch_error of Agent_protocol.Error.t

val create
  :  env:Eio_unix.Stdenv.base
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
        -> (Chat_response.Moderation.Tool_moderation.t option, string) result)
  -> Chat_response.In_memory_stream.Tool_dispatch.t
