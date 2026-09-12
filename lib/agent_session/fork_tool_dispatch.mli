open Core

(** Temporary child conversation dispatch. The borrow is captured by the trusted
    driver inside its actual built-in fork invocation. Each call validates that
    expiring owner and branch identity, and the host executes a Delegated_agent
    child with no root provider-history binding. Outcomes remain actor-owned;
    child conversation history is maintained only by the child stream driver. *)
val create
  :  input:Operation_worker.Input.t
  -> borrowed:Native_tool_invocation.borrowed
  -> source:string
  -> parent_call_id:string
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> prepare:
       (selected:Chat_response.Tool_capability.t
        -> parent:Agent_protocol.Invocation.t
        -> id:Agent_protocol.Id.Invocation.t
        -> Chat_response.In_memory_stream.Tool_dispatch.request
        -> (Chat_response.Moderation.Tool_moderation.t option, string) result)
  -> execute:
       (borrowed:Native_tool_invocation.borrowed
        -> selected:Chat_response.Tool_capability.t
        -> reference:Chat_response.Tool_capability.reference
        -> invocation:Agent_protocol.Invocation.t
        -> run_native:Chat_response.In_memory_stream.Tool_dispatch.native_runner option
        -> authorize:(unit -> unit)
        -> (Agent_protocol.Invocation.t, Agent_protocol.Error.t) result)
  -> for_fork:
       (source:string
        -> parent_call_id:string
        -> Chat_response.In_memory_stream.Tool_dispatch.t)
  -> Chat_response.In_memory_stream.Tool_dispatch.t
