open Core

type result =
  { resolved : Agent_protocol.Invocation.t
  ; runtime_requests : Chat_response.Moderation.Runtime_request.t list
  }

(** Execute an already compiled one-off program as a persisted Script child of
    an active native invocation. Narrows the borrow to the artifact's exact live
    bindings and rejects mismatches before admission. The host must have compiled
    the caller's actual submitted source using [One_off_script.prepare_in_domain].

    Fresh globals, [main : json -> json task], native pre-tool moderation, current
    authorization/revocation and disclosure use the shared standalone services.
    Only the actual model caller may publish a provider output. This function
    initializes no session or root model turn and creates no synthetic moderator
    declaration. An authorized selected tool may itself call a model.

    [limits], [max_nested_calls] and [max_invocation_depth] are host-selected policy.
    The deadline is narrowed against the parent's and covers the owned callback,
    including policy/tool waits. Nested ChatML execution inherits active resource
    budgets through the native borrow, including host domain handoffs. Idle/event
    descendants and final public registration still need integration.
    [runtime_requests] must be consumed by the owning runtime. *)
val run
  :  ?observer:Agent_protocol.Invocation.observer
  -> ?allocation_bytes:int
  -> ?max_invocation_depth:int
  -> env:Eio_unix.Stdenv.base
  -> prepared:Chat_response.One_off_script.t
  -> borrowed:Native_tool_invocation.borrowed
  -> script_tools:Script_tool_calls.t
  -> input:Jsonaf.t
  -> limits:Chatmd_shell_spec.Chatmd_script_spec.limits
  -> max_nested_calls:int
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> moderate_tool:
       (Agent_protocol.Invocation.t
        -> Chat_response.Moderation.Tool_call.t
        -> (Chat_response.Moderation.Outcome.t option, string) Core.Result.t)
  -> prepare_outcome:(Agent_protocol.Invocation.outcome -> (unit, string) Core.Result.t)
  -> unit
  -> (result, Agent_protocol.Error.t) Core.Result.t
