open Core

(** Owned nested moderator-tool dispatch, with exact managed admission, current
    policy and disclosure, and atomic moderator snapshot/result persistence.
    Uses the caller's real actor handoff without a native wrapper invocation or
    synthetic provider history. Runtime requests are emitted to the owning scope
    only after the moderator commits. All tools belonging to the active manager
    are rejected by the handler's scoped Tool.call bridge before reentrant waits. *)
val create
  :  definition:Chat_response.Managed_tool_registry.t
  -> manager:Chat_response.Moderator_manager.t
  -> history:(unit -> History_entry.t list)
  -> available_tools:Openai.Responses.Request.Tool.t list
  -> session_meta:Jsonaf.t
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> Script_tool_calls.t
  -> Script_tool_calls.moderator_dispatch
