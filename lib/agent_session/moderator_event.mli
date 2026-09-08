(** Actor handoff supplied by [Session_actor.with_idle_queued_moderator_event_tools].
    The claim must persist ownership before calling the handler, and atomically
    persist the supplied checkpoint and runtime requests before returning from
    [commit]. It must retain ownership through the callback's return. *)
type claim =
  snapshot:Session.Moderator_state.Identity_snapshot.t
  -> (executing:Agent_protocol.Moderator_execution.t
      -> event:Session.Snapshot.t
      -> execute:Native_tool_invocation.executor
      -> commit:
           (snapshot:Session.Moderator_state.Identity_snapshot.t
            -> requests:Agent_protocol.Invocation.follow_up
            -> (unit, Agent_protocol.Error.t) result)
      -> (unit, Agent_protocol.Error.t) result)
  -> (bool, Agent_protocol.Error.t) result

(** Execute one queued event with the installed definition's native Tool.call
    bridge and actor-owned lineage. Reads the manager's exact checkpoint before
    claiming it, compares the selected head under the manager lock, and commits
    the prospective state/queue/outcome intent before local installation. Native
    results carry durable observation intent; wakeup failure cannot erase them.

    Returns None for an empty queue or unavailable actor. Failure/cancellation
    retain the actor's event disposition and do not retry effects or retire heads.
    The returned runtime requests are already durable: a host must apply their
    retained intent, not schedule the returned requests independently. [history]
    reads current canonical history after the claim without entering the manager.
    Each call has a fresh Tool.call budget and a scope that expires before return.

    This internal composition helper does not install startup/foreground routing,
    wakeup polling, deadlines, interactive permission ownership or public tools. *)
val run_queued_idle
  :  claim:claim
  -> script_tools:Script_tool_calls.t
  -> manager:Chat_response.Moderator_manager.t
  -> history:(unit -> History_entry.t list)
  -> available_tools:Openai.Responses.Request.Tool.t list
  -> session_meta:Jsonaf.t
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> unit
  -> (Chat_response.Moderation.Outcome.t option, Agent_protocol.Error.t) result
