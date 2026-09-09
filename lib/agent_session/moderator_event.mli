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
    Without [script_tools], Tool.call returns [invocation.unavailable]; the
    manager's default native callback is never used.

    This internal composition helper does not install startup/foreground routing,
    wakeup polling, deadlines, interactive permission ownership or public tools. *)
val run_queued_idle
  :  claim:claim
  -> ?script_tools:Script_tool_calls.t
  -> manager:Chat_response.Moderator_manager.t
  -> history:(unit -> History_entry.t list)
  -> available_tools:Openai.Responses.Request.Tool.t list
  -> session_meta:Jsonaf.t
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> unit
  -> (Chat_response.Moderation.Outcome.t option, Agent_protocol.Error.t) result

(** Ordinary event composition using [Session_actor.with_ordinary_moderator_event].
    The claim must be bound to this same event and its real operation (or idle
    startup/resume). Compares the captured event, executes the compiled v1 handler
    with its scoped native capabilities and commits checkpoint/request intent.
    No queue head is consumed. Returned requests are already durable; the host
    must pair their consumption with its actual scheduling/action boundary. *)
val run_ordinary
  :  event:Chat_response.Moderation.Event.t
  -> claim:claim
  -> ?script_tools:Script_tool_calls.t
  -> manager:Chat_response.Moderator_manager.t
  -> history:(unit -> History_entry.t list)
  -> available_tools:Openai.Responses.Request.Tool.t list
  -> session_meta:Jsonaf.t
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> unit
  -> (Chat_response.Moderation.Outcome.t option, Agent_protocol.Error.t) result

(** Bind ordinary events, queued events and deferred observations to an actual
    foreground worker. The installed checkpoint/source must already match the
    actor. Native calls use scoped invocation executors; missing native services
    return an explicit unavailable result. Safe-point draining is bounded to 256
    total event/observation callbacks and ends immediately on halt.

    Turn-only requests use the existing stream policy/budget and are acknowledged
    before provider dispatch. Compaction requests (including their dependent turn)
    remain durable for the actor's idle follow-up scheduler. The worker's terminal
    transaction retires unadmitted turns and settles an actual end-session action.
    Install these handlers before emitting the submitted-item event. *)
val foreground_handlers
  :  ?script_tools:Script_tool_calls.t
  -> capabilities:Operation_worker.Capabilities.t
  -> manager:Chat_response.Moderator_manager.t
  -> session_meta:Jsonaf.t
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> unit
  -> ( Chat_response.In_memory_stream.moderator_event_handlers
       , Agent_protocol.Error.t )
       result
