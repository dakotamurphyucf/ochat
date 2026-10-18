open! Core

(** Host-only adapter from the one physically bound saved-Agent source callback
    to prepared Driver entries and the same Root native/Job/managed kernel.
    The returned runner contains no source selector, provider factory or
    independent capability registry. It is installed exactly once in the
    original SourceRuntime Bound state before the Host exposes the executor.
    [runtime_policy] is the Host's captured daemon policy and is lent unchanged
    to every recursive selected moderator. *)
val create
  :  live:Developer_live_resources.live
  -> root:Developer_root_owner.run
  -> native:Developer_native_executor.t
  -> runtime_policy:Chat_response.Runtime_semantics.policy
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

(** Bind the same trusted Live/Root/native package for parser-owned nested
    Agent content. The returned callback requires the immediate Saved or
    Content parent to have installed its exact [Child_prepared] and native Child
    in the current fiber. It compares the lower Content target against that
    physical parent and checks the original saved ancestry, then enters
    [with_selected_content_child] and the existing child native/managed kernel.
    Each recursively selected child installs its own lexical parent and admits
    its captured declaring/caller accounting into the SAME child Budget before
    conversion. No budget gate or source Scope spans the nested Agent callback.
    An arbitrary equal revision or copied callback
    cannot open a second child owner. There is no ordinary Driver, ambient
    Fetch, foreign provider or literal fallback on denial. *)
val create_content
  :  live:Developer_live_resources.live
  -> root:Developer_root_owner.run
  -> native:Developer_native_executor.t
  -> runtime_policy:Chat_response.Runtime_semantics.policy
  -> authorize:
       (Agent_protocol.Host_invocation.t
        -> Chat_response.Tool_capability.binding
        -> (unit, Agent_protocol.Error.t) result)
  -> prepare_output:
       (Openai.Responses.Tool_output.Output.t
        -> (Jsonaf.t, Agent_protocol.Error.t) result)
  -> ( Agent_session.Captured_agent_runner.selected_content_runner
       , Agent_protocol.Error.t )
       result

(** Build the inseparable pre-target Context loan and post-target Tool runner
    from this exact Live/Root/Native package. The first callback requires the
    current execute-scoped child Context under Root's authentic selected
    inference frame, compares it with the retained [child_key] and physically
    selected Native implementation, and lends only that guarded Context to
    the lower parser callback. The second enters the resulting opaque Tool
    target through the same Child_prepared/native/Job kernel. Both deny after
    revocation; neither function is independently a source or provider grant.
    Content/Model descendants retain the parent's active Agent-call depth. *)
val create_tool
  :  live:Developer_live_resources.live
  -> root:Developer_root_owner.run
  -> native:Developer_native_executor.t
  -> runtime_policy:Chat_response.Runtime_semantics.policy
  -> authorize:
       (Agent_protocol.Host_invocation.t
        -> Chat_response.Tool_capability.binding
        -> (unit, Agent_protocol.Error.t) result)
  -> prepare_output:
       (Openai.Responses.Tool_output.Output.t
        -> (Jsonaf.t, Agent_protocol.Error.t) result)
  -> (Developer_live_resources.Selected_tool_callbacks.t, Agent_protocol.Error.t) result
