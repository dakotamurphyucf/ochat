open Core

type result =
  { outcomes : Chat_response.Moderation.Outcome.t list
  ; budget_exhausted : bool
  }

(** Drain eligible initial outcomes through the actor's atomic selection/claim
    and the manager's prospective checkpoint. Each iteration rechecks eligibility
    after releasing the previous borrow; concurrent drainers cannot duplicate
    handling. Event input uses only disclosed persisted outcomes.

    The budget defaults to 32, bounded to 1..256. Exhaustion means another safe
    point should probe for work, not that another record was definitely present.
    A committed end-session request stops the drain. The host applies returned
    runtime requests and schedules the next safe point. State/queued events and
    each acknowledgement are already durable; the list is not their durable store.

    Failed handling stops this drain and retains the separate observation failure
    through the actor. Earlier acknowledgements remain committed. Exceptions and
    cancellation propagate after protected actor cleanup; no tool is retried.
    [history] must read current canonical history without entering the manager.
    [on_tool_call], when supplied, must enforce current scoped authority and avoid
    recursive moderator hooks. Normal runtime and idle draining are not installed
    by this function. *)
val drain
  :  ?max_observations:int
  -> ?on_tool_call:
       (name:string
        -> args:Jsonaf.t
        -> (Chat_response.Moderation.Capabilities.tool_call_result, string) Result.t)
  -> capabilities:Operation_worker.Capabilities.t
  -> observer:Agent_protocol.Invocation.observer
  -> manager:Chat_response.Moderator_manager.t
  -> history:(unit -> History_entry.t list)
  -> available_tools:Openai.Responses.Request.Tool.t list
  -> session_meta:Jsonaf.t
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> unit
  -> (result, Agent_protocol.Error.t) Result.t

(** The same bounded drain under idle actor ownership, with no foreground
    operation. Runtime requests are retained with each acknowledgement. The host
    must apply their durable intent after this returns; it must not also schedule
    the returned requests independently. The host must wake the idle drain and
    supply an appropriately scoped [on_tool_call] before exposing tool effects.
    Unavailable sessions return no work. This does not install normal runtime
    binding or a follow-up consumer itself; [Runtime_owner] supplies that idle
    integration for an installed v1 manager. [claim] must be the actor's scoped
    [with_idle_moderator_observation] operation with its observer bound. Passing
    it explicitly keeps the drain independent of runtime construction. *)
val drain_idle
  :  ?max_observations:int
  -> ?on_tool_call:
       (name:string
        -> args:Jsonaf.t
        -> (Chat_response.Moderation.Capabilities.tool_call_result, string) Result.t)
  -> claim:
       ((observing:Agent_protocol.Invocation.t
         -> commit:
              (resolved:Agent_protocol.Invocation.t
               -> snapshot:Session.Moderator_state.Identity_snapshot.t
               -> (unit, Agent_protocol.Error.t) Result.t)
         -> (unit, Agent_protocol.Error.t) Result.t)
        -> (bool, Agent_protocol.Error.t) Result.t)
  -> manager:Chat_response.Moderator_manager.t
  -> history:(unit -> History_entry.t list)
  -> available_tools:Openai.Responses.Request.Tool.t list
  -> session_meta:Jsonaf.t
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> unit
  -> (result, Agent_protocol.Error.t) Result.t

(** Bounded idle draining with a fresh native Tool.call scope for each claimed
    observation. The scope captures the admitted definition's capabilities and
    expires before the next claim; its call budget does not span the batch.
    [claim] must supply the actor's executor from
    [with_idle_moderator_observation_tools]. [definition] must be the exact
    definition installed in [manager]. Runtime requests are durably retained as
    with [drain_idle]. Native child observations join later eligible work; this
    neither re-enters the handler nor bypasses shared policy or publication rules. *)
val drain_idle_with_tools
  :  ?max_observations:int
  -> script_tools:Script_tool_calls.t
  -> definition:Chat_response.Extension_compiler.definition
  -> claim:
       ((observing:Agent_protocol.Invocation.t
         -> execute:Native_tool_invocation.executor
         -> commit:
              (resolved:Agent_protocol.Invocation.t
               -> snapshot:Session.Moderator_state.Identity_snapshot.t
               -> (unit, Agent_protocol.Error.t) Result.t)
         -> (unit, Agent_protocol.Error.t) Result.t)
        -> (bool, Agent_protocol.Error.t) Result.t)
  -> manager:Chat_response.Moderator_manager.t
  -> history:(unit -> History_entry.t list)
  -> available_tools:Openai.Responses.Request.Tool.t list
  -> session_meta:Jsonaf.t
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> unit
  -> (result, Agent_protocol.Error.t) Result.t

(** Foreground draining with one native Tool.call scope per claimed observation.
    The worker capabilities bind each claim and native child to the active
    operation. Unlike [drain], follow-up requests remain durably retained: the
    host must consume their intent at its scheduling boundary and must not also
    schedule the returned requests independently. [definition] must be the exact
    definition installed in [manager]. *)
val drain_foreground_with_tools
  :  ?max_observations:int
  -> script_tools:Script_tool_calls.t
  -> definition:Chat_response.Extension_compiler.definition
  -> capabilities:Operation_worker.Capabilities.t
  -> observer:Agent_protocol.Invocation.observer
  -> manager:Chat_response.Moderator_manager.t
  -> history:(unit -> History_entry.t list)
  -> available_tools:Openai.Responses.Request.Tool.t list
  -> session_meta:Jsonaf.t
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> unit
  -> (result, Agent_protocol.Error.t) Result.t
