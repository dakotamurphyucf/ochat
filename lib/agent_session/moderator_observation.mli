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
