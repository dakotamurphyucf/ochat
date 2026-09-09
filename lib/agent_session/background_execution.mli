open Core

type result =
  { resolved : Agent_protocol.Invocation.t
  ; runtime_requests : Chat_response.Moderation.Runtime_request.t list
  }

(** Execute a reconstructible request under an already claimed job's actor
    executor. The caller must retain [Session_actor.with_job_invocations] for the
    entire call; this adapter neither claims nor completes jobs.

    Re-admits pinned tools and compiler policy, constructs a persisted Script job
    root and routes native/managed targets through the shared script tool service.
    Script jobs run their exact compiled [main] with fresh globals. The aggregate
    host budget includes preparation and descendants, while native wrappers do not
    consume a ChatML depth slot. [deadline] is the same durable ceiling supplied to
    the actor scope; remaining time cannot exceed the stored request budget.

    [moderate_tool] and [prepare_outcome] are required owning-host policy hooks.
    They must preserve current moderator admission/checkpoint ownership and output
    disclosure; an absent moderator bridge must fail, never bypass moderation.
    Native runtime requests are collected and returned for the owning host to
    consume. No provider history, session or model request is synthesized.
    Missing/stale admission rejects without executing an implementation.
    This internal adapter does not install scheduler dispatch, launch admission,
    moderator handoffs or public Job/Tool.spawn operations. *)
val run
  :  ?observer:Agent_protocol.Invocation.observer
  -> env:Eio_unix.Stdenv.base
  -> job:Agent_protocol.Job.t
  -> deadline:Agent_protocol.Timestamp.t
  -> execute:Native_tool_invocation.executor
  -> request:Chat_response.Background_request.t
  -> policy:Chat_response.One_off_request.policy
  -> script_tools:Script_tool_calls.t
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> moderate_tool:
       (Agent_protocol.Invocation.t
        -> Chat_response.Moderation.Tool_call.t
        -> (Chat_response.Moderation.Outcome.t option, string) Core.Result.t)
  -> prepare_outcome:(Agent_protocol.Invocation.outcome -> (unit, string) Core.Result.t)
  -> unit
  -> (result, Agent_protocol.Error.t) Core.Result.t
