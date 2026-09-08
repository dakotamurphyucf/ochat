open Core

(** Scoped native Tool.call bridge for an executing moderator invocation. This
    uses the persisted invocation service, never a raw runner or provider history.
    A prepared script's captured capability subset is the authority ceiling;
    the current registry is checked again after authorizing waits. *)
type t

val create
  :  registry:(unit -> Chat_response.Tool_capability.t)
  -> moderator_names:String.Set.t
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> is_halted:(unit -> bool)
  -> requires_active_moderator:(Chat_response.Tool_capability.reference -> bool)
  -> authorize:
       (Agent_protocol.Invocation.t
        -> Chat_response.Tool_capability.binding
        -> (unit, Agent_protocol.Error.t) result)
  -> prepare_output:
       (Openai.Responses.Tool_output.Output.t
        -> (Jsonaf.t, Agent_protocol.Error.t) result)
  -> defer_observation:
       (Agent_protocol.Invocation.t -> (unit, Agent_protocol.Error.t) result)
  -> t

(** Bind calls to the dispatched parent for the duration of [f]. Escaped callbacks
    fail after the scope ends. The actor must still recognize the active parent.
    Each scope allows at most 100 attempts, matching the moderator context ABI.
    Only captured native names are routed here; own moderator tools fail with
    moderator_reentrancy. Standalone routing is a separate service.

    [authorize] must enforce current policy without re-entering the active
    moderator. If a decision from that moderator is required, the dedicated
    predicate must return true and execution is refused before effects.
    [is_halted] must use actor/lifecycle state, not the held manager lock.

    After a child outcome is saved, [defer_observation] must retain its
    non-authorizing observation for a later safe point. It must not invoke the
    active moderator. Failure is returned separately without replacing or retrying
    the saved result. The caller owns durable observation queue integration;
    this bridge does not itself install a queue or normal runtime features. *)
val with_invocation
  :  t
  -> prepared:Chat_response.Extension_compiler.t
  -> capabilities:Operation_worker.Capabilities.t
  -> parent:Agent_protocol.Invocation.t
  -> ((name:string
       -> args:Jsonaf.t
       -> (Chat_response.Moderation.Capabilities.tool_call_result, string) result)
      -> 'a)
  -> 'a
