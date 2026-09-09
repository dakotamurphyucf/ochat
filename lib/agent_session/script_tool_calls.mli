open Core

(** Scoped native Tool.call bridge for standalone handlers and moderator invocations,
    observations and events. This
    uses the persisted invocation service, never a raw runner or provider history.
    A prepared script's captured capability subset is the authority ceiling;
    the current registry is checked again after authorizing waits. *)
type t

(** Read the owning host's current lifecycle policy. *)
val is_halted : t -> bool

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

(** Reuse the same live native registry, policy and disclosure service for model
    calls. The stream still supplies final-target authorization; the owning host
    should delegate these native names to this policy service to avoid duplicate
    approval requests. This does not grant additional capabilities. *)
val native_dispatch
  :  t
  -> input:Operation_worker.Input.t
  -> capabilities:Operation_worker.Capabilities.t
  -> Chat_response.In_memory_stream.Tool_dispatch.t

(** Recheck the captured definition's exact live native selection. Extra current
    tools do not widen it; missing/replaced bindings fail. This is a pure policy
    boundary check and must also run after any authorization wait. *)
val validate_definition
  :  t
  -> Chat_response.Extension_compiler.definition
  -> (unit, string) result

(** Bind calls to the dispatched parent for the duration of [f]. Escaped callbacks
    fail after the scope ends. The actor must still recognize the active parent.
    Each scope allows at most 100 attempts, matching the moderator context ABI.
    Only captured native names are routed here; own moderator tools fail with
    moderator_reentrancy. Standalone routing is a separate service.

    [authorize] must enforce current policy without re-entering the active
    moderator. If a decision from that moderator is required, the dedicated
    predicate must return true and execution is refused before effects.
    [is_halted] must use actor/lifecycle state, not the held manager lock.

    The child carries durable observation intent from admission, bound to the
    prepared moderator script ID/source digest. After its outcome is saved,
    [defer_observation] requests a later safe-point drain. It must not invoke the
    active moderator. Failure is returned separately without replacing or retrying
    the saved result. Failure also leaves the persisted observation intent intact.
    The caller owns exclusive claim, atomic handler checkpoint/acknowledgement and
    recovery integration; this bridge does not install a drain or runtime features. *)
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

(** Native bridge for a standalone handler under its actual dispatched parent.
    Child invocations use Script origin and retain the parent's session, generation
    and deadline. Only the prepared native subset is accessible. If supplied,
    [observer] must identify the owning conversation moderator, not the tool script.
    [moderate] runs the required pre-tool hook under host ownership after original
    schema checks. Rewrites and redirects retain routing fingerprints; final
    targets must remain in the captured subset and pass native schema/authority
    checks after approval. Rejections never reach native authorization or effects.
    The host supplies current admission through [authorize] and owns observation
    draining. This primitive does not install moderation or public dispatch. *)
val with_standalone
  :  ?observer:Agent_protocol.Invocation.observer
  -> t
  -> prepared:Chat_response.Extension_compiler.t
  -> capabilities:Operation_worker.Capabilities.t
  -> parent:Agent_protocol.Invocation.t
  -> moderate:
       (Chat_response.Moderation.Tool_call.t
        -> (Chat_response.Moderation.Tool_moderation.t option, string) result)
  -> ((name:string
       -> args:Jsonaf.t
       -> (Chat_response.Moderation.Capabilities.tool_call_result, string) result)
      -> 'a)
  -> 'a

(** Revalidate the one-off artifact's exact selected live bindings against this
    host's current registry. Does not run source or authorizing callbacks. *)
val validate_one_off : t -> Chat_response.One_off_script.t -> (unit, string) result

(** Reuse standalone native moderation/routing and disclosure for a prepared
    one-off program under its actual borrowed Script invocation. The source and
    capability fingerprints must match that dispatched child. No synthetic tool
    declaration or moderator event is constructed. [max_nested_calls] is host
    policy for this call scope; shared recursive budgets remain the caller's job. *)
val with_one_off
  :  ?observer:Agent_protocol.Invocation.observer
  -> t
  -> prepared:Chat_response.One_off_script.t
  -> limits:Chatmd_shell_spec.Chatmd_script_spec.limits
  -> max_nested_calls:int
  -> borrowed:Native_tool_invocation.borrowed
  -> moderate:
       (Chat_response.Moderation.Tool_call.t
        -> (Chat_response.Moderation.Tool_moderation.t option, string) result)
  -> ((name:string
       -> args:Jsonaf.t
       -> (Chat_response.Moderation.Capabilities.tool_call_result, string) result)
      -> 'a)
  -> 'a

(** Tool.call scope for an already claimed Tool_observed event. Captures the
    moderator's admitted registry directly from the complete definition, so no
    synthetic moderator-handled tool is required. Checks the exact script source,
    input/output limits, current native binding and policy, and the same 100-call
    budget as invocation handlers. The actor executor must belong to this exact
    observing callback. Children carry Moderator origin, this observation's
    invocation as parent and durable observation intent. No authorizing hook can
    be deferred; recursive moderator tools fail before effects. Escaped callbacks
    expire on return. This does not install runtime bindings or standalone tools. *)
val with_observation
  :  t
  -> definition:Chat_response.Extension_compiler.definition
  -> execute:Native_tool_invocation.executor
  -> observing:Agent_protocol.Invocation.t
  -> ((name:string
       -> args:Jsonaf.t
       -> (Chat_response.Moderation.Capabilities.tool_call_result, string) result)
      -> 'a)
  -> 'a

(** Tool.call scope for an actor-claimed ordinary moderator event. Captures the
    complete definition's admitted native subset, requiring its exact source and
    a Running receipt. The supplied executor must own that same event. Children
    retain parent_event and observation intent, without a synthetic invocation or
    provider-history parent. Shares policy, value limits, 100-attempt budget and
    callback expiration with other moderator scopes. Receipts have no deadline
    field; event cancellation and deadlines remain the host's responsibility.
    No active-moderator authorizing decision may be deferred until after effects. *)
val with_event
  :  t
  -> definition:Chat_response.Extension_compiler.definition
  -> execute:Native_tool_invocation.executor
  -> executing:Agent_protocol.Moderator_execution.t
  -> ((name:string
       -> args:Jsonaf.t
       -> (Chat_response.Moderation.Capabilities.tool_call_result, string) result)
      -> 'a)
  -> 'a
