open Core

(** Scoped Tool.call bridge for native and installed standalone managed targets,
    used by standalone handlers and moderator invocations,
    observations and events. This
    uses the persisted invocation service, never a raw runner or provider history.
    A prepared script's captured capability subset is the authority ceiling;
    the current registry is checked again after authorizing waits.
    The compact Tool.call API returns Complete's value or Pending's initial
    acknowledgement in Ok. The persisted invocation retains the full work
    reference; tools that need to expose that ID in a compact reply should include
    it in their acknowledgement schema. Fail/cancellation retain Error codes. *)
type t

(** Read the owning host's current lifecycle policy. *)
val is_halted : t -> bool

(** Bind this service to the actual background owner's lifecycle. Keeps all
    capability, permission, disclosure and managed dispatch services intact.
    The owning actor must still enforce invocation ownership/cancellation. *)
val with_lifecycle : t -> is_halted:(unit -> bool) -> t

(** Qualified background host policy: persist handler actions with their outcome
    instead of forwarding ephemeral runtime requests. Requires actor-owned commit
    and follow-up consumption; this setting adds no execution authority. *)
val with_durable_requests : t -> t

(** Qualified host injection; does not change this service's tool ceiling. *)
val with_job_service : t -> Script_job_service.t -> t

val with_subscription_service : t -> Script_subscription_service.t -> t

(** Host display observer for native descendants, after normal tool admission.
    The host must preserve ownership, disclosure and bounded/nonblocking delivery.
    The callback expires when its actual native runner returns. *)
val with_progress
  :  t
  -> emit:(Agent_protocol.Invocation.t -> Ochat_function.Progress.t -> unit)
  -> t

(** Bind progress disclosure to the root job's verified captured selection.
    Nested managed calls cannot expose progress from private dependencies outside
    that ceiling, even though they may execute those dependencies. *)
val with_progress_ceiling : t -> ceiling:Chat_response.Tool_capability.t -> t

(** The caller must pass its verified, active owner and exact dependency subset.
    Failure/exception aborts starts before error adaptation into a tool outcome. *)
val with_job_scope
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> selected:Chat_response.Tool_capability.t
  -> error:(string -> 'error)
  -> (Script_job_service.scope option -> ('a, 'error) result)
  -> ('a, 'error) result

val durable_requests : t -> bool

(** Actual moderator handlers/events only. [originating] captures a dispatched
    tool's compiled declaration; ordinary/observation events pass None. One-off
    and standalone dispatch keep the separate job-only scope above. *)
val with_moderator_work
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> selected:Chat_response.Tool_capability.t
  -> source:Agent_protocol.Invocation.observer
  -> originating:Script_subscription_service.origin option
  -> error:(string -> 'error)
  -> (jobs:Script_job_service.scope option
      -> subscriptions:Script_subscription_service.scope option
      -> ('a, 'error) result)
  -> ('a, 'error) result

val validate_pending_work
  :  jobs:Script_job_service.scope option
  -> subscriptions:Script_subscription_service.scope option
  -> fallback:(Agent_protocol.Invocation.work -> (unit, string) result)
  -> Agent_protocol.Invocation.work
  -> (unit, string) result

(** Host dispatch accessors. These expose current binding/policy services, not
    permission to execute implementations outside an owned invocation. *)
val current_capabilities : t -> Chat_response.Tool_capability.t

val authorize
  :  t
  -> Agent_protocol.Invocation.t
  -> Chat_response.Tool_capability.binding
  -> (unit, Agent_protocol.Error.t) result

type moderator_dispatch =
  execute:Native_tool_invocation.moderator_executor
  -> native_execute:Native_tool_invocation.executor
  -> selected:Chat_response.Tool_capability.t
  -> reference:Chat_response.Tool_capability.reference
  -> invocation:Agent_protocol.Invocation.t
  -> prepare_output:
       (Openai.Responses.Tool_output.Output.t
        -> (Jsonaf.t, Agent_protocol.Error.t) result)
  -> (Agent_protocol.Invocation.t, Agent_protocol.Error.t) result

(** Install the owning manager's atomic handoff, separate from ordinary native
    admission. The factory receives the actual caller scope and must preserve its
    reentrancy restrictions. It must recheck authority after all owner/policy waits. *)
val with_moderator_dispatch : t -> dispatch:(t -> moderator_dispatch) -> t

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

(** Install captured standalone managed targets for nested Tool.call execution.
    Each selected capability runs only its pinned compiled implementation with
    its declared dependencies, under the same actor and current host policy.
    The caller's own capability selection is unchanged. Moderator-tool handoff
    retains its separate owner/reentrancy requirements. *)
val with_managed_tools
  :  t
  -> env:Eio_unix.Stdenv.base
  -> definition:Chat_response.Managed_tool_registry.t
  -> execution_limits:(Chat_response.Extension_compiler.t -> Chatml_execution.limits)
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

(** Recheck the captured definition's exact live capability selection. Extra current
    tools do not widen it; missing/replaced bindings fail. This is a pure policy
    boundary check and must also run after any authorization wait. *)
val validate_definition
  :  t
  -> Chat_response.Extension_compiler.definition
  -> (unit, string) result

(** Bind calls to the dispatched parent for the duration of [f]. Escaped callbacks
    fail after the scope ends. The actor must still recognize the active parent.
    Each scope allows at most 100 attempts, matching the moderator context ABI.
    Only captured names are routed here; own moderator tools fail with
    moderator_reentrancy. Standalone managed targets require [with_managed_tools].

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

(** Native/standalone children of an actual managed moderator handler. The
    private admission and delegated borrow must identify the same dispatched
    handler. Child origin is Moderator, and tools belonging to its active
    moderator fail before attempting another handoff. *)
val with_managed_invocation
  :  t
  -> execution:Chat_response.Managed_tool_registry.execution
  -> borrowed:Native_tool_invocation.borrowed
  -> ((name:string
       -> args:Jsonaf.t
       -> (Chat_response.Moderation.Capabilities.tool_call_result, string) result)
      -> 'a)
  -> 'a

(** Tool bridge for a standalone handler under its actual dispatched parent.
    Child invocations use Script origin and retain the parent's session, generation
    and deadline. Only the prepared capability subset is accessible. If supplied,
    [observer] must identify the owning conversation moderator, not the tool script.
    [moderate] runs the required pre-tool hook under host ownership after original
    schema checks. Rewrites and redirects retain routing fingerprints; final
    targets must remain in the captured subset and pass schema/authority
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

type background_target =
  { invocation : Agent_protocol.Invocation.t
  ; completion_schema : Jsonaf.t option
  }

(** Execute one selected target as a child of an actor-dispatched Script job root.
    Uses the same pre-tool routing, native/managed dispatch, current policy,
    disclosure and observation handling as synchronous script calls, while
    retaining the complete structured outcome (including failure details).
    The borrow must belong to the job's owned invocation service. A missing
    moderator handoff is not permission to bypass a managed moderator target.
    Admission/observation failures are returned separately from saved outcomes. *)
val call_background
  :  ?observer:Agent_protocol.Invocation.observer
  -> t
  -> borrowed:Native_tool_invocation.borrowed
  -> limits:Chatmd_shell_spec.Chatmd_script_spec.limits
  -> max_nested_calls:int
  -> moderate:
       (Chat_response.Moderation.Tool_call.t
        -> (Chat_response.Moderation.Tool_moderation.t option, string) result)
  -> name:string
  -> args:Jsonaf.t
  -> (background_target, string) result

(** Reuse standalone moderation/routing and disclosure for a prepared
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
    input/output limits, current capability binding and policy, and the same 100-call
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
    complete definition's admitted capability registry, requiring its exact source and
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
