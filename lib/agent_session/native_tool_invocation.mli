open Core

(** Scoped host admission/outcome persistence, without unrelated foreground
    capabilities. The host controls its lifetime and cancellation ownership. *)
type executor =
  invocation:Agent_protocol.Invocation.t
  -> (dispatched:Agent_protocol.Invocation.t
      -> (Agent_protocol.Invocation.outcome, Agent_protocol.Error.t) result)
  -> (Agent_protocol.Invocation.t, Agent_protocol.Error.t) result

(** The same actor-backed transition contract over the owner-neutral record.
    The developer implementation is [Developer_invocation_owner.with_invocation];
    conversation callers keep their existing [executor] adapter. *)
type owned_executor =
  invocation:Agent_protocol.Host_invocation.t
  -> (active:Developer_invocation_owner.active
      -> dispatched:Agent_protocol.Host_invocation.t
      -> (Agent_protocol.Invocation.outcome, Agent_protocol.Error.t) result)
  -> (Agent_protocol.Host_invocation.t, Agent_protocol.Error.t) result

(** Actor-owned handoff whose commit atomically saves moderator state and result.
    It has no provider-history or general session mutation capability. *)
type moderator_executor =
  invocation:Agent_protocol.Invocation.t
  -> (dispatched:Agent_protocol.Invocation.t
      -> commit:
           (resolved:Agent_protocol.Invocation.t
            -> snapshot:Session.Moderator_state.Identity_snapshot.t
            -> (unit, Agent_protocol.Error.t) result)
      -> (unit, Agent_protocol.Error.t) result)
  -> (unit, Agent_protocol.Error.t) result

(** Fiber-local identity for native policy/approval adapters. This identifies the
    current actor-dispatched invocation, not authority to approve it. Nested scopes
    shadow their parent; fibers inheriting the binding and surviving its scope
    see Expired. Returning to an unbound caller restores Unbound.
    An expired binding must not fall back to an unrelated active model operation. *)
type scope =
  | Unbound
  | Active of Agent_protocol.Invocation.t
  | Expired

val current_scope : unit -> scope

(** Expiring borrow of the current native invocation's real actor executor and
    verified capability ceiling. It provides admission/result persistence and
    narrowing, without foreground history, moderator ownership or a policy bypass. *)
type borrowed

(** A developer/owner-neutral borrow never projects as a conversation borrow.
    The old [current_scope]/[borrow] report Expired in an active developer scope
    so Session-only services cannot fall back to an unrelated operation. *)
type owned_borrowed

(** Expiring borrow of a physically selected developer invocation. Its issuer
    and direct-parent proof come from the stable selected executor, not an
    equal run ID. This separate scope never falls back to a conversation or
    legacy no-child owner. *)
type selected_borrowed

val borrow_selected : unit -> (selected_borrowed, Agent_protocol.Error.t) result
val selected_borrowed_issuer : selected_borrowed -> Developer_invocation_owner.Selected.t

val selected_borrowed_parent
  :  selected_borrowed
  -> Developer_invocation_owner.Selected.active

val selected_borrowed_capabilities
  :  selected_borrowed
  -> (Chat_response.Tool_capability.t, Agent_protocol.Error.t) result

val borrow_owned : unit -> (owned_borrowed, Agent_protocol.Error.t) result
val owned_borrowed_invocation : owned_borrowed -> Agent_protocol.Host_invocation.t
val owned_borrowed_execution_context : owned_borrowed -> Chatml_execution.context

val owned_borrowed_capabilities
  :  owned_borrowed
  -> (Chat_response.Tool_capability.t, Agent_protocol.Error.t) result

val require_owned_native_admission
  :  owned_borrowed
  -> binding:Chat_response.Tool_capability.binding
  -> (unit, Agent_protocol.Error.t) result

val select_owned_tools
  :  owned_borrowed
  -> names:string list
  -> (owned_borrowed, Agent_protocol.Error.t) result

val execute_owned_borrowed : owned_borrowed -> owned_executor

(** Fails outside an active native callback, including a fiber whose inherited
    binding has expired. Never falls back to a different foreground operation. *)
val borrow : unit -> (borrowed, Agent_protocol.Error.t) result

(** Trusted streaming driver only: capture the active built-in fork's exact
    capability ceiling, actor executors and expiring lifetime. Direct children
    of this borrow use Delegated_agent origin without provider/history bindings.
    A same-name replacement or an unrelated native callback cannot acquire it. *)
val borrow_for_fork : unit -> (borrowed, Agent_protocol.Error.t) result

val borrowed_invocation : borrowed -> Agent_protocol.Invocation.t

(** Budget ancestry captured at native dispatch, including across a host domain
    handoff. Execution must pass it to [Chatml_execution.run]; it cannot reset or
    extend the owning scope's limits/lifetime. *)
val borrowed_execution_context : borrowed -> Chatml_execution.context

(** Actual selected tool bindings verified by the dispatch boundary, never a
    registration closure's broader registry. Fails before capability validation
    or after scope expiration. Reading this registry does not skip current policy. *)
val borrowed_capabilities
  :  borrowed
  -> (Chat_response.Tool_capability.t, Agent_protocol.Error.t) result

(** Require the exact currently executing native handler's completed admission.
    Ordinary dispatched scopes and capability borrows are not permission proofs.
    Only the native execution dispatcher creates this evidence after authorization and post-wait
    binding resolution. Checks the current invocation, capability identity and
    physical native implementation. Nested invocations cannot reuse a parent's
    admission; inherited callbacks reject after the handler returns. This query
    issues no approval or new execution authority. Current host policy/resource
    checks still belong to the metadata owner. *)
val require_native_admission
  :  borrowed
  -> binding:Chat_response.Tool_capability.binding
  -> (unit, Agent_protocol.Error.t) result

(** Record only the actual readonly query service's private response. This checks
    the live borrow and scope/selection; final output matching and atomic outcome
    persistence belong to the actor. Recording alone creates no model history. *)
val record_authoring_reference
  :  borrowed
  -> Chat_response.Authoring_context.response
  -> (unit, Agent_protocol.Error.t) result

(** Narrow this borrow to exact names already in its verified ceiling. The new
    borrow shares the same lifetime; it cannot restore previously removed tools.
    Its direct child's context must use the narrowed registry fingerprint. *)
val select_tools
  :  borrowed
  -> names:string list
  -> (borrowed, Agent_protocol.Error.t) result

(** Admit a direct child with the scope's origin (Script for a native borrow,
    Moderator for a verified managed moderator handler, Delegated_agent for a
    verified driver fork), the same session/generation and a
    deadline no later than its parent's and the selected ceiling's fingerprint.
    Uses the lending scope's actor executor,
    rechecking expiration both before admission and before the callback. The child
    callback receives its own scoped identity and may borrow it for descendants;
    its borrow expires on return, restoring the enclosing scope. The host
    still validates shared budgets and current policy;
    native effects must pass through [run_scoped]. Existing actor ownership rules
    apply, so borrowing never grants a new event/idle/foreground owner. The caller
    must join child work within the native callback's cancellation scope. *)
val execute_borrowed : borrowed -> executor

(** Optional moderator handoff inherited from the actual foreground actor scope.
    The returned adapter requires the same direct Script or verified fork child, selected ceiling,
    deadline and lifetime checks as [execute_borrowed], before waiting and again
    on entry. It never performs ordinary native admission first. Permission,
    source identity, reentrancy and schema checks remain the dispatcher's job.
    Acquiring this adapter does not keep its lending scope alive. *)
val moderator_executor : borrowed -> moderator_executor option

(** Enter only the implementation identified by an exact managed admission in
    the current actor-dispatched scope. Verifies the admitted target is selected,
    then lends its captured dependencies to [f]; the caller's ceiling is restored
    afterwards. Standalone children use Script origin; actual moderator handler
    children use Moderator origin. Native child callbacks start their own Script
    borrows. Only trusted host dispatch may call this function, and [f] must run
    the admitted compiled handler after current policy checks, never caller source. *)
val with_managed_scope
  :  Chat_response.Managed_tool_registry.execution
  -> (borrowed -> ('a, Agent_protocol.Error.t) result)
  -> ('a, Agent_protocol.Error.t) result

(** Trusted actor callback adapter for moderator dispatch. Establishes only the
    already dispatched identity and caller selection, so existing permission
    adapters can identify its owner. The host must supply real actor executors;
    this validates protocol/selection identity, not tool authorization. *)
val with_dispatched_scope
  :  execute:executor
  -> ?moderator_execute:moderator_executor
  -> selected:Chat_response.Tool_capability.t
  -> invocation:Agent_protocol.Invocation.t
  -> (unit -> ('a, Agent_protocol.Error.t) result)
  -> ('a, Agent_protocol.Error.t) result

(** Host-owned managed implementation dispatch. The callback receives a verified
    source-bound admission and a borrow limited to that implementation's declared
    dependencies. It must execute that compiled target and validate its result;
    caller-provided one-off source must never receive this delegated borrow.
    [run] keeps the handler's resource/budget scope active through the continuation,
    which performs common disclosure, schema and owned-work validation. It must
    release provisional work on continuation failure and select surviving starts
    only after continuation success. Internal host failures remain Error outcomes
    until cleanup; explicitly returned tool failures pass through the continuation. *)
type managed_dispatch =
  { definition : Chat_response.Managed_tool_registry.t
  ; current : unit -> Chat_response.Tool_capability.t
  ; run :
      'a.
      Chat_response.Managed_tool_registry.execution
      -> borrowed
      -> (validate_work:(Agent_protocol.Invocation.work -> (unit, string) result)
          -> outcome:Agent_protocol.Invocation.outcome
          -> ('a, Agent_protocol.Invocation.outcome) result)
      -> ('a, Agent_protocol.Invocation.outcome) result
  }

type owned_managed_dispatch =
  { definition : Chat_response.Managed_tool_registry.t
  ; current : unit -> Chat_response.Tool_capability.t
  ; run :
      'a.
      Chat_response.Managed_tool_registry.owned_execution
      -> owned_borrowed
      -> (validate_work:(Agent_protocol.Invocation.work -> (unit, string) result)
          -> outcome:Agent_protocol.Invocation.outcome
          -> ('a, Agent_protocol.Invocation.outcome) result)
      -> ('a, Agent_protocol.Invocation.outcome) result
  }

(** A managed selected callback receives a stage operation for the current
    physical native terminal only. It returns IDs admitted through the shared
    capacity ledger, and both this operation and the selected borrow expire
    with the callback. The result continuation names the exact subset that
    survives alongside its canonical outcome. *)
type selected_managed_dispatch =
  { definition : Chat_response.Managed_tool_registry.t
  ; current : unit -> Chat_response.Tool_capability.t
  ; run :
      'a.
      Chat_response.Managed_tool_registry.owned_execution
      -> selected_borrowed
      -> effect_entry:(unit -> (unit, Agent_protocol.Error.t) result)
      -> stage_background:
           (request:Chat_response.Background_request.t
            -> checkpoint:(unit -> unit)
            -> (Agent_protocol.Id.Job.t, Agent_protocol.Error.t) result)
      -> (validate_work:(Agent_protocol.Invocation.work -> (unit, string) result)
          -> outcome:Agent_protocol.Invocation.outcome
          -> selected_jobs:Agent_protocol.Id.Job.t list
          -> ('a, Agent_protocol.Invocation.outcome) result)
      -> ('a, Agent_protocol.Invocation.outcome) result
  }

(** Trusted selected dispatch specialization of the common Function/Custom/
    managed policy. The host passes one exact physically scoped dispatch bridge
    and seal continuation from a registered native terminal. [effect_entry]
    checks fresh root/current-parent authority at the actual handler entry,
    including after a managed handler's own yields. Failure never silently
    selects provisional work; Pending requires the scope's exact Job ID. *)
val run_owned_selected
  :  owner:Developer_invocation_owner.Selected.t
  -> parent:Developer_invocation_owner.Selected.parent
  -> dispatch:Developer_invocation_owner.Selected.dispatch_storage
  -> frozen:(unit -> (Agent_protocol.Invocation.outcome * 'selection) option)
  -> enter_callback:
       (dispatched:Agent_protocol.Host_invocation.t
        -> (unit
            -> ( Agent_protocol.Invocation.outcome * 'selection option
                 , Agent_protocol.Error.t )
                 result)
        -> ( Agent_protocol.Invocation.outcome * 'selection option
             , Agent_protocol.Error.t )
             result)
  -> managed:selected_managed_dispatch option
  -> registry:(unit -> (Chat_response.Tool_capability.t, Agent_protocol.Error.t) result)
  -> reference:Chat_response.Tool_capability.reference
  -> invocation:Agent_protocol.Host_invocation.t
  -> authorize:
       (Agent_protocol.Host_invocation.t
        -> Chat_response.Tool_capability.binding
        -> (unit, Agent_protocol.Error.t) result)
  -> prepare_output:
       (Openai.Responses.Tool_output.Output.t
        -> (Jsonaf.t, Agent_protocol.Error.t) result)
  -> effect_entry:
       (Chat_response.Tool_capability.binding -> (unit, Agent_protocol.Error.t) result)
  -> stage_background:
       (Chat_response.Tool_capability.binding
        -> request:Chat_response.Background_request.t
        -> checkpoint:(unit -> unit)
        -> (Agent_protocol.Id.Job.t, Agent_protocol.Error.t) result)
  -> seal:
       (Agent_protocol.Invocation.outcome
        -> selected_jobs:Agent_protocol.Id.Job.t list
        -> ('selection option, Agent_protocol.Error.t) result)
  -> ( 'selection Developer_invocation_owner.Selected.Completion.t
       , Agent_protocol.Error.t )
       result

(** Developer calls enter the same native/managed admission kernel as
    [run_scoped_with_managed]. [owner] is trusted host wiring: it supplies the
    persisted actor executor, original ceiling, current selected registry and
    authenticated currentness check. Selection, schema, policy, post-wait
    re-resolution, exact physical native admission, disclosure and output/work
    validation happen once in the shared kernel. The authorizer may wait, but
    must use a developer permission subject bound to this exact invocation;
    no Session attachment or grant is inferred from its ID. *)
val run_owned_root
  :  owner:Developer_invocation_owner.t
  -> managed:owned_managed_dispatch option
  -> reference:Chat_response.Tool_capability.reference
  -> invocation:Agent_protocol.Host_invocation.t
  -> authorize:
       (Agent_protocol.Host_invocation.t
        -> Chat_response.Tool_capability.binding
        -> (unit, Agent_protocol.Error.t) result)
  -> prepare_output:
       (Openai.Responses.Tool_output.Output.t
        -> (Jsonaf.t, Agent_protocol.Error.t) result)
  -> (Agent_protocol.Host_invocation.t, Agent_protocol.Error.t) result

(** A direct child of this exact active owned borrow. The implementation derives
    its opaque [Child active] parent proof from [parent]; it cannot substitute a
    Root claim or use another invocation with matching owner coordinates. The
    borrow expires with its creating callback across inherited fibers/domains. *)
val run_owned_child
  :  parent:owned_borrowed
  -> managed:owned_managed_dispatch option
  -> reference:Chat_response.Tool_capability.reference
  -> invocation:Agent_protocol.Host_invocation.t
  -> authorize:
       (Agent_protocol.Host_invocation.t
        -> Chat_response.Tool_capability.binding
        -> (unit, Agent_protocol.Error.t) result)
  -> prepare_output:
       (Openai.Responses.Tool_output.Output.t
        -> (Jsonaf.t, Agent_protocol.Error.t) result)
  -> (Agent_protocol.Host_invocation.t, Agent_protocol.Error.t) result

(** Common owned dispatch with managed targets installed. Performs capability,
    schema and policy checks and repeats managed admission after authorization.
    Managed outcomes use the same disclosure and canonical persistence path as
    native Invocation_v1 results. No implementation runs on a missing/stale
    binding; the caller's selected registry is restored after execution.
    Managed Pending requires its live handler's work validator and revalidates
    the disclosed acknowledgement against the declared success schema. *)
val run_scoped_with_managed
  :  on_progress:(Agent_protocol.Invocation.t -> Ochat_function.Progress.t -> unit) option
  -> managed:managed_dispatch option
  -> moderator_execute:moderator_executor option
  -> execute:executor
  -> registry:(unit -> Chat_response.Tool_capability.t)
  -> reference:Chat_response.Tool_capability.reference
  -> invocation:Agent_protocol.Invocation.t
  -> is_halted:(unit -> bool)
  -> authorize:
       (Agent_protocol.Invocation.t
        -> Chat_response.Tool_capability.binding
        -> (unit, Agent_protocol.Error.t) result)
  -> prepare_output:
       (Openai.Responses.Tool_output.Output.t
        -> (Jsonaf.t, Agent_protocol.Error.t) result)
  -> (Agent_protocol.Invocation.t, Agent_protocol.Error.t) result

(** Child conversation variant with the same managed/native validation and
    owned executor requirements. The driver preserves native events and fork
    execution only after the final binding is revalidated and authorized. *)
val run_scoped_in_driver
  :  run_native:Chat_response.In_memory_stream.Tool_dispatch.native_runner
  -> managed:managed_dispatch option
  -> moderator_execute:moderator_executor option
  -> execute:executor
  -> registry:(unit -> Chat_response.Tool_capability.t)
  -> reference:Chat_response.Tool_capability.reference
  -> invocation:Agent_protocol.Invocation.t
  -> is_halted:(unit -> bool)
  -> authorize:
       (Agent_protocol.Invocation.t
        -> Chat_response.Tool_capability.binding
        -> (unit, Agent_protocol.Error.t) result)
  -> prepare_output:
       (Openai.Responses.Tool_output.Output.t
        -> (Jsonaf.t, Agent_protocol.Error.t) result)
  -> (Agent_protocol.Invocation.t, Agent_protocol.Error.t) result

(** Common native dispatch for a host-owned scope. Performs the same current
    capability, policy, input and output checks as [run]. In particular, [execute]
    must be a real actor-backed scope, not a direct call to the supplied callback.
    This grants no foreground/history authority. An explicitly registered
    [Tool_capability.Invocation_v1] result is decoded only after disclosure and
    validated as the single invocation outcome. Ordinary native output stays
    opaque. Pending envelopes are rejected until owned-work validation is installed. *)
val run_scoped
  :  execute:executor
  -> registry:(unit -> Chat_response.Tool_capability.t)
  -> reference:Chat_response.Tool_capability.reference
  -> invocation:Agent_protocol.Invocation.t
  -> is_halted:(unit -> bool)
  -> authorize:
       (Agent_protocol.Invocation.t
        -> Chat_response.Tool_capability.binding
        -> (unit, Agent_protocol.Error.t) result)
  -> prepare_output:
       (Openai.Responses.Tool_output.Output.t
        -> (Jsonaf.t, Agent_protocol.Error.t) result)
  -> (Agent_protocol.Invocation.t, Agent_protocol.Error.t) result

(** Internal common native invocation path for model and synchronous script
    origins. The registered implementation retains its native shell/file policy
    wrappers. No raw runner is exposed to the caller. [registry] supplies the
    current selected capabilities and is rechecked after authorizing waits.

    [authorize] must enforce current invocation policy and any required pre-tool
    moderator decision before returning. An active moderator's authorizing hook
    must fail before effects rather than being deferred. [prepare_output] must
    apply output disclosure/redaction and return a bounded structured value.
    Neither callback's diagnostics nor runner exceptions are copied to outcomes.
    [is_halted] is checked before admission policy and after authorizing waits;
    it must read current host lifecycle state. Recorded pre-tool rejection takes
    precedence and executes no authorization or implementation callback.

    Admission/outcome persistence uses the operation's [with_invocation] service;
    no moderator snapshot is borrowed or provider history manufactured. The
    caller publishes a real model call using [publish_invocation_output] and runs
    post-observation once. A script caller consumes the returned recorded outcome
    directly. Background ownership and standalone execution are separate services.
    This internal path does not enable model tools or install Tool.call by itself. *)
val run
  :  capabilities:Operation_worker.Capabilities.t
  -> registry:(unit -> Chat_response.Tool_capability.t)
  -> reference:Chat_response.Tool_capability.reference
  -> invocation:Agent_protocol.Invocation.t
  -> is_halted:(unit -> bool)
  -> authorize:
       (Agent_protocol.Invocation.t
        -> Chat_response.Tool_capability.binding
        -> (unit, Agent_protocol.Error.t) result)
  -> prepare_output:
       (Openai.Responses.Tool_output.Output.t
        -> (Jsonaf.t, Agent_protocol.Error.t) result)
  -> (Agent_protocol.Invocation.t, Agent_protocol.Error.t) result

(** Foreground driver variant of [run]. The trusted adapter receives only the
    native implementation revalidated after authorization and its validated
    payload. It preserves driver-specific execution and live tool events inside
    the same expiring invocation scope. Script/background callers use [run_scoped]
    and cannot supply a driver or acquire foreground/history authority. *)
val run_in_driver
  :  run_native:Chat_response.In_memory_stream.Tool_dispatch.native_runner
  -> capabilities:Operation_worker.Capabilities.t
  -> registry:(unit -> Chat_response.Tool_capability.t)
  -> reference:Chat_response.Tool_capability.reference
  -> invocation:Agent_protocol.Invocation.t
  -> is_halted:(unit -> bool)
  -> authorize:
       (Agent_protocol.Invocation.t
        -> Chat_response.Tool_capability.binding
        -> (unit, Agent_protocol.Error.t) result)
  -> prepare_output:
       (Openai.Responses.Tool_output.Output.t
        -> (Jsonaf.t, Agent_protocol.Error.t) result)
  -> (Agent_protocol.Invocation.t, Agent_protocol.Error.t) result
