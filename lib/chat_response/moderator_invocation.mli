open Core
module I = Agent_protocol.Invocation
module R = Chatml_host_runtime
module L = Chatml.Chatml_lang

type t

(** Per-invocation Tool.call attempt ceiling exposed in the moderator context. *)
val max_nested_calls : int

(** Pure input preparation for both original-call validation before pre hooks and
    final invocation admission after rewrites. Checks protocol/schema bounds and
    the ChatML value projection's depth, array and byte limits. Limits must come
    from a validated script declaration. Performs no effects or authorization. *)
val prepare_input
  :  ?control:L.execution_control
  -> prepared:Extension_compiler.t
  -> limits:Chatmd_shell_spec.Chatmd_script_spec.limits
  -> Jsonaf.t
  -> (L.value, string) result

(** Host-only dispatch scope. The invocation must already be dispatched under
    current owner policy. Its implementation revision is the prepared handler's
    fingerprint; its capability fingerprint is the selected registry fingerprint.
    This checks identity/input, not current authorization or actor ownership.
    [validate_work] must recheck that Pending refers to admitted work owned by
    this invocation's session/generation with a valid completion path. It must
    not publish or mutate state. [control] charges input/schema/context projection
    within the caller's current execution scope and checks the combined context.
    [None] retains the low-level declaration/protocol bounds without a shared
    execution budget. *)
val create
  :  control:L.execution_control option
  -> prepared:Extension_compiler.t
  -> invocation:I.t
  -> limits:Chatmd_shell_spec.Chatmd_script_spec.limits
  -> validate_work:(I.work -> (unit, string) result)
  -> (t, string) result

val event : t -> L.value
val invocation : t -> I.t

(** Shared standalone admission and ABI projection. Requires a dispatched
    standalone declaration with the exact prepared identity. Produces context
    and input values without constructing or delivering a moderator event.
    Authorization and persisted ownership remain the host's responsibility. *)
val create_standalone
  :  control:L.execution_control option
  -> prepared:Extension_compiler.t
  -> invocation:I.t
  -> limits:Chatmd_shell_spec.Chatmd_script_spec.limits
  -> validate_work:(I.work -> (unit, string) result)
  -> (t, string) result

(** Standalone ABI for an exact managed-capability admission. The invocation
    records the caller's selection and registered implementation revision, while
    [available_tools] contains only the declared implementation's dependencies.
    The private admission links both identities without rewriting persisted data. *)
val create_managed_standalone
  :  control:L.execution_control option
  -> execution:Managed_tool_registry.execution
  -> limits:Chatmd_shell_spec.Chatmd_script_spec.limits
  -> validate_work:(I.work -> (unit, string) result)
  -> (t, string) result

(** Moderator ABI for the same private managed admission contract. Does not
    acquire the moderator or authorize execution; the actor/manager owner must
    atomically commit its result and proposed state. *)
val create_managed
  :  control:L.execution_control option
  -> execution:Managed_tool_registry.execution
  -> limits:Chatmd_shell_spec.Chatmd_script_spec.limits
  -> validate_work:(I.work -> (unit, string) result)
  -> (t, string) result

val context : t -> L.value
val input : t -> L.value

(** Shared origin projection for invocation contexts and observation events. *)
val origin_value : I.origin -> L.value

(** Shared result validation for a returned standalone outcome or a moderator
    resolution. Checks JSON projection, success schema, error envelopes, owned
    Pending references and serialized size. Does not resolve or publish. *)
val decode_outcome
  :  ?control:L.execution_control
  -> t
  -> L.value
  -> (I.outcome, string) result

(** Add the v1 transactional resolution operation and JSON-only emit/timer
    adapters to the normal host registry. No tool implementation runs here. *)
val operations : R.op_def list -> R.op_def list

(** Normalize v1 buffered JSON emits for the legacy local-effect decoder. Rejects
    resolution outside a dispatched invocation. *)
val ordinary_effects : L.eff list -> (L.eff list, string) result

(** Validate a tagged JSON payload and wrap it in the v1 Internal_event envelope.
    Shared by emit/timer adapters and host event admission. *)
val internal_event : ?control:L.execution_control -> L.value -> (L.value, string) result

type failure =
  | Unhandled
  | Duplicate_resolution
  | Wrong_id
  | Invalid_output
  | Invalid_state
  | Suspended
  | Handler_failed
  | Session_ended

(** Bounded serializable state snapshot shared by all extensibility-v1 event
    phases. Limits must come from a validated declaration. An active [control]
    also reserves the snapshot conversion/serialization estimate before copying. *)
val snapshot_state
  :  ?control:L.execution_control
  -> limits:Chatmd_shell_spec.Chatmd_script_spec.limits
  -> L.value
  -> (Chatml.Chatml_value_codec.Snapshot.t, string) result

(** Run Tool_invoked on an already borrowed, serialized moderator runtime.
    Exactly one resolution is validated before the runtime commits state/effects.
    [prepare_commit] receives the prospective runtime state/queue/halt and
    normalized ordinary effects (resolution removed). It validates any host transaction, returning
    an infallible installer. No provider output or persistence is performed by
    this function itself. Failures leave buffered effects uncommitted. External
    work is never automatically retried or undone. Serializable moderator state
    is defensively copied and restored on failure; mutable globals are excluded.
    The caller owns serialization, cancellation and conversion to terminal
    failures. Pure evaluation interruption is not provided by task limits alone.

    [on_failure] receives a host classification, independent of any diagnostic
    text supplied by a script. It runs after failed execution has rolled back;
    the callback must not re-enter the owning moderator.

    [execution] is the runner whose control was installed in [runtime]. It
    bounds pure evaluation and task effects under the current lexical scope.
    [control] is the alternative for a caller that already opened that runner's
    scope around input/context projection; it must be the runtime's active
    control. Do not combine it with [execution] or [execution_context].
    Without either control path this adapter retains only legacy task-step limits.
    The v1 moderator manager opens its owned runner before projection. [execution_context]
    carries caller budget ancestry across host domain handoffs. Supplying that
    context without a controlled runner is rejected. *)
val run
  :  ?on_failure:(failure -> unit)
  -> ?control:L.execution_control
  -> ?execution:Chatml_execution.runner
  -> ?execution_context:Chatml_execution.context
  -> t
  -> runtime:R.session
  -> context:L.value
  -> prepare_commit:
       (resolved:I.t -> transaction:R.transaction -> (unit -> unit, string) result)
  -> (I.t, string) result
