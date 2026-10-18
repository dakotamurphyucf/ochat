open! Core

(** One physical developer Async_tool runner. The scheduler offers an authentic
    Root.Job_candidate; this module obtains the saved request from the entered
    Store Claim and uses the existing Worker. Tool and Script targets share this
    physical package and result-selection path. It owns no second writer,
    capacity ledger, scheduler, Session handle or cancellation scope. *)
type t

module Availability : sig
  type t =
    | Runnable
    | Malformed_supported
    | Agent_artifact_installer_unavailable
    | Issuer_source_installer_unavailable
    | Unsupported_kind
  [@@deriving equal, sexp_of]
end

(** Bind the native executor and the ONE Root-owned result publisher to this
    exact physical Root, selected registry and original actor. [results] is
    issued by [Developer_root_owner.Job_result.bind] once for the run and is
    also borrowed by any installed Model runner; this package cannot create a
    second pending table or independently renew its result cap. [managed_definition]
    is the exact definition from this Root's trusted live-resource package, not an authority supplied by a
    job payload. [agent_runtime] borrows the assembler's already constructed
    runtime for the same physical native implementations and switch lifetime;
    it is never reconstructed or owned by this runner. At construction, every
    binding in [root]'s native ceiling must resolve to the exact same live
    runtime reference and physical implementation (managed bindings remain
    owned by the installed definition). The finite root job-policy array ceiling bounds this
    constructor comparison before selected-reference allocation. The separate
    registered-source traversal is bounded at dispatch under the same ceiling. A
    missing runtime permits ordinary Tool/Script work, but a valid v2 saved
    Agent.run artifact remains installer-unavailable and unclaimed. A matching runtime is still
    only a borrowed comparison source: the entered Claim, selected binding,
    original/current actor and registered source are rechecked after waits at
    the effect boundary. [jobs] must be issued by this same physical root; the
    constructor installs it through the existing native with_job_service path
    so managed Tool callbacks can stage actual child jobs and use their selected
    Developer_tool_v1 Model.spawn admission. It also retains that same service
    for the bare Script's claimed Worker lexical scope. It is not optional. The native
    kernel still re-resolves the exact binding,
    permission, parent and original/current actor after waits.
    [authorize_for_runtime] is the one Live-bound native policy factory: this
    runner supplies the actual root or restored Claim runtime when installed.
    [None] is the existing Script-only package; selected-profile mode denies
    it, while an explicit trusted callback can preserve that historical route.
    The factory verifies a Shell exemption against an exact installed runtime. A saved
    request cannot select a runtime or profile by name. [prepare_output] is
    a trusted existing host service, never authority
    inferred from a job ID. *)
val create
  :  root:Developer_root_owner.run
  -> results:Developer_root_owner.Job_result.t
  -> env:Eio_unix.Stdenv.base
  -> managed_definition:Chat_response.Managed_tool_registry.t option
  -> agent_runtime:Chat_response.Agent_runtime.t option
  -> jobs:Developer_script_job_service.t
  -> authorize_for_runtime:
       (Chat_response.Agent_runtime.t option
        -> Developer_native_permission.Mode.authorizer)
  -> prepare_output:
       (Openai.Responses.Tool_output.Output.t
        -> (Jsonaf.t, Agent_protocol.Error.t) result)
  -> (t, Agent_protocol.Error.t) result

(** Comparison only: this package was physically constructed for the exact
    retained Root instance. It grants no claim or dispatch authority. *)
val issued_by_root : t -> Developer_root_owner.run -> bool

(** Pair one process-local approval owner with this same physical Root. The
    entered Tool or Script Worker lends its actual Claim only during the
    callback; the owner cannot be invoked from a candidate or saved payload. *)
val with_approval_owner
  :  t
  -> Developer_approval_owner.t
  -> (t, Agent_protocol.Error.t) result

(** Scheduling classification only, not execution authority. Decode the
    bounded saved payload from the authentic candidate. Tool and Script are
    runnable, including Tools with saved v2 Agent.run references when the
    actual runtime is installed. Without that runtime, a valid saved artifact
    remains [Agent_artifact_installer_unavailable] and unclaimed. A v3 child
    issuer remains [Issuer_source_installer_unavailable] and Queued when the
    paired Job service has no installed Host issuer package. With one installed
    native/managed Tool and Script work is scheduling-runnable, but exact
    issuer restoration occurs only inside the physical Claim. A v3 Tool with a
    distinct saved Agent target is scheduling-runnable only with the paired
    Host issuer service installed. The physical Claim then prepares the exact
    issuer native runtime and admits the target against that same runtime;
    it cannot try the legacy Root Agent runtime. Runnable
    classification grants no source authority: the entered Worker re-decodes
    its Claim, and the actual selected native callback restores the exact pin.
    A malformed saved Async_tool payload is [Malformed_supported]: the same
    runner physically claims it, decodes again inside the Worker callback and
    persists Failed without a handler effect. Other job kinds remain unclaimed.
    A foreign or stale candidate
    cannot execute: the physical claim path rechecks it.
    The later active Claim is decoded again before either execution. This
    projection never grants an effect or result. *)
val availability
  :  t
  -> Developer_root_owner.Job_candidate.t
  -> (Availability.t, Agent_protocol.Error.t) result

(** Require [Runnable] or [Malformed_supported], then delegate this exact
    candidate to the existing
    physical Job_candidate.with_claimed (Tool) or
    Developer_script_job_service.with_claimed_worker (Script). Saturated,
    not-due or stale work returns [Ok None] without a callback. Under the
    physically entered Claim: require Async_tool and the admitted target form;
    require an exact selected ordinary Agent.run source binding for a v2
    artifact before any effect; decode
    and re-admit its immutable payload with the root's policy; rebind its pinned
    selected capability after preparation; derive the invocation owner,
    parent_job/attempt/depth, input and deadline from that Claim and request.
    A copied job or unrelated Result_root cannot enter this path.

    For supported v3 native/managed Tool and Script, both branches borrow the SAME installed Host
    issuer package from [jobs], re-decode the actual Claim payload, restore
    its source and selected native/managed runtime, then bind pins against
    that runtime before any handler effect. Restoration polls the same host
    budget for cancellation and deadline without consuming interpreter fuel;
    bounded compilation/custody work and later execution retain their own
    accounting. A Tool's optional agent_artifact is restored afterward as a
    distinct target under its existing exact binding checks against the SAME
    newly prepared claimed issuer runtime, never [agent_runtime] from [create].
    The selected native executor is constructed from [root]
    inside this package and calls the restricted claimed child path with the
    actual active proof. It performs
    durable Dispatching, selected policy/permission, final root/Claim/deadline
    checks and exact terminal submission. A completed native outcome becomes
    the matching Inline Completion in a proposed terminal job state; an owned
    Pending(Job id) becomes Waiting_completion with the same invocation,
    selected work, request deadline/bounds and completion schema from the
    exact selected Managed_tool_registry definition (None for native).
    Managed admission uses this same immutable definition and exact binding;
    its code-rebind path is required to preserve declared schemas. A changed
    binding fails the existing native admission before a handler effect.
    Pending(Subscription) and unselected/unowned Job work are typed failures.
    No result or dependency field comes from the caller. For a bare Script,
    One host budget starts before [Background_request.prepare] recompiles its
    exact saved One_off_v1 source after Claim entry and rebinds the saved tool
    subset after the compiler wait. The interpreter narrows the remaining
    deadline and inherits that budget; a malformed saved Async_tool request
    fails inside the physical Worker callback before compilation or effects.
    The interpreter uses the one live lexical scope to install Job operations
    and selected native Tool.call effects. Each Tool.call uses [run_job_child]
    with the same [active] proof and that pinned subset; a wider root registry
    cannot add tools. Normal script failure becomes a bounded Inline Failed
    completion. Expiry, including expiry before compilation, admits its entire
    Expired envelope against the saved output policy. Task.catch rolls back
    discarded local Job.start effects; the
    lexical owner seals the exact surviving effects and the Worker writes the
    parent and children
    in one Store Prepared. A bare Script has no Model.spawn builtin; a managed
    Developer_tool_v1 child keeps its independent authorized Model path.

    The Worker's post-join [select] path calls the physically bound result
    publisher exactly once for an Inline terminal state, wraps its frozen
    Inline/Artifact result in Worker.Selected, and acknowledges only an exact
    Store Installed receipt. For Script, the same Store-selected handoff also
    includes its exact surviving child rows; an old-epoch omission withholds
    the lexical acknowledgement and releases only proven unpublished capacity.
    An uncertain intent/Blob/terminal save retains the same publisher token
    and capacity; no callback, native effect or result selection is replayed.
    Cancellation and unexpected exceptions keep their original
    identity/backtrace while the Worker performs its protected exact-attempt
    cleanup. This method does not install a scheduler.
    [on_capacity_decision] is a trusted nonyielding one-shot scheduling
    observer. Worker admission reports the real capacity decision; early
    decode/currentness exit or exception invokes the same observer as fallback.
    It grants no Claim, effect, or result authority. *)
val run_candidate
  :  t
  -> ?on_capacity_decision:(Developer_job_worker.Capacity_decision.t -> unit)
  -> Developer_root_owner.Job_candidate.t
  -> (Agent_protocol.Developer_job.t option, Agent_protocol.Error.t) result
