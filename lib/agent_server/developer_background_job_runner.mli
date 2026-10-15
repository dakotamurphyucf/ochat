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
    | Unsupported_kind
  [@@deriving equal, sexp_of]
end

(** Bind the publisher and native executor to [root]'s actual Store, selected
    registry and original actor. The Result_root is derived inside [create]
    from the root's retained admitted handle; [blobs] must be the coordinator
    physically bound to that Store. [sw] is the owning run/service switch and
    outlives every joined worker and retained publisher selection. [managed]
    is the host's selected managed dispatcher, not an authority supplied by a
    job payload. [jobs] must be issued by this same physical root; the
    constructor installs it through the existing native with_job_service path
    so managed Tool callbacks can stage actual child jobs and use their selected
    Developer_tool_v1 Model.spawn admission. It also retains that same service
    for the bare Script's claimed Worker lexical scope. It is not optional. The native
    kernel still re-resolves the exact binding,
    permission, parent and original/current actor after waits. [authorize] and
    [prepare_output] are trusted existing host services, never authority
    inferred from a job ID. *)
val create
  :  sw:Eio.Switch.t
  -> root:Developer_root_owner.run
  -> store:Agent_store.Developer_run_store.t
  -> blobs:Agent_store.Blob_store.t
  -> publisher_limits:Agent_store.Developer_result_publisher.Limits.t
  -> storage:Agent_store.Developer_result_publisher.Storage.t
  -> env:Eio_unix.Stdenv.base
  -> managed:Agent_session.Native_tool_invocation.selected_managed_dispatch option
  -> jobs:Developer_script_job_service.t
  -> authorize:
       (Agent_protocol.Host_invocation.t
        -> Chat_response.Tool_capability.binding
        -> (unit, Agent_protocol.Error.t) result)
  -> prepare_output:
       (Openai.Responses.Tool_output.Output.t
        -> (Jsonaf.t, Agent_protocol.Error.t) result)
  -> (t, Agent_protocol.Error.t) result

(** Comparison only: this package was physically constructed for the exact
    retained Root instance. It grants no claim or dispatch authority. *)
val issued_by_root : t -> Developer_root_owner.run -> bool

(** Scheduling classification only, not execution authority. Decode the
    bounded saved payload from the authentic candidate. Tool and Script are
    runnable; v2 Agent.run artifacts remain unavailable until their physical
    installer exists, so the scheduler leaves those Queued without a claim.
    A malformed saved Async_tool payload is [Malformed_supported]: the same
    runner physically claims it, decodes again inside the Worker callback and
    persists Failed without a handler effect. Other job kinds and a valid but
    uninstalled v2 artifact remain unclaimed. A foreign or stale candidate
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
    reject an uninstalled v2 agent artifact before any effect; decode
    and re-admit its immutable payload with the root's policy; rebind its pinned
    selected capability after preparation; derive the invocation owner,
    parent_job/attempt/depth, input and deadline from that Claim and request.
    A copied job or unrelated Result_root cannot enter this path.

    The selected native executor is constructed from [root] inside this
    package and calls run_job_child with the actual active proof. It performs
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
