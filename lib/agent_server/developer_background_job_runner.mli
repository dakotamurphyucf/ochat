open! Core

(** One physical developer Async_tool runner. The scheduler offers an authentic
    Root.Job_candidate; this module obtains the saved request from the entered
    Store Claim and uses the existing Worker. Tool targets are installed here;
    a Script target can join this same physical package and result-selection
    path rather than adding another worker wrapper. It owns no second writer,
    capacity ledger, scheduler, Session handle or cancellation scope. *)
type t

module Availability : sig
  type t =
    | Runnable
    | Script_target_unavailable
    | Agent_artifact_installer_unavailable
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
    Developer_tool_v1 Model.spawn admission. It is not optional. The native
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

(** Scheduling classification only, not execution authority. Decode the
    bounded saved payload from the authentic candidate. Script targets and v2
    Agent.run artifacts are reported as unavailable until their physical
    installers exist; the scheduler leaves them Queued without claiming them.
    A malformed payload returns Error rather than an unavailable state, so it
    cannot loop silently as though an executor were merely absent. A foreign
    or stale candidate cannot execute: the physical claim path rechecks it.
    The later active Claim is decoded again before native execution. This
    projection never grants an effect or result. *)
val availability
  :  t
  -> Developer_root_owner.Job_candidate.t
  -> (Availability.t, Agent_protocol.Error.t) result

(** Require [Runnable], then delegate this exact candidate to
    Job_candidate.with_claimed. Saturated,
    not-due or stale work returns [Ok None] without a callback. Under the
    physically entered Claim: require Async_tool and the Tool request form;
    reject an uninstalled v2 agent artifact before any native effect; decode
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
    No result or dependency field comes from the caller.

    The Worker's post-join [select] path calls the physically bound result
    publisher exactly once for an Inline terminal state, wraps its frozen
    Inline/Artifact result in Worker.Selected, and acknowledges only an exact
    Store Installed receipt. An uncertain intent/Blob/terminal save retains
    the same publisher token and capacity; no handler callback, native effect
    or result selection is replayed. Cancellation and unexpected exceptions
    keep their original identity/backtrace while the Worker performs its
    protected exact-attempt cleanup. This method does not install a scheduler.

    A Script implementation will reuse this module's physical package and
    post-join selection helper through one authentic Worker entrypoint. Until
    installed, Script_target_unavailable is a non-claiming scheduling result. *)
val run_candidate
  :  t
  -> Developer_root_owner.Job_candidate.t
  -> (Agent_protocol.Developer_job.t option, Agent_protocol.Error.t) result
