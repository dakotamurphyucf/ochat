open! Core

(** Script Job operations borrowed from one physically admitted developer run.
    The sole lexical kernel and Background_job_operations grammar are shared
    with conversation scripts. This module retains no Session handle, job
    scheduler, document writer, result bytes or independent capacity ledger. *)
type t

(** Physical composition check for the selected native executor. *)
val issued_by_root : t -> Developer_root_owner.run -> bool

module Source_parent : sig
  (** Borrowed proof belonging to one live Root action, Native terminal or
      claimed Worker scope. The service creates it from its existing physical
      scope; source callbacks cannot construct it from a job ID or request.
      It expires with the enclosing scope and never escapes into a saved job. *)
  type t

  (** Bounded current actor/selection and exact physical parent check. This
      may yield for current Store/host evidence. The caller repeats it after
      source I/O and before returning an artifact; the lexical service repeats
      its own check before stage. No read grant follows from the artifact. *)
  val check
    :  t
    -> binding:Chat_response.Tool_capability.binding
    -> (unit, Agent_protocol.Error.t) result

  (** Comparison-only metadata derived from the checked physical parent.
      Native carries its actual Host invocation ID; Worker carries the entered
      Claim's job ID and attempt. It cannot replace [check]. *)
  val launch : t -> (Chat_response.Source_launch.t, Agent_protocol.Error.t) result
end

module Scope : sig
  type t

  (** Trusted selected native kernel calls this once after its authentic
      Managed.admit_owned succeeds and before invoking user code. The opaque
      owned execution must name this physical terminal, original target and
      Dispatching context. Its exact prepared declaration supplies the lexical
      dependency ceiling; every pin is re-resolved against the current root.
      The existing kernel may rebind only before any start, even a rolled-back
      one. This comparison change grants no capability. The native dispatch
      fingerprint remains the caller's original target selection. *)
  val admit_managed
    :  t
    -> Chat_response.Managed_tool_registry.owned_execution
    -> (unit, Agent_protocol.Error.t) result

  (** Physically stage one request returned by the existing selected native
      managed callback. Its effect-entry policy has already checked the exact
      binding; this method records the returned ID in the same lexical Job
      scope and rolls it back on any failed post-stage check. *)
  val stage_background
    :  t
    -> request:Chat_response.Background_request.t
    -> checkpoint:(unit -> unit)
    -> (Agent_protocol.Id.Job.t, Agent_protocol.Error.t) result

  (** Install the existing Job.* grammar and an expiring Model.spawn handler
      while this exact lexical scope and its real root action or native callback
      are active. The model handler uses the shared recipe grammar and the
      issuing physical Action/Native_terminal admission; it cannot acquire
      provider authority from a runtime config. Escaped handlers reject.
      This config alone leaves standalone asynchronous execution disabled. *)
  val install
    :  ?control:Chatml.Chatml_lang.execution_control
    -> t
    -> Chatml_host_runtime.runtime_config
    -> Chatml_host_runtime.runtime_config

  (** The selected managed installer is bound to the actual
      Managed_tool_registry.owned_prepared implementation by the native policy
      kernel, never to a caller-supplied target name. Enable only the closed
      Model_spawn filter when that implementation has Developer_tool_v1 and the
      physical root pins a provider selection. Otherwise return no async mode.
      In either case the installed handler repeats lexical and physical
      admission at effect entry. Both Single and Prepared consume this pair. *)
  val install_selected
    :  ?control:Chatml.Chatml_lang.execution_control
    -> t
    -> prepared:Chat_response.Extension_compiler.t
    -> Chatml_host_runtime.runtime_config
    -> Agent_session.Native_tool_invocation.managed_script_install

  (** Separate exact surviving Job.start IDs from ordinary effects using the
      existing Background_job_operations grammar. IDs must have been staged by
      this scope. The physical Action/Native_terminal still seals/publishes the
      selected subset. *)
  val split_starts
    :  t
    -> Chatml.Chatml_lang.eff list
    -> ( Agent_protocol.Id.Job.t list * Chatml.Chatml_lang.eff list
         , Agent_protocol.Error.t )
         result

  (** Pending(Job id) is valid only for an ID returned by this exact lexical
      invocation. Physical native policy still checks canonical outcome and
      that the same ID survives its selected subset. *)
  val validate_work
    :  t
    -> Agent_protocol.Invocation.work
    -> (unit, Agent_protocol.Error.t) result
end

(** Construct only from the actual physical owner and a result disclosure
    service issued by that same Store. [current_actor] is a trusted host
    accessor, sampled after yielding result reads; a supplied actor never
    replaces the root's original authenticated principal. [prepare_source]
    must use the admitted capture and authentic native implementation to
    attach a Tool's code artifact; it cannot be caller source or a fabricated
    conversation launch owner. The root's admitted resource policy is used,
    never a script-selected wider fallback. *)
val create
  :  root:Developer_root_owner.run
  -> store:Agent_store.Developer_run_store.t
  -> disclosure:Developer_result_disclosure.t
  -> env:Eio_unix.Stdenv.base
  -> current_actor:(unit -> Operator_authorization.t)
  -> prepare_source:
       (parent:Source_parent.t
        -> binding:Chat_response.Tool_capability.binding
        -> request:Chat_response.Background_request.t
        -> checkpoint:(unit -> unit)
        -> ( Chat_response.Background_request.Agent_artifact.t option
             , Agent_protocol.Error.t )
             result)
  -> (t, Agent_protocol.Error.t) result

(** Keep one lexical scope around the *whole* physical root action and its
    Store selection. The callback evaluates with [Scope.install] and returns
    effects; this function parses exact starts, calls Action.seal, prepares a
    one-shot lexical acknowledgement inside the still-live action, then lets
    with_root_action join children and durably settle its opaque selection.
    Only its successful physical return invokes the acknowledgement. On error
    or cancellation, per-ID cleanup consults the exact Action ledger: live
    unsealed work rolls back, while sealed/queued/uncertain work remains under
    the root's physical handoff. No acknowledgement follows an uncertain save
    and no callback/effect is replayed. [phase] is an Active checkpoint when
    selecting work; the root enforces its own closed/interrupted rules. *)
val with_root_action
  :  t
  -> phase:Agent_store.Developer_run_store.Checkpoint.phase
  -> (scope:Scope.t
      -> sw:Eio.Switch.t
      -> ('a * Chatml.Chatml_lang.eff list, Agent_protocol.Error.t) result)
  -> ('a * Chatml.Chatml_lang.eff list, Agent_protocol.Error.t) result

module Native_selection : sig
  type t

  (** The exact physically sealed selection, retained with its one-shot
      lexical acknowledgement. This is not an invocation completion or a
      result-read grant. *)
  val physical : t -> Developer_root_owner.Native_terminal.Selection.t
end

(** Borrow a lexical Job scope from one actual registered Native_terminal.
    This function checks physical terminal/root identity. A direct Child
    context requires the exact live [selected_borrowed] from its parent
    invocation; the parent's ID and narrowed capability ceiling are derived
    from that expiring proof and compared with the terminal context and root
    registry. Root and claimed-job children have no direct native borrow and
    normally use the root's actual current selected registry. [ceiling], when
    supplied for a claimed-job child, pins the admitted Tool request's exact
    subset: every binding is re-resolved in the current root selection and the
    terminal context must match its fingerprint. Job.start_tool/script cannot
    regain other root-selected tools after a wait. A direct Child cannot
    substitute [ceiling] for its expiring parent borrow. The existing selected
    invocation kernel still re-resolves its binding and checks direct-parent
    liveness at effect entry; this module repeats the borrowed ceiling after
    yielding request preparation and before physical staging.
    Developer_native_executor keeps its existing shared Function/Custom/
    managed policy, dispatch, permission, callback and completion pipeline.
    It calls [seal_native] from that pipeline's canonical outcome continuation
    and [acknowledge_native] only after Native_terminal.submit returns the
    durable terminal revision. A late failure leaves the exact sealed pair
    with Native_terminal; lexical cleanup cannot abort its selected jobs.
    This scope includes the protected terminal submission, not just the
    managed-handler callback, so acknowledgement never precedes durability. *)
val with_native_scope
  :  t
  -> terminal:Developer_root_owner.Native_terminal.t
  -> borrowed:Agent_session.Native_tool_invocation.selected_borrowed option
  -> ?ceiling:Chat_response.Tool_capability.t
  -> check_parent_now:(unit -> (unit, Agent_protocol.Error.t) result)
  -> (scope:Scope.t -> ('a, Agent_protocol.Error.t) result)
  -> ('a, Agent_protocol.Error.t) result

(** Seal an exact subset of this scope's staged IDs with the canonical native
    outcome. A prior seal can only be reused with the same physical terminal,
    outcome and IDs. Prepare lexical acknowledgement after the physical seal,
    then retain it in [Native_selection.t]; no publication occurs here. *)
val seal_native
  :  Scope.t
  -> terminal:Developer_root_owner.Native_terminal.t
  -> outcome:Agent_protocol.Invocation.outcome
  -> selected_jobs:Agent_protocol.Id.Job.t list
  -> (Native_selection.t, Agent_protocol.Error.t) result

(** Return only this scope's already frozen physical outcome/selection. The
    native policy's repeated completion path must reuse this same wrapper,
    never recreate an acknowledgement for an arbitrary matching ID list. *)
val frozen_native
  :  Scope.t
  -> terminal:Developer_root_owner.Native_terminal.t
  -> (Agent_protocol.Invocation.outcome * Native_selection.t) option

(** Trusted native executor calls this nonyielding method only after its exact
    Native_terminal.submit succeeded. The scope/selection must be identical
    and live; repetition rejects. A terminal save error or unknown result must
    not call it. Physical terminal custody, not this acknowledgement, decides
    whether jobs were published. *)
val acknowledge_native
  :  Scope.t
  -> Native_selection.t
  -> (unit, Agent_protocol.Error.t) result

(** Borrow the one lexical Job kernel around an entire physically claimed
    Worker Script-root operation, including its final Store transaction.
    [candidate] must come from this service's exact Root. The callback uses
    the registered Worker's child Switch; this function does not create a
    second worker, scheduler, result publisher or Job grammar. Its Scope
    stages through the new physical Worker_action, parses returned Job.start
    effects, seals the exact surviving IDs and prepares lexical acknowledgement
    before the Worker callback/children join. The Worker then chooses the
    result [select] once and atomically installs parent state plus selected
    children through its existing claimed Prepared/recovery path.

    The lexical scope remains live until that Worker returns. A proven
    Selected_installed disposition alone invokes its one-shot acknowledgement;
    an old-epoch omission of nonempty selected children returns a typed stale
    outcome after the parent terminal/result was durably recorded, without
    acknowledging or releasing any published child. An uncertain write never
    acknowledges; cleanup defers sealed/attempted reservations to the Worker.
    [active] is the same live Claim proof that a selected native child needs;
    the lexical Scope cannot manufacture or retain it. Callback exceptions
    and Eio cancellation preserve their original identity.
    Before entering the lexical scope, the saved payload of this exact
    Store-issued candidate is decoded under the root's admitted policy and its
    capability pins rebound to an exact subset of the current root registry.
    The scope and each later effect recheck that subset; extra root tools never
    become visible to this job. This is pure admission, not script compilation
    before the physical Claim. Bare Worker Script roots use One_off_v1 and do
    not install Model.spawn;
    the existing Root/Native Developer_tool_v1 path remains unchanged. *)
val with_claimed_worker
  :  t
  -> candidate:Developer_root_owner.Job_candidate.t
  -> ?on_capacity_decision:(Developer_job_worker.Capacity_decision.t -> unit)
  -> ?select:
       (claim:Agent_store.Developer_run_store.Claim.t
        -> proposed:Agent_protocol.Developer_job.State.t
        -> (Developer_job_worker.Selected.t option, Agent_protocol.Error.t) result)
  -> (active:Agent_session.Developer_job_scope.active
      -> scope:Scope.t
      -> sw:Eio.Switch.t
      -> ( Agent_protocol.Developer_job.State.t * Chatml.Chatml_lang.eff list
           , Agent_protocol.Error.t )
           result)
  -> (Agent_protocol.Developer_job.t option, Agent_protocol.Error.t) result
