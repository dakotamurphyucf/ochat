open! Core

(** Script Job operations borrowed from one physically admitted developer run.
    The sole lexical kernel and Background_job_operations grammar are shared
    with conversation scripts. This module retains no Session handle, job
    scheduler, document writer, result bytes or independent capacity ledger. *)
type t

(** Physical composition check for the selected native executor. *)
val issued_by_root : t -> Developer_root_owner.run -> bool

module Scope : sig
  type t

  (** Physically stage one request returned by the existing selected native
      managed callback. Its effect-entry policy has already checked the exact
      binding; this method records the returned ID in the same lexical Job
      scope and rolls it back on any failed post-stage check. *)
  val stage_background
    :  t
    -> request:Chat_response.Background_request.t
    -> checkpoint:(unit -> unit)
    -> (Agent_protocol.Id.Job.t, Agent_protocol.Error.t) result

  (** Install only the existing Job.* grammar while this exact lexical scope
      and its real root action or native callback are active. The returned
      config contains no durable capability; escaped handlers reject. *)
  val install
    :  ?control:Chatml.Chatml_lang.execution_control
    -> t
    -> Chatml_host_runtime.runtime_config
    -> Chatml_host_runtime.runtime_config

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
       (binding:Chat_response.Tool_capability.binding
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
    use the root's actual current selected registry. The existing selected
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
