open! Core

(** One actual daemon/embedded composition's configured execution policy owner.
    No timer, worker, independent permit/resource registry or activating lookup. *)
type t

(** Pure admission after the actual Factory exists; validates config again.
    [server_id] is the composition's actual Store ID; every entry check also
    goes through this Factory's real registry/Actor/Owner/Handle association.
    Register close on supplied actual switch and immediate Startup_cleanup
    adoption. No IO, target loading, initializer or credentials are acquired.

    The composition creates no host in Operator_only or when config is absent.
    Execute/On_demand differ only in existing selected-owner reachability;
    configuring an ID never selects it. Same Eio domain. [now] must not yield. *)
val create
  :  Recurrence_execution_configuration.t
  -> factory:Session_factory.t
  -> server_id:Agent_protocol.Id.Server.t
  -> sw:Eio.Switch.t
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> (t, Agent_protocol.Error.t) result

module Controller : sig
  (** Actual registered Factory controller + fresh original guarded proof. No
      constructor/decoder. Its guard retains this exact host/config row/expiry
      and actual entry association, never lifecycle/control/generation permission.
      Pausing does not cancel already-admitted work. The existing activation custody retains its
      existing lifecycle/source/control/first-effect obligations. *)
  type t

  (** Fresh selected original: Send_messages + View_session_transcript, with
      Create_sessions only when this controller has explicit captured permission.
      Never Administer_configuration or the controller's saved creator. *)
  val actor : t -> Operator_authorization.t

  (** Current configured read/write policy for this actual controller state.
      Nonyielding; no Actor RPC/Store IO/Owner lock. *)
  val authorize
    :  t
    -> Agent_session.Session_state.t
    -> (unit, Agent_protocol.Error.t) result

  (** Pass with [actor] to the EXISTING reserve_recurrence_child port. Requires
      explicit captured permission and this actual controller's Fresh_child
      definition. Actual Actor/Factory creation still checks original scope,
      source/control/lifecycle/grace/profile/native/admitted artifact and ACKs.
      Lower Factory retains/repeats this same policy through real creation. *)
  val authorize_creation
    :  t
    -> Agent_session.Session_state.t
    -> Agent_session.Recurrence_definition.t
    -> (unit, Agent_protocol.Error.t) result

  (** The configured automatic-settlement choice for this exact controller and
      definition target family/edge. No historical-only substitute on denial.
      Lower actual automatic/source/birth/control eligibility and target proof
      remain required; this DATA policy issues no execution permission. *)
  val automatic_policy
    :  t
    -> Agent_session.Session_state.t
    -> Agent_session.Recurrence_definition.t
    -> (bool, Agent_protocol.Error.t) result
end

(** Select only an actual already-registered entry of this Factory. Never calls
    Registry.load, Local_selection.select or ensure_loaded. A registered stopped
    owner can provide factual reconciliation; new work still needs lower gates. *)
val controller
  :  t
  -> Session_registry.entry
  -> (Controller.t, Agent_protocol.Error.t) result

module Execution : sig
  (** Exact actual controller/target/intent and configured target-family grant.
      Retains current original proofs; no constructor/decoder or proof rematch.
      These methods only forward to the SAME existing Recurrence_run_service;
      there is no alternate preparation, activation, classification or cleanup.
      Binding the immutable Intent here prevents a selected existing-target
      context being reused to bypass the separate captured-launch grant. *)
  type t

  val activate
    :  t
    -> attempt:Agent_session.Recurrence_execution_attempt.t
    -> (Recurrence_run_service.Outcome.t, Agent_protocol.Error.t) result

  val retry
    :  t
    -> observation:Agent_session.Session_actor.No_effects_observation.t
    -> previous:Agent_session.Recurrence_execution_attempt.t
    -> request:Agent_session.Run_admission_request.Headless.t
    -> (Recurrence_run_service.Attempt.t, Agent_protocol.Error.t) result

  (** Retry's actual Intent must equal this context; no override is accepted. *)
  val resume_retry
    :  t
    -> retry:Agent_session.Session_actor.Recurrence_retry.t
    -> (Recurrence_run_service.Attempt.t, Agent_protocol.Error.t) result

  val reconcile
    :  t
    -> attempt:Agent_session.Recurrence_execution_attempt.t
    -> (Recurrence_run_service.Reconciliation.t, Agent_protocol.Error.t) result
end

module Target : sig
  (** Actual acquired target and target-family grant, before source preview or
      Intent construction. Holds one original target proof and the same actual
      controller/entry/child association used by the eventual Execution. No
      constructor, decoder, resource owner or permission registry. *)
  type t

  (** Current configured Send_messages + View_session_transcript original.
      The proof retains its original absolute expiry and host/entry association;
      binding an Intent neither refreshes nor replaces it. *)
  val actor : t -> Operator_authorization.t

  (** Current nonyielding host target policy for effect-free source preview.
      Callers compose this with the original proof and actual source owners,
      repeating checks after yields. No runtime, source, lifecycle or execution
      authority is created by this policy check. *)
  val authorize
    :  t
    -> Agent_session.Session_state.t
    -> (unit, Agent_protocol.Error.t) result

  (** Bind only the exact selected family, actual entry/child and immutable
      Intent. Keeps the SAME target proof/grant; no reissuance, unbound service
      or family override. Actual service/Factory still validates request source,
      original policy, canonical CAS and all lower admission/activation custody. *)
  val execution
    :  t
    -> intent:Agent_session.Recurrence_execution_intent.t
    -> (Execution.t, Agent_protocol.Error.t) result
end

(** Target must be an exact configured local controller-target edge, including
    self only when explicitly listed. Both actual entries/Factory association
    are checked. Selection neither loads the target nor binds a proposed source;
    Target.execution later requires Existing_session/User_submission Single_turn
    with no child, preventing this grant from becoming captured-launch policy. *)
val existing_target
  :  Controller.t
  -> target:Session_registry.entry
  -> (Target.t, Agent_protocol.Error.t) result

(** Requires explicit captured permission, actual Factory-produced linked child,
    actual registered target and exact controller frontier reservation/association
    plus target's real Spec delegation locator. The opaque child's exact session
    and reference must match; reference DATA alone issues no authority. Actual
    reads occur outside Actor/Owner locks; later policies use pure current entry/
    config/link predicates, with no RPC. No runtime is warmed or source owner
    constructed. Target.execution later checks the exact child/Intent and actual
    explicit captured Single_turn or Workflow mode. *)
val captured_target
  :  Controller.t
  -> child:Session_factory.Recurrence_child.t
  -> target:Session_registry.entry
  -> (Target.t, Agent_protocol.Error.t) result

(** Nonblocking revocation of every proof from THIS host. Idempotent, same
    domain. Existing scheduler/Factory/runtime owners cancel/join their actual
    work before Actor/Store teardown; this policy owner has no private work to
    pretend to join. An unrelated host/context is not modified. *)
val close : t -> unit

(** Acquire only this actual selected Controller's exact configured local edge
    through the real Factory metadata owner. Configuration alone never selects a
    controller. The configured original principal/absolute expiry and real target
    provenance/capacity/policy remain required across recovery. No runtime warms. *)
val acquire_existing_target
  :  Controller.t
  -> session:Agent_protocol.Session_ref.t
  -> (Target.t, Agent_protocol.Error.t) result

(** The same Target original/policy through the actual Factory's joined immutable
    source preview; no Intent, runtime or executable authority is manufactured. *)
val preview_target_source
  :  Target.t
  -> (Session_factory.Recurrence_source_preview.t, Agent_protocol.Error.t) result

module Dispatch_occupancy : sig
  module Known : sig
    type t = private
      { occurrence : Agent_session.Recurrence_identity.Occurrence.t
      ; target : Agent_protocol.Session_ref.t
      }
  end

  module Unknown : sig
    (** An unreadable historical catalog conservatively covers every local target,
        including targets removed from this restart-bound configuration. *)
    type t = private
      { controller : Agent_protocol.Session_ref.t
      ; error : Agent_protocol.Error.t
      }
  end

  type t = private
    { known : Known.t list
    ; unknown : Unknown.t list
    }
end

(** Read actual immutable configured-controller states, including indexed and
    unselected owners, through the existing Factory/Registry read lifetime.
    No load, selection or effects. Per-controller failure retains its Error and
    conservative all-local-target uncertainty rather than reporting empty occupancy.
    Factual reconciliation remains available while new overlapping work defers.
    Host closure/absolute expiry denies globally.
    Actual target provenance/source/authority gates remain required before work. *)
val pending_occupancy : t -> (Dispatch_occupancy.t, Agent_protocol.Error.t) result

(** Obtain the genuine retained captured child through factual metadata recovery
    and its actual reciprocal stored association. No creation permit or new
    Enabled/grace qualification; current original visibility/captured-family
    policy still applies. Missing facts never create a replacement. *)
val acquire_captured_target
  :  Controller.t
  -> occurrence:Agent_session.Recurrence_identity.Occurrence.t
  -> (Target.t, Agent_protocol.Error.t) result
