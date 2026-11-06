open! Core

(** Process-local runtime ownership for one durable session. *)

type t

exception Cleanup_failed of Agent_protocol.Error.t

module Scope_outcome : sig
  module Cleanup : sig
    module Failure : sig
      type t =
        | Protocol of Agent_protocol.Error.t
        | Raised of exn * Stdlib.Printexc.raw_backtrace
    end

    (** Immutable DATA from the actual finalizers. No public constructor can
        assert that a scope acquired or released resources. *)
    type t

    type status =
      | Never_acquired
      | Joined
      | Failed of Failure.t * Failure.t list

    val status : t -> status

    (** Combine actual captured reports, retaining the ordered first failure and
        every secondary failure. This cannot certify a missing required scope;
        callers must record that absence as a failure. *)
    val combine : t -> t -> t

    (** Negative DATA only. This cannot assert acquisition or a successful join. *)
    val failed : Failure.t -> t
  end

  type 'a primary =
    | Returned of ('a, Agent_protocol.Error.t) result
    | Raised of exn * Stdlib.Printexc.raw_backtrace

  (** The callback outcome and all actually acquired cleanup outcomes remain
      separate, including when a primary failure would conceal cleanup failure.
      Neither this value nor a successful join grants source completion. *)
  type 'a t

  val primary : 'a t -> 'a primary
  val cleanup : 'a t -> Cleanup.t

  (** Restore the ordinary scope contract: a callback Error or exception remains
      primary; cleanup failure is reported after successful work. Exceptions and
      cancellation retain their original raw backtrace. *)
  val resume : 'a t -> ('a, Agent_protocol.Error.t) result
end

val create
  :  actor:Agent_session.Session_actor.t
  -> initial:Agent_session.Runtime_builder.t option
  -> build:(unit -> (Agent_session.Runtime_builder.t, Agent_protocol.Error.t) result)
  -> t

(** Construct an owner whose accepted stop retirement must first complete a host
    dependency barrier. The callback runs outside the owner mutex after new
    admissions are excluded, before cancelling/releasing existing leases. A
    returned error retains resources and is shared by concurrent unload callers.
    The callback must not unload this same owner. For these owners [close] only
    excludes new admissions; [close_and_wait] joins the dependency barrier before
    cancelling leases or retiring resources. [closing=true] distinguishes permanent
    owner closure from reusable unload/early stop preparation. The barrier must
    handle shutdown and explicit stop without confusing their durable intent. *)
val create_with_unload
  :  before_unload:(closing:bool -> (unit, Agent_protocol.Error.t) result)
  -> actor:Agent_session.Session_actor.t
  -> initial:Agent_session.Runtime_builder.t option
  -> build:(unit -> (Agent_session.Runtime_builder.t, Agent_protocol.Error.t) result)
  -> t

(** Actual shared-host constructor with explicit run preparation custody. The
    build callback propagates [Some] to its actor-owned initialization/reservation
    calls. Ordinary runtime loading passes [None]; no ambient owner is inferred. *)
val create_with_run_unload
  :  before_unload:(closing:bool -> (unit, Agent_protocol.Error.t) result)
  -> actor:Agent_session.Session_actor.t
  -> initial:Agent_session.Runtime_builder.t option
  -> build:
       (Agent_session.Run_preparation.t option
        -> (Agent_session.Runtime_builder.t, Agent_protocol.Error.t) result)
  -> t

val is_loaded : t -> bool

(** Callback-free negative scheduling snapshot under the existing Owner mutex.
    False defers before freezing a request; true neither reserves the source lane
    nor grants execution authority across a yield. The actual Factory stage and
    Actor guards remain mandatory. Same creating Eio domain. *)
val source_lane_idle : t -> bool

(** Deliberate physical association, not equality of session IDs. Call in this
    owner's Eio domain; no Actor request, callback, mutation or authority grant. *)
val belongs_to_actor : t -> Agent_session.Session_actor.t -> bool

(** Shared constructor outcome. Callback completion carries factual closed data
    instead of a runtime value and skips every runtime consumer. *)
module Construction_result : sig
  (** Factual callback completion, never runtime installation, execution
      permission or handoff. Callback_completed skips the supplied runtime consumer;
      its closed data distinguishes control from genuine terminal settlement. *)
  type 'a t =
    | Built of 'a
    | Callback_completed of Agent_session.Session_actor.Goal_callback_completion.t
end

(** Existing Factory constructor for a source that has already passed actual
    retained-artifact verification. Its opaque context selects the exact initial
    admitted continuation or existing Goal episode owner; neither is coerced into
    the other. No raw IDs, caller guard or alternate executor replace it. Called
    only inside the consumed source stage, after the first-effect fence, and must
    preserve selected source/parent resource ownership. *)
type deferred_builder =
  source:Agent_session.Runtime_builder.Source.t
  -> construction:Agent_session.Session_actor.Runtime_construction.t
  -> ( Agent_session.Runtime_builder.t Construction_result.t
       , Agent_protocol.Error.t )
       result

(** Actual factory constructor retaining an explicitly supplied original actor
    through initialization. The proof option is supplied only by authenticated
    loading or genuine Run preparation, never by the stored session creator. *)
val create_with_authenticated_run_unload
  :  before_unload:(closing:bool -> (unit, Agent_protocol.Error.t) result)
  -> actor:Agent_session.Session_actor.t
  -> initial:Agent_session.Runtime_builder.t option
  -> build:
       (original_actor:Operator_authorization.t option
        -> Agent_session.Run_preparation.t option
        -> (Agent_session.Runtime_builder.t, Agent_protocol.Error.t) result)
  -> t

module Prepared_delegated_load : sig
  type t

  val binding : t -> Agent_session.Session_actor.Delegated_load.Source_binding.t
  val check_basis : t -> (unit, Agent_protocol.Error.t) result
end

module Delegated_load_source : sig
  (** Actual same-stage Factory source selection. No caller-created handle,
      public issuer getter, replacement or deserialization. *)
  type t

  val prepare
    :  t
    -> source:Agent_session.Runtime_builder.Source.t
    -> (Prepared_delegated_load.t, Agent_protocol.Error.t) result
end

module Delegated_load_retention : sig
  type t =
    | Keep_loaded
    | Release_if_stopped
end

(** Concrete Factory-owned driver consuming the already-reserved SAME source
    stage, never reserving a second stage. Runs outside Owner mutex. Returned
    summary is factual post-Init state, not execution or activation authority. *)
type delegated_loader =
  source:Delegated_load_source.t
  -> retention:Delegated_load_retention.t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

(** Same source-stage lease and cleanup as admitted construction. Capture the
    actual Actor preparation before pending cancellation; close its binding
    before releasing the lease; join acquired resources before factual end and
    stage clear. No NoEffects evidence or callback permission is issued. *)
val with_delegated_load_source_reported
  :  t
  -> source_owner:Agent_session.Runtime_source_owner.t
  -> custody:Agent_session.Session_actor.Delegated_load.Custody.t
  -> (Delegated_load_source.t -> ('a, Agent_protocol.Error.t) result)
  -> 'a Scope_outcome.t

(** Consume genuine ordinary child load custody through the SAME deferred
    constructor/Init/graph/install core. Ready/no-original reconstruction retains
    actual Installed_resume callback custody after this stage joins. Ordinary
    installation never contributes activation-handoff history. Conditional
    release samples actual desired state after Init, captures its summary before
    cleanup and joins stopped retirement before source-stage clear. *)
val load_delegated_runtime
  :  t
  -> prepared:Prepared_delegated_load.t
  -> retention:Delegated_load_retention.t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

(** The actual verified-source Factory supplies this additional cold constructor
    and its hidden issuer key. This owner captures its own immutable issuer
    association, independently of its Actor. Ordinary constructors have no such
    association; copying an Actor or its read-only identity cannot authenticate a
    second runtime owner. The Factory-owned delegated runtime driver privately
    retains the SAME supplied issuer. Some delegated_load is supplied only for
    genuine delegated children; no saved creator or new Goal/Run is inferred.
    Ordinary loading of other owners retains its existing authenticated builder.
    No issuer getter or replacement exists. *)
val create_with_deferred_run_unload
  :  source_owner:Agent_session.Runtime_source_owner.t
  -> before_unload:(closing:bool -> (unit, Agent_protocol.Error.t) result)
  -> actor:Agent_session.Session_actor.t
  -> initial:Agent_session.Runtime_builder.t option
  -> build:
       (original_actor:Operator_authorization.t option
        -> Agent_session.Run_preparation.t option
        -> (Agent_session.Runtime_builder.t, Agent_protocol.Error.t) result)
  -> build_deferred:deferred_builder
  -> delegated_load:delegated_loader option
  -> t

val ensure_loaded : t -> (unit, Agent_protocol.Error.t) result

(** Current original actor is passed to a supporting initializer only. Existing
    runtime ownership/policy still controls loading; this supplies no source,
    execution or instruction permission. The authenticated Factory builder retains
    this actor through cold legacy ChatML construction and its [Session_resume]
    callback. Host builders made without the authenticated constructor reject a
    supplied actor on cold load rather than silently discarding it. An already
    loaded runtime is checked without replaying its constructor. *)
val ensure_loaded_authenticated
  :  t
  -> actor:Operator_authorization.t
  -> (unit, Agent_protocol.Error.t) result

val unload : t -> (unit, Agent_protocol.Error.t) result

(** Begin and join dependency cancellation after a durable stop request, even
    while this owner's foreground operation is still cleaning up. Does not retire
    this runtime or acquire its mutex. Call outside actor commit callbacks and
    owner locks. The host barrier must tolerate repeated/concurrent calls;
    [unload_and_wait] rechecks it before retirement. *)
val prepare_dependency_stop : t -> (unit, Agent_protocol.Error.t) result

(** Join descendant cleanup before an externally owned resource scope closes.
    Does not enter this owner's mutex or close its runtime. The host barrier sees
    [closing=true], preserving running intent during shutdown while joining owned
    descendants. Suitable for a delegated scope's revocation callback, which must
    never recursively close its own runtime. *)
val prepare_dependency_close : t -> (unit, Agent_protocol.Error.t) result

(** After committing a session stop, exclude new runtime admission, cancel and
    join foreground terminal checkpoints and existing execution leases, then
    detach the worker. Foreground cancellation precedes the dependency barrier.
    Permanent close preserves durable running intent and deferred messages while
    preventing any new foreground admission. Delegation resource
    borrows survive ordinary stop; workspace cleanup must still check references.
    The non-waiting [unload] used before reset rejects every retained borrow. Waits
    outside the owner mutex so worker finalizers can finish. Keep the actor alive
    through this call, and call from outside a retained background callback.
    Accepted-stop cleanup survives caller cancellation; the
    owner remains reusable for a later authorized session start. Concurrent stop
    cleanup requests join the same retirement and observe its success or failure;
    they do not race into a spurious Conflict or close the runtime twice. Cleanup
    remains callable after [close], including retry after a dependency failure;
    this never reopens runtime admission. Failed reusable retirement also keeps
    execution admission closed until this cleanup is explicitly retried; retained
    resources do not imply that [ensure_loaded] may resume execution. *)
val unload_and_wait : t -> (unit, Agent_protocol.Error.t) result

(** Run maintenance only with no installed runtime or background lease, excluding
    reload until the callback finishes. [None] defers; this does not unload an
    active runtime. Acquire this owner before the actor checkpoint. Exceptions
    and cancellation release ownership without poisoning subsequent operations. *)
val with_unloaded
  :  t
  -> (unit -> ('a, Agent_protocol.Error.t) result)
  -> ('a option, Agent_protocol.Error.t) result

(** Atomically reserve eviction of an already inactive actor only when its owner
    has no runtime, lease or unload in progress. On success admission is permanently
    closed; the caller must finish normal entry closure outside the owner mutex.
    A failed reservation leaves the owner unchanged. Actor inactivity must be
    established separately by the registry. *)
val reserve_inactive_close : t -> bool

(** Retain one loaded runtime for a background worker without holding the owner
    mutex while [f] executes. Independent workers may run concurrently or await
    each other's results. Loading remains serialized; temporary unload and
    administration return Conflict while any worker owns the runtime.

    The callback must use the actual actor/job invocation and moderator services;
    retaining this runtime grants no session or tool authority. Join all work
    within [f] and do not retain the runtime after return. Callback exceptions and caller
    cancellation release ownership without poisoning the mutex.

    [close] rejects new work and cancels callback contexts. Retirement waits for
    the final callback to unwind, so cleanup cannot close resources still in use.
    This is lifetime ownership, not generic scheduler dispatch or job admission. *)
val with_background_runtime
  :  t
  -> (Agent_session.Runtime_builder.t -> ('a, Agent_protocol.Error.t) result)
  -> ('a, Agent_protocol.Error.t) result

(** Retain resources for an actual executing tool/event owner. During held
    authored startup, the Actor must select the exact live callback or descendant
    borrow before this lease can use the installed runtime. Supplied identifiers
    alone grant no access. The ordinary supervisor remains excluded until final
    startup handoff. Outside that staged lifetime this uses the normal background
    lease. This is resource retention, not permission to execute an effect. *)
val with_execution_runtime
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> (Agent_session.Runtime_builder.t -> ('a, Agent_protocol.Error.t) result)
  -> ('a, Agent_protocol.Error.t) result

(** Borrow only an already installed runtime, admitted under the existing owner
    gate and cancellable/joined by stop or close. Closed, unloading, retiring and
    unloaded owners reject; this never invokes the builder or initializer.
    Installed execution checks run outside the mutex under the same physical
    cancellation lease, followed by a pure installed-owner check. The callback runs outside the gate;
    no runtime, native resource or callback may escape its lifetime. Error,
    cancellation and exceptions release the same existing lease. This grants no
    source, session or tool authority. *)
val with_loaded_runtime
  :  t
  -> (Agent_session.Runtime_builder.t -> ('a, Agent_protocol.Error.t) result)
  -> ('a, Agent_protocol.Error.t) result

(** Fixed Factory source restoration with the actual private issuer key. It must
    match both this actual owner's and its Actor's immutable identities. Borrows only an already
    loaded runtime's already-realized native capabilities, without constructing
    resources or exposing its worker/model execution closures. Ordinary owner
    retirement/cancellation/execution checks remain mandatory. The callback is
    the Factory's static source selection, never an execution escape. Its real
    existing lease finalizer is reported separately from its primary outcome;
    a unit lease-finished signal alone is not cleanup success. *)
val with_loaded_source_capabilities_reported
  :  t
  -> source_owner:Agent_session.Runtime_source_owner.t
  -> (Chat_response.Tool_capability.t -> ('a, Agent_protocol.Error.t) result)
  -> 'a Scope_outcome.t

(** Same existing lifetime lease with explicitly attributable constructor
    commits. The actor must have issued [preparation] first. Its actual current
    execution phase is checked outside the owner mutex before ordinary exclusion,
    lease admission or native construction; a pending retry cannot enter through
    an escaped preparation. This check grants no permission across a yield and
    every later constructor/lease guard remains in force. Every exit releases
    the runtime lease, including cancellation and unexpected exceptions. *)
val with_prepared_run_runtime
  :  t
  -> preparation:Agent_session.Run_preparation.t
  -> (Agent_session.Runtime_builder.t -> ('a, Agent_protocol.Error.t) result)
  -> ('a, Agent_protocol.Error.t) result

module Prepared : sig
  (** Exclusive staged lifetime under this actual runtime owner. It contains the
      actor's separate source binding; neither value alone permits execution.
      No runtime/native setup or initializer has run by acquiring this lease. *)
  type t

  val binding : t -> Agent_session.Session_actor.Source_binding.t

  (** Non-yielding callback-free check in the creating Eio domain. Checks the
      exact owner/stage/runtime resource and actor source binding. A consumed
      startup phase never passes this old staged check. *)
  val check_basis : t -> (unit, Agent_protocol.Error.t) result
end

(** Reserve a staged source lifetime without loading a runtime. Binding happens
    outside the owner mutex; stop/close/replacement irrevocably closes it before
    cancellation or joins. Ordinary eager loading rejects until this exact
    startup's acknowledged input handoff; its same physical runtime may then
    serve normal resource leases while lexical cleanup finishes. Competing
    source stages remain excluded. A successful completed retirement permits a new
    cold stage once resources are unloaded; unfinished or failed retirement does
    not. [f] cannot retain Prepared or its borrowed runtime lifetime after return.
    The caller still ends its original Run preparation.
    Errors/cancellation release the existing lease and preserve original failures.
    Acquisition is not held Run admission or permission to start effects. *)
val with_prepared_source
  :  t
  -> preparation:Agent_session.Run_preparation.t
  -> source:Agent_session.Runtime_builder.Source.t
  -> (Prepared.t -> ('a, Agent_protocol.Error.t) result)
  -> ('a, Agent_protocol.Error.t) result

(** The same source-stage lease and finalizers, retaining callback Error/exn and
    the actual inner lease release plus outer stage retirement/clear outcomes
    separately. The report returns only after every acquired target finalizer
    has joined. It does not end the Actor preparation, complete a source or prove
    absence of effects. Never-acquired is derived from actual admission. *)
val with_prepared_source_reported
  :  t
  -> preparation:Agent_session.Run_preparation.t
  -> source:Agent_session.Runtime_builder.Source.t
  -> (Prepared.t -> ('a, Agent_protocol.Error.t) result)
  -> 'a Scope_outcome.t

module Deferred_source : sig
  (** The same exclusive source stage, reserved before the actual Actor issues
      its NEW deferred preparation. No second lease or preparation registry.
      It belongs to this lexical CPS and issuing Eio domain. *)
  type t

  val preparation : t -> Agent_session.Run_preparation.Deferred.t

  (** The actual retry issued alongside [preparation], if this is a retry lane.
      Its presence grants neither controller publication nor target admission. *)
  val retry : t -> Agent_session.Session_actor.Recurrence_retry.t option

  (** Bind the selected source once, while retaining this exact reserved stage.
      Selection is consumed before the Actor call, including rejected or raised
      binding. No retry/fallback or retained Prepared after callback return. *)
  val prepare
    :  t
    -> source:Agent_session.Runtime_builder.Source.t
    -> (Prepared.t, Agent_protocol.Error.t) result

  module Decision : sig
    type 'a t =
      | Retained of Agent_protocol.Run_receipt.t
      | Fresh of 'a
  end

  module Report : sig
    type 'a t

    (** Actual accepted Deferred, also retained if pending cancellation prevented
        the callback from running. The Factory must close it through its Actor
        after joined cleanup; callback capture alone cannot cover that cut. *)
    val deferred : 'a t -> Agent_session.Run_preparation.Deferred.t option

    (** Captured together with the actual Deferred before pending cancellation,
        even if source selection or the callback never starts. *)
    val retry : 'a t -> Agent_session.Session_actor.Recurrence_retry.t option

    val outcome : 'a t -> 'a Decision.t Scope_outcome.t
  end
end

(** Match the hidden Factory key against both this owner's and its Actor's
    captured identities, then reserve this owner's existing source stage
    under its mutex before deferred Actor issuance. Current ordinary leases
    reject reservation; all new runtime entry remains excluded continuously.
    Actor issuance runs outside the owner mutex, and its actual ACK is captured
    before pending cancellation. Retained DATA never invokes [f]; Fresh invokes
    it once. Reports actual target cleanup without replacing the primary outcome
    or closing the Actor preparation. Factory owns the later exact close and
    private source-completion issuance, after any parent cleanup inside [f]. *)
val with_effect_deferred_source_reported
  :  t
  -> source_owner:Agent_session.Runtime_source_owner.t
  -> actor:Operator_authorization.t
  -> authorize:(Agent_session.Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> request:Agent_session.Run_admission_request.Headless.t
  -> (Deferred_source.t -> ('a, Agent_protocol.Error.t) result)
  -> 'a Deferred_source.Report.t

(** Reserve the existing source stage before the selected callback command's
    durable claim and initial preparation. The Actor consumes that same retained
    original once; no operator is reconstructed from command data. The report
    retains the actual preparation through cancellation for joined Factory cleanup. *)
val with_native_goal_start_source_reported
  :  t
  -> source_owner:Agent_session.Runtime_source_owner.t
  -> selection:Agent_session.Session_actor.Goal_native_start.Selection.t
  -> (Deferred_source.t -> ('a, Agent_protocol.Error.t) result)
  -> 'a Deferred_source.Report.t

(** Reserve the same exclusive source stage before consuming the exact old
    observation into one actual retry and its next Deferred. Source binding and
    all finalizers use the same one-shot stage as initial issuance. The Factory
    must factually close the reported Deferred after joined target/parent cleanup;
    this report alone cannot make the retry resumable or admission-ready. *)
val with_recurrence_retry_source_reported
  :  t
  -> source_owner:Agent_session.Runtime_source_owner.t
  -> observation:Agent_session.Session_actor.No_effects_observation.t
  -> actor:Operator_authorization.t
  -> authorize:(Agent_session.Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> intent:Agent_session.Recurrence_execution_intent.t
  -> previous:Agent_session.Recurrence_execution_attempt.t
  -> request:Agent_session.Run_admission_request.Headless.t
  -> (Deferred_source.t -> ('a, Agent_protocol.Error.t) result)
  -> 'a Deferred_source.Report.t

(** Re-enter this same source stage only through the same opaque retry's actual
    joined pre-controller-write closure. The Actor retains the frozen request;
    callers cannot replace its key, ordinal, principal, CAS or digest. *)
val resume_recurrence_retry_source_reported
  :  t
  -> source_owner:Agent_session.Runtime_source_owner.t
  -> retry:Agent_session.Session_actor.Recurrence_retry.t
  -> actor:Operator_authorization.t
  -> authorize:(Agent_session.Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> (Deferred_source.t -> ('a, Agent_protocol.Error.t) result)
  -> 'a Deferred_source.Report.t

(** Consume this exact Prepared/Held/Checked link into an actual scoped
    constructor or loaded-runtime reuse. The old staged owner is not widened.
    This may construct native resources/content and execute startup after the
    joint effect fence. Loaded reuse does not reinitialize. [f] runs outside the
    owner mutex and cannot retain the runtime/startup owner after return.
    This API is not a successful input/worker handoff or an activation receipt;
    actual handoff is separate and must occur before execution can escape. Error,
    cancellation or missing handoff factually interrupts the exact startup Run
    and closes only locally constructed resources. An already loaded runtime is
    retained. Primary errors/exceptions survive cleanup; cleanup failure after
    successful work is reported. No replay or no-effects claim is made. *)
val with_held_runtime
  :  t
  -> prepared:Prepared.t
  -> held:Agent_session.Session_actor.Held_run.t
  -> checked:Agent_session.Session_actor.Admitted_activation.Checked.t
  -> (Agent_session.Runtime_builder.t
      -> Agent_session.Session_actor.Admitted_activation.Continuation.t
      -> ('a, Agent_protocol.Error.t) result)
  -> ('a, Agent_protocol.Error.t) result

(** Recurrence compatibility entry. Projects the SAME checked association into
    [with_held_runtime]; scoped lifetime, cleanup and error laws are identical. *)
val with_held_recurrence_runtime
  :  t
  -> prepared:Prepared.t
  -> held:Agent_session.Session_actor.Held_run.t
  -> checked:Agent_session.Session_actor.Recurrence_activation.Checked.t
  -> (Agent_session.Runtime_builder.t
      -> Agent_session.Session_actor.Admitted_activation.Continuation.t
      -> ('a, Agent_protocol.Error.t) result)
  -> ('a, Agent_protocol.Error.t) result

module Held_input_runtime : sig
  (** The same actual prepared/held runtime under its scoped source lease. It is
      not a new source, permission or runtime getter. Only User_submission is
      supported here; authored lifecycle release is a separate owner. *)
  type t

  (** Run the actual selected parser, install a cold runtime only after parsing
      joins, then attach the recorded input through the existing same-Run root
      ACK. Returns the ORIGINAL Admission, not another admission or a completion
      receipt. Parser effects are not rolled back. A reached writer cannot replay.
      The callback receives the exact runtime and actor-issued input scope; its
      returned DATA is checked and recorded by that scope before handoff. *)
  val submit
    :  t
    -> prepare:
         (runtime:Agent_session.Runtime_builder.t
          -> scope:Agent_session.Session_actor.Held_input_scope.t
          -> ( Agent_session.Runtime_input_preparation.Prepared_input.t
               , Agent_protocol.Error.t )
               result)
    -> (Agent_protocol.Run_receipt.t, Agent_protocol.Error.t) result
end

(** The actual input/adoption consumer. Cold construction remains scoped until
    its exact Actor installation, then this owner retains the resources before
    the root writer can run. Loaded reuse preserves the same physical runtime
    and does not initialize again. A successful root ACK transfers normal runtime
    ownership; lexical cleanup cannot close it. After an installed failure, exact
    retirement is joined by the outer prepared-source cleanup AFTER its own lease
    releases. Cleanup never closes resources underneath an installed worker.
    Errors/cancellation retain their primary outcome. No NoEffects claim follows
    from a rejected root, because parsing may already have produced effects. *)
val with_held_input_runtime
  :  t
  -> prepared:Prepared.t
  -> held:Agent_session.Session_actor.Held_run.t
  -> checked:Agent_session.Session_actor.Admitted_activation.Checked.t
  -> (Held_input_runtime.t -> ('a, Agent_protocol.Error.t) result)
  -> ('a, Agent_protocol.Error.t) result

(** Consumed Goal form of the same construction bracket. An actual control
    skips runtime/input execution and remains separate from the admission. *)
val with_held_input_runtime_result
  :  t
  -> prepared:Prepared.t
  -> held:Agent_session.Session_actor.Held_run.t
  -> checked:Agent_session.Session_actor.Admitted_activation.Checked.t
  -> (Held_input_runtime.t -> ('a, Agent_protocol.Error.t) result)
  -> ('a Construction_result.t, Agent_protocol.Error.t) result

(** Complete the same held authored Run using its actual runtime startup.
    Already acknowledged legacy initialization is not replayed. A pending v1
    startup uses the Actor's exact lifecycle claim and joins before aggregate
    release. Returns the original Admission only after final handoff ACK; this
    never admits another Run. Installation and negative cleanup share the input
    consumer's resource ownership rules. Cancellation remains observable during
    startup, and completed effects are not rolled back on failure. *)
val start_held_authored_runtime
  :  t
  -> prepared:Prepared.t
  -> held:Agent_session.Session_actor.Held_run.t
  -> checked:Agent_session.Session_actor.Admitted_activation.Checked.t
  -> (Agent_protocol.Run_receipt.t, Agent_protocol.Error.t) result

(** Consumed Goal form of the same construction bracket. An actual control
    skips runtime/input execution and remains separate from the admission. *)
val start_held_authored_runtime_result
  :  t
  -> prepared:Prepared.t
  -> held:Agent_session.Session_actor.Held_run.t
  -> checked:Agent_session.Session_actor.Admitted_activation.Checked.t
  -> (Agent_protocol.Run_receipt.t Construction_result.t, Agent_protocol.Error.t) result

(** Cancellable auxiliary inference lifetime without loading a runtime. A loaded
    runtime is retained and its selected execution supplied; otherwise f receives
    None and must use explicit host selection. Stop/administration excludes and
    joins this lease. No initializer/manager/tool setup occurs. Run selection,
    inference and joined cleanup inside f, outside actor/owner callbacks. Primary
    errors/exceptions remain authoritative if cleanup also fails. *)
val with_auxiliary_execution_lifetime
  :  t
  -> (Inference_client.Execution.t option -> ('a, Agent_protocol.Error.t) Result.t)
  -> ('a, Agent_protocol.Error.t) Result.t

(** Retain native/compiled resources for an independently owned descendant.
    Admission uses the same current loaded-runtime check as a background borrow.
    Ordinary [unload_and_wait] detaches the parent's worker and permits a fresh runtime to
    load, but keeps these exact resources alive until the final resource borrower
    returns. Permanent [close_and_wait] cancels and joins all such borrowers,
    including resources detached by an earlier or concurrent ordinary unload.
    [with_unloaded] maintenance and administration defer/reject while borrowed.

    This is a trusted host lifetime primitive, not independent execution authority.
    The host must separately authorize the lifetime and revalidate current source,
    policy, linkage and revocation before child effects. Do not run the parent's
    worker or moderator through this borrow or treat its historical services as
    current authority. Use only properly inherited resource bindings, and join all
    child work before returning. Last-borrower cleanup reports any deferred close
    failure to that borrower. Factory integration and stopped-parent restoration
    remain separate requirements. *)
val with_delegation_resources
  :  t
  -> (Agent_session.Runtime_builder.t -> ('a, Agent_protocol.Error.t) result)
  -> ('a, Agent_protocol.Error.t) result

(** Register independently constructed resources against this ancestor's lifetime
    without loading or entering its execution runtime. Usable for stopped owners;
    no worker/manager is constructed and no execution-authority check is implied.
    The trusted host must check durable delegation and current policy before IO.
    Run resource construction, all descendant use and scope cleanup inside [f].
    Ordinary stop preserves this borrow; permanent close cancels/joins it.
    Reset-style unload, administration and with_unloaded maintenance remain
    excluded while borrowed. Admission rejects a closed owner or current unload.
    Call from outside actor checkpoints and owner locks. *)
val with_resource_lifetime
  :  t
  -> (unit -> ('a, Agent_protocol.Error.t) result)
  -> ('a, Agent_protocol.Error.t) result

(** Read the installed immutable source while retaining its runtime. Legacy
    moderators reject. Check persisted halt state in the authority guard; querying
    the manager's mutable state here would reenter its execution lock during native
    policy effects. This only identifies availability; it does not approve a
    child call. Actual policy execution must use the actor-owned event handoff. *)
val moderation_source
  :  t
  -> (Agent_protocol.Invocation.observer option, Agent_protocol.Error.t) result

(** Evaluate an owned child candidate using this parent's live manager, actor
    event/native services and history. The host authorizer must validate private
    delegation and the still-active child owner, repeatedly after waits. The parent
    actor commits decision, checkpoint and UI notices together; runtime requests
    stay with the parent. Replays do not rerun policy effects or notices. *)
val prepare_delegated_tool
  :  t
  -> delegation:Agent_protocol.Moderator_execution.delegation
  -> event:Chat_response.Moderation.Event.t
  -> authorize:(unit -> (unit, Agent_protocol.Error.t) result)
  -> (Agent_protocol.Moderator_execution.Decision.t, Agent_protocol.Error.t) result

(** [with_administration t f] excludes concurrent runtime loading while [f]
    prepares and commits stopped state. Preserve the previous runtime on failure;
    retire it after success without turning an accepted commit into a failed
    response because of cleanup. Cancellation and exceptions do not poison the
    owner mutex. Do not call runtime-owner operations from [f]. *)
val with_administration
  :  t
  -> (unit -> ('a, Agent_protocol.Error.t) result)
  -> ('a, Agent_protocol.Error.t) result

(** Hold the owner fence across fresh pure admission validation, old resource
    retirement, selected-state commit, initialization and final stopped-runtime
    retirement. No leases or unloading may exist. [before_initialize] is a pure
    allocator update after the durable commit; none of the callbacks may reenter
    this owner.

    A failed commit leaves the old durable configuration stopped and rebuildable.
    Once [commit] succeeds, initialization failure retains the selected durable
    configuration and its ordinary failure evidence; it never rolls back effects
    or permits recommitting a stale revision. The returned summary describes the
    actual initialized state. Exceptions and cancellation propagate unchanged
    after releasing the owner fence. *)
val reinitialize_administration
  :  t
  -> validate:(unit -> (unit, Agent_protocol.Error.t) result)
  -> commit:(unit -> (unit, Agent_protocol.Error.t) result)
  -> before_initialize:(unit -> unit)
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

(** Original admission is checked under this owner fence before old resources
    are joined. [commit] reconciles only tracking ACK drift against that original
    basis. No initialization occurs; failures and cancellation propagate. *)
val retire_administration
  :  t
  -> validate:(unit -> (unit, Agent_protocol.Error.t) result)
  -> commit:(unit -> ('a, Agent_protocol.Error.t) result)
  -> ('a, Agent_protocol.Error.t) result

val parse_user_content
  :  t
  -> id:History_entry.Id.t
  -> Agent_protocol.Session.Message_content.t
  -> (History_entry.t, Agent_protocol.Error.t) result

(** Producer identity is supplied by a trusted authenticated adapter, never taken
   from the helper payload. This bridge requires a qualified runtime and commits
   the receipt with its captured queue frame before installing the live queue. *)

val submit_ingress
  :  t
  -> producer:Agent_protocol.Id.Principal.t
  -> registration_id:Agent_protocol.Id.Capability.t
  -> namespace:string
  -> key:Agent_protocol.Idempotency_key.t
  -> payload:Jsonaf.t
  -> (Agent_session.External_ingress.receipt, Agent_protocol.Error.t) result

(** Load the pinned runtime and prepare one external event under the actor
    checkpoint gate. Persist the schedule delivery and queue checkpoint together
    before installing the live append. Rejection does not mutate the live queue.
    Delivery retains a cancellable runtime lease without holding the owner mutex
    across the checkpoint wait, so delegated policy checks can consult the owner.
    Stop retirement cancels and joins the lease before releasing resources.
    Retirement of this lease returns Interrupted to a still-live caller; it must
    not cancel the shared scheduler switch. Caller cancellation still propagates.
    A separate work mutex preserves serialization with idle moderator draining. *)
val deliver_schedule
  :  t
  -> Agent_protocol.Schedule.t
  -> (unit, Agent_protocol.Error.t) result

(** [drain_idle_moderator] handles pending invocation observations and queued
    internal events when the actor can grant an idle moderator borrow. An installed
    runtime's deferred startup/resume activation is also polled while runnable,
    before observation/queued-event handling. It never activates a stopped actor;
    failed activation is not bypassed by later idle event work. Each
    observation and owned event batch is bounded to 32 handlers; durable follow-up
    requests are consumed together before another event drain, even when no
    internal event or observation is queued. Only the installed v1 source can
    handle an observation. Failed handlers retain their separate failure and
    are never replayed. The result requests another idle probe when a batch
    exhausts its budget, queued events remain, or follow-up work was scheduled.
    When the runtime supplies script-tool services, every observation gets its
    own actor-bound native scope using the manager's exact admitted definition.
    Queued v1 events use the same scoped service through persisted event claims.
    Host-backed legacy events use their installed per-head claim and never the
    aggregate idle borrow: each head reads current canonical history, checks the
    retained source's failed/interrupted claims, and owns its checkpoint/request/
    notification ACK. Explicit standalone embeddings without that adapter keep
    the aggregate legacy compatibility path. Event requests are applied once
    after the batch, with termination ending the batch early.
    Unretired failed/interrupted event claims suppress further queue polling for
    their source/generation, while saved native outcomes remain observable.
    Native child results join subsequent bounded observation work. A runtime
    without these services returns invocation.unavailable for Tool.call. A
    completed event also requests another probe for newly created observations.
    Pending deferred user submissions also trigger a probe after an idle callback
    releases its borrow, even without a notification or moderator turn request.
    A poll finding no pending work under the owner mutex publishes no execution
    lease, so it cannot exclude a queued source-stage acquisition. Real work is
    selected, loaded and leased under the same mutex; draining then retains that
    cancellable lease rather than the owner mutex across moderator work.
    Cancellation confined to
    the owned event returns Interrupted, keeping the shared scheduler alive;
    cancellation of the caller still propagates. Later polling and administration
    remain usable. Runtime installation and each callback's ownership transfer
    retain protected lifecycle boundaries; owned legacy batches remain cancellable
    between and during handlers. Normal v1 declaration admission
    remains separate; this does not enable public features. *)
val drain_idle_moderator : t -> (bool, Agent_protocol.Error.t) result

(** [execute_model_job t ~recipe ~payload] executes nested model work under a
    stop-sensitive runtime lease, without holding the owner mutex during provider
    work. Cancellation releases the lease; Stop and close join it before releasing
    runtime resources. The original failure or cancellation remains primary. *)
val execute_model_job
  :  t
  -> allocate_inference:Agent_session.Inference_allocation.Port.t
  -> inference_context:Inference_runtime.Context.t
  -> capture_recipe_target:
       (Inference.Request.Target.t -> (unit, Agent_protocol.Error.t) result)
  -> recipe:string
  -> payload:Jsonaf.t
  -> (Agent_session.Runtime_builder.model_job_outcome, Agent_protocol.Error.t) result

type background_result =
  | Completed of Agent_protocol.Completion.t
  | Pending of Agent_protocol.Job.dependency

(** Execute a qualified Async_tool request through retained runtime ownership and
    the actor's exact job-attempt scope. Queue/retry time counts against the stored
    execution budget from Job.created_at. Generic results retain Completion data;
    moderator pre-tool requests use their persisted event intent. Unconsumed
    managed-handler/native requests still fail explicitly until their owning
    integration is installed. Does not admit or finish the job. *)
module Background_job : sig
  (** Non-executing validated DATA with the exact Job occurrence/payload, host
      policy ceiling, effective request and absolute deadline. No resource or
      Job claim is acquired. *)
  type t

  val prepare
    :  policy:Chat_response.One_off_request.policy
    -> Agent_protocol.Job.t
    -> (t, Agent_protocol.Error.t) result

  val deadline : t -> Agent_protocol.Timestamp.t
end

(** Shared actual executor/result body inside an already retained SAME runtime
    bracket and actual with_job_execution. Rechecks physical installation and
    opaque owner, exact Job/payload/deadline and identical executor policy. This
    acquires no lease/claim; callers must join the surrounding bracket. *)
val execute_background_job_runtime
  :  t
  -> runtime:Agent_session.Runtime_builder.t
  -> execution:Agent_session.Session_actor.job_execution
  -> request:Background_job.t
  -> (background_result, Agent_protocol.Error.t) result

(** Ordinary-attributed sibling: borrows its existing ordinary background runtime
    after exact actual scope validation, then uses the SAME executor/result body.
    A Goal owner rejects; it must use the genuine Factory resource route. *)
val execute_background_job_in_scope
  :  t
  -> execution:Agent_session.Session_actor.job_execution
  -> request:Background_job.t
  -> (background_result, Agent_protocol.Error.t) result

val execute_background_job
  :  t
  -> Agent_protocol.Job.t
  -> (background_result, Agent_protocol.Error.t) result

(** Atomically acknowledge a terminal model job and append its event using the
    same actor/checkpoint ownership as schedule delivery. *)
val deliver_model_job_completion
  :  t
  -> Agent_protocol.Job.t
  -> (unit, Agent_protocol.Error.t) result

(** Save a source-bound generic terminal event and its job acknowledgement
    atomically. The moderator's current job selection is checked before projection. *)
val deliver_background_job_completion
  :  t
  -> Agent_protocol.Job.t
  -> (unit, Agent_protocol.Error.t) result

(** Deliver an immutable actual terminal attempt through the existing moderator
    queue, independently of a later latest-job retry. The actor rechecks Pending
    custody/current authority and commits its queue append atomically. *)
val deliver_run_job_completion
  :  t
  -> Agent_session.Run_job_delivery.t
  -> (unit, Agent_protocol.Error.t) result

(** [close] permanently prevents runtime reload and is safe to request from a
    background callback. For owners with a host dependency barrier, it defers
    background cancellation and resource retirement to [close_and_wait]. Foreground
    admission is excluded and active foreground execution is cancelled immediately.
    For ordinary owners it
    cancels background callbacks and retires after their cleanup, or immediately
    if no callback or foreground operation owns the runtime. An active foreground
    operation is cancelled; [close_and_wait] must join its terminal checkpoint
    before resources or the actor are retired. An in-progress unload retains control
    of retirement. The actor must remain running through cleanup. *)
val close : t -> unit

(** Close and join foreground/dependency/background cleanup before shutting down the actor or
    its persistence writer. Concurrent accepted-stop cleanup is joined. A typed
    dependency failure raises [Cleanup_failed], retaining resources for retry;
    other exceptions retain their original backtrace. Call from the external
    session lifecycle, never a retained callback (which would await itself). *)
val close_and_wait : t -> unit

module For_testing : sig
  (** Hold loaded runtime ownership while installing a deterministic actor-state
      fixture. Polling resumes after the callback. Do not re-enter owner methods. *)
  val with_loaded_runtime
    :  t
    -> (unit -> ('a, Agent_protocol.Error.t) result)
    -> ('a, Agent_protocol.Error.t) result
end

(** Historical actual root handoff of this exact owner. Pure same-domain read of
    the live source stage and its sticky ACK captured before stage release,
    including lost replies or later cleanup failure. A new/reopened owner starts
    false. Retirement preserves this fact; it grants no fresh effect permission.
    No mutex, Actor RPC, clock or host callback is invoked. *)
val has_activation_handoff : t -> bool

module Prepared_goal_continuation : sig
  (** One selected source in the same exclusive Owner source_stage. This is
      existing-Run custody, never an initial Run_preparation/Prepared/Held value. *)
  type t

  val binding : t -> Agent_session.Session_actor.Goal_continuation.Source_binding.t
  val check_basis : t -> (unit, Agent_protocol.Error.t) result
end

module Goal_resume_source : sig
  (** The existing exclusive source stage plus the actual issued preparation.
      Callback-local, same creating Eio domain, no second source/lease registry. *)
  type t

  val preparation : t -> Agent_session.Session_actor.Goal_continuation.Preparation.t

  (** One selected source only. Consume before yielding the Actor bind call;
      rejected/raised/uncertain binding cannot select another source. *)
  val prepare
    :  t
    -> source:Agent_session.Runtime_builder.Source.t
    -> (Prepared_goal_continuation.t, Agent_protocol.Error.t) result

  module Decision : sig
    type 'a t =
      | Retained of Agent_session.Goal_command_receipt.t
      | Fresh of 'a
  end

  module Report : sig
    type 'a t

    (** Capture the actual issued preparation before pending cancellation or
        callback entry; missing means no issuance, never cleanup success. *)
    val preparation
      :  'a t
      -> Agent_session.Session_actor.Goal_continuation.Preparation.t option

    val outcome : 'a t -> 'a Decision.t Scope_outcome.t
  end
end

(** Validate the private Factory issuer against this Owner AND its actual Actor,
    then reserve the SAME existing source stage under Owner mutex before Begin.
    Existing ordinary borrowers fail reservation; new borrowers remain excluded
    continuously until real target cleanup joins. Begin and all policy/Actor
    operations run outside Owner mutex. Capture issued preparation under finite
    cancellation protection, then check cancellation before caller/source work.
    Retained invokes no callback; Fresh invokes once. Actual inner lease release
    and outer stage retirement/clear are both reported. This does not end the
    preparation or claim NoEffects. Factory owns parent joins and factual close. *)
val with_goal_resume_source_reported
  :  t
  -> source_owner:Agent_session.Runtime_source_owner.t
  -> actor:Operator_authorization.t
  -> authorize:(Agent_session.Session_state.t -> (unit, Agent_protocol.Error.t) result)
  -> session:Agent_protocol.Session_ref.t
  -> request:Agent_session.Goal_resume_request.t
  -> (Goal_resume_source.t -> ('a, Agent_protocol.Error.t) result)
  -> 'a Goal_resume_source.Report.t

(** Continue the SAME existing Goal episode inside its actual selected source
    stage. Loaded runtime reuse performs no initialization or lifecycle replay.
    Cold construction uses the exact command context, genuine Starting/Idle
    transitions and the shared Factory constructor, then joins actual
    Session_resume before deciding Started, Waiting or Settled. No new Run,
    episode, account or input is admitted. Primary errors/cancellation preserve
    their outcome; the enclosing Factory report retains actual receipt facts.
    Runtime adoption requires the Actor's immutable actual final outcome,
    including a validated Wait/Settlement, never a caller success flag. *)
val resume_goal_episode
  :  t
  -> prepared:Prepared_goal_continuation.t
  -> continuation:Agent_session.Session_actor.Goal_continuation.t
  -> (Agent_session.Session_actor.Goal_resume_result.t, Agent_protocol.Error.t) result

module Prepared_goal_execution : sig
  type t

  val binding : t -> Agent_session.Session_actor.Goal_execution_runtime.Source_binding.t
  val check_basis : t -> (unit, Agent_protocol.Error.t) result
end

module Goal_execution_source : sig
  type t

  val prepare
    :  t
    -> source:Agent_session.Runtime_builder.Source.t
    -> (Prepared_goal_execution.t, Agent_protocol.Error.t) result

  module Decision : sig
    type 'a t =
      | No_work
      | Suspended_goal of Agent_session.Goal_execution_selection.Suspension.t
      | Fresh of 'a
  end

  module Report : sig
    type 'a t

    (** Captured under protection before post-RPC cancellation can escape, even
        when no source/callback ran. Factory owns exact factual close after joins. *)
    val preparation
      :  'a t
      -> Agent_session.Session_actor.Goal_execution_runtime.Preparation.t option

    val outcome : 'a t -> 'a Decision.t Scope_outcome.t
  end
end

(** Hidden Factory key matches BOTH actual Owner and Actor. Reserve SAME source
    stage before Begin outside mutex; no active lease/coexisting preparation.
    Select once, bind once, revoke source before joining lease. No NoEffects or
    command receipt; primary Error/exn/cancel remains separate from actual cleanup.
    Factual close is Factory-owned after target plus generated-parent joins. *)
val with_goal_wake_source_reported
  :  t
  -> source_owner:Agent_session.Runtime_source_owner.t
  -> wake:Agent_protocol.Run_wake.t
  -> (Goal_execution_source.t -> ('a, Agent_protocol.Error.t) result)
  -> 'a Goal_execution_source.Report.t

(** SAME stage/CPS and cleanup core; only issuance differs. Goal-attributed scope
    required; caller must remain inside actual Actor.with_job_execution. No_work/
    Suspended are impossible for this branch: stale owner fails, Held does not
    abandon a Running Job. Ordinary Jobs use unchanged ordinary runtime APIs. *)
val with_goal_job_source_reported
  :  t
  -> source_owner:Agent_session.Runtime_source_owner.t
  -> owner:Agent_session.Session_actor.Job_runtime_owner.t
  -> (Goal_execution_source.t -> ('a, Agent_protocol.Error.t) result)
  -> 'a Goal_execution_source.Report.t

(** Resource bracket for SAME still-active Job. No authority from [f], which must
    use actual outer with_job_execution services and join its work. Cold Init and
    Session_resume use the common constructor, then the Actor binds the actual
    physical install and joined lifecycle into this SAME Job scope before callback
    entry. No second durable Job claim or attempt increment; completion remains
    the scheduler's existing owner.
    Loaded reuse does no Init/resume. Exceptions/cancellation/primary failure and
    all actual cleanup retain existing semantics. *)
val with_goal_job_runtime
  :  t
  -> prepared:Prepared_goal_execution.t
  -> execution:Agent_session.Session_actor.Goal_execution_runtime.t
  -> (Agent_session.Runtime_builder.t -> ('a, Agent_protocol.Error.t) result)
  -> ('a Construction_result.t, Agent_protocol.Error.t) result

module Loaded_job_runtime : sig
  type 'a t =
    | No_runtime
    | Reused of 'a
end

(** Already-admitted exact Job resource borrower. Never constructs or reserves a
    source stage, and never reacquires Enabled/quiescence permission. No_runtime
    denotes genuine absence before acquisition only; other failures stay errors.
    The callback joins under the same ordinary cancellable lease used elsewhere;
    Stop/retirement joins it. Does not bypass an unrelated prepared source. *)
val with_loaded_goal_job_runtime
  :  t
  -> owner:Agent_session.Session_actor.Job_runtime_owner.t
  -> (Agent_session.Runtime_builder.t -> ('a, Agent_protocol.Error.t) result)
  -> ('a Loaded_job_runtime.t, Agent_protocol.Error.t) result

(** Fixed single-occurrence Goal wake consumer. Uses the actual shared runtime
    construction/residency core and immutable producer/claim or Operation ACK.
    Notification bounds/policy come from the same Factory host provider; neither
    a callback return nor policy DATA certifies transfer. Loaded reuse skips Init
    and resume; cold lifecycle Wait/Settlement is preserved. *)
val run_goal_wake_runtime
  :  t
  -> prepared:Prepared_goal_execution.t
  -> execution:Agent_session.Session_actor.Goal_execution_runtime.t
  -> notification_policy:Chat_response.One_off_request.policy
  -> max_notifications:int
  -> ( Agent_session.Session_actor.Goal_execution_runtime.Wake_result.t
       , Agent_protocol.Error.t )
       result

(** Same exclusive source stage, reserved before consuming the actual accepted
    native command selection. Uses that command's captured original loan/policy;
    no DTO, actor lookup or second preparation is constructed by this wrapper. *)
val with_native_goal_resume_source_reported
  :  t
  -> source_owner:Agent_session.Runtime_source_owner.t
  -> selection:Agent_session.Session_actor.Goal_native_resume.Selection.t
  -> (Goal_resume_source.t -> ('a, Agent_protocol.Error.t) result)
  -> 'a Goal_resume_source.Report.t
