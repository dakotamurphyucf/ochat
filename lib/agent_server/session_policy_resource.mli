open! Core

(** One actual Factory entry's source-bound physical runtime. Domain confined;
    the Cell owns partial construction before any resource can escape. It is
    neither an execution admission nor a source selector. *)
type t

module Key : sig
  type t

  (** Capture factual identity from the authentic Handle, Actor, selected source
      and current State. No revision equality or stored DATA creates authority.
      The Factory separately checks live construction and source eligibility. *)
  val capture
    :  actor:Agent_session.Session_actor.t
    -> handle:Agent_store.Session_store.Handle.t
    -> source:Agent_session.Runtime_builder.Source.t
    -> profile:Agent_session.Permission_policy.t
    -> expected:Agent_session.Session_state.t
    -> (t, Agent_protocol.Error.t) result

  (** Pure factual key for the existing verified ordinary-root cold path.
      Does not compile source, invoke callbacks or mint source preparation. *)
  val capture_authored
    :  actor:Agent_session.Session_actor.t
    -> handle:Agent_store.Session_store.Handle.t
    -> revision:Agent_session.Prompt_revision.t
    -> profile:Agent_session.Permission_policy.t
    -> expected:Agent_session.Session_state.t
    -> (t, Agent_protocol.Error.t) result
end

module Native_source_key : sig
  (** Native-only physical/source identity DATA, distinct from Product readiness.
      Captured by the trusted Factory on its actual Actor/Handle domain after
      current consumer/source admission; this key creates no permission. *)
  type t

  (** Validates the selected source/target family and actual Handle/session/profile
      association. Exact installed full source option is retained: absent index
      or source stays None, including lawful Pending native setup. Does not
      project an installation from a checkpoint, run Init or require Ready.
      Current source/Goal/private/profile authorization remains separate and must
      repeat around every actual wait or resource disclosure. No Actor RPC. *)
  val capture
    :  actor:Agent_session.Session_actor.t
    -> handle:Agent_store.Session_store.Handle.t
    -> source:Agent_session.Runtime_builder.Source.t
    -> profile:Agent_session.Permission_policy.t
    -> expected:Agent_session.Session_state.t
    -> (t, Agent_protocol.Error.t) result

  (** Creating domain first, then exact physical Actor/Handle, generation/spec,
      artifact/content, invocation/family/pins, profile and full source option.
      Does not compare initialization, lifecycle outcome, queue or revision and
      cannot authorize use after their actual consumer guards reject. None is
      not a wildcard for a later installed source. *)
  val equal : t -> t -> bool

  (** Pure domain-first current factual association of this retained source key.
      Exact Actor/Handle/session/generation/spec/profile and ACTUAL installed full
      source option; no Ready requirement, checkpoint projection, source IO or
      authorization. Immutable family/artifact/invocation were checked against
      the executable Source at actual first construction. *)
  val check_current
    :  t
    -> actor:Agent_session.Session_actor.t
    -> handle:Agent_store.Session_store.Handle.t
    -> profile:Agent_session.Permission_policy.t
    -> expected:Agent_session.Session_state.t
    -> (unit, Agent_protocol.Error.t) result
end

module Failure : sig
  type t = Constructor_inference_graph.Failure.t =
    | Protocol of Agent_protocol.Error.t
    | Raised of exn * Stdlib.Printexc.raw_backtrace
end

module Cleanup_target : sig
  type cell = t

  (** Identity of one actual Build/Native_build cleanup unit. Allocated only by
      successful Empty -> Building/Native_building election; never by a getter,
      pin, source key, promotion, current Owner lookup or missing membership.
      Factual custody only: no current-use, source, authority or close grant. *)
  type t

  module Facets : sig
    (** Actual objects currently attached to this same unit. None is absence,
        never a wildcard or equality proof. Native is Some only for an actual
        native-first Prepared_native/native_resource. Ordinary/loaded-first
        Product has product=Some and native=None, even though Builder can obtain
        its Product.native_identity. Promotion may attach both actual objects.
        These facts prove neither publication ACK nor current Owner affinity. *)
    type t =
      { native : Agent_session.Runtime_builder.Native_identity.t option
      ; product : Agent_session.Runtime_builder.Product_identity.t option
      }
  end

  module Status : sig
    type t =
      | Held of Facets.t
      | Released
  end

  (** Pure creating-domain check, then SAME Cell provenance, before inspecting
      phase. Held requires that phase's exact same target, including partial,
      failed attachment, Retained and Closing; it does not check current use.
      Released is sticky evidence from this unit's actual full/partial positive
      cleanup and does not inspect or retire a replacement unit. Empty, different
      unit or missing membership never manufactures Released. Foreign Cell or
      domain returns Error. No locking, waiting, resource allocation or authority renewal. *)
  val check : t -> cell:cell -> (Status.t, Agent_protocol.Error.t) result

  (** Deliberate immutable identity equality, independent of lifecycle state.
      Equality grants no permission and does not establish Owner affinity. *)
  val equal : t -> t -> bool
end

module Lease : sig
  type t

  (** Stored identity of this actual cleanup unit, including retained failed
      custody. Getter is not current-use authority; check enforces domain/Cell. *)
  val cleanup_target : t -> Cleanup_target.t

  val product : t -> Agent_session.Runtime_builder.Product.t
  val runtime : t -> Agent_session.Runtime_builder.t
  val check_key : t -> key:Key.t -> (unit, Agent_protocol.Error.t) result

  (** Pure SAME active Cell/Actor/Handle/key/full source/Ready check. The actual
      Factory-resolved profile digest must match both original key and State.
      Issues factual completed-extension Product affinity only, no permission,
      source preparation, Manager snapshot or runtime activation. *)
  val policy_access
    :  t
    -> actor:Agent_session.Session_actor.t
    -> handle:Agent_store.Session_store.Handle.t
    -> profile:Agent_session.Permission_policy.t
    -> expected:Agent_session.Session_state.t
    -> (Agent_session.Runtime_builder.Policy_access.t, Agent_protocol.Error.t) result

  (** Check the SAME active Cell/Actor/Handle/key/profile/Product against an
      already-issued actual restored access. Neither issues new eligibility from
      DATA nor refreshes the selected lifecycle or starts an ordinary worker. *)
  val check_restored_policy_access
    :  t
    -> access:Agent_session.Runtime_builder.Restored_policy_access.t
    -> actor:Agent_session.Session_actor.t
    -> handle:Agent_store.Session_store.Handle.t
    -> profile:Agent_session.Permission_policy.t
    -> expected:Agent_session.Session_state.t
    -> (unit, Agent_protocol.Error.t) result

  (** Release only this actual binding reference. The last reference joins the
      exact physical closer; failure retains it for an explicit factual retry.
      Repeated close never repeats physical releases or selects another source. *)
  val close : t -> (unit, Agent_protocol.Error.t) result

  (** Execute/await the SAME release once. Last-close callers joining the same
      Cell producer receive its SAME immutable diagnostic groups/primary. A new
      retained retry is a new actual producer. After success, the SAME target's
      actual successful report is retained for repeated last-release calls,
      including when close_pending completed the target between calls. Native
      promotion shares that receipt with its Product alias. Creating-domain
      checks still precede receipt access; no physical effects replay or new
      resource authority. *)
  val close_reported
    :  t
    -> (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t
end

module Constructor_graph : sig
  type t

  (** Nonyielding same-domain, active Lease, exact physical resource and live
      native-driver check. Retains one unallocated slot BEFORE actual graph
      creation may yield. Factual completion and release do not require a healthy
      driver. *)
  val reserve : Lease.t -> (t, Agent_protocol.Error.t) result

  (** Immediate nonfallible ownership assignment of the protected creation result.
      Later source/Lease retirement cannot discard an already-issued graph. *)
  val allocated : t -> Graph_tracking.t -> unit

  val inference : t -> Agent_session.Runtime_builder.Pending_legacy_resume.Inference.t

  (** Only after creation failed before graph publication and no future lexical
      force exists. Cannot discard an allocated graph. *)
  val abandon_unallocated : t -> unit

  val callback_exited : t -> unit

  (** Same graph factual finish after actual callback exit. Error retains owner
      for the existing outside-lock cleanup port, including with surviving loans. *)
  val finish : t -> (unit, Agent_protocol.Error.t) result
end

module Partial_close : sig
  module Adoption : sig
    (** Exact original driver DATA. Closed_runtime retains the actual adopted
        runtime and its original close observation; not current loaded state. *)
    type t =
      | Original_running
      | Not_created
      | Closed_runtime of
          { runtime : Agent_session.Runtime_builder.t
          ; observation : unit Agent_session.Close_observation.t
          }
  end

  type t

  (** Fixed Factory construction scope closer, registered before its driver
      starts. It owns its tracking seal/finish. The readonly adoption getter
      describes ONLY that driver's original completion. Creating domain only.
      No owner selection or additional execution authority is supplied here. *)
  val create
    :  close:Agent_session.Runtime_builder.Close.t
    -> adoption:(unit -> Adoption.t)
    -> t
end

module Build : sig
  type t

  (** Stored identity of this actual cleanup unit, including retained failed
      custody. Getter is not current-use authority; check enforces domain/Cell. *)
  val cleanup_target : t -> Cleanup_target.t

  (** Attach each actual owner nonyieldingly at its creation boundary. *)
  val tracking : t -> Graph_tracking.t -> (unit, Agent_protocol.Error.t) result

  (** Install exactly once in the actual Tracking phase, before driver fork.
      Original-running refuses finitely before invoking the closer. On original
      completion, fallback Product cleanup remains owed only for Not_created;
      Closed_runtime must match the actual captured Binding including native
      affinity, and never replays that adopted Product's physical closer.
      Physical registration replaces this partial route with the authentic
      final runtime's sole closer, which forwards the same construction scope. *)
  val partial_close : t -> Partial_close.t -> (unit, Agent_protocol.Error.t) result

  val product
    :  t
    -> product:Agent_session.Runtime_builder.Product.t
    -> binding:Agent_session.Runtime_builder.Product.Binding.t
    -> (unit, Agent_protocol.Error.t) result

  (** Full Factory closer includes the actual source/private-switch join and
      existing Graph factual retry owner, not only Builder's inner releases. *)
  val physical
    :  t
    -> Agent_session.Runtime_builder.t
    -> (unit, Agent_protocol.Error.t) result

  val binding
    :  t
    -> (Agent_session.Runtime_builder.Product.Binding.t, Agent_protocol.Error.t) result

  (** Only after the full actual Factory closer returned positively. *)
  val closed : t -> (unit, Agent_protocol.Error.t) result

  val fail : t -> Failure.t -> unit

  (** Only after the real initialization/checkpoint ACK and complete resource
      attachment. The returned first lease owns physical lifetime before escape. *)
  val publish : t -> key:Key.t -> (Lease.t, Agent_protocol.Error.t) result

  (** Issue access only from the SAME Build-owned physical Product/runtime.
      The Factory separately retains actual construction and source authority. *)
  val restored_policy_access
    :  t
    -> source:Agent_session.Runtime_builder.Source.t
    -> basis:Agent_session.Policy_restore_basis.t
    -> expected:Agent_session.Session_state.t
    -> ( Agent_session.Runtime_builder.Restored_policy_access.t
         , Agent_protocol.Error.t )
         result

  (** Process-local restored-resource publication, not a new initialization ACK.
      Actual Factory construction admission and final writer/source checks must
      remain live. The recovered full checkpoint/selected Completed receipt
      already owns its original durable ACK. Checks SAME actual Product access,
      original source key and complete current construction basis immediately
      before nonyielding publication. Returns the Owner baseline lease before any
      yield; each callback must independently pin another reference. *)
  val publish_restored
    :  t
    -> key:Key.t
    -> access:Agent_session.Runtime_builder.Restored_policy_access.t
    -> expected:Agent_session.Session_state.t
    -> (Lease.t, Agent_protocol.Error.t) result
end

module Retained_selection : sig
  type t =
    | No_retained_product
    | Retained of Lease.t
end

(** Nonyielding same-domain pin, before source/Actor/Manager preparation.
    With no expected Product, Empty or a live native unit with no attachment
    reports No_retained_product. The latter still requires the subsequent
    construction selector to validate and promote that same unit; it does not
    authorize a new native allocation. An expected Product that disappeared,
    an unselected Live resource, pending construction, aborted native driver,
    closing or retained failed cleanup rejects. Never allocates a Build. *)
val pin_retained
  :  t
  -> expected_product:Agent_session.Runtime_builder.Product_identity.t option
  -> (Retained_selection.t, Agent_protocol.Error.t) result

module Selection : sig
  type t =
    | Build of Build.t
    | Reuse of Lease.t
end

val create : unit -> t

val select
  :  t
  -> key:Key.t
  -> expected_product:Agent_session.Runtime_builder.Product_identity.t option
  -> (Selection.t, Agent_protocol.Error.t) result

(** Pure factual pending for existing Owner eligibility gates; never cleanup.
    Includes unfinished constructor graphs while shared physical resources live. *)
val pending : t -> bool

(** Outside the Owner mutex after actual lease, ACK and foreground joins.
    Healthy Live physical resources owned by surviving loans are untouched;
    their exited constructor graphs may receive factual finish retries. Exceptions
    retain their original backtrace and same cleanup owner. *)
val close_pending : t -> (unit, Agent_protocol.Error.t) result

(** Same actual operation as [close_pending], retaining each known independent
    Cell leg before its original first-error projection. Report reads/joins do
    not mint another group for the same actual Cell producer. A successful target
    retains this exact report for its outstanding last-release callers before
    the Cell becomes empty; the report is never reused for another target. *)
val close_pending_reported
  :  t
  -> (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t

module Native_lease : sig
  (** An actual native-first loan or an actual existing Product Cell.Lease.
      Loaded-first loans share that Product's original refcount and closer;
      they never allocate a second native unit or construct another Manager.
      Retained key/current checks and resource projections enforce the SAME
      current-use eligibility as a fresh pin: unfinished/failed attachment or
      pending Product constructor cleanup rejects. Custody alone is not use. *)
  type t

  (** Same target as the actual native resource or loaded-first Product Lease.
      Individual pin release does not certify this target Released. *)
  val cleanup_target : t -> Cleanup_target.t

  val identity : t -> Agent_session.Runtime_builder.Native_identity.t
  val check_key : t -> key:Native_source_key.t -> (unit, Agent_protocol.Error.t) result

  (** Current unit key after any genuine Init ACK, never an old consumer's key.
      Loaded-first loans additionally check their original Product/checkpoint
      basis. Health and factual affinity only; fresh Factory authority is separate. *)
  val check_current
    :  t
    -> actor:Agent_session.Session_actor.t
    -> handle:Agent_store.Session_store.Handle.t
    -> profile:Agent_session.Permission_policy.t
    -> expected:Agent_session.Session_state.t
    -> (unit, Agent_protocol.Error.t) result

  (** Check the exact physical Source already observed under this active loan.
      This metadata check can succeed during an in-progress attachment on the
      same native unit. It still requires open current custody and rejects
      failed attachment, pending Product constructor cleanup and displacement. *)
  val check_retained_source
    :  t
    -> source:Agent_session.Runtime_builder.Source.t
    -> actor:Agent_session.Session_actor.t
    -> handle:Agent_store.Session_store.Handle.t
    -> profile:Agent_session.Permission_policy.t
    -> expected:Agent_session.Session_state.t
    -> (unit, Agent_protocol.Error.t) result

  val with_resources
    :  t
    -> f:(Agent_session.Runtime_builder.resources -> ('a, Agent_protocol.Error.t) result)
    -> ('a, Agent_protocol.Error.t) result

  (** Same actual active/current loan and resource view as [with_resources],
      including loaded-first borrowed source lease. Supplies only the SAME unit's
      retained executable Source; missing selection rejects before f. No new
      borrow, preparation, permission or closer. All dependent use stays scoped. *)
  val with_source_resources
    :  t
    -> f:
         (source:Agent_session.Runtime_builder.Source.t
          -> Agent_session.Runtime_builder.resources
          -> ('a, Agent_protocol.Error.t) result)
    -> ('a, Agent_protocol.Error.t) result

  (** Factual release of this SAME loan remains available when current use is
      rejected by attachment construction/failure or constructor cleanup. *)
  val close : t -> (unit, Agent_protocol.Error.t) result

  (** Last-release repetition forwards the SAME target's retained successful
      report, including a close_pending repair and native-to-Product promotion.
      Domain checks precede access; no completed physical effect is replayed. *)
  val close_reported
    :  t
    -> (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t
end

(** Nonactivating identity of the Source already selected by this SAME Cell.
    This observation creates neither a loan nor execution permission. *)
module Native_source : sig
  type cell = t
  type t

  (** Pure and creating-domain first. [expected_native] comes from the actual
      Owner's current binding/Available native membership. None is returned
      only for Empty with expected_native=None. Present partial, retained,
      closing, displaced or foreign evidence refuses rather than falling back.
      Reuses current native eligibility and the exact Actor/Handle/generation/
      profile/source-key checks. No select, pin, reference increment or callback. *)
  val capture
    :  cell
    -> expected_native:Agent_session.Runtime_builder.Native_identity.t option
    -> actor:Agent_session.Session_actor.t
    -> handle:Agent_store.Session_store.Handle.t
    -> profile:Agent_session.Permission_policy.t
    -> expected:Agent_session.Session_state.t
    -> (t option, Agent_protocol.Error.t) result

  val source : t -> Agent_session.Runtime_builder.Source.t
  val identity : t -> Agent_session.Runtime_builder.Native_identity.t

  (** Pure current validation of the captured physical Cell/unit and exact
      immutable selected Source, without substituting a replacement. Factual
      release does not require this current-use observation to remain valid. *)
  val check
    :  t
    -> profile:Agent_session.Permission_policy.t
    -> expected:Agent_session.Session_state.t
    -> (unit, Agent_protocol.Error.t) result
end

module Native_build : sig
  type t

  (** Stored identity of this actual cleanup unit, including retained failed
      custody. Getter is not current-use authority; check enforces domain/Cell. *)
  val cleanup_target : t -> Cleanup_target.t

  (** Each actual partial owner is single-assignment before preparation is
      captured. Scope may be attached before graph tracking so the fixed common
      closer owns failures while the driver allocates that graph. Both must be
      present before a prepared unit can be captured or published. Attach each
      actual allocation immediately, before its next await. *)

  val tracking : t -> Graph_tracking.t -> (unit, Agent_protocol.Error.t) result

  (** Attach actual scope and fixed Factory composite full closer before any
      await. This sole common closer includes actual parent reported cleanup;
      local Scope.close alone cannot prove the same join. *)
  val scope
    :  t
    -> Runtime_native_scope.t
    -> close:
         (unit -> (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t)
    -> (unit, Agent_protocol.Error.t) result

  val prepared
    :  t
    -> Agent_session.Runtime_builder.Prepared_native.t
    -> source:Agent_session.Runtime_builder.Source.t
    -> (unit, Agent_protocol.Error.t) result

  val publish
    :  t
    -> key:Native_source_key.t
    -> (Native_lease.t, Agent_protocol.Error.t) result

  val fail : t -> Failure.t -> unit
end

module Native_selection : sig
  type t =
    | Build of Native_build.t
    | Reuse of Native_lease.t
end

val select_native
  :  t
  -> key:Native_source_key.t
  -> expected_native:Agent_session.Runtime_builder.Native_identity.t option
  -> (Native_selection.t, Agent_protocol.Error.t) result

(** Pure current-state pin of the actual retained unit, BEFORE any source or
    ancestor scope can be reopened. Never allocates a Build. Uses the unit's
    current exact native key; for a loaded-first Product, validates its real
    initialized Product/checkpoint key then retains an ORIGINAL Product lease.
    Native full-source None remains None even if Product Key projects a source
    from its actual checkpoint. This is not an equality wildcard or permission.
    Factory must freshly validate the actual Reference/private/current grant
    before this nonyielding final pin and take custody before the next wait. *)
val pin_native
  :  t
  -> expected_native:Agent_session.Runtime_builder.Native_identity.t
  -> actor:Agent_session.Session_actor.t
  -> handle:Agent_store.Session_store.Handle.t
  -> profile:Agent_session.Permission_policy.t
  -> expected:Agent_session.Session_state.t
  -> (Native_lease.t, Agent_protocol.Error.t) result

(** Distinct from existing Build: its failure cleanup MUST NOT call the native
    unit's full Product/physical closer while existing native loans survive. *)
module Promotion : sig
  type t

  (** Stored identity of this actual cleanup unit, including retained failed
      custody. Getter is not current-use authority; check enforces domain/Cell. *)
  val cleanup_target : t -> Cleanup_target.t

  val prepared : t -> Agent_session.Runtime_builder.Prepared_native.t

  (** Allocate and retain this attachment-local Manager/wakeup cleanup owner
      BEFORE Builder may allocate or await. It cannot close shared native N. *)
  val attachment
    :  t
    -> Agent_session.Runtime_builder.Product_attachment.t
    -> (unit, Agent_protocol.Error.t) result

  (** Capture each actual allocated Product/binding/runtime immediately. No second
      native/context/scope ownership is installed. *)
  val product
    :  t
    -> product:Agent_session.Runtime_builder.Product.t
    -> binding:Agent_session.Runtime_builder.Product.Binding.t
    -> (unit, Agent_protocol.Error.t) result

  val physical
    :  t
    -> Agent_session.Runtime_builder.t
    -> (unit, Agent_protocol.Error.t) result

  val binding
    :  t
    -> (Agent_session.Runtime_builder.Product.Binding.t, Agent_protocol.Error.t) result

  val restored_policy_access
    :  t
    -> source:Agent_session.Runtime_builder.Source.t
    -> basis:Agent_session.Policy_restore_basis.t
    -> expected:Agent_session.Session_state.t
    -> ( Agent_session.Runtime_builder.Restored_policy_access.t
         , Agent_protocol.Error.t )
         result

  (** Consume only this SAME ordinary constructor's actor-issued successful Init
      ACK, including its exact before/after installed-source OPTIONS. Plain native
      initialization may remain None -> None; None -> Some is permitted only when
      that genuine owned Init installed this exact source. Every other native-key
      field must remain unchanged. Existing native loans retain the SAME unit;
      its current factual key advances once, never via equality relaxation.
      This handoff is DATA, survives lexical Init end, and grants no admission.
      Recording a known ACK remains required on a later failed Product attachment;
      a missing/uncertain ACK cannot be inferred from Ready DATA. Requires the
      attached actual Product/Binding/runtime, its optional Manager, same native
      identity and Source_handoff.check against current expected State. The same
      accepted handoff is idempotent; an unrelated handoff cannot advance N. *)
  val advance_initialized
    :  t
    -> acknowledgement:Agent_session.Session_actor.Initialization_scope.Source_handoff.t
    -> native_key:Native_source_key.t
    -> key:Key.t
    -> expected:Agent_session.Session_state.t
    -> (unit, Agent_protocol.Error.t) result

  (** Real ordinary publication, never634 policy permission. Pending-at-begin
      requires the recorded SAME successful Init handoff above. Already-Ready
      ordinary construction keeps its existing authenticated Runtime_construction
      and actual checkpoint ACK; it requires unchanged exact native/Product keys,
      without inventing a Pending handoff or a policy Caller. Both require the
      actual same attached Product/Binding and current ordinary Factory guards.
      No publication completes Init or turns a native loan into permission. *)
  val publish_ordinary : t -> key:Key.t -> (Lease.t, Agent_protocol.Error.t) result

  val publish_restored
    :  t
    -> key:Key.t
    -> access:Agent_session.Runtime_builder.Restored_policy_access.t
    -> expected:Agent_session.Session_state.t
    -> (Lease.t, Agent_protocol.Error.t) result

  (** Actual successful callback completion without Product publication. Join
      the captured attachment-local runtime first; only success discharges M and
      permits a later genuinely admitted attachment on SAME N. Existing native
      loans and their closer remain unchanged. Cleanup failure leaves this exact
      partial owner for [fail]/retry, never invents a callback failure or Ready. *)
  val closed : t -> (unit, Agent_protocol.Error.t) result

  (** Records the original failure and partial attachment. Existing native leases
      remain attached to the same native unit, with their real current consumer
      authorization checks; no stale permission is extended. Deny another
      Product construction/fallback while this partial attachment remains. Owner
      may release its baseline, but a surviving native loan still prevents full
      close. Final Cell election after ALL native/Product refs join closes the
      partial attachment plus shared unit exactly once, retaining failure custody. *)
  val fail : t -> Failure.t -> unit
end

(** Ordinary initialization and GENUINE 634 policy restoration retain their
    distinct real Actor admissions. Restored publication still requires actual
    Ready/latest Completed and exact full raw checkpoint. Ordinary publication
    retains its actual Pending Init ACK or Ready construction/checkpoint route.
    This reservation is neither admission.
    A promotion pin is reserved nonyieldingly before allocation/await. It is
    converted once to the first Product lease at publication, not duplicated.
    GENUINE 634 admission is separately required for restored publication.
    This only reserves same-Cell/same-native attachment; it never changes an old
    native loan into policy/Run/lifecycle authority. Native loans remain valid
    across success AND failed attachment, with their original source/grant checks.
    No full physical close is reachable through ordinary Build failure cleanup. *)
val begin_native_promotion
  :  t
  -> loan:Native_lease.t
  -> native_key:Native_source_key.t
  -> key:Key.t
  -> (Promotion.t, Agent_protocol.Error.t) result

module Product_selection : sig
  type t =
    | Build of Build.t
    | Reuse of Lease.t
    | Promote of Promotion.t
end

(** Closed Factory construction selection under the caller's actual ordinary or
    restored admission. Empty/Live preserve [select] exactly. A native-only unit
    checks its actual current key, same Actor/Handle/profile/construction, exact
    installed source option when Ready, open driver and absent attachment, then
    reserves M without yielding. Pending source advancement still requires the
    SAME genuine Init ACK. No fallback, source lookup, permission or new unit. *)
val select_for_product
  :  t
  -> key:Key.t
  -> profile:Agent_session.Permission_policy.t
  -> expected:Agent_session.Session_state.t
  -> expected_product:Agent_session.Runtime_builder.Product_identity.t option
  -> (Product_selection.t, Agent_protocol.Error.t) result
