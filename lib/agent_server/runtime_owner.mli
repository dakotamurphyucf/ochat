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

  (** Prepend actual already captured inner cleanup before this report's actual
      outer cleanup. Preserve the original primary/exception/raw trace. Neither
      operation creates a successful cleanup report or certifies missing scopes.
      Nested callers must project only [primary] through the outer callback, then
      combine actual cleanup before the final [resume]. *)
  val prepend_cleanup : 'a t -> cleanup:Cleanup.t -> 'a t

  (** Restore the ordinary scope contract: a callback Error or exception remains
      primary; cleanup failure is reported after successful work. Exceptions and
      cancellation retain their original raw backtrace. *)
  val resume : 'a t -> ('a, Agent_protocol.Error.t) result
end

module Cleanup_recovery : sig
  type owner = t
  type t

  module Snapshot : sig
    type t

    type progress =
      | Joined
      | Retryable
      | Blocked
      | Terminal
    [@@deriving equal, sexp_of]

    (** NEXT permissible action, not a promise of global recoverability.
        Retryable if any exact duty may be attempted (even with other Terminal
        duties); otherwise Blocked if an original producer/dependency is unsettled;
        otherwise Terminal if immutable negatives remain; Joined only when every
        originally owned duty has an actual positive receipt or legitimate custody
        transfer. No global Actor/physical-cell quiescence is implied.
        A retry can therefore move Retryable to Terminal without losing a failure. *)
    val progress : t -> progress

    val failures : t -> Scope_outcome.Cleanup.Failure.t list

    (** All actual latest negatives in original duty order. Some Failed when
        any exist, even if other duties are Deferred; None when required duties
        are unattempted/unfinished without a negative; Some Joined only at Joined.
        Snapshot accessors inspect immutable data only, never the mutable holder. *)
    val cleanup : t -> Scope_outcome.Cleanup.t option

    (** Paired result frozen when this immutable snapshot was published. Actual
        current negatives keep their exact raw exception/backtrace and explicit
        diagnostic producer; successful results retain incoming diagnostic groups
        with no selected primary. This result is independent of custody progress.
        Outer Error means unfinished or unavailable producer evidence, not a new
        physical failure receipt. Reading performs no cleanup, holder observation,
        diagnostic issuance or delivery mutation; old snapshots never change. *)
    val observation
      :  t
      -> ( (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t
           , Agent_protocol.Error.t )
           result
  end

  (** SAME original completed cleanup, forever. None while original producer
      runs. Snapshot is separately immutable; readers during retry see the last
      completed snapshot. Neither observer allocates/completes work.
      Both holder observations are synchronous. Foreign creating-domain access
      returns Error before reading mutable state; no work or snapshot is created. *)
  val original : t -> (Scope_outcome.Cleanup.t option, Agent_protocol.Error.t) result

  (** Fresh immutable view of the SAME original diagnostic recipe, or None while
      its original producer runs. Late completion of an already selected child
      may appear in a fresh view; previously returned views never change.
      Creating-domain validation precedes holder observation. This synchronous
      read performs no cleanup, consumes no legacy delivery and grants no retry,
      custody or disposal proof. [diagnostics_complete] concerns original
      diagnostic completeness only; an empty view does not prove cleanup. *)
  val original_view : t -> (Cleanup_original_view.t option, Agent_protocol.Error.t) result

  val snapshot : t -> (Snapshot.t option, Agent_protocol.Error.t) result

  (** External factual operation only. Explicit Owner avoids storing Owner.t in
      early source/lease data or a recursive Owner dependency. Check creating
      domain, private Owner tag, physical Actor and captured source-owner identity
      before mutation/IO. This association is not source/operation authority.
      [after] must be this exact holder's latest completed non-Joined snapshot.
      Refuse foreign/stale/concurrent/original-running before yielding. Reserve
      nonyieldingly in the same-domain holder BEFORE mutex acquisition (which
      can yield). Under protected execution validate exact Owner/source exclusion
      under its mutex, then drive retained operations outside it. Gate refusal
      releases reservation without replacing snapshot or inventing a failure.
      Publish new immutable snapshot before releasing an executed reservation.
      Admitted execution is protected: caller cancellation is deferred while
      the finite set of retained duties runs. This is NOT a wall-clock bound:
      an actual close/canonical IO can await its real owner while protected.
      Live original producers return Blocked rather
      than being waited here; observation cancellation never becomes producer failure.
      Actual failures are DATA in snapshot with original exception/rawtrace; lifecycle refusals
      are outer Error. Original reports never change. Successful duties do not
      replay; no new source, Begin/Init/install or opaque closer reconstruction.
      Retryable means an existing exact owner permits an attempt, not promised
      healing. Known immutable negatives remain; required deferred End duties run
      only after their captured prefix is positively complete. *)
  val retry
    :  t
    -> owner:owner
    -> after:Snapshot.t
    -> (Snapshot.t, Agent_protocol.Error.t) result
end

module Source_consumer : sig
  (** One exact existing lexical source stage and its actual completion report.
      Only authentic registered source callbacks issue it. No execution/source
      authority, Cell identity, whole-Owner retirement or retry permission. *)
  type t

  (** Nonyielding same-domain check of this source's own open gate. Never checks
      or selects a replacement source from the current Owner. *)
  val check : t -> (unit, Agent_protocol.Error.t) result

  (** Nonyielding exact-source revocation. Closes this stage's construction and
      source gates without cancelling a fiber, acquiring a mutex, forking or
      changing Owner pending/unloading. Safe to repeat after its stage ended. *)
  val revoke : t -> (unit, Agent_protocol.Error.t) result

  (** EXTERNAL observer only: never from this source callback/driver/finalizer,
      the Actor mailbox, or Owner/Registry locks. Revoke the SAME source, invoke
      its captured actual source-lease cancellation, then await its existing
      actual finalizer report. The observer remains cancellable; interruption
      does not resolve/change the source report or certify cleanup. Source work
      and its original protected finalizers keep their existing ownership.
      Raised cancellation-callback errors keep their original exception/trace.
      Repeated joins return the SAME immutable report, including failure. This
      never retries failed cleanup, retires physical resources on its own, or
      certifies whole-Owner/Cell retirement. Foreign-domain use returns Error. *)
  val join : t -> (Scope_outcome.Cleanup.t, Agent_protocol.Error.t) result

  (** The SAME original lexical source cleanup holder; observation is separate
      from cancellation/join and grants no new source authority. Failed original
      source cleanup continues to exclude fresh source and administration
      admission until the SAME holder is canonically Joined; physical release
      alone does not discharge an owed source End. An acknowledged handoff keeps
      its existing binding-specific execution checks. *)
  val cleanup : t -> Cleanup_recovery.t
end

module Resource_retirement : sig
  (** One actual Owner retirement attempt. The opaque handle belongs to the
      creating Owner/domain and retains its immutable completion outcome and
      Whole-owner or exact-Cell coverage independently of required phase strength.
      A narrower Cell completion never certifies Whole-owner cleanup. It is not
      an execution/source grant or a certificate for another resource. *)
  type t

  (** One claimed producer for the actual retained target. No caller can construct
      or retarget it. Immediate claim captures Owner custody under its existing mutex;
      signalled work captures physical custody after actual source/admin finalizers. *)
  type work

  type claim =
    | Run of work
    | Join of t

  val completion : work -> t

  (** Observe only this attempt's actual result. None is Queued/Running, not
      successful cleanup. Observation neither starts cleanup nor retries it.
      Wrong-domain use raises Cleanup_failed; no completion is fabricated. *)
  val peek : t -> unit Scope_outcome.primary option

  (** External producer only: outside the Actor mailbox, this Owner's callbacks,
      retained native/source driver and all Owner/Registry locks. Execute the
      claimed attempt once and publish every actual protocol result, exception
      or cancellation with its original raw trace. Returning from run is not the
      result projection; callers still await the immutable completion. A producer
      and fail_to_start compete for the single Queued transition; after run starts,
      fail_to_start cannot overwrite or unblock its result. Duplicate
      producer invocation must not repeat physical cleanup. *)
  val run : work -> unit

  (** Settle a producer that could not be launched. Negative evidence only: no
      physical close or automatic retry is performed. Owner custody/exclusion
      remains for the explicit retry, and the original failure is observable. *)
  val fail_to_start : work -> Scope_outcome.Cleanup.Failure.t -> unit

  (** External join only. Returns the actual protocol result or reraises the
      original exception/cancellation/raw trace. This never retries cleanup or
      creates an acquired/Joined Scope_outcome report. Repeated observers see
      the same outcome; observing an old attempt cannot join a newer one. A
      cancelled observer still awaiting completion does not acknowledge an unseen
      failure. Capturing and projecting an actual completed Raised Cancelled
      outcome does acknowledge that actual producer outcome. *)
  val await : t -> (unit, Agent_protocol.Error.t) result
end

(** External, creating-domain operation. Under the existing Owner mutex, capture
    actual current native/partial/Product/policy/foreground custody and claim
    complete physical resource retirement, or join its same pending attempt.
    An already-active source/admin stage is refused with a retryable Conflict
    BEFORE publishing any election or mutating that stage. Its finalizer may
    itself join ordinary unloading; this operation does not wait that stage or
    claim its cleanup joined. The integration must join the actual stage under
    its separately owned admission exclusion before retrying this claim.
    Install retirement exclusion before returning Run and before launch can yield.
    No source/grant/execution guard is required merely to release resources.

    Reuses/strengthens an existing weaker Whole-owner ordinary unload under
    continuous exclusion. An elected exact-Cell completion refuses this broader
    direct claim; permanent joined close treats it only as an ordering barrier
    before selecting its own Whole-owner work. Its eventual result must cover the requested complete retirement;
    joining only the weaker phase is insufficient. Existing permanent closed is
    independent: this reusable operation never sets it or reopens a closed Owner.
    Permanent close preserves a source-finalizer ordinary prefix while signalled
    work is pending, then joins the same complete attempt;
    a new attempt for an already closed Owner requests complete retirement.

    Unobserved failed attempts with retained custody are joined, not implicitly
    retried. An original diagnostic may remain on a completed settled handle
    after its current Owner slot clears; that handle stays immutable, but no
    longer authorizes retry or blocks later ordinary admission. Partial/failed
    release targets remain the SAME Owner/Cell targets, never reconstructed from
    IDs or source data. If an unregistered surviving native borrower remains,
    fail closed retaining custody; the host must join its actual consumer cleanup.
    No registry Entry/Actor/writer/Handle disposal or logical descendant walk.

    Not mailbox/on_revoked safe: lock acquisition may yield. Use [signal_resource_retirement] to install the same pending attempt without
    waiting, then launch its external producer. Source-stage admission and native-loan release rules remain
    the existing actual custody checks. *)
val claim_resource_retirement
  :  t
  -> (Resource_retirement.claim, Agent_protocol.Error.t) result

(** Creating-domain, NONYIELDING signal: no mutex acquisition, Actor/Store RPC,
    fork, cancellation or physical cleanup. Installs pending admission exclusion
    before returning Run. A duplicate joins the SAME retained immutable attempt,
    including failure. A settled diagnostic-only handle may have left the current
    Owner slot; observing it cannot select a replacement or authorize retry.
    If no actual source/admin extent exists, an existing ordinary unload
    may be strengthened/joined without creating pending.

    A new pending record captures only this Owner plus association to an ALREADY
    actual source/admin extent, if any. It captures NO leases/binding/native loan/
    closed snapshot. An in-flight admitted stage attaches its actual extent to
    the same record before publication. Resource custody is captured only after
    actual extents/ordinary producer join and atomic pending -> unloading move.

    External work ownership is exactly existing Resource_retirement.run and
    fail_to_start. Mailbox/on_revoked caller must return without running/awaiting
    its own source/admin extent. *)
val signal_resource_retirement
  :  t
  -> (Resource_retirement.claim, Agent_protocol.Error.t) result

(** NONYIELDING explicit retry of the SAME Owner's exact current failed handle
    in pending or unloading. Validates domain/mutex/physical current identity and
    Finished failure with retained unsettled custody. A settled/cleared old
    failure is no longer current retry authority. Retains old immutable outcome;
    creates one new All_resources
    attempt using the SAME run/join/fail_to_start API. No automatic retry on signal
    or ordinary claim. Current live extent only is associated with the new attempt;
    an already completed old extent failure is not silently replayed forever.
    If a queued/prelude retry fails before physical promotion, the next explicit
    retry carries the actual retained failed unloading producer, rather than
    replaying an intervening launch failure or losing that physical lineage.
    Coverage remains immutable; another-coverage failed physical predecessor
    refuses before publishing the successor. *)
val retry_signalled_resource_retirement
  :  t
  -> failed:Resource_retirement.t
  -> (Resource_retirement.claim, Agent_protocol.Error.t) result

(** Explicit retry of this Owner's exact current failed attempt. A different
    Owner/domain/attempt, unfinished attempt or replaced target rejects. The
    retained target cannot be replaced by fresh work between failure and retry.
    The old handle keeps its original result; a successful new attempt does not
    rewrite it. No native/ancestor preparation or callback is replayed. *)
val retry_resource_retirement
  :  t
  -> failed:Resource_retirement.t
  -> (Resource_retirement.claim, Agent_protocol.Error.t) result

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
    preserve selected source/parent resource ownership. [source_consumer] names
    this SAME registered lexical stage; it is issued before this callback and
    never reconstructed from the Actor, prepared code or Init observations. It
    carries no Cell target or dependency registration by itself. *)
type deferred_builder =
  initialization_cleanup:Initialization_capture.t
  -> source_consumer:Source_consumer.t
  -> source:Agent_session.Runtime_builder.Source.t
  -> expected_product:Agent_session.Runtime_builder.Product_identity.t option
  -> construction:Agent_session.Session_actor.Runtime_construction.t
  -> ( Agent_session.Runtime_builder.Attachment.t Construction_result.t
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

  (** Issued after this actual source lease is installed; no handle is made
      when cancellation ownership is absent. *)
  val consumer : t -> Source_consumer.t

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

module Delegated_loader : sig
  (** One invocation callback, explicitly raw or paired. Neither adapter grants
      source ownership, cleanup custody or replay authority. A legacy callback's
      unpaired negative has an opaque actual callback boundary; it does not claim
      to recover the identity of an already flattened parent cause. *)
  type t

  val legacy : delegated_loader -> t

  val observed
    :  (source:Delegated_load_source.t
        -> retention:Delegated_load_retention.t
        -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result
             Agent_session.Close_observation.t)
    -> t
end

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

(** SAME acquired source-stage engine as the reported wrapper. Preserve the
    supplied actual callback observation and selected source-cleanup origin before
    raw projection. The existing source consumer retains cleanup separately; this
    immutable observation grants no authority. Creating-domain/source checks,
    callback and finalizers have exactly their ordinary invocation lifetime. *)
val with_delegated_load_source_observed
  :  t
  -> source_owner:Agent_session.Runtime_source_owner.t
  -> custody:Agent_session.Session_actor.Delegated_load.Custody.t
  -> (Delegated_load_source.t
      -> ('a, Agent_protocol.Error.t) result Agent_session.Close_observation.t)
  -> ('a, Agent_protocol.Error.t) result Agent_session.Close_observation.t

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

(** Selected result of the SAME deferred constructor/Init/install/finalizer
    invocation. Existing raw [load_delegated_runtime] resumes this observation.
    Original End and physical cleanup remain separate actual duties; no callback
    or cleanup is replayed by observing or projecting this immutable value. *)
val load_delegated_runtime_observed
  :  t
  -> prepared:Prepared_delegated_load.t
  -> retention:Delegated_load_retention.t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result
       Agent_session.Close_observation.t

(** The actual verified-source Factory supplies this additional cold constructor
    and its hidden issuer key. This owner captures its own immutable issuer
    association, independently of its Actor. Ordinary constructors have no such
    association; copying an Actor or its read-only identity cannot authenticate a
    second runtime owner. The Factory-owned delegated runtime driver privately
    retains the SAME supplied issuer. Some delegated_load is supplied only for
    genuine delegated children; no saved creator or new Goal/Run is inferred.
    Ordinary loading of other owners retains its existing authenticated builder.
    No issuer getter or replacement exists. *)
module Policy_resource_cleanup : sig
  type t

  (** One trusted Factory Cell's factual port, never a resource registry or
      executor. [pending] must not yield or mutate: it exposes retained partial
      construction/failed cleanup to existing admission and maintenance fences.
      [close_pending] runs outside Owner locks after actual leases and native
      cleanup owners join. It retries only the same retained teardown facts;
      healthy products with surviving resource loans are no-work. Errors remain
      retained; no returned failure is a proof of no write or completed close. *)
  val create
    :  pending:(unit -> bool)
    -> close_pending:(unit -> (unit, Agent_protocol.Error.t) result)
    -> t

  (** Reported variant of the SAME fixed cleanup port. One actual callback is
      stored; original Cell groups/raw primary are forwarded without minting a
      new failure for result projection. Existing [create] remains opaque and
      preserves its one actual callback's legacy result. A negative observation
      without a nonempty declared primary retains all supplied groups and owns
      one opaque boundary failure; no primary is guessed from secondary payloads.
      No custody authority. *)
  val create_reported
    :  pending:(unit -> bool)
    -> close_pending:
         (unit -> (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t)
    -> t

  (** Bind the SAME actual Factory Cell. Pending and close project that one Cell's
      existing methods, with no second closer or target allocation. The descriptor
      supplies factual Cell affinity to Resource_consumer; legacy callback
      constructors remain opaque and cannot issue that association. *)
  val of_cell : Session_policy_resource.t -> t
end

module Resource_consumer : sig
  type owner = t

  (** One actual Held Cell unit captured against this Owner's fixed descriptor.
      Retains the Owner and exact target, not just their current identities. No
      execution, source, cancellation or close authority is issued. This factual
      association does not prove publication into the current Owner binding. *)
  type t

  module Status : sig
    (** Read-only public projection; the private Cell facets are not exposed. *)
    type t =
      | Held
      | Released
    [@@deriving equal, sexp_of]
  end

  (** Nonyielding: check the Owner creating domain before reading its descriptor
      or target, then validate the SAME Cell and require actual Held. Foreign
      targets, Released units and opaque callback descriptors refuse. No Cell,
      cleanup target, native identity, work or permission is fabricated. *)
  val capture
    :  owner
    -> target:Session_policy_resource.Cleanup_target.t
    -> (t, Agent_protocol.Error.t) result

  (** Nonyielding domain check then SAME captured Owner Cell and target. Released
      is sticky positive cleanup for that exact unit, including after replacement.
      Does not touch the replacement, cancel/wait/close, or renew authority. *)
  val check : t -> (Status.t, Agent_protocol.Error.t) result

  (** Nonyielding association of this authentic Held Cell unit with the SAME
      open Source consumer. Both creating domains are checked before mutable
      reads, then exact Owner affinity and fixed Cell provenance. First binding
      is write-once on BOTH actual identities: repeating SAME Source and target
      is idempotent; another Source or target refuses. A new genuine capture may
      be associated with another actual Source. The exact Source recovery retains
      its association after the live stage clears. Released target or closed Source refuses. Source and Resource cleanup
      remain separate obligations; this grants no borrow/retirement authority. *)
  val bind_source : t -> source:Source_consumer.t -> (unit, Agent_protocol.Error.t) result

  module Request : sig
    type t =
      | Released
      | Retirement of Resource_retirement.claim
  end

  (** Creating-domain, NONYIELDING election for this SAME captured unit. Checks
      its fixed Cell target and actual Source/resource affinity before installing
      exclusion or strengthening work. Reuses the existing Owner engine only
      when all affected custody is proven to belong to that unit; mixed or
      unassociated custody refuses before effects. A matching original failure
      is joined even after physical release; this never implicitly retries it.
      [Released] is physical DATA only: the caller must still join this token's
      exact bound Source and retain its real cleanup report. It is not a fabricated
      retirement, Source completion, or certificate for a replacement resource.
      A fresh Held election refuses after permanent close has started. Joining
      an already covering Whole attempt, retaining an exact original handle, and
      its permitted explicit retry remain separate existing operations.
      Run/Join producers obey [Resource_retirement]'s external-only requirements. *)
  val signal : t -> (Request.t, Agent_protocol.Error.t) result

  (** NONYIELDING explicit retry of this token's SAME retained failed completion.
      Keeps the old handle immutable, retains exact physical predecessor coverage,
      and joins an already elected successor of that exact recovery. Does not
      authorize a broader Whole-owner retry or replay a successful physical leg.
      A mismatched, unfinished or unavailable retained attempt refuses. *)
  val retry
    :  t
    -> failed:Resource_retirement.t
    -> (Request.t, Agent_protocol.Error.t) result
end

(** Read-only downstream dependency identity. Ordinary ancestry and Native
    provider leases share this Source-or-actual-Cell association. These values
    issue no execution, close, retirement or cancellation permission. Actual
    first-party relation registration remains coupled to the external drain. *)
module Native_consumer : sig
  type owner = t
  type t

  (** Bind the actual downstream Owner, after domain/open-source and exact
      source recovery affinity checks. Retained cleanup uses this SAME Owner;
      no current-ID lookup, callback replay or new retry permission is granted. *)
  val source : owner:owner -> Source_consumer.t -> (t, Agent_protocol.Error.t) result

  val resource : Resource_consumer.t -> t

  (** Nonyielding: creating domains first, then exact actual Owner affinity and
      admission lifetime. Source uses its authentic recovery affinity and must
      remain open; Resource retains the actual Owner and must still be Held in
      its fixed Cell. Factory must check against its SAME selected target Owner
      before existing Actor/Handle/private-reference authorization. A valid
      consumer for another Owner cannot authorize this edge. Cleanup later uses
      original factual ownership rather than renewing this admission check. *)
  val check_owner : t -> owner:owner -> (unit, Agent_protocol.Error.t) result
end

(** Optional [consumer] on the fixed native, loaded-source and ordinary lifetime
    ports associates the SAME admitted provider lease with this authentic
    downstream Source or Cell consumer. It is not an execution grant. Callers
    must check [Native_consumer.check_owner] against their actual downstream
    Owner before borrowing; the provider separately captures its own actual
    physical basis. The registered dependency is joined externally during
    retirement, outside Owner locks and its own driver. Omission preserves the
    existing unassociated lexical behavior. *)

(** Factory-only ordinary-root warm binding. Called outside the Owner mutex
    under its exact source/resource stage. First pin the same selected Product;
    only initial Empty/None returns absence. Selected errors never cold-fallback.
    [expected] is the original Actor source/Stop basis captured before waits. *)
module Ordinary_warm : sig
  type selection =
    | No_retained_product
    | Attached of
        { attachment : Agent_session.Runtime_builder.Attachment.t
        ; expected : Agent_session.Session_actor.Ordinary_attachment_basis.t
        }

  type prepare =
    expected_product:Agent_session.Runtime_builder.Product_identity.t option
    -> original_actor:Operator_authorization.t option
    -> run_preparation:Agent_session.Run_preparation.t option
    -> (selection, Agent_protocol.Error.t) result
end

module Policy_restoration_source : sig
  (** Actual Owner's lexical source stage. No Run/Init/lifecycle admission is
      invented. Factory must not retain this value beyond its fixed restore port.
      Checks and physical publication belong to the creating Eio domain. *)
  type t

  (** SAME registered lexical source association supplied by the actual Owner
      callback before Factory restoration. This read-only accessor neither
      chooses a Cell target nor registers a dependent borrow. *)
  val consumer : t -> Source_consumer.t

  val check : t -> (unit, Agent_protocol.Error.t) result

  module Publication : sig
    module Custody : sig
      type t =
        | Caller_retains
        | Owner_retains
    end

    type t

    val custody : t -> Custody.t

    (** Resume the original returned result or cancellation/exception with its
        captured raw backtrace, AFTER Factory records actual custody. This
        reports no physical cleanup or durable acknowledgement. *)
    val resume : t -> (unit, Agent_protocol.Error.t) result
  end

  (** Issues the genuine Actor purpose against the SAME captured request and
      governance, and captures it before pending cancellation. The repeatable
      authorize closure is run outside Actor and Owner locks. One prepare only. *)
  val prepare
    :  t
    -> source_owner:Agent_session.Runtime_source_owner.t
    -> source:Agent_session.Runtime_builder.Source.t
    -> basis:Agent_session.Policy_restore_basis.t
    -> (Agent_session.Session_actor.Policy_restoration.t, Agent_protocol.Error.t) result

  (** Transfers the actual first Cell lease into this Owner's pending anchor
      BEFORE the promotion RPC can yield. On subsequent Error/exn the same loan
      remains owned for joined cleanup; it is never interpreted as Empty. The
      returned report distinguishes caller versus Owner custody, including
      cancellation/exn. Inspect its custody BEFORE resuming the primary. Publish
      exactly once from the fixed Factory port after its final adjacent checks. *)
  val publish
    :  t
    -> restoration:Agent_session.Session_actor.Policy_restoration.t
    -> loan:Session_policy_resource.Lease.t
    -> access:Agent_session.Runtime_builder.Restored_policy_access.t
    -> Publication.t
end

module Native_resource_origin : sig
  module Route : sig
    type t =
      | Direct of Agent_store.Delegation_store.Reference.t
      | Ancestry of Agent_session.Delegation_authority.Resource_path.Prefix.t
  end

  module Validation : sig
    (** Actual current validator result, never cached permission. Ancestry is
        the literal observation of the SAME supplied prefix and original leaf. *)
    type t =
      | Direct
      | Ancestry of Agent_session.Delegation_authority.Resource_path.Observation.t
  end

  (** Immutable actual Factory ingress association. Factory validator closes over
     SAME original Source_target (actual Actor/Owner/Handle), not a successful
     grant, lexical Native_resource_admission token or Actor Claim. The validator
     freshly re-resolves current private/source/host facts whenever invoked.
     This value never selects a different leaf or creates execution permission. *)
  type t

  val create
    :  source_owner:Agent_session.Runtime_source_owner.t
    -> route:Route.t
    -> authorize:
         (route:Route.t
          -> target:Agent_session.Session_state.t
          -> (Validation.t, Agent_protocol.Error.t) result)
    -> t

  val route : t -> Route.t

  (** DATA-only next prefix of SAME frozen ancestry. Direct returns None; direct
     callers retain original immediate-parent operation. No IO/new record/grant.
     Same original validator/leaf association, no new Factory issuer. *)
  val parent : t -> (t option, Agent_protocol.Error.t) result
end

module Native_resource_admission : sig
  (** One actual Owner-issued consumer extent: exact Factory source_owner,
      Reference/target/domain, original per-call current-authority closure and
      lexical closure state. Never retained in an anchor as future permission. *)
  type t

  val origin : t -> Native_resource_origin.t

  (** Exact terminal borrowed edge for Direct and Ancestry origins. *)
  val reference : t -> Agent_store.Delegation_store.Reference.t

  (** SAME actual outside-lock check engine. Checks domain/extent/target before
      any callback, invokes current guard and authentic original validator,
      validates constructor/exact prefix issuer/terminal target association, then
      rechecks extent/current guard after waits. Returns that actual immutable
      observation, not a last-success getter or persistent grant. Fixed Factory
      access still validates selected source/profile/current read-set via Basis.
      Cancellation and validator errors retain their existing order. *)
  val observe
    :  t
    -> expected:Agent_session.Session_state.t
    -> (Native_resource_origin.Validation.t, Agent_protocol.Error.t) result

  (** Unit projection of [observe], with one execution of the SAME check engine.
      Not a substitute for fixed Factory private linkage/grants. *)
  val check
    :  t
    -> expected:Agent_session.Session_state.t
    -> (unit, Agent_protocol.Error.t) result
end

module Native_source_stage : sig
  (** Existing selected source stage with Native_setup purpose. Same stage
      excludes competing ordinary/policy construction; never a Run preparation. *)
  type t

  (** The SAME registered lexical source; not the later retained Cell lifetime. *)
  val consumer : t -> Source_consumer.t

  val check : t -> (unit, Agent_protocol.Error.t) result
  val admission : t -> Native_resource_admission.t

  (** One prepare. Captures actual Actor token before cancellation can resume;
      finalizer closes then ends it, preserving primary and real cleanup report. *)
  val prepare
    :  t
    -> source_owner:Agent_session.Runtime_source_owner.t
    -> source:Agent_session.Runtime_builder.Source.t
    -> expected:Agent_session.Session_state.t
    -> (Agent_session.Session_actor.Native_setup.t, Agent_protocol.Error.t) result

  module Publication : sig
    module Custody : sig
      type t =
        | Caller_retains
        | Owner_retains
    end

    type t

    val custody : t -> Custody.t
    val resume : t -> (unit, Agent_protocol.Error.t) result
  end

  (** Actual first native lease moves into existing Owner baseline custody
      nonyieldingly BEFORE publication/cancellation can escape. Inspect custody
      before resume on EVERY returned Error or raised cancellation/exception.
      No separate Actor Published permission is created. Anchor keeps only actual
      loan/native/source facts; no old admission/authorize callback as authority. *)
  val publish
    :  t
    -> setup:Agent_session.Session_actor.Native_setup.t
    -> loan:Session_policy_resource.Native_lease.t
    -> Publication.t
end

module Native_resource_access : sig
  (** Fixed ports bound by actual Factory to SAME Actor/Handle/Cell. Every call
      receives THIS consumer admission; prepare/pin/check independently re-resolve
      original Reference, exact ancestor/target linkage, original resource grant,
      selected source/profile and current private authority. No current grant is
      inferred from a serializable Reference or successful supplied closure. *)
  type t

  (** [check_custody] is fixed to the SAME Actor/Handle/Cell. [Resources]
      checks current use. [Retained_source] compares the exact selected Source
      already held under the loan, including during same-unit attachment. Both
      revalidate current State/profile and require the actual existing loan. *)
  type custody_check =
    | Resources
    | Retained_source of Agent_session.Runtime_builder.Source.t

  val create
    :  prepare:
         (admission:Native_resource_admission.t
          -> stage:Native_source_stage.t
          -> (unit, Agent_protocol.Error.t) result)
    -> pin:
         (admission:Native_resource_admission.t
          -> expected_native:Agent_session.Runtime_builder.Native_identity.t
          -> (Session_policy_resource.Native_lease.t, Agent_protocol.Error.t) result)
    -> check:
         (Session_policy_resource.Native_lease.t
          -> admission:Native_resource_admission.t
          -> expected:Agent_session.Session_state.t
          -> (unit, Agent_protocol.Error.t) result)
    -> check_custody:
         (Session_policy_resource.Native_lease.t
          -> custody_check
          -> (unit, Agent_protocol.Error.t) result)
    -> t

  (** One actual preparation callback, forwarding its selected observation.
      The raw constructor remains an opaque callback boundary. Neither form
      changes pin/check validation or grants admission. *)
  val create_observed
    :  prepare:
         (admission:Native_resource_admission.t
          -> stage:Native_source_stage.t
          -> (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t)
    -> pin:
         (admission:Native_resource_admission.t
          -> expected_native:Agent_session.Runtime_builder.Native_identity.t
          -> (Session_policy_resource.Native_lease.t, Agent_protocol.Error.t) result)
    -> check:
         (Session_policy_resource.Native_lease.t
          -> admission:Native_resource_admission.t
          -> expected:Agent_session.Session_state.t
          -> (unit, Agent_protocol.Error.t) result)
    -> check_custody:
         (Session_policy_resource.Native_lease.t
          -> custody_check
          -> (unit, Agent_protocol.Error.t) result)
    -> t
end

module Policy_resource_access : sig
  type loan = Session_policy_resource.Lease.t
  type t

  (** Factory binds both ports to the same actual Cell/Actor/Handle. Pin is
      nonyielding; prepare_access resolves the current profile outside Owner
      locks and validates the same active loan. Neither activates a runtime or
      grants admission. Owner rechecks lifecycle/source after every await. *)
  val create
    :  pin:
         (expected_product:Agent_session.Runtime_builder.Product_identity.t
          -> (loan, Agent_protocol.Error.t) result)
    -> prepare_access:
         (loan
          -> expected:Agent_session.Session_state.t
          -> ( Agent_session.Runtime_builder.Policy_access.t
               , Agent_protocol.Error.t )
               result)
    -> t

  (** Attach the real Factory's fixed restoration/validation ports. Existing
      create remains retained-only. restore is called only from an explicit
      missing selection under the actual Owner source stage; selected failures
      never choose this path. check_restored must check same Cell/Actor/Handle,
      profile and access; neither callback grants effect authority. *)
  val with_restoration
    :  t
    -> restore:
         (stage:Policy_restoration_source.t
          -> request:Agent_session.Script_tool_calls.preparation
          -> governance:Agent_session.Session_actor.Causal_governance.t
          -> authorize:(unit -> (unit, Agent_protocol.Error.t) result)
          -> (unit, Agent_protocol.Error.t) result)
    -> check_restored:
         (loan
          -> access:Agent_session.Runtime_builder.Restored_policy_access.t
          -> expected:Agent_session.Session_state.t
          -> (unit, Agent_protocol.Error.t) result)
    -> t
end

(** Advanced Factory construction. Each builder receives an invocation-scoped
    [initialization_cleanup] port for its original initialization facts. Forward
    it only within that invocation; do not retain or reuse it for another build.
    The simpler custom constructors require no such transport. The single
    [delegated_load] callback explicitly selects Legacy or Observed transport;
    forward actual known observations with [Delegated_loader.observed]. *)
val create_with_deferred_run_unload
  :  source_owner:Agent_session.Runtime_source_owner.t
  -> before_unload:(closing:bool -> (unit, Agent_protocol.Error.t) result)
  -> policy_resource_cleanup:Policy_resource_cleanup.t
  -> policy_resource_access:Policy_resource_access.t option
  -> native_resource_access:Native_resource_access.t option
  -> actor:Agent_session.Session_actor.t
  -> initial:Agent_session.Runtime_builder.Attachment.t option
  -> build:
       (initialization_cleanup:Initialization_capture.t
        -> original_actor:Operator_authorization.t option
        -> Agent_session.Run_preparation.t option
        -> (Agent_session.Runtime_builder.Attachment.t, Agent_protocol.Error.t) result)
  -> build_deferred:deferred_builder
  -> ordinary_warm:Ordinary_warm.prepare option
  -> delegated_load:Delegated_loader.t option
  -> t

(** [capture_cleanup] observes the actual standalone Cleanup_request bracket,
    including a loaded/no-Init request with no initializer duties. Called outside
    the Owner mutex within its finalizer, before construction work. A raised
    capture skips work and remains the original primary. Source/delegated paths
    keep their existing source holders; no outer Borrow is manufactured. *)
val ensure_loaded
  :  ?capture_cleanup:(Cleanup_recovery.t -> unit)
  -> t
  -> (unit, Agent_protocol.Error.t) result

(** Current original actor is passed to a supporting initializer only. Existing
    runtime ownership/policy still controls loading; this supplies no source,
    execution or instruction permission. The authenticated Factory builder retains
    this actor through cold legacy ChatML construction and its [Session_resume]
    callback. Host builders made without the authenticated constructor reject a
    supplied actor on cold load rather than silently discarding it. An already
    loaded runtime is checked without replaying its constructor. *)
val ensure_loaded_authenticated
  :  ?capture_cleanup:(Cleanup_recovery.t -> unit)
  -> t
  -> actor:Operator_authorization.t
  -> (unit, Agent_protocol.Error.t) result

(** Finite detachment refuses active or borrowed execution and every outstanding
    descendant Slot/ACK before mutating the runtime. Does not cancel work or wait
    on remote roots to make an ordinary unload eligible. Use the strong close or
    stop retirement path for explicit cancellation and retained factual drain. *)
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
    resources do not imply that [ensure_loaded] may resume execution.
    When signalled pending retirement exists, public Stop runs/joins it before
    returning. Its first failure is delivered unchanged; only a subsequent
    explicit Stop may retry the current failed attempt. A signal arriving during
    ordinary cleanup is joined before success. The exact source finalizer uses
    a private actual-extent-authorized ordinary prefix to avoid self-joining. *)
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
    has no runtime, active background borrower or unload in progress. Detached
    relation metadata remains owned and must be joined by normal entry closure;
    this reservation does not certify cleanup success. On success admission is
    permanently closed; the caller must finish normal entry closure outside the
    owner mutex before removing the entry. A failed reservation leaves the owner
    unchanged. Actor inactivity must be established separately by the registry. *)
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

(** Retain the SAME admitted runtime and provide its installed activation bound
    to THIS lexical lease. [activate = None] preserves actual absence; [Some run]
    invokes the original activation with the fixed per-invocation cleanup sink.
    It adds no execution authority. Keep the callback/activation within [f]; the
    closure checks its creating domain and original-open lease before invoking
    even an extension activation; wrong-domain/expired use returns Conflict before
    invoking the activation.
    Admission and cancellation
    have the same behavior as [with_background_runtime]. *)
val with_background_runtime_activation
  :  t
  -> (Agent_session.Runtime_builder.t
      -> activate:(unit -> (bool, Agent_protocol.Error.t) result) option
      -> ('a, Agent_protocol.Error.t) result)
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
  :  ?consumer:Native_consumer.t
  -> t
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
    separately. Actual intermediate disposal failures are retained in observation
    order even if a later finalizer succeeds; callback failures are not reclassified
    as cleanup failures. The report returns only after every acquired target finalizer
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

  (** The authentic Source consumer issued for this SAME reserved stage,
      after its existing pending-cancellation check and before the caller may
      acquire any parent dependency. No new preparation or execution grant. *)
  val consumer : t -> Source_consumer.t

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
module Lease_outcome : sig
  type 'a t

  module Primary_stage : sig
    type t =
      | Admission
      | Admission_batch
      | Before_callback
      | Callback
      | After_callback
      | Scope_exit_before_entry
      | Scope_exit
    [@@deriving equal, sexp_of]
  end

  (** Actual immutable original source selection, including Callback for a
      positive callback. Inspect Scope.primary separately. Before-entry scope
      exit means the actual Cancel.sub function never ran, not that capture or
      callback was absent. Entered scope exits may be real finalizer/seal escapes.
      No effects, producer minting, retry, delivery or custody authority. *)
  val primary_stage : 'a t -> Primary_stage.t option

  val outcome : 'a t -> 'a Scope_outcome.t
  val cleanup : 'a t -> Cleanup_recovery.t

  (** Actual unit-result projection paired before any raw resume. SAME known
      producer groups/selected primary are forwarded; this immutable DATA neither
      retries cleanup nor consumes legacy diagnostic delivery. Cleanup custody
      remains the separate [cleanup] holder. *)
  val observation
    :  unit t
    -> (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t
end

(** Holder captured before original admission can yield. Callback only publishes
    actual holder; it may not yield/reenter Owner or inject duties. Invocation is
    outside Owner locks inside the already-established original finalizer bracket,
    BEFORE admission can yield. If capture raises, skip admission, preserve it as
    original primary with rawtrace, and run/seal actual cleanup normally. No orphan
    holder or admission remains. Existing wrappers resume exactly the original
    Scope_outcome, including cleanup primary ordering. No outer borrow is
    reconstructed during retry. *)
val with_resource_lifetime_outcome
  :  ?consumer:Native_consumer.t
  -> t
  -> capture:(Cleanup_recovery.t -> unit)
  -> (unit -> ('a, Agent_protocol.Error.t) result)
  -> 'a Lease_outcome.t

val with_delegation_resources_outcome
  :  t
  -> capture:(Cleanup_recovery.t -> unit)
  -> (Agent_session.Runtime_builder.t -> ('a, Agent_protocol.Error.t) result)
  -> 'a Lease_outcome.t

(** Observed unit callbacks share the SAME original lease engine and admission,
    finalizers and capture timing as the outcome ports above. Execute one actual
    callback; preserve its known groups before projection. Admission/pre-callback
    and post-callback check failures retain their own actual control provenance.
    A callback failure remains primary over normally reported cleanup, while
    independent cleanup groups are kept. Existing raw precedence for an uncaught
    finalizer escape is preserved. These ports create no new cleanup authority
    or replay permission. *)
val with_resource_lifetime_observed
  :  ?consumer:Native_consumer.t
  -> t
  -> capture:(Cleanup_recovery.t -> unit)
  -> (unit -> (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t)
  -> unit Lease_outcome.t

val with_delegation_resources_observed
  :  t
  -> capture:(Cleanup_recovery.t -> unit)
  -> (Agent_session.Runtime_builder.t
      -> (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t)
  -> unit Lease_outcome.t

val with_background_runtime_observed
  :  t
  -> capture:(Cleanup_recovery.t -> unit)
  -> (Agent_session.Runtime_builder.t
      -> (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t)
  -> unit Lease_outcome.t

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

(** Factual stopped-policy source lookup from SAME already retained Product.
    Pins before awaited State/profile access, never cold-loads or invokes a
    callback. Before pinning, unresolved inference facts may settle outside the
    Owner mutex; other lifecycle/claim cleanup blockers keep their original
    rejection. Admission rechecks every guard after maintenance. The returned
    immutable observer grants no later claim authority. *)
val retained_moderation_source
  :  t
  -> (Agent_protocol.Invocation.observer option, Agent_protocol.Error.t) result

module Delegated_policy_resource : sig
  (** Scoped factual custody of ONE actual selected policy Product. Neither a
      claim nor policy/Goal admission. Both retained and restored selections pin
      that same Product with resource-only Owner custody; no Manager, native
      runtime, access object or closer escapes. Cannot outlive its callback. *)
  type t

  (** Creating-domain, live extent and exact Owner/resource association only.
      This check is not execution permission or a replacement for authorizing
      the real private edge/current Caller/governance. *)
  val check_current : t -> (unit, Agent_protocol.Error.t) result

  (** Check the current scoped resource, then physical identity with the immutable
      Source selected by its SAME already pinned Product. May wait for the Owner
      mutex, like [check_current]. No selection, load, permission or claim is
      issued. A different Source with equal contents still rejects. *)
  val check_source
    :  t
    -> source:Agent_session.Runtime_builder.Source.t
    -> (unit, Agent_protocol.Error.t) result

  (** Evaluate SAME preparation/governance captured by the resource extent, with
      this step's transformed Pre_tool_call. Fresh State/access, a DISTINCT real
      Cell loan and the existing Exclusive execution wrapper are acquired for
      this SAME Product. Same-Actor active claim reentry and unrelated Exclusive
      leases retain their original rejection. Both the original and this call's
      host authorizers remain required; nested policy tools use their own real
      Caller, never this request as ambient authority.

      Return the actual completed delegated receipt (including its validated
      decision) already produced by the existing Actor/Manager path. No new
      receipt or source proof is issued. Capture/holder/checkpoint/follow-up and
      current Goal/canonical checks keep their original order and authority.
      Actual inner cleanup remains separate from primary protocol failure or
      original exception/raw trace. The caller must record [cleanup], project
      only [primary] through its enclosing plain callback, then prepend cleanup
      before the final ordinary [resume]. *)
  val evaluate_reported
    :  t
    -> delegation:Agent_protocol.Moderator_execution.delegation
    -> event:Chat_response.Moderation.Event.t
    -> authorize:(unit -> (unit, Agent_protocol.Error.t) result)
    -> Agent_protocol.Moderator_execution.t Scope_outcome.t
end

(** Retain the actual retained/restored policy Product without allocating a claim
    or execution wrapper. Source restoration uses only the existing fixed Factory
    port under the genuine request/governance/source admission, and its stage
    ends before [f]. Preparation may have its existing admitted effects; this is
    not an atomic no-effects guarantee. A failed unit restoration preflight or
    its cleanup refuses admission before [f] and retains actual cleanup custody.
    [authorize_restoration] is confined to that source stage. The returned
    resource retains only the ordinary [authorize] for actual effects.

    The transient resource lease keeps the existing policy survives-Stop=false
    behavior; lawful Independent native resource retention is separate. No Owner
    mutex, Actor mailbox, Registry reservation or source stage spans [f]. Every
    evaluation refreshes current access, not the resource identity, and rechecks
    its original actual Caller/Goal/private/source guards. Actual cleanup reports
    remain separate from callback Error/exception/raw trace; do not wrap the plain
    legacy operation to reconstruct those reports. *)
val with_delegated_policy_resource_reported
  :  ?authorize_restoration:(unit -> (unit, Agent_protocol.Error.t) result)
  -> t
  -> request:Agent_session.Script_tool_calls.preparation
  -> governance:Agent_session.Session_actor.Causal_governance.t
  -> authorize:(unit -> (unit, Agent_protocol.Error.t) result)
  -> f:(Delegated_policy_resource.t -> ('a, Agent_protocol.Error.t) result)
  -> 'a Scope_outcome.t

(** Evaluate an owned child candidate using this parent's live manager, actor
    event/native services and history. The host authorizer must validate private
    delegation and the still-active child owner, repeatedly after waits. The parent
    actor commits decision, checkpoint and UI notices together; runtime requests
    stay with the parent. Replays do not rerun policy effects or notices.

    Before fresh admission, exact retained inference facts may settle outside
    the Owner mutex. This maintenance grants no execution authority and does not
    close live graphs. Existing lifecycle/claim/resource blockers still reject;
    all admission guards recheck after the await. Factual failure, cancellation
    or exception prevents admission without replacing an earlier terminal result.

    Required SAME Caller governance from the actual preparation pipeline.
    Registered callers bind through their genuine Factory issuer and private
    ancestry; zero targets do not select standalone compatibility. The Bound
    adds currentness checks, never parent effect permission or a second loan.

    An explicitly missing selection may use the Factory's fixed restoration
    port under this Owner's real source stage. Its first actual Cell lease stays
    in the Owner anchor; each callback pins a distinct lease. Restored_unloaded
    capture/effect guards apply in BOTH desired states, without selecting an
    ordinary worker or running lifecycle/queued work. Later explicit activation
    binds the same Product before releasing that baseline. *)
val prepare_delegated_tool
  :  t
  -> request:Agent_session.Script_tool_calls.preparation
  -> governance:Agent_session.Session_actor.Causal_governance.t
  -> delegation:Agent_protocol.Moderator_execution.delegation
  -> event:Chat_response.Moderation.Event.t
  -> authorize:(unit -> (unit, Agent_protocol.Error.t) result)
  -> (Agent_protocol.Moderator_execution.Decision.t, Agent_protocol.Error.t) result

(** [with_administration t f] excludes concurrent runtime loading while [f]
    prepares and commits stopped state. Preserve the previous runtime on failure;
    retire it after success without turning an accepted commit into a failed
    response because of cleanup. Cancellation and exceptions do not poison the
    owner mutex. Actual resource close/join runs outside that mutex, while the
    exact retired resource remains owned until success. Do not call
    runtime-owner operations from [f].

    On all three administration functions below, [capture_cleanup] receives the
    SAME original holder after an actual administration transition or delegated
    source lease is registered, before that allocation's fallible cleanup or
    commit. The callback is nonyielding, runs outside the owner mutex under
    cancellation protection, and already has an installed cleanup finalizer.
    If it raises, its exception and raw backtrace remain the primary outcome
    while owned cleanup still runs. Rejection before allocation skips capture. *)
val with_administration
  :  ?capture_cleanup:(Cleanup_recovery.t -> unit)
  -> t
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
    after releasing the owner fence. Resource close/join releases the mutex;
    the same administration transition still excludes fresh runtime admission.
    Read-only Owner observations may progress during that join. An accepted
    concurrent unload/retirement permanently invalidates this selected admin
    transition before its commit; rejoin returns Conflict even if the competing
    retirement has already finished. Failed admission does not retire it. *)
val reinitialize_administration
  :  ?capture_cleanup:(Cleanup_recovery.t -> unit)
  -> t
  -> validate:(unit -> (unit, Agent_protocol.Error.t) result)
  -> commit:(unit -> (unit, Agent_protocol.Error.t) result)
  -> before_initialize:(unit -> unit)
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

(** Original admission is checked under this owner fence before old resources
    are joined. [commit] reconciles only tracking ACK drift against that original
    basis. No initialization occurs; failures and cancellation propagate. *)
val retire_administration
  :  ?capture_cleanup:(Cleanup_recovery.t -> unit)
  -> t
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

module Schedule_delivery : sig
  (** Actual ordinary timer delivery outcome. [Deferred error] retains the
      original transient refusal. It certifies either the exact pure ordinary
      preparation busy guard before any preparation effect, or an actual delivery
      lease admission that acquired no lease and whose cleanup did not fail.
      It grants no execution, source or retry authority by itself. *)
  type t =
    | Delivered
    | Deferred of Agent_protocol.Error.t
end

(** Attempt the existing delivery while retaining source-owned admission evidence.
    Only the initial pure ordinary-preparation busy guard or a clean no-acquisition
    transient Conflict after successful preparation returns [Deferred]. Later
    preparation failures, admitted callback/queue failures, unknown writes and
    cleanup failures retain the existing Error/exception
    contract. Caller cancellation propagates; local retirement interruption of
    an admitted lease remains [Interrupted]. The scheduler must still use the
    actor's generation/terminal-checked retry operation. *)
val deliver_schedule_attempt
  :  t
  -> Agent_protocol.Schedule.t
  -> (Schedule_delivery.t, Agent_protocol.Error.t) Result.t

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
    A finite inference-usage sweep runs outside the owner mutex before selecting
    pending work. Deferred initial confirmation retains unfinished facts while
    ordinary polling proceeds; it grants no execution authority. Actual failures
    prevent work selection. Factual progress alone does not make the result true.
    A poll finding no pending work under the owner mutex publishes no execution
    lease, so it cannot exclude a queued source-stage acquisition. Real work is
    selected, loaded and leased under the same mutex; draining then retains that
    cancellable lease rather than the owner mutex across moderator work.
    Cancellation confined to
    the owned event returns Interrupted, keeping the shared scheduler alive;
    cancellation of the caller still propagates. Later polling and administration
    remain usable. Runtime installation and each callback's ownership transfer
    retain protected lifecycle boundaries; owned legacy batches remain cancellable
    between and during handlers. The standalone aggregate legacy fallback keeps
    its finite claim/drain/capture block cancellation-protected. Its exact
    Actor-owned completion retains uncertain metadata writes and acknowledged
    operation cleanup; errors do not invoke an unscoped failure transition.
    Retirement joins callback exit outside the owner mutex before resources
    close. Normal v1 declaration admission
    remains separate; this does not enable public features.

    [capture_cleanup] receives the actual lexical cleanup holder after its
    finalizer is installed, outside the Owner lock. A capture exception remains
    the original primary and the installed finalizer still seals the holder.
    Preflight rejection before lease allocation does not capture. Actual installed
    activation/legacy claims enroll in THIS invocation's holder. *)
val drain_idle_moderator
  :  ?capture_cleanup:(Cleanup_recovery.t -> unit)
  -> t
  -> (bool, Agent_protocol.Error.t) result

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
    of retirement. The actor must remain running through cleanup. Retained
    synchronous constructor Job settlement and its callback can remain pending
    until explicit [unload_and_wait] or [close_and_wait], even when the producer
    has already exited. A cancellation request is not a completed producer join;
    finite close does not detach that pending runtime or claim its cleanup joined.

    Failed close retains the exact retired resource for explicit unload/close
    retry. It is never returned as loaded again. Unborrowed pending cleanup
    blocks constructing another runtime or replacing its source; a separate
    already-loaded current runtime retains its original execution checks. A positive retry removes that resource only
    after its close closure returns successfully. Errors remain errors, including
    an unavailable canonical writer that requires recovery. Actual close/join
    runs outside the Owner mutex. Concurrent cleanup callers join the same close
    attempt and observe its exact outcome; they do not retry a failed attempt
    implicitly. Read-only observations do not trigger cleanup. Maintenance and
    inactive-close reservation remain excluded until retained cleanup is settled.
*)
val close : t -> unit

(** Close and join foreground/dependency/background cleanup before shutting down the actor or
    its persistence writer. Concurrent accepted-stop cleanup is joined, including
    an actual All-resources retirement accepted while this close's final cleanup
    yields. The same invocation captures and joins that exact attempt outside the
    Owner mutex before its final disposal check; a failed late attempt is not
    retried implicitly, and completed physical stages are not replayed. A typed
    dependency failure raises [Cleanup_failed], retaining resources for retry.
    Retained aggregate legacy completion must also retire successfully; an
    uncertain write or unresolved acknowledged cleanup prevents raw close and
    remains an error rather than replaying the callback or persistence writer;
    other exceptions retain their original backtrace. Call from the external
    session lifecycle, never a retained callback (which would await itself).

    [capture_report], when supplied, runs after THIS actual invocation's report
    is created and before its original legacy return/exception is resumed. It
    executes outside the Owner state mutex, under joined-close serialization.
    It is a trusted nonyielding, normal-returning assignment hook: do not call
    cleanup or retain an executable observer. While no new resource-retirement
    work has been accepted, a cached disposed invocation supplies the SAME report
    without re-executing any stage. Inspection does not consume diagnostics.
    An uncached Strict failure acknowledges only its actual
    selected producer; historical/independent secondary diagnostics remain
    observable and available for subsequent cached legacy presentation. *)
val close_and_wait : ?capture_report:(Session_disposal_report.t -> unit) -> t -> unit

module Disposal_report : sig
  type t = Session_disposal_report.t

  type disposition =
    | Retained
    | Joined
  [@@deriving equal, sexp_of]

  (** Joined is actual permanent runtime custody settlement, independently of
      immutable cleanup diagnostics. It grants no source/execution admission and
      does not alter an original cleanup report or snapshot. *)
  val disposition : t -> disposition

  val failures : t -> Scope_outcome.Cleanup.Failure.t list
end

(** Run the SAME permanent close/join stages, retaining all actual diagnostics.
    A known report-only source barrier can admit the remaining existing suffix
    only after its exact physical/producer/factual proof; unknown/live failure
    retains custody. For a bound canonical backend's failed Initial input or
    authored startup, this reported path may release process-local custody after
    the SAME completed permanent retirement, exact failed End, and original
    producer and callback joins are proven. Authored release is limited to its
    first lifecycle write and its final aggregate after a confirmed lifecycle.
    The release preserves the journal, End and Run observations and permanently
    denies Actor effects. Strict close never initiates release.
    An interrupted release retains its exact request before dispatch; a later
    reported call observes/resumes that request before any close prefix. Original
    diagnostics remain, including the first actual release-invocation failure.
    The actual positive final report is cached until new resource-retirement
    work is accepted. Such work supersedes the cache: the next joined close
    executes the owed cleanup and forwards the prior report's SAME diagnostic
    groups without consuming or reminting them. Earlier reports remain unchanged.
    With no newly accepted work, repeated close does not touch an already disposed
    Actor. Legacy calls still project the cached diagnostic with its original raw
    trace. Creating-domain misuse raises before mutable observation. Actual admitted
    cleanup is cancellation-protected and may await its real owner. Concurrent
    joined callers serialize on the same coordinator. This is external teardown;
    an Owner cleanup callback must not recursively join the same Owner close. *)
val close_and_wait_reported : t -> Disposal_report.t

module For_testing : sig
  (** Observe the already selected exact Cell completion of a live joined-close
      prelude in its creating domain. No election or producer is returned. *)
  val joined_close_cell : t -> Resource_retirement.t option

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
    including a validated Wait/Settlement, never a caller success flag. A fresh
    continuation Turn retains the SAME joined runtime through cancellable
    ancestor admission and exact native acknowledgement. Uncertain writers keep
    the Actor's actual prepared owner; retirement must join its factual cleanup
    before disposing the runtime and reports failure if that cannot be proven. *)
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

module Native_release : sig
  type t

  type status =
    | Held
    | Released
  [@@deriving equal, sexp_of]

  (** Creating-domain check before reading actual mutable loan custody.
      Released is sticky only after SAME Native_lease.close returned positive,
      including actual nonlast adoption release. Missing Owner list membership
      or a different current native identity never supplies this evidence.
      Held does not grant use, repin, authority or whole-Cell cleanup. *)
  val status : t -> (status, Agent_protocol.Error.t) result

  (** Factual SAME pin only: reject an Available/live callback with finite
      Conflict; join Closing, retry Retiring, or observe sticky Released.
      No authorize/current getter/repin/preparation/Owner-wide retry. EXTERNAL
      caller only after actual dependent scope exit: never from this SAME loan's
      finalizer, source/borrow callback or Owner lock (joining Closing there can
      self-join). Errors and raw raised traces remain observable; failed target
      stays in its original Owner custody. *)
  val close : t -> (unit, Agent_protocol.Error.t) result

  (** SAME factual invocation, forwarding its original producer carriers.
      After actual completion it may transfer an already returned, sealed
      provider report out of detached admission metadata. It never retires a
      historical consumer or changes the original report/selected failure. *)
  val close_observed
    :  t
    -> (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t
end

(** Short-lived observation of the actual installed Source in this Owner's
    SAME Cell. Does not select/pin/load resources, execute a policy or create
    permission. No actual Cell type or constructor is exposed. *)
module Native_source : sig
  type t

  val source : t -> Agent_session.Runtime_builder.Source.t

  (** Read the SAME observation's actual Owner Actor outside its mutex.
      May yield. Domain/current Owner guards run before and after the read.
      Never finds a replacement Registry entry, activates or selects resources.
      Caller must compare its original read-set and recheck this SAME Source
      against the returned State; [check] itself remains pure. *)
  val state : t -> (Agent_session.Session_state.t, Agent_protocol.Error.t) result

  (** Pure/non-yielding, creating-domain first. Rejects permanent provider close,
      administration/retirement/construction and lost actual unit membership;
      compares the SAME Cell, native identity and immutable Source with its
      captured Actor/Handle/generation/profile key. It never checks the first
      consumer's lexical guard and is not required for factual release/retry. *)
  val check
    :  t
    -> profile:Agent_session.Permission_policy.t
    -> expected:Agent_session.Session_state.t
    -> (unit, Agent_protocol.Error.t) result
end

(** Capture may acquire the existing Owner mutex/yield. Under it only pure Cell
    operations run, with no Actor/Store/host callbacks. The expected native unit
    is derived from actual Owner binding/restored/Available membership, never
    supplied by caller identity. None means actual Empty with no Owner unit.
    With a retained foreground retirement receipt, absence additionally requires
    its exact successful whole-Owner join and no pending or failed cleanup.
    Missing Cell association, stale/foreign/partial/closed evidence is Error.
    Factory must check the saved observation around actual waits and immediately
    before pure pin/check. Construction and cleanup are never performed here. *)
val observe_native_source
  :  t
  -> handle:Agent_store.Session_store.Handle.t
  -> profile:Agent_session.Permission_policy.t
  -> expected:Agent_session.Session_state.t
  -> (Native_source.t option, Agent_protocol.Error.t) result

module Native_resource_custody : sig
  (** SAME actual consumer loan extent/native unit/physical owner. Does not
     authorize original or later leaf, inspect grants, create Caller or execute.
     Retaining this token does not extend its physical dependent scope or create
     another loan or closer.
     Factual physical ownership survives a failed current-use check; cleanup
     NEVER requires this check to pass or interprets its Error as loan absent. *)
  type t

  (** Creating domain is checked first. Permanent close of the actual provider
      Owner revokes current use even while its loan is still Available; this is
      independent of the original consumer's lexical closure. Then check the
      actual active loan/native identity/current source-key/profile and SAME
     Cell current-use eligibility only, including check_native_borrow rejection
     for Attaching/Failed_attachment/unfinished constructor cleanup. No bypass.
     Deliberately independent of first leaf path/grant/Handle/Stop; the physical
     Cell's actual Handle association remains mandatory. Checks
     around waits; no currentness result is saved for later use. Stored internal
     resource/capability views use this factual guard and every real consumer
     separately validates its own source/path before disclosure/effects. *)
  val check : t -> (unit, Agent_protocol.Error.t) result

  (** Revalidate only the exact selected Source already retained under this
      actual loan. An in-progress same-unit attachment preserves this fact.
      Failed attachment, displacement, failed current-Source checks and provider
      retirement revoke it. No execution, resource projection or new loan. *)
  val check_source
    :  t
    -> source:Agent_session.Runtime_builder.Source.t
    -> (unit, Agent_protocol.Error.t) result
end

module Native_borrow_outcome : sig
  module Preparation_failure : sig
    type stage =
      | Admission
      | Source_callback
      | Source_admission
      | Selection
      | Lease_admission
      | Lease_callback
    [@@deriving equal, sexp_of]

    type t

    val stage : t -> stage

    val observation
      :  t
      -> (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t
  end

  module Pin : sig
    type t =
      | Not_acquired
      | Acquired of
          { release : Native_release.t
          ; cleanup : Scope_outcome.Cleanup.t
          }
  end

  module Cleanup : sig
    (** Owns the actual provider report and original carriers independently of
        admission metadata. Failed physical duties stay retained; eligible SAME
        pin repair may release detached metadata after complete custody proof.
        This ownership grants no consumer retirement or extra retry authority. *)
    type t

    val preparation : t -> Scope_outcome.Cleanup.t
    val pin : t -> Pin.t
    val outer_lease : t -> Scope_outcome.Cleanup.t

    (** EXACT legacy leg order: preparation, returned-pin close, wrapper lease.
        A prerequisite failure before borrow is not a cleanup leg here. *)
    val combined : t -> Scope_outcome.Cleanup.t

    (** Actual preparation cleanup, pin close, then wrapper cleanup only. Callback
        and pre-admission failures are excluded. A preparation finalizer escape
        remains negative even when the legacy optional preparation report is
        absent; absence alone never proves this observation positive. *)
    val observation
      :  t
      -> (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t

    (** Pure factual eligibility: Some SAME witness only if the acquired pin
        failed and original preparation/outer-lease cleanup AND their paired
        observations are positive. A finalizer escape cannot qualify through an
        absent legacy preparation report.
        None for another negative or no failed pin. Constructors are private;
        this is not an authority or fabricated success report. After actual
        dependent scope exit, consumers may call Native_release.close on this
        witness only. Old cleanup reports remain immutable. *)
    val pin_only_failure : t -> Native_release.t option

    (** Read-only exact normally returned wrapper cleanup holder. Check creating
        domain and provider Owner affinity before mutable phase; return only its
        actual Complete latest snapshot with sealed original diagnostics. The
        original preparation must be positive, actual acquired pin Joined and
        original outer cleanup negative. No holder for an escaping outer engine.
        None is ineligibility, never success; running/retrying/unsealed/foreign
        access returns Error. Does not change ordinary snapshot-during-retry
        semantics or authorize reconstruction/consumer retirement. *)
    val outer_recovery
      :  t
      -> owner:Cleanup_recovery.owner
      -> ( (Cleanup_recovery.t * Cleanup_recovery.Snapshot.t) option
           , Agent_protocol.Error.t )
           result
  end

  type 'a t

  (** Actual old admission-time retry_native_releases projection, if reached.
      A failure belongs to pre-existing loans still owned by that provider.
      It creates no pin/witness or new-borrow cleanup; primary retains the exact
      protocol error/raised trace. This operation never retries those loans. *)
  val preadmission : 'a t -> unit Scope_outcome.primary option

  (** SAME selected original negative at an explicitly captured preparing-primary
      boundary. Callback stages identify the actual invocation, not proof that
      no runtime/work occurred. Only a consumer independently proving Not_created
      and joined dependent cleanup may treat it as an already-delivered
      preparation primary. Prior-loan cleanup, source finalizer/entry cleanup,
      post-callback and entered-scope escapes are excluded. None means unknown or
      not classified, never success. Immutable DATA: no effects, mint or grant. *)
  val preparation_failure : 'a t -> Preparation_failure.t option

  (** Actual provider preparation report if entered, before primary projection.
      Its cleanup is immutable here; recovery remains with its original owner. *)
  val preparation : 'a t -> unit Scope_outcome.t option

  val primary : 'a t -> 'a Scope_outcome.primary
  val cleanup : 'a t -> Cleanup.t

  (** Immutable selected result, preserving actual callback/finalizer precedence,
      all actual groups, and the selected original trace. This is observation,
      independently of cleanup custody and the optional legacy report. *)
  val observation
    :  'a t
    -> ('a, Agent_protocol.Error.t) result Agent_session.Close_observation.t

  (** Compatibility projection preserves the existing primary, cleanup leg
      order and Scope_outcome.resume behavior exactly. No retry is performed. *)
  val to_scope_outcome : 'a t -> 'a Scope_outcome.t

  val resume : 'a t -> ('a, Agent_protocol.Error.t) result
end

(** Same original guard/pin/finalizer engine as with_native_resources_reported.
    capture_release is nonyielding, runs after the real pin/Native_loan is
    published in its provider Owner AND recorded in the local owned ref, outside
    the Owner mutex but within the existing protected returned-pin custody,
    before f/check/cancellation resumes.
    The exact SAME witness appears in Pin.Acquired. A throwing capture takes
    the actual pin's ordinary finalizer; no returned pin is abandoned. *)
val with_native_resources_outcome
  :  ?consumer:Native_consumer.t
  -> t
  -> source_owner:Agent_session.Runtime_source_owner.t
  -> reference:Agent_store.Delegation_store.Reference.t
  -> authorize:
       (reference:Agent_store.Delegation_store.Reference.t
        -> target:Agent_session.Session_state.t
        -> (unit, Agent_protocol.Error.t) result)
  -> capture_release:(Native_release.t -> unit)
  -> f:
       (check:(unit -> (unit, Agent_protocol.Error.t) result)
        -> Agent_session.Runtime_builder.resources
        -> ('a, Agent_protocol.Error.t) result)
  -> 'a Native_borrow_outcome.t

(** Same bracket, also disclosing its SAME retained selected Source. *)
val with_native_source_resources_outcome
  :  ?consumer:Native_consumer.t
  -> t
  -> source_owner:Agent_session.Runtime_source_owner.t
  -> reference:Agent_store.Delegation_store.Reference.t
  -> authorize:
       (reference:Agent_store.Delegation_store.Reference.t
        -> target:Agent_session.Session_state.t
        -> (unit, Agent_protocol.Error.t) result)
  -> capture_release:(Native_release.t -> unit)
  -> f:
       (check:(unit -> (unit, Agent_protocol.Error.t) result)
        -> source:Agent_session.Runtime_builder.Source.t
        -> Agent_session.Runtime_builder.resources
        -> ('a, Agent_protocol.Error.t) result)
  -> 'a Native_borrow_outcome.t

(** SAME shared native bracket with a paired callback. All guards, prepare,
    pin, checks and finalizers execute once in their original order. Newly
    executed validation failures own their actual control boundary; known
    callback/preparation observations are forwarded without re-catching them.
    Creating-domain misuse retains the raw port's typed admission refusal. *)
val with_native_resources_observed
  :  ?consumer:Native_consumer.t
  -> t
  -> source_owner:Agent_session.Runtime_source_owner.t
  -> reference:Agent_store.Delegation_store.Reference.t
  -> authorize:
       (reference:Agent_store.Delegation_store.Reference.t
        -> target:Agent_session.Session_state.t
        -> (unit, Agent_protocol.Error.t) result)
  -> capture_release:(Native_release.t -> unit)
  -> f:
       (check:(unit -> (unit, Agent_protocol.Error.t) result)
        -> Agent_session.Runtime_builder.resources
        -> ('a, Agent_protocol.Error.t) result Agent_session.Close_observation.t)
  -> 'a Native_borrow_outcome.t

(** Same bracket, also disclosing its SAME retained selected Source. *)
val with_native_source_resources_observed
  :  ?consumer:Native_consumer.t
  -> t
  -> source_owner:Agent_session.Runtime_source_owner.t
  -> reference:Agent_store.Delegation_store.Reference.t
  -> authorize:
       (reference:Agent_store.Delegation_store.Reference.t
        -> target:Agent_session.Session_state.t
        -> (unit, Agent_protocol.Error.t) result)
  -> capture_release:(Native_release.t -> unit)
  -> f:
       (check:(unit -> (unit, Agent_protocol.Error.t) result)
        -> source:Agent_session.Runtime_builder.Source.t
        -> Agent_session.Runtime_builder.resources
        -> ('a, Agent_protocol.Error.t) result Agent_session.Close_observation.t)
  -> 'a Native_borrow_outcome.t

(** SAME projected bracket, retaining the immutable Direct/full-prefix origin
    only for lexical admission and exposing separate actual-loan custody. The
    physical dependent [consumer] remains independent of the original ingress
    leaf. Existing Direct wrappers preserve their old callbacks and never invoke
    the new custody check. This does not grant execution to a later caller. *)
val with_native_source_resources_for_origin_observed
  :  ?consumer:Native_consumer.t
  -> t
  -> source_owner:Agent_session.Runtime_source_owner.t
  -> origin:Native_resource_origin.t
  -> capture_release:(Native_release.t -> unit)
  -> f:
       (check:(unit -> (unit, Agent_protocol.Error.t) result)
        -> custody:Native_resource_custody.t
        -> source:Agent_session.Runtime_builder.Source.t
        -> Agent_session.Runtime_builder.resources
        -> ('a, Agent_protocol.Error.t) result Agent_session.Close_observation.t)
  -> 'a Native_borrow_outcome.t

(** Canonical operation. SAME per-consumer admission/real native loan retained for
    all of f; callback resources may not escape. Constructor invokes supplied
    check after each actual wait before more effects/disclosure, not an automatic
    interceptor of arbitrary Eio operations. Fixed Factory ports independently
    resolve current private authority. A selected failure never chooses cold
    fallback. Lawful Independent resource grants may survive parent Stop;
    Owned/Invocation_owned retain their original stop_epoch, desired Running and
    lifetime requirements. Every fixed port/per-consumer guard enforces its actual
    source family. Source/grant/admin/permanent close revokes and joins through
    existing Owner lifetime.

    Primary and actual preparation/source-stage/native-loan finalizers remain
    separate via EXISTING Scope_outcome, including cancellation/raw backtrace.
    Joined means this scope/loan joined, not shared physical close or NoEffects.
    Factory callers record Parent_cleanup BEFORE Scope_outcome.resume. *)
val with_native_resources_reported
  :  t
  -> source_owner:Agent_session.Runtime_source_owner.t
  -> reference:Agent_store.Delegation_store.Reference.t
  -> authorize:
       (reference:Agent_store.Delegation_store.Reference.t
        -> target:Agent_session.Session_state.t
        -> (unit, Agent_protocol.Error.t) result)
  -> f:
       (check:(unit -> (unit, Agent_protocol.Error.t) result)
        -> Agent_session.Runtime_builder.resources
        -> ('a, Agent_protocol.Error.t) result)
  -> 'a Scope_outcome.t

(** SAME admission, native loan, checks and actual cleanup report as
    [with_native_resources_reported], also disclosing the SAME unit's retained
    selected Source. Missing selection rejects before f; it never means
    No_moderator. Source/resources and their dependent live bindings remain
    scoped to f under its existing explicit check-after-wait law. No preparation,
    new borrow, effect permission or cleanup owner is introduced. *)
val with_native_source_resources_reported
  :  t
  -> source_owner:Agent_session.Runtime_source_owner.t
  -> reference:Agent_store.Delegation_store.Reference.t
  -> authorize:
       (reference:Agent_store.Delegation_store.Reference.t
        -> target:Agent_session.Session_state.t
        -> (unit, Agent_protocol.Error.t) result)
  -> f:
       (check:(unit -> (unit, Agent_protocol.Error.t) result)
        -> source:Agent_session.Runtime_builder.Source.t
        -> Agent_session.Runtime_builder.resources
        -> ('a, Agent_protocol.Error.t) result)
  -> 'a Scope_outcome.t

(** Optional compatibility projection only: reported |> Scope_outcome.resume.
    Source-completion callers must use and inspect the reported operation. *)
val with_native_resources
  :  t
  -> source_owner:Agent_session.Runtime_source_owner.t
  -> reference:Agent_store.Delegation_store.Reference.t
  -> authorize:
       (reference:Agent_store.Delegation_store.Reference.t
        -> target:Agent_session.Session_state.t
        -> (unit, Agent_protocol.Error.t) result)
  -> f:
       (check:(unit -> (unit, Agent_protocol.Error.t) result)
        -> Agent_session.Runtime_builder.resources
        -> ('a, Agent_protocol.Error.t) result)
  -> ('a, Agent_protocol.Error.t) result

(** Factual retry of this Owner's retained native-loan release targets only.
    Creating domain is checked before reading mutable custody. Retries the SAME
    Retiring/Closing loans captured by prior actual release attempts; does not
    close Available baseline/consumer loans, acquire resources, authorize use,
    unload a shared parent or re-enter ancestor source scopes. Returned errors,
    exceptions/cancellation and original backtraces remain observable; failed
    targets remain owned here for another explicit retry.

    The actual Factory native composite calls this after local dependent drain
    and the once-only ancestor CPS exit when a parent Scope_outcome.Cleanup failed.
    That report is evidence; the exact loan retained in this Owner is custody. *)
val retry_native_resource_cleanup : t -> (unit, Agent_protocol.Error.t) result
