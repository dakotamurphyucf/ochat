open! Core
module P = Agent_protocol
module Store = Agent_store.Developer_run_store

(** Trusted daemon host composition for command custody over the ONE
    Developer_run_store.
    This module neither writes a second file nor owns a second scheduler or
    result ledger. Store.Command remains the sole durable phase writer. *)
type t

module Disclosure_policy : sig
  type t

  (** Trusted nonyielding host decision for an authenticated current actor
      and the exact persisted historical row. IDs/rows are comparison data,
      never bearer grants. Rechecked after every Store wait. *)
  val create : authorize:(Operator_authorization.t -> Store.Command.Row.t -> bool) -> t
end

module Control_policy : sig
  type t

  (** Separate trusted nonyielding host decision for explicit cancellation of
      this exact active physical command. Receipt-read permission does NOT
      grant control. The original/current actor, run owner, key and callback
      generation are checked before cancellation and after yielding join/Store
      work. A historical or foreign row cannot select a callback. *)
  val create : authorize:(Operator_authorization.t -> Store.Command.Row.t -> bool) -> t
end

module Recovery_adapter : sig
  type t

  (** Trusted host composition over the exact physical startup claim/window.
      Settle authentic abandoned job and inference inventory through the
      existing Store Recovery APIs. No new Root/effect may enter; the adapter
      must not finish the window or release the claim. This lifecycle owns
      those operations and retains their custody on uncertain failure. The
      adapter is daemon-owned so a passive historical await uses the same
      recovery path without borrowing a transport request. *)
  val create
    :  recover:
         (claim:Store.Run_claim.t
          -> handle:Store.handle
          -> window:Store.Recovery.t
          -> (unit, P.Error.t) result)
    -> t
end

module Slot : sig
  type t
end

module View : sig
  type t

  val row : t -> Store.Command.Row.t
  val receipt : t -> Store.Command.Receipt.t option
end

module Active : sig
  type t

  module Started : sig
    (** One callback-local proof that the exact fresh Start was durably
        installed and its process-local Execution_start consumed. It is bound
        to this Active's exact row, owner, capture and callback generation,
        expires with Active, and is one-use for the interpreter. A historical
        read or reconciliation cannot mint it. The lower Store ack alone is
        not Root authority. *)
    type t

    val issued_by_root : t -> Developer_root_owner.run -> bool
    val consume_for : t -> root:Developer_root_owner.run -> (unit, P.Error.t) result
  end

  module Preparing : sig
    (** Exact fresh, durable Execution_start consumed once for this physical
        Active/row/callback generation BEFORE native runtime construction.
        It is not Root or execution authority. It expires when the callback
        exits; an uncertain Start never issues this value and permits no
        runtime construction. *)
    type t

    (** Bind at most once to the subsequently admitted physical Root. The
        Root must carry the SAME original actor token, Store owner and capture
        as the prepared Active, and be current after its yielding check.
        Failure consumes this binding attempt; another Root cannot replace it.
        This neither writes nor consumes another durable Start. The returned
        Started remains one-use for the interpreter. *)
    val bind_root : t -> root:Developer_root_owner.run -> (Started.t, P.Error.t) result

    (** Comparison only: [started] must be the unique result of binding this
        Preparing token's same Active/callback generation, and both must
        remain live. This does not consume a second Start or grant execution. *)
    val issued_started : t -> Started.t -> bool
  end

  val view : t -> View.t

  (** Nonyielding lexical Started check for this exact current Active and
      physical Root. It accepts the Started token after its one interpreter
      consumption, while this same callback is still live; it never revives
      an expired callback or creates another Start. Caller also rechecks the
      Root/Store/source after yielding approval work. *)
  val check_started_now
    :  t
    -> started:Started.t
    -> root:Developer_root_owner.run
    -> actor:Operator_authorization.t
    -> (unit, P.Error.t) result

  (** Commit and consume the unique fresh durable Start for this Active
      before any native/shell/MCP initialization or language effect. [actor]
      is the original authenticated token from the captured request; current
      principal/owner and the trusted nonyielding [admit] are rechecked at the
      exact Store boundary. No Root exists yet, so this returns only opaque
      preparation custody. A failed/unknown Start yields no token and no
      native runtime may start. A later callback failure settles a truthful
      started partial-effect receipt; same-key retry never replays it. *)
  val start_preparation
    :  t
    -> actor:Operator_authorization.t
    -> admit:(unit -> bool)
    -> (Preparing.t, P.Error.t) result

  (** Live only, inside this exact daemon-owned callback. Commit the first
      uninterrupted Start under physical Root/current-actor authorization and
      consume its Execution_start once before returning to the language
      interpreter. Installed/reconciled/old Start returns a typed no-replay
      failure. A copied row or second call cannot enter an effect. Existing
      callers use this checked adapter over the same Start transaction. *)
  val start_live
    :  t
    -> root:Developer_root_owner.run
    -> admit:(unit -> bool)
    -> (Started.t, P.Error.t) result

  (** Transfer the exact finished physical startup claim and window to a
      Registry Admitting slot from this same live callback. [accept] is a
      trusted, NONYIELDING callback: it validates the same Store, physical
      handle/claim/window and installs both tokens in Admitting before
      returning Ok. Only then does Lifecycle mark its custody transferred,
      with no yield or exception-producing work between the two steps. A
      rejection leaves Lifecycle owning the claim. No historical row, owner
      coordinate or repeated callback can transfer it. *)
  val transfer_finished_claim
    :  t
    -> handle:Store.handle
    -> accept:
         (claim:Store.Run_claim.t -> window:Store.Recovery.t -> (unit, P.Error.t) result)
    -> (unit, P.Error.t) result
end

(** [sw] is the daemon owner switch, never a transport request switch. The
    active bound is validated and counts callbacks plus startup/reservation
    slots; a retained unknown Store Pending or quarantined Root retains its
    occupancy. One stable run has Cold/Recovering/Ready/Quarantined state.
    Ready is process-local lifecycle evidence, not a durable execution grant.
    Construction preadmits this lifecycle's own conservative Check, Fixture
    and Live terminal receipts against the actual Store terminal, aggregate
    and whole-document byte caps; a too-small configuration is rejected before
    any callback can start.
    The number of retained run entries is bounded by Store.Limits.max_runs;
    a clean Ready entry may be evicted only after proving it has no active
    callback, claim, Pending or Root custody. Eviction cannot trigger startup
    Recovery while another same-run command is still live. Live and
    Quarantined entries are never silently evicted. *)
val create
  :  sw:Eio.Switch.t
  -> store:Store.t
  -> max_active_commands:int
  -> disclosure:Disclosure_policy.t
  -> control:Control_policy.t
  -> recovery:Recovery_adapter.t
  -> (t, P.Error.t) result

(** Reserve this run's host startup/admission slot without yielding, before
    any historical read. A second same-run startup or epoch replacement waits
    outside the slot or returns typed Busy; it cannot race Recovery.finish
    against a newly Reserved row. Other run IDs are independent. All exits
    release clean slots, including cancellation/exception. A fresh Reserved
    row abandoned before launch is finished as Interrupted through the same
    Store writer; uncertain cleanup retains its exact selection and occupancy
    for later reconciliation. This is not a Store Claim. *)
val with_slot
  :  t
  -> run_id:P.Id.Developer_run.t
  -> f:(Slot.t -> ('a, P.Error.t) result)
  -> ('a, P.Error.t) result

(** Authenticate and query the exact physical Result_root before source
    capture or epoch admission. A key miss with unresolved Store Pending is
    an error, not absence. Existing rows recheck current actor/disclosure
    after I/O. [kind] and [request_fingerprint] come from the host's already
    validated canonical original request and are compared to the durable row
    before allowing an Existing return. This layer cannot construct or change
    that original request. A same-process retained Pending is
    reconciled by its exact owner before retrying this read, never inferred
    absent from a cached key miss. If this exact callback has already joined
    but its selected terminal save failed, a same-key authorized retry resumes
    only that immutable terminal bookkeeping under protected custody; it
    never invokes the callback again or mints a Start acknowledgement. *)
val lookup
  :  t
  -> Slot.t
  -> actor:Operator_authorization.t
  -> key:P.Idempotency_key.t
  -> kind:Store.Command.Receipt.Kind.t
  -> request_fingerprint:Chatml_source.Digest.t
  -> (View.t option, P.Error.t) result

(** Run exactly once for an abandoned/reopened physical run while its Slot is
    held, before ANY new command reservation. For a Cold persisted run, use
    Result_root.observe_latest, then admit_epoch with the persisted latest
    checkpoint owner/capture and previous=Some exact observation (never None),
    acquire the physical Run_claim, open Recovery and finish it. The Store
    excludes an in-process owner/Pending, checks the same owner/capture after
    its own waits and never grants execution from that historical handle.
    Missing run needs no recovery. Ready runs skip this operation, so later
    pure Check/Fixture submissions cannot interrupt an active command.
    Quarantined runs retain their claim and deny new work; exact reconciliation
    may resume. A clean finished claim and exact handle are retained for
    same-epoch reuse, bounded by Store max_runs, without consuming an active
    callback slot. A changed epoch joins old callbacks and releases this claim
    before Store.admit_epoch. A Root epoch replacement first joins its old
    callbacks/worker/owner before recovery. *)
val ensure_startup_recovered : t -> Slot.t -> (unit, P.Error.t) result

(** Observe the exact retained finished startup handle under this run Slot.
    The Host may reuse it only after its fresh candidate has the same
    persisted principal, owner and capture. Store.admit_epoch must NOT be
    called for that reuse: even an identical call mints a different physical
    handle token. No claim or new epoch is created by this getter. *)
val ready_epoch : t -> Slot.t -> (Store.handle option, P.Error.t) result

(** Conclusively release an untransferred finished claim before the Host
    selects a different epoch. Requires no active callback, unlaunched
    reservation, Pending or unfinished recovery under this same Slot.
    Failure retains exact custody for retry. Transferred claims are owned by
    Registry and cannot be released here. *)
val release_finished_epoch : t -> Slot.t -> (unit, P.Error.t) result

(** After a genuinely new Store.admit_epoch, establish its physical claim,
    run normal terminal-only Recovery and finish that window BEFORE first
    Command.reserve. An identical already-finished handle is reused. A new
    window requires no same-run Active callback, Pending or unlaunched
    reservation; the Slot alone does not join callback bodies. An uncertain
    window stays quarantined and cannot be replaced. A transferred claim
    permits only its exact same handle. *)
val ensure_admitted_claim : t -> Slot.t -> handle:Store.handle -> (unit, P.Error.t) result

(** After joined Root/Registry retirement has conclusively released its
    transferred exact claim, clear Lifecycle's comparison-only marker. The
    marker cannot release Registry custody; a still-live claim or any
    same-run callback denies this operation. Host uses it before a changed
    epoch admission. *)
val confirm_transferred_retired
  :  t
  -> Slot.t
  -> handle:Store.handle
  -> (unit, P.Error.t) result

(** Reserve only after [ensure_startup_recovered] and fresh source/capture
    admission. Current original actor, selected source/Root policy and latest
    Active owner/capture are checked by host and Store at prepare and before
    rename. An already installed key is comparison-only and never emits Start.
    On uncertain write, retain exact Store.Command.Pending and block another
    mutation; reprepare only the same row after proven Absent. *)
val reserve
  :  t
  -> Slot.t
  -> handle:Store.handle
  -> key:P.Idempotency_key.t
  -> kind:Store.Command.Receipt.Kind.t
  -> request_fingerprint:Chatml_source.Digest.t
  -> admit:(unit -> bool)
  -> (View.t, P.Error.t) result

(** Register the exact Reserved command as Active while its [Slot] is still
    held. Only a fresh reservation returned to that exact Slot carries a
    one-use launch proof: an Unchanged/historical View or a View retained after
    its Slot expires cannot launch, even if its row remains Reserved. Transfer
    occupancy without a gap before yielding, then fork its
    callback under this lifecycle's daemon-owned switch, never a transport
    request switch. A separate per-Active child switch is passed to [f] and
    joined before terminal settlement; cancellation targets that callback and
    its children, leaving other daemon commands live. This same path owns
    Check, Fixture and Live; the callback receives an expiring physical
    [Active.t]. Live MUST call
    [Active.start_live] before any language/native/job/provider effect. Pure
    kinds cannot call it. The callback chooses its truthful bounded receipt;
    this module joins it and persists Finish through Store.Command. Typed
    callback Error and unexpected exception/cancellation are caught inside
    this owned fiber and use kind-specific conservative terminal mapping;
    original identity/backtrace remain in its owned diagnostic outcome for
    joined host inspection. Raw exception text, provider credentials and
    source bytes never appear in [View] or [await_terminal]. An unhandled
    child exception cannot cancel the daemon switch. A retained unknown
    terminal save keeps the same selected receipt and per-run occupancy. The
    caller receives the initial persisted
    View after registration; disconnect only drops that caller's waiter.
    Failure to register/fork selects a bounded Interrupted terminal and
    performs exact cleanup before propagating the original exception; an
    uncertain terminal save retains the selected receipt and occupancy for
    same-key retry. *)
val launch
  :  t
  -> Slot.t
  -> handle:Store.handle
  -> reserved:View.t
  -> f:(Active.t -> sw:Eio.Switch.t -> (Store.Command.Receipt.t, P.Error.t) result)
  -> (View.t, P.Error.t) result

(** Comparison-only preflight of the exact live Slot, self-join and existing
    old-row Control_policy. It performs no cancellation or Store mutation and
    grants no future authority: [retire_run] repeats the same predicate before
    its first cancellation. A host can call this immediately before retaining
    its own Retiring custody so a denied request cannot strand that state. *)
val check_retirement
  :  t
  -> Slot.t
  -> actor:Operator_authorization.t
  -> (unit, P.Error.t) result

(** Nonyielding comparison under the exact current same-run Slot. This lets
    the host refuse a second same-capture Live key while a provisional first
    callback is still preparing, without implicitly cancelling that callback.
    It is scheduling information, not permission or cleanup authority. *)
val has_active : t -> Slot.t -> (bool, P.Error.t) result

(** Host-only same-run epoch retirement under the exact still-live admission
    Slot. A replacement source/actor is authenticated against the persisted
    principal and current method/source policy BEFORE this call. [actor] must
    be current and pass this lifecycle's existing distinct Control_policy,
    same-principal check and old-kind method scope for EVERY exact active old
    Row; any rejection happens before the first cancellation. A retained Root
    whose originating command has finished also needs the host's Live/control
    check on that authentic origin Row before Root retirement. No external
    command key or receipt-read permission grants retirement. Reject a call
    from any current callback of this run before cancelling anything. Cancel
    and join all its callback children, reconcile their exact selected terminal
    receipts and settle an unlaunched reservation through Store.Command's one
    writer. The Slot stays owned by the caller, excluding another admission or
    launch until the host retires its Root/scheduler attachment and admits the
    next epoch. Success proves no active callback, command Pending, unlaunched
    row, startup Recovery or claim-only custody remains. Unknown writes retain
    their exact Pending/occupancy for a later retry; cancellation during join
    preserves original identity after protected cleanup. This neither closes
    the Lifecycle nor cancels other runs. *)
val retire_run : t -> Slot.t -> actor:Operator_authorization.t -> (unit, P.Error.t) result

(** A passive read authenticates the exact persisted row and disclosure BEFORE
    entering Cold startup Recovery. An authorized Cold restarted nonterminal
    row then completes joined Recovery under the same-run slot and rereads its
    terminal interruption; it never awaits a vanished callback. Already
    terminal historical rows need no startup Recovery. Await snapshots wait
    for this Lifecycle's same-run terminal writer, so a sibling command's
    transient Pending does not make a settled callback look failed. Pending
    from another Store writer or uncertain custody still returns typed
    Conflict. Otherwise passive authenticated wait only detaches this waiter
    on cancellation; the
    callback belongs to [sw]. Unexpected callback failure appears only as
    bounded, kind-correct persisted status/diagnostic or a typed retained-
    custody error, never raw exception/secret-bearing text. Explicit cancel
    requires [Control_policy], joins that exact physical callback and children,
    then settles truthful terminal/partial-effects bookkeeping. Missing,
    foreign and historical keys cannot cancel a new run. *)
val await_terminal
  :  t
  -> actor:Operator_authorization.t
  -> run_id:P.Id.Developer_run.t
  -> key:P.Idempotency_key.t
  -> (View.t, P.Error.t) result

val cancel_and_wait
  :  t
  -> actor:Operator_authorization.t
  -> run_id:P.Id.Developer_run.t
  -> key:P.Idempotency_key.t
  -> (View.t, P.Error.t) result

(** Daemon-owned first shutdown phase. Reject an in-flight borrowed Slot or
    self-join before changing state. Stop admission, cancel/join every command
    callback and settle its exact pending/unlaunched/terminal bookkeeping.
    Retain finished and transferred Run_claim custody while the same daemon
    detaches its scheduler and closes physical Roots. Repeated calls retry
    unsettled bookkeeping without launching or replaying effects. *)
val stop_and_join : t -> (unit, P.Error.t) result

(** After [stop_and_join] and physical Root/Registry cleanup, release the
    remaining untransferred finished claims and verify each transferred claim
    was released by its actual Registry owner. Uncertain custody remains
    retained for a retry; the Store closes only after this succeeds. *)
val close_and_wait : t -> (unit, P.Error.t) result
