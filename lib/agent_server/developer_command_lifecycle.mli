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

  val view : t -> View.t

  (** Live only, inside this exact daemon-owned callback. Commit the first
      uninterrupted Start under physical Root/current-actor authorization and
      consume its Execution_start once before returning to the language
      interpreter. Installed/reconciled/old Start returns a typed no-replay
      failure. A copied row or second call cannot enter an effect. *)
  val start_live
    :  t
    -> root:Developer_root_owner.run
    -> admit:(unit -> bool)
    -> (Started.t, P.Error.t) result
end

(** [sw] is the daemon owner switch, never a transport request switch. The
    active bound is validated and counts callbacks plus startup/reservation
    slots; a retained unknown Store Pending or quarantined Root retains its
    occupancy. One stable run has Cold/Recovering/Ready/Quarantined state.
    Ready is process-local lifecycle evidence, not a durable execution grant.
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
    release clean slots, including cancellation/exception; unknown custody
    moves the run to Quarantined instead. This is not a Store Claim. *)
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
    may resume. A clean finished temporary claim is released. A Root epoch
    replacement first joins its old callbacks/worker/owner before recovery. *)
val ensure_startup_recovered : t -> Slot.t -> (unit, P.Error.t) result

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
    held, transferring occupancy without a gap before yielding, then fork its
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
    Failure to register/fork performs exact cleanup before returning. *)
val launch
  :  t
  -> Slot.t
  -> handle:Store.handle
  -> reserved:View.t
  -> f:(Active.t -> sw:Eio.Switch.t -> (Store.Command.Receipt.t, P.Error.t) result)
  -> (View.t, P.Error.t) result

(** A Cold restarted nonterminal row first completes joined startup Recovery
    under the same-run slot and rereads its terminal interruption; it never
    awaits a vanished callback. Otherwise passive authenticated wait only
    detaches this waiter on cancellation; the
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

(** Stop admissions, cancel/join callbacks and retain unresolved physical
    claims/Pending for safe retry. The registry and Store remain daemon-owned
    and are closed afterward, in that order. *)
val close_and_wait : t -> (unit, P.Error.t) result
