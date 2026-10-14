(** Existing-capacity adapter for one developer run's physical workers. This
    module does not poll queues, dispatch native tools or disclose results. *)
open! Core

type t

type close_policy =
  | Retire_unclaimed
  | Retain_unclaimed

(** Bind the actual daemon-wide capacity ledger and store to one stable
    host/run/principal. Trusted host wiring selects [project_source] and the
    workspace conflict domain from its admitted capture/workspace; neither
    coordinate is accepted from a scheduler request. Each job's capacity key
    is derived internally from those bound values and the exact observed
    owner/kind/depth. [now] must be a non-yielding trusted clock sample.
    [max_quarantined] is at least positive [max_active], so every admitted
    worker has a cleanup slot. [Retire_unclaimed] is the standalone default.
    A higher root owner with an unresolved selected Store handoff uses
    [Retain_unclaimed]: the worker still cancels, joins and reconciles physical
    claims, while that owner alone retires unclaimed capacity after exact
    selected settlement. *)
val create
  :  ?close_policy:close_policy
  -> sw:Eio.Switch.t
  -> store:Agent_store.Developer_run_store.t
  -> initial:Agent_store.Developer_run_store.handle
  -> capacity:Job_capacity.t
  -> project_source:Chatml_source.Id.t
  -> workspace_conflict_domain:string
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> max_active:int
  -> max_quarantined:int
  -> unit
  -> (t, Agent_protocol.Error.t) result

(** The physical issuer whose active child-proof is accepted by native
    admission. Current principal/capture/selection and effect policy are
    separately checked by the host at each effect boundary. *)
val issuer : t -> Agent_session.Developer_job_scope.t

(** A terminal result selected by trusted host code after the worker callback
    and its children have joined. This is a retention token, not a Store writer
    or result-read grant. Its [ensure_attempted] closure owns one already frozen
    result preparation; retrying it must use the same outcome, reference, intent
    and bytes, and must never invoke the job handler or select another result.
    It must durably establish the intent and matching staged data, then move the
    publisher token to non-discardable attempted custody before returning. Thus
    a following Store CAS can never precede publisher attempt protection.
    Construction is pure. [after] must preserve [proposed]'s terminal status,
    attempt, timestamps and retry fields; it may only replace a validated
    inline result with an artifact descriptor of the same outcome. The worker
    checks this token's physical Claim against its own before retaining it,
    then owns its invocation and lifetime. Byte/digest matching remains the
    publisher's responsibility. [acknowledge_installed] receives only an actual
    Store-issued Installed receipt whose job/attempt/result the worker matched
    to [after]. It can retire the exact intent best-effort: a typed cleanup
    error leaves the marker for retention, while an unexpected exception
    propagates with its original backtrace after the installed claim and
    capacity are settled. Neither can turn a durable Store installation into
    a failed job or release an unproved claim. *)
module Selected : sig
  type t

  val create
    :  claim:Agent_store.Developer_run_store.Claim.t
    -> proposed:Agent_protocol.Developer_job.State.t
    -> after:Agent_protocol.Developer_job.State.t
    -> ensure_attempted:(unit -> (unit, Agent_protocol.Error.t) result)
    -> acknowledge_installed:
         (Agent_store.Developer_run_store.Installed.t
          -> (unit, Agent_protocol.Error.t) result)
    -> (t, Agent_protocol.Error.t) result

  val after : t -> Agent_protocol.Developer_job.State.t
end

(** Project exact [lookup] through Host_job.of_developer, then acquire from
    Job_capacity.try_acquire_host_job in the SAME shared capacity ledger as
    conversation jobs. Recheck the exact job/attempt after lease acquisition;
    durably change Queued to Running before entering [f]. Saturated/not-due
    returns [Ok None] with no callback. Unknown claim persistence never enters
    [f]; retain/quarantine the lease pending bounded exact reconciliation.
    Claim entry is one-shot even if commit/reconcile returns the same token.

    [f] runs inside a registered Eio cancellation scope with its own child
    Switch. It returns the proposed end-of-worker state: terminal,
    Waiting_completion or Queued retry. Waiting_permission is an interim state
    in the same live attempt and cannot be returned as an ended worker; its
    resumption does not increment attempt or acquire another lease. The switch
    joins before this module persists the exact state from a fresh Observation
    and releases capacity. A later due Queued retry alone increments attempt.
    An older epoch can record only terminal bookkeeping. Error becomes Failed;
    unexpected exception and actual Eio cancellation attempt protected
    Failed/Cancelled bookkeeping, then preserve original identity.

    On a fresh CAS conflict, the owner rereads and retries a finite number of
    times only while the same physical claim/attempt/immutable launch remains.
    It never reruns [f]. Uncertain persistence uses exact Prepared
    reconciliation, not a blind retry; unresolved state quarantines the lease.
    A returned artifact descriptor must already have separate result-custody
    proof at the higher actor boundary. [select], when supplied, runs exactly
    once after successful callback/child join and valid normalization, before result
    publication I/O. It can only freeze a Selected token or decline selection;
    it must not start an effect, write an intent, stage bytes, or call Store.
    Callback/child failures never select a successful result. A selection error
    follows the ordinary Failed path before anything is selected.

    Once Selected exists, the worker records it with the real Claim and capacity
    before calling [ensure_attempted]. Any error, cancellation, or uncertain write
    retains that exact token in the existing quarantine; it cannot turn into a
    new Failed/Interrupted result. If no Store Prepared exists, retry exact
    preparation before the existing [settle_transition]. If a Store Prepared
    was already attempted, reconcile it first: Installed acknowledges the
    selection; proven Absent may re-ensure the same preparation before retry.
    This ordering also applies during bounded reconciliation/close.
    Existing terminal-state epoch behavior preserves the chosen terminal while
    Store validates the original Claim and permits only old-epoch terminal
    bookkeeping. Only an exact Installed receipt can trigger best-effort intent
    retirement. An unresolved selection retains capacity and blocks fresh
    admission. The selector cannot invoke the terminal continuation twice. *)
val with_claimed_attempt
  :  t
  -> expected:Agent_store.Developer_run_store.Observation.t
  -> lookup:Agent_store.Developer_run_store.Job_lookup.t
  -> ?select:
       (claim:Agent_store.Developer_run_store.Claim.t
        -> proposed:Agent_protocol.Developer_job.State.t
        -> (Selected.t option, Agent_protocol.Error.t) result)
  -> (active:Agent_session.Developer_job_scope.active
      -> sw:Eio.Switch.t
      -> (Agent_protocol.Developer_job.State.t, Agent_protocol.Error.t) result)
  -> (Agent_protocol.Developer_job.t option, Agent_protocol.Error.t) result

module Cancellation : sig
  type t =
    | No_live_claim
    | Requested_self
    | Joined
end

(** Target only the actually registered callback whose entered physical Claim
    was issued by this Worker and matches [job]'s immutable identity and
    attempt. A copied
    Running row cannot mint a Claim or cause an unrelated callback to be
    cancelled. [No_live_claim] is not proof that the durable job is inactive:
    admission, terminal persistence or restart recovery may be in flight.
    [Requested_self] signals this callback's cancellation without self-join;
    the owning Eio scope delivers it at its normal cancellation boundary.
    Otherwise cancel the exact Eio context and join its callback/children
    before [Joined]. The callback's
    existing terminal persistence and quarantine own capacity throughout;
    this operation never writes a job state or releases a lease. *)
val cancel_claimed
  :  t
  -> job:Agent_protocol.Developer_job.t
  -> (Cancellation.t, Agent_protocol.Error.t) result

(** Before cancelling a durable Waiting_completion row or queued retry, join
    any registered worker callback for this exact immutable job/attempt and
    reject an unresolved claim or terminal-write quarantine. This covers the
    handoff after the worker saved an ended wait/retry but before unregistering.
    Calling from that same callback rejects rather than self-joining. An
    absent registered call is not itself proof of the status: the root must
    reread a fresh Store Observation after this call and use exact CAS. *)
val join_ended_attempt
  :  t
  -> job:Agent_protocol.Developer_job.t
  -> (unit, Agent_protocol.Error.t) result

(** Actual next Store handle for this stable run identity. Revoke old active
    effects and retire only unclaimed old-epoch reservations; old claimed
    leases stay charged until physical cleanup. Foreign/future/equal epochs
    reject without changing registry or capacity. *)
val advance_epoch
  :  t
  -> new_current:Agent_store.Developer_run_store.handle
  -> (unit, Agent_protocol.Error.t) result

(** Finite active/quarantine registries reject saturation before lease/entry;
    an existing worker's cleanup is never dropped. One bounded reconciliation
    pass cannot replay a callback. Unknown I/O remains quarantined and blocks
    admission for this run until trusted recovery or daemon shutdown. *)
val reconcile_quarantined : t -> (unit, Agent_protocol.Error.t) result

(** Exclude entries, cancel/join actual callbacks and children, attempt bounded
    reconciliation, then permit Store.close_and_wait. Self-close rejects before
    revocation. If an unknown quarantine remains, keep the owner closed and
    lease retained; caller owns retry/shutdown ordering. Never clear unrelated
    conversation/developer capacity from the shared ledger. *)
val close_and_wait : t -> (unit, Agent_protocol.Error.t) result
