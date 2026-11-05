open! Core

(** Actor-confined process-local phase and preparation custody. No scheduler,
    persisted state, target permit or surviving monotonic origin. *)
type t

type preparation
type prepared
type validation

module Completion : sig
  (** Adjacent actual wall/absolute-monotonic service samples. No clock is read,
      serialized or authenticated by this data constructor. The producer must
      pass this exact wall into its actual Run terminal transition. *)
  type sample

  type plan

  (** Process-local association with exact immutable terminal facts. Presence
      includes invalid live timing: it must never be treated as recovery absence.
      This value grants no target observation or rearm authority. *)
  type t

  val sample : wall:Agent_protocol.Timestamp.t -> monotonic:Mtime.t -> sample

  (** The exact wall paired with this sample; reading it obtains no new clock or
      completion authority. *)
  val wall : sample -> Agent_protocol.Timestamp.t

  val timing_error : t -> Agent_protocol.Error.t option
end

module Completion_phase : sig
  (** Same-runtime, exact future-state bookkeeping only. No clock, permission or
      terminal observation can be recovered from this value. *)
  type t

  type source =
    | Live_completion
    | Recovery_adoption

  type preparation =
    | Prepared of t
    | Timing_unavailable of Agent_protocol.Error.t

  val source : t -> source
end

val create : origin:Mtime.t -> t
val set_host : t -> Recurrence_planning_host.t -> (unit, Agent_protocol.Error.t) result

val snapshot
  :  t
  -> wall:Agent_protocol.Timestamp.t
  -> monotonic:Mtime.t
  -> (Recurrence_phase.Snapshot.t, Agent_protocol.Error.t) result

(** Before persistence, validate newly definitive reciprocal Run/Admission and
    exact Terminal receipt/payload facts, including the producer's sampled wall.
    A fact mismatch rejects. Invalid monotonic timing is retained explicitly in
    the plan instead of preventing truthful terminal persistence. No ordinary
    Run without a recurrence origin acquires a binding. *)
val prepare_completions
  :  t
  -> sample:Completion.sample
  -> previous:Session_state.t
  -> current:Session_state.t
  -> (Completion.plan, Agent_protocol.Error.t) result

(** Install only after the exact prepared candidate's ACK/state assignment and
    before publication. The exact future-state-pruned map is prepared before
    persistence; installation is assignment only, with no clock reads, expected
    failure or IO. The plan must
    belong to this runtime; a foreign plan is a programming error. *)
val install_completions : t -> Completion.plan -> unit

(** Known-evidence lookup, not raw-reference equality or authentication. A live
    importer separately validates any saved extended reference against the
    actual target payload. None means no surviving process anchor; a present
    timing error remains present. A present binding with contradictory known
    evidence conflicts instead of choosing the recovery-absence branch.
    Inventory follows the existing retained Run
    origin bound and is pruned against actual committed immutable evidence. *)
val completion
  :  t
  -> origin:Recurrence_run_origin.t
  -> terminal:Recurrence_run_reference.t
  -> (Completion.t option, Agent_protocol.Error.t) result

(** Prepare the exact enabled fixed-delay successor in [state]. Fact, identity,
    basis or candidate mismatches return Error and must reject import. Expected
    invalid live timing instead returns Timing_unavailable, allowing the actual
    importer to prepare one historical evidence-plus-hold candidate from its
    original state. It must not report that timing diagnostic as a failed ACK.

    A valid live sample uses its actual receipt wall and absolute-monotonic
    position relative to this runtime's origin. No sample prepares the existing
    Pending_adoption wall-reconciliation path, preserving overdue lateness and
    misfire policy; it never directly installs Clock.recover. The complete future
    binding map and all fallible arithmetic are prepared before persistence. *)
val prepare_completion_phase
  :  t
  -> completion:Completion.t option
  -> basis:Recurrence_delay_basis.t
  -> entry:Recurrence_entry.t
  -> state:Session_state.t
  -> wall:Agent_protocol.Timestamp.t
  -> monotonic:Mtime.t
  -> (Completion_phase.preparation, Agent_protocol.Error.t) result

(** After the exact candidate ACK/state assignment and before publication.
    Same-owner invariant and assignment only; no clock, IO or expected failure. *)
val install_completion_phase : t -> Completion_phase.t -> unit

val ids : Session_state.t -> Agent_protocol.Id.Recurrence.t list

(** Prune incompatible bindings on every actual committed state installation.
    The state must have passed the ordinary committed-transition validation: it
    preserves terminal Runs and receipts immutably. Recovery creates an empty
    runtime; decoded state alone cannot retain or restore completion samples.
    No expected error or clock acquisition is introduced after durable ACK. *)
val retain_committed : t -> Session_state.t -> unit

val needs_adoption : previous:Session_state.t -> current:Session_state.t -> bool

(** Only a genuine Paused-to-Enabled control change adopts the precommit clock
    pair, after ACK. Repeated receipts/already-enabled controls do not reanchor. *)
val adopt_control
  :  t
  -> previous:Session_state.t
  -> current:Session_state.t
  -> snapshot:Recurrence_phase.Snapshot.t
  -> unit

val status
  :  t
  -> state:Session_state.t
  -> controller:Recurrence_controller.t
  -> (Recurrence_planning_status.t, Agent_protocol.Error.t) result

(** Serialized actor acquisition. Caller additionally checks actual runtime and
    lifecycle admission fences. Genuine Current and original host proof are
    required; absent/denied host or ineligible controller returns no work. *)
val begin_due
  :  t
  -> state:Session_state.t
  -> controller:Recurrence_controller.t
  -> recurrence_id:Agent_protocol.Id.Recurrence.t
  -> snapshot:Recurrence_phase.Snapshot.t
  -> (preparation option, Agent_protocol.Error.t) result

(** Outside mailbox: installed host's bounded Local read and pure planner only.
    Cancellation propagates; owning caller must release the acquired token. *)
val prepare : preparation -> (prepared, Agent_protocol.Error.t) result

(** Inside mailbox: exact issuer/token, original proof, actual source and exact
    recurrence basis, plus fresh genuine Current. No other actor/runtime lock
    is borrowed. Validation supplies no target authority. *)
val validate
  :  t
  -> prepared
  -> state:Session_state.t
  -> controller:Recurrence_controller.t
  -> (validation, Agent_protocol.Error.t) result

val delta : validation -> Session_delta.t option

(** Final non-yielding original-proof/Current/basis check immediately before the
    actor's existing persistence call. Runtime/lifecycle fences remain actor-owned. *)
val check_current
  :  t
  -> validation
  -> state:Session_state.t
  -> (unit, Agent_protocol.Error.t) result

(** After actual ACK/state install and before yielding publication; or an exact
    unchanged basis with final currentness checks. Ephemeral bookkeeping only. *)
val install_phase : t -> validation -> state:Session_state.t -> unit

(** Idempotent issuer/token-bound cleanup; an old token cannot release a newer
    preparation. Safe while lifecycle/runtime admission is closed. *)
val release : t -> preparation -> unit

(** Admission-time grace for an exact retained selected row. Live interval
    anchors follow elapsed time across cursor advance, Ready-to-protected moves
    and unresolved execution recording. Calendar, or recovered intervals lacking such an anchor, explicitly
    reconcile the current wall against the retained UTC nominal. A failed live
    anchor never falls back to wall/zero. No authority is granted. *)
val selection_lateness
  :  t
  -> entry:Recurrence_entry.t
  -> nominal:Recurrence_nominal.t
  -> snapshot:Recurrence_phase.Snapshot.t
  -> (int64 option, Agent_protocol.Error.t) result
