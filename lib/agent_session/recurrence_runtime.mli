open! Core

(** Actor-confined process-local phase and preparation custody. No scheduler,
    persisted state, target permit or surviving monotonic origin. *)
type t

type preparation
type prepared
type validation

val create : origin:Mtime.t -> t
val set_host : t -> Recurrence_planning_host.t -> (unit, Agent_protocol.Error.t) result

val snapshot
  :  t
  -> wall:Agent_protocol.Timestamp.t
  -> monotonic:Mtime.t
  -> (Recurrence_phase.Snapshot.t, Agent_protocol.Error.t) result

val ids : Session_state.t -> Agent_protocol.Id.Recurrence.t list

(** Prune incompatible bindings on every actual committed state installation.
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
    anchors follow elapsed time across cursor advance and Ready-to-protected
    moves. Calendar, or recovered intervals lacking such an anchor, explicitly
    reconcile the current wall against the retained UTC nominal. A failed live
    anchor never falls back to wall/zero. No authority is granted. *)
val selection_lateness
  :  t
  -> entry:Recurrence_entry.t
  -> nominal:Recurrence_nominal.t
  -> snapshot:Recurrence_phase.Snapshot.t
  -> (int64 option, Agent_protocol.Error.t) result
