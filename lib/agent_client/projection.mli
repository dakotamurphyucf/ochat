(** Pure public authoritative projection and bounded transient view. *)
type t

type synchronization =
  | Current
  | Snapshot_required of Agent_protocol.Error.t

(** New attachment snapshot resets transient receipt/draft/fence observations.
    Public.Snapshot itself has already passed domain admission. *)
val install_snapshot
  :  ?live_limits:Live_projection.Limits.t
  -> Agent_protocol.Public.Snapshot.t
  -> t

val snapshot : t -> Agent_protocol.Public.Snapshot.t
val synchronization : t -> synchronization

(** Retains the last admitted authoritative snapshot for inspection, clears live
    drafts/future data and prevents further reduction until a fresh snapshot. *)
val mark_stale : t -> Agent_protocol.Error.t -> t

(** Session-bound contiguous durable sequence/revision. Hidden events advance
    ordering slots; typed payloads/replacements validate exact anchors. No raw
    canonical payload decoder or Public-to-canonical import exists. Sequence
    arithmetic is checked at int64 boundaries. *)
val apply_event
  :  t
  -> Agent_protocol.Public.Durable.t
  -> (t, Agent_protocol.Error.t) result

val apply_live_event
  :  t
  -> Agent_protocol.Event.Recoverable.t
  -> (t, Agent_protocol.Error.t) result

val live : t -> Live_projection.t

(** Last actual durable terminal observation until a newer operation starts.
    Snapshot installation clears it; idle snapshots do not fabricate a terminal. *)
val terminal_operation : t -> Agent_protocol.Operation.t option
