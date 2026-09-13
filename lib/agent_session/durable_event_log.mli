open! Core

(** Bounded in-memory replay index for journal-backed durable events. *)

type replay =
  | Available of Agent_protocol.Event.Durable.t list
  | Snapshot_required

type t

val create
  :  capacity:int
  -> Agent_protocol.Event.Durable.t list
  -> (t, Agent_protocol.Error.t) result

val append : t -> Agent_protocol.Event.Durable.t list -> unit

(** Capture before reading the actor snapshot, then await outside the actor and
    this log's mutex. Resolves on the next nonempty committed append, including
    appends made before the caller begins awaiting. Broadcast, not consumption;
    cancelled waiters require no registration cleanup. A wakeup is a reason to
    recheck state and authority, never proof of a particular completion predicate. *)
val changed : t -> unit Eio.Promise.t

(** [replay t ~after_sequence ~through_sequence] returns retained events in
    [(after_sequence, through_sequence]]. It requests a snapshot when the
    first required sequence predates the retained window. *)
val replay : t -> after_sequence:int64 -> through_sequence:int64 -> replay

val oldest_sequence : t -> int64 option
val latest_sequence : t -> int64 option

type history_epoch =
  | Replacement of int64
  | Window_start of int64
[@@deriving equal, sexp]

(** Bind an output snapshot to its most recent retained history replacement, or
    the replay floor when no replacement remains. This detects deletion of an
    unread output even when the consumed prefix is unchanged. Window eviction
    may conservatively expire a cursor. A snapshot older than the retained window
    requires refresh; restored snapshots without a replay suffix get a bounded
    fresh anchor. Reads the replay window atomically without changing it. *)
val history_epoch
  :  t
  -> through_sequence:int64
  -> (history_epoch, Agent_protocol.Error.t) result

(** Scan the complete retained replay window under its mutex. Validate ownership,
    continuity, full payloads and replacement/status projections before returning
    any references. Bounds events and serialized bytes. The owning actor must keep
    publication serialized while these references are used; this method alone is
    not a complete deletion proof. *)
val retained_references
  :  t
  -> session_id:Agent_protocol.Id.Session.t
  -> candidates:Agent_protocol.Id.Blob.t list
  -> max_events:int
  -> max_bytes:int
  -> (Agent_protocol.Id.Blob.t list, Agent_protocol.Error.t) result
