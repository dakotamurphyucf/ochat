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

(** [replay t ~after_sequence ~through_sequence] returns retained events in
    [(after_sequence, through_sequence]]. It requests a snapshot when the
    first required sequence predates the retained window. *)
val replay : t -> after_sequence:int64 -> through_sequence:int64 -> replay

val oldest_sequence : t -> int64 option
val latest_sequence : t -> int64 option

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
