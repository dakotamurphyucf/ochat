(** Pure composition of one validated edit, exact archive reference, retained
    recovery and optional actual Turn. The actor owns writer/runtime admission,
    archive capture and persistence; this module creates no effects. *)
type continuation =
  | Save_only
  | Unavailable of Agent_protocol.History_edit.Continuation.unavailable
  | Start of Agent_protocol.Operation.t

type t

val create
  :  Session_state.t
  -> plan:History_edit.t
  -> edit:Agent_protocol.History_edit.t
  -> archive:Session_state.Compaction_archive.t
  -> continuation:continuation
  -> (t, Agent_protocol.Error.t) result

val delta : t -> Session_delta.t
val payloads : t -> Agent_protocol.Event.Durable.Payload.t list
val admission : t -> Turn_admission.t option
val continuation : t -> Agent_protocol.History_edit.Continuation.t
val edited_entry : t -> Agent_protocol.History.entry
val archived_revision : t -> int64
