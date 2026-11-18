(** Pure facts for one actual legacy Job completion append. This grants no
    Manager borrow or continuation: Actor owns the exact installed resource,
    original-source classification, terminal loan and ACK boundary. *)
type t

(** Validate the original full queue child independently from known-snapshot
    equality, and require one exact existing Model/Async payload append. Historical
    prefix rows remain unavailable. No Run or execution obligation is created. *)
val prepare
  :  Session_state.t
  -> job:Agent_protocol.Job.t
  -> before:Session.Moderator_state.Identity_snapshot.t
  -> snapshot:Jsonaf.t option
  -> capture:Moderator_queue.Capture.t
  -> (t, Agent_protocol.Error.t) result

val index : t -> Run_state.t
val source : t -> Agent_protocol.Run_source.t
val metadata : t -> Moderator_queue.t
val entry : t -> Moderator_queue.Entry.t
val checkpoint : t -> Jsonaf.t
