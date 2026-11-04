(** Pure checkpoint facts for one actual legacy callback. This planner neither
    selects a callback nor lends authority; the actor must own the exact Manager
    borrow, original source and completed receipt and revalidate before commit. *)
type t

(** Preserve the actual known suffix under Keep/Consume_head and number only the
    newly appended values. Each fresh row names [completed] and the supplied
    original attribution (None remains unavailable). The final carrier validates
    these facts against actual retained producers. Reject source/queue changes,
    a non-completed receipt or existing finite queue/payload limits. *)
val prepare
  :  metadata:Moderator_queue.t
  -> before:Session.Moderator_state.Identity_snapshot.t
  -> snapshot:Session.Moderator_state.Identity_snapshot.t
  -> consumption:Moderator_queue.Consumption.t
  -> completed:Agent_protocol.Moderator_execution.t
  -> origin:Agent_protocol.Inference_origin.t option
  -> (t, Agent_protocol.Error.t) result

val metadata : t -> Moderator_queue.t
val appended : t -> Moderator_queue.Entry.t list

(** Exact completed-producer append: preserve the known checkpoint except one
    new queue value and advance Keep by one captured ordinal. The actor proves
    the actual producer/payload/source independently; capture grants no custody. *)
val append_delivery
  :  metadata:Moderator_queue.t
  -> before:Session.Moderator_state.Identity_snapshot.t
  -> snapshot:Session.Moderator_state.Identity_snapshot.t
  -> capture:Moderator_queue.Capture.t
  -> (t, Agent_protocol.Error.t) result
