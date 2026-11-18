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

(** Plan the appended suffix of an actual resolved Invocation handler or
    observation, preserving the entire existing queue with Keep. Require a
    validated Resolved/Published receipt whose generation and snapshot match the
    captured source; an optional observation observer must also match. Its absence
    supplies no source proof: the actor must own the actual Manager/source borrow.
    Fresh rows name the Invocation and its original inference attribution.
    Without observation intent they additionally retain the actual checkpoint's
    full installation as [Queue_producer.Invocation_at_source]; an existing
    observer-backed Invocation keeps its original producer representation.
    Neither form grants admission. Apply the same finite
    suffix/queue limits as [prepare]. The actor separately owns and validates
    the exact borrow, original admission and native candidate. *)
val prepare_invocation
  :  metadata:Moderator_queue.t
  -> before:Session.Moderator_state.Identity_snapshot.t
  -> snapshot:Session.Moderator_state.Identity_snapshot.t
  -> resolved:Agent_protocol.Invocation.t
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
