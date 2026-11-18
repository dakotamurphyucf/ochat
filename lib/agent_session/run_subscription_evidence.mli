(** Pure terminal DATA for actual owned Subscriptions. This is not an authority
    issuer, delivery, wake or callback completion. *)

(** [next] is the exact projection of the full sequential [delta], validated by
    Session_delta.apply. Select only explicit Subscription_changed/expired/cancelled
    leaves and exact work retained by [run] or admitted in the same callback.
    A new Subscription must have a real unarmed creation leaf. Open/armed rows,
    whole-state replacement and already-terminal rows never backfill proof.

    The first open-to-terminal leaf supplies the immutable winning epoch, full
    context, completion and completed_at. Its exact final record must remain in
    [next]. Source, generation, session and original invocation/parent Job are
    unchanged. Succeeded/Failed/Cancelled/Expired map respectively to
    Succeeded/Failed/Cancelled/Limited. New proofs use run.revision+1.

    Every winner may still be published as a correlated result notification.
    This collector never abandons a Subscription_delivery Wait. *)
val capture
  :  Session_state.t
  -> next:Session_state.t
  -> delta:Session_delta.t
  -> run:Agent_protocol.Run.t
  -> owned_work:Agent_protocol.Run_work.t list
  -> (Agent_protocol.Run_work.Terminal.t list, Agent_protocol.Error.t) result

val requires_completion_sample : Session_state.t -> delta:Session_delta.t -> bool

(** Whether the exact delta contains one of this producer's mutation leaves. *)
val has_mutations : Session_delta.t -> bool
