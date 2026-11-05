(** Pure publication/failure DATA for actual owned notification Deliveries.
    Accepted_wake never proves the separate Operation completed. *)

(** [next] is the exact validated projection of the sequential [delta]. A
    Committed proof requires the actual Delivery_committed leaf, matching history
    identity, Runtime_notification provenance and exact Notification_history
    envelope. A matching final status or unrelated history entry is insufficient.
    Full immutable context, actual creator/source and attempt are retained. A
    subscription-bound delivery must retain the exact terminal subscription's
    ID, winning epoch, invocation and completion. Publication records Succeeded
    even when its completion payload reports an adverse Subscription winner.

    An actual Failed leaf is final only when its tool_error.retryable=false.
    A retryable failure remains unsettled. No historical status backfill; a new
    delivery needs its Pending first-attempt creation leaf. New proofs use the
    next Run revision, sharing its existing callback or producer transaction. *)
val capture
  :  Session_state.t
  -> next:Session_state.t
  -> delta:Session_delta.t
  -> run:Agent_protocol.Run.t
  -> owned_work:Agent_protocol.Run_work.t list
  -> (Agent_protocol.Run_work.Terminal.t list, Agent_protocol.Error.t) result

(** Reject reuse of an exact owned Delivery with immutable Failed proof, even if
    its Run is already terminal. Protocol retry for unowned deliveries remains
    unchanged. This is checked before the common settlement's terminal-Run skip
    and whole replacement skip. Restored DATA cannot reopen a delivery whose
    retained old or incoming Run has immutable Failed evidence; it issues no
    retrospective proof. *)
val check_retries
  :  Session_state.t
  -> delta:Session_delta.t
  -> (unit, Agent_protocol.Error.t) result

val requires_completion_sample : Session_state.t -> delta:Session_delta.t -> bool

(** Whether the exact delta contains one of this producer's mutation leaves. *)
val has_mutations : Session_delta.t -> bool
