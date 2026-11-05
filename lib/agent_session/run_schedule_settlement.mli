(** Pure evidence for actual retained Schedule mutations. This does not execute,
    authorize, recover or reconstruct a callback. Source retirement precedes it. *)

(** Return the actual projected state together with only NEW immutable producer outcomes for [run], selected by the exact actual
    sequential staged delta, which this function projects using Session_delta.apply. Each selected
    ID must still refer to the same retained session/generation/creator/source.
    New rows add proof only from an actual Scheduled creation leaf followed by a
    validated terminal mutation of selected new work in this same callback.
    Still-live rows and already-terminal unchanged rows add no proof.
    A proof is stamped at exactly the next Run revision. Existing contradictory
    evidence rejects; an exact existing proof is not duplicated.

    Delivered with a real count increment records Succeeded; a no-delivery skip
    records Limited; Cancelled and Failed preserve their distinct outcomes. A
    queued callback is a separate obligation, not proof supplied by this helper.
    The callback transition uses this before action validation, sharing its one
    revision with its actual Completed callback evidence. *)
val capture
  :  Session_state.t
  -> delta:Session_delta.t
  -> run:Agent_protocol.Run.t
  -> owned_work:Agent_protocol.Run_work.t list
  -> ( Session_state.t * Agent_protocol.Run_work.Terminal.t list
       , Agent_protocol.Error.t )
       result

(** Project only explicit actual Schedule_changed leaves (including batches).
    Whole-state replacement/restoration never backfills evidence. Preserve the
    incoming Run index and exact canonical source/Stop interruption recognized
    by Run_retirement.matches_interrupt. An already terminal previous Run is
    immutable. Every other Run already advanced by the same candidate must contain the exact captured proofs; it
    cannot be silently skipped or advanced for a second time.

    Standalone producer writes add all new proofs and settle once per Run.
    Successful delivery leaves Waiting live for its genuine callback. A matching
    Wait whose producer definitively cannot deliver (skip/cancel/failure) retires
    Interrupted with known producer evidence and truthful unresolved custody.
    Unrelated Waiting and other Runs retain their lifecycle. *)
val prepare
  :  ?startup_run:Agent_protocol.Run.t
  -> Session_state.t
  -> delta:Session_delta.t
  -> now:Agent_protocol.Timestamp.t
  -> (Session_delta.t, Agent_protocol.Error.t) result

(** Conservative clock-selection predicate over explicit terminal mutations of
    live retained work. It grants nothing and may be true before source retirement
    proves no new terminal is needed. The actor samples one genuine adjacent pair
    before preparing the candidate and carries its exact wall/sample to install. *)
val requires_completion_sample : Session_state.t -> delta:Session_delta.t -> bool
