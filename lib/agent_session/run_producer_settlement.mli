(** One pure settlement owner for actual Schedule, Subscription and Delivery
    producer outcomes. It extends the existing Schedule settlement path; it does
    not execute, authorize, restore or reconstruct callbacks. *)

(** Project the exact sequential staged delta once and combine NEW immutable
    evidence from the three finite producer collectors. Stamped at precisely the
    next Run revision, shared with the callback's completion when applicable.
    Existing contradictory evidence rejects; exact evidence is not duplicated.
    Whole-state replacement and unchanged historical terminal rows add nothing. *)
val capture
  :  Session_state.t
  -> delta:Session_delta.t
  -> run:Agent_protocol.Run.t
  -> owned_work:Agent_protocol.Run_work.t list
  -> ( Session_state.t * Agent_protocol.Run_work.Terminal.t list
       , Agent_protocol.Error.t )
       result

(** Preserve the incoming Run index. Canonical source/Stop retirement and old
    terminal Runs remain immutable. An already-advanced incoming Run must contain
    the exact captured proofs; it cannot skip them or advance for a second time.
    Standalone writes append all producer proofs and settle ONCE per Run.

    A Schedule skip/cancel/failure or final failure of the exact awaited Delivery
    retires its now-undeliverable Wait with known evidence and truthful unresolved
    custody. Subscription adverse winners preserve Waiting: their correlated
    result notification may still deliver the genuine wake. All aggregate outcome
    precedence, pending Finish and strict Finish rules remain unchanged. *)
val prepare
  :  ?startup_run:Agent_protocol.Run.t
  -> Session_state.t
  -> delta:Session_delta.t
  -> now:Agent_protocol.Timestamp.t
  -> (Session_delta.t, Agent_protocol.Error.t) result

(** Join a companion producer delta with an already-selected Job outcome before
    either advances its Run. [state] and [index] must retain the exact [previous];
    [projected] is the actual validated companion projection. [terminal_work]
    includes every unchanged prior proof and each genuine new proof at revision+1.
    Exact impossible-Wait retirement uses original retained occurrence evidence;
    otherwise the existing aggregate settlement law performs the single advance.
    It grants no authority and issues no proof. Both this module's standalone
    producer path and Run_job_completion use this same operation. *)
val settle
  :  ?startup_run:Agent_protocol.Run.t
  -> Run_state.t
  -> state:Session_state.t
  -> projected:Session_state.t
  -> previous:Agent_protocol.Run.t
  -> owned_work:Agent_protocol.Run_work.t list
  -> terminal_work:Agent_protocol.Run_work.Terminal.t list
  -> now:Agent_protocol.Timestamp.t
  -> (Run_state.t, Agent_protocol.Error.t) result

(** Conservative actual-terminal-mutation predicate; one genuine adjacent wall /
    monotonic completion sample is selected before preparing the writer candidate
    and the SAME sample reaches recurrence completion installation. *)
val requires_completion_sample : Session_state.t -> delta:Session_delta.t -> bool
