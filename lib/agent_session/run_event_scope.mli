(** Route actual startup, owned operation/job and retained queued callbacks to
    one Run. Positive retained queued-event evidence is selected globally before
    checking live ownership/source; its drainer operation cannot replace it when
    that causal Run is terminal or stale. Observation and unrelated internal
    events do not acquire authority.
    This selection grants no native scope: the actor separately checks its live
    borrow, current host authorization and exact source installation. *)
val select
  :  Session_state.t
  -> executing:Agent_protocol.Moderator_execution.t
  -> (Agent_protocol.Run.t option, Agent_protocol.Error.t) result

(** Initial Single_turn callback shape only, not authority. Used after the
    actor's actual source selection to require a checkpoint even when the
    callback stages no action/work. [select] and live actor checks remain
    necessary before admitting any resulting transition. *)
val is_initial_single_turn
  :  Agent_protocol.Run.t
  -> Agent_protocol.Moderator_execution.context
  -> bool
