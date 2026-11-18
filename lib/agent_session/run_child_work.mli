(** Pure same-transaction custody for new child work of a retained live parent.
    The actor supplies its actual physical parent's occurrence and freshly
    selected children. These values and the result grant no execution authority.
    The prospective work projection must preserve the complete existing Run index
    and installed moderator source; independent index/source changes reject.
    No callback/checkpoint/action or terminal receipt is fabricated. *)
val prepare
  :  Session_state.t
  -> run:Agent_protocol.Run.t
  -> parent:Agent_protocol.Run_work.t
  -> work_state:Session_state.t
  -> children:Agent_protocol.Run_work.t list
  -> now:Agent_protocol.Timestamp.t
  -> (Session_delta.t, Agent_protocol.Error.t) result

(** Add actual new Jobs selected by a genuine registered observation callback.
    The actor authenticates the physical borrow and current Run authority before
    this computation and again before persistence. The exact retained invocation
    must still be Observing, with a positive original Run classification matching
    the current source, generation and nonterminal Run. Each supplied Job must be
    new in [work_state], absent from the prior state, and carry that same origin.
    This adds only their real Job occurrences; it does not invent an Invocation
    or moderator-event parent obligation, callback receipt or action. The generic
    [prepare] operation's retained-parent invariant remains separate. *)
val prepare_observation
  :  Session_state.t
  -> run:Agent_protocol.Run.t
  -> observing:Agent_protocol.Invocation.t
  -> work_state:Session_state.t
  -> children:Agent_protocol.Job.t list
  -> now:Agent_protocol.Timestamp.t
  -> (Session_delta.t, Agent_protocol.Error.t) result

(** Add actual new Jobs from an actor-authenticated legacy callback. The exact
    retained receipt stays Running in both projections and its immutable event
    provenance must positively name this same live Run. This introduces no Event
    work obligation or Run action service. The actor separately checks the exact
    Callback_scope/current original authority before and after preparation. *)
val prepare_callback
  :  Session_state.t
  -> run:Agent_protocol.Run.t
  -> executing:Agent_protocol.Moderator_execution.t
  -> work_state:Session_state.t
  -> children:Agent_protocol.Job.t list
  -> now:Agent_protocol.Timestamp.t
  -> (Session_delta.t, Agent_protocol.Error.t) result

(** New selected Jobs from the exact actor-registered held Init/startup owner.
    The actor supplies the actual live Run after physical Init/activation/source
    validation. Checks the same complete retained index, installed source,
    generation and exact new Job origins as other child joins; creates neither
    an Event nor a parent-work placeholder. Pure DATA, never an Init issuer. *)
val prepare_initialization
  :  Session_state.t
  -> run:Agent_protocol.Run.t
  -> work_state:Session_state.t
  -> children:Agent_protocol.Job.t list
  -> now:Agent_protocol.Timestamp.t
  -> (Session_delta.t, Agent_protocol.Error.t) result
