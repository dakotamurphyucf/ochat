(** Producer proof for a future exact job Wait. This does not authorize a job or
    invoke it. The actor passes its actual publisher and validated staged/current
    generic job before committing the wait and publishing work effects. *)
type t

(** The existing artifact publisher has a fixed bounded descriptor. Without it,
    the captured generic request must bound the entire invocation outcome to at
    most 512 encoded JSON bytes. Larger or unprovable promises fail Resource_limit
    before the Wait is admitted; terminal occurrences use their actual frame.
    The completion tag changes add at most three bytes; actual host failures,
    cancellation and interruption use the same 515-byte normalization below.
    The complete carrier additionally charges JSON-in-string escaping, bounded
    identifiers/source and its disposition headroom, not raw completion bytes. *)
val capture
  :  Agent_protocol.Job.t
  -> publisher:Agent_store.Job_result_store.Publisher.t option
  -> (t, Agent_protocol.Error.t) result

val storage : t -> Agent_store.Job_result_store.Publisher.Storage.t

(** Validate every actual host completion, including failures returned outside
    the worker's business-outcome guard. Above the captured inline ceiling,
    success/failure retains a bounded truthful result-limit failure; cancellation
    retains its cancelled outcome with a bounded diagnostic. [true] marks this
    storage limitation for immutable adverse evidence. Never fabricates success. *)
val normalize_completion
  :  t
  -> Agent_protocol.Completion.t
  -> (Agent_protocol.Completion.t * bool, Agent_protocol.Error.t) result

(** Before committing Wait, terminal jobs prove their actual complete frame fits;
    future jobs prove their captured producer contract. Ownership/eligibility is
    checked independently by Run_transition against the same prospective state. *)
val validate_wait
  :  Agent_protocol.Job.t
  -> run:Agent_protocol.Run.t
  -> publisher:Agent_store.Job_result_store.Publisher.t option
  -> (unit, Agent_protocol.Error.t) result

(** Derive future immutable proof/next-attempt space from existing durable owned
    jobs and their captured retry policy. Charges 4096 encoded bytes per remaining
    attempt (proof and possible next work key), plus 4096 per live run for its
    terminal receipt. Rejects a graph whose configured future attempts exceed
    Run_limits before dispatch or any other durable mutation can consume space. *)
val bookkeeping_reserve
  :  Run_state.t
  -> jobs:Agent_protocol.Job.t list
  -> (int, Agent_protocol.Error.t) result

(** Actual host cancellation/interruption diagnostics have no business-result
    effect. Bound every outcome to the same 515-byte terminal envelope while
    preserving adverse status; does not prove a future producer or authorize work. *)
val normalize_host_control
  :  Agent_protocol.Completion.t
  -> (Agent_protocol.Completion.t * bool, Agent_protocol.Error.t) result
