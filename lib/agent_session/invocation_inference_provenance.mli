(** Immutable attribution claims, separate from current execution permission.
    These pure lookups do not authenticate a physical owner; only the actual
    actor admission may use them after its ordinary owner/source checks. No Goal
    selector or current operation is inferred. *)

(** Classify a genuinely admitted operation by its retained Run evidence. The
    actor must supply its exact current operation, never an arbitrary ID. A
    unique retained Run remains named even after terminal disposition. *)
val of_operation
  :  runs:Run_state.t option
  -> Agent_protocol.Operation.t
  -> (Agent_protocol.Inference_origin.t, Agent_protocol.Error.t) result

(** Return positive stored evidence. Unavailable classification rejects;
    no later callback or caller may migrate it to unbound. *)
val of_invocation
  :  Agent_protocol.Invocation.t
  -> (Agent_protocol.Inference_origin.t, Agent_protocol.Error.t) result

val of_job
  :  Agent_protocol.Job.t
  -> (Agent_protocol.Inference_origin.t, Agent_protocol.Error.t) result

(** Select only this exact actual event's Run, operation or acknowledged Job
    ancestry. Positive retained queued-event evidence takes precedence over its
    physical drainer operation and remains named after terminal disposition.
    This is immutable evidence, not permission to execute. A genuine parent Job
    may have unavailable evidence; native admission
    preserves that absence. The caller authenticates its registered event borrow. *)
val of_event
  :  runs:Run_state.t option
  -> jobs:Agent_protocol.Job.t list
  -> Agent_protocol.Moderator_execution.t
  -> (Agent_protocol.Inference_origin.t option, Agent_protocol.Error.t) result

(** Validate the complete final carrier after ordered deltas or adoption. A
    named Run must exist in the same session/source generation. Known parents,
    observer source and retained Invocation work cannot contradict its origin.
    Historical missing origin remains loadable but is unavailable to lookups.
    An origin is attribution evidence and adds no live Run-work obligation. *)
val validate
  :  invocations:Agent_protocol.Invocation.t list
  -> jobs:Agent_protocol.Job.t list
  -> events:Agent_protocol.Moderator_execution.t list
  -> runs:Run_state.t option
  -> (unit, Agent_protocol.Error.t) result

(** Replacements and adopted carriers preserve every existing row's original
    optional classification, including unavailable historical evidence. *)
val validate_transition
  :  previous:Agent_protocol.Invocation.t list
  -> incoming:Agent_protocol.Invocation.t list
  -> (unit, Agent_protocol.Error.t) result
