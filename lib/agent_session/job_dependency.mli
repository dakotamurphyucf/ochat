open Core

(** Validate eventual data under the original result budget and optional completion
    schema. Invalid data becomes a bounded failure, with no rejected value leaked. *)
val completion
  :  Agent_protocol.Job.dependency
  -> Agent_protocol.Completion.t
  -> (Agent_protocol.Completion.t, Agent_protocol.Error.t) result

(** Verify a persisted wait against the Pending invocation and its actual launched
    child job. Retains the exact parent attempt, generation and deadline. Together
    with launch ancestry/depth validation, dependencies cannot form a cycle. *)
val validate
  :  invocations:Agent_protocol.Invocation.t list
  -> jobs:Agent_protocol.Job.t list
  -> Agent_protocol.Job.t
  -> (unit, Agent_protocol.Error.t) result
