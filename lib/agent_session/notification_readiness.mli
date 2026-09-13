(** Acknowledgement ancestry for durable conversation delivery. This is an
    ordering check over retained records, not a grant of disclosure authority or
    proof of a provider safe point. A caller must check those separately. *)
type blockage =
  | Waiting of string
  | Rejected of string
[@@deriving equal, sexp]

(** Owned intents follow their correlation, creator and referenced job launch
    through nested invocation/job/event ancestry. Model calls require Published;
    nested calls require a recorded outcome and their enclosing acknowledgement.
    Historical unowned intents preserve the original direct-publication contract.
    Waiting is retryable after state changes; Rejected needs explicit disposition,
    not an unbounded retry loop. No record is changed or provider output invented. *)
val check
  :  invocations:Agent_protocol.Invocation.t list
  -> jobs:Agent_protocol.Job.t list
  -> events:Agent_protocol.Moderator_execution.t list
  -> Agent_protocol.Delivery.t
  -> (unit, blockage) result
