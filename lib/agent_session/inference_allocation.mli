(** One durably acknowledged allocation and its final application-submission
    guard. Neither wrapping a Handle nor constructing a Port grants authority;
    the actor-issued port checks the exact physical execution and graph owner.
    There is no wire codec or script constructor. *)
type t

(** A strict owned allocation or final-submission rejection. *)
exception Rejected of Agent_protocol.Error.t

val handle : t -> Inference_ledger.Handle.t
val submission_guard : t -> Inference_runtime.Submission_guard.t

module Host : sig
  (** Called only after the existing canonical ledger owner acknowledges this
      allocation. The supplied guard retains the original execution lifetime. *)
  val create
    :  handle:Inference_ledger.Handle.t
    -> submission_guard:Inference_runtime.Submission_guard.t
    -> t
end

type allocation = t

module Port : sig
  (** An explicit host callback captured at actual execution admission. A stale
      owned port rejects; it must never fall back to unattributed allocation. *)
  type t =
    graph:Inference_graph_owner.t
    -> relation:Transcript.Scope.relation
    -> configuration:Inference.Observation.Configuration.t
    -> (allocation, Agent_protocol.Error.t) result
end
