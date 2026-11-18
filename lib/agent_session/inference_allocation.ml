type t =
  { handle : Inference_ledger.Handle.t
  ; submission_guard : Inference_runtime.Submission_guard.t
  }

exception Rejected of Agent_protocol.Error.t

let handle t = t.handle
let submission_guard t = t.submission_guard

module Host = struct
  let create ~handle ~submission_guard = { handle; submission_guard }
end

type allocation = t

module Port = struct
  type t =
    graph:Inference_graph_owner.t
    -> relation:Transcript.Scope.relation
    -> configuration:Inference.Observation.Configuration.t
    -> (allocation, Agent_protocol.Error.t) result
end
