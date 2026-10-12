open! Core
module Selection = Agent_protocol.Prompt_dependency_selection

module Failure = struct
  type t =
    | Rejected of Agent_protocol.Error.t
    | Uncertain of Agent_protocol.Error.t
end

type t =
  { status : unit -> (Selection.Status.t, Agent_protocol.Error.t) Result.t
  ; update :
      Selection.Update_request.t
      -> checkpoint:(unit -> unit)
      -> (Selection.Update_result.t, Failure.t) Result.t
  }

let create ~status ~update = { status; update }
let status t = t.status ()
let update t request ~checkpoint = t.update request ~checkpoint
