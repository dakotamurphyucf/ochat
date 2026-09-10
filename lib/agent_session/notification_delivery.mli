(** A nonexecuting proposal over one actor snapshot. Capability rebinding checks
    the publisher's captured ceiling, never the entire registry as an implicit
    replacement. The actor must check revision, lifecycle and source before save. *)
type action =
  | Publish of Agent_protocol.Delivery.t
  | Fail of Agent_protocol.Delivery.t

type t = private
  { session_id : Agent_protocol.Id.Session.t
  ; generation : int
  ; revision : int64
  ; source : Agent_protocol.Invocation.observer
  ; actions : action list
  }

val prepare
  :  state:Session_state.t
  -> source:Agent_protocol.Invocation.observer
  -> current_capabilities:Chat_response.Tool_capability.t
  -> policy:Chat_response.One_off_request.policy
  -> max_count:int
  -> (t, Agent_protocol.Error.t) result
