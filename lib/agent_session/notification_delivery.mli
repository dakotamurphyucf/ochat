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

type idle = private
  { pending : t
  ; wakes : Agent_protocol.Delivery.t list
  ; discarded_wakes : Agent_protocol.Delivery.t list
  }

(** Snapshot-only scheduling hint for owned current-generation pending data or
    committed, unsettled wake receipts. Does not authorize runtime loading. *)
val has_idle_work : Session_state.t -> bool

(** Rechecks up to [max_count] pending publications and [max_count] committed wake
    receipts independently, so waiting acknowledgements cannot starve recovery.
    A recovered wake never reinserts history. Removed source/permissions retire its
    wake request while preserving the already committed data and business result. *)
val prepare_idle
  :  state:Session_state.t
  -> source:Agent_protocol.Invocation.observer
  -> current_capabilities:Chat_response.Tool_capability.t
  -> policy:Chat_response.One_off_request.policy
  -> max_count:int
  -> (idle, Agent_protocol.Error.t) result
