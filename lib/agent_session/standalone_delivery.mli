(** Nonexecuting, disclosure-checked admission proposal for the host's standalone
    adapter. Only a real published Pending invocation's owned terminal job may
    produce it. The actor must revalidate the snapshot and shared quotas before
    committing the intent. It neither inserts history nor requests a model call. *)
type t = private
  { revision : int64
  ; delivery : Agent_protocol.Delivery.t
  }

val prepare
  :  state:Session_state.t
  -> invocation_id:Agent_protocol.Id.Invocation.t
  -> job_id:Agent_protocol.Id.Job.t
  -> completion:Agent_protocol.Completion.t
  -> current_capabilities:Chat_response.Tool_capability.t
  -> delivery_id:Agent_protocol.Id.Delivery.t
  -> now:Agent_protocol.Timestamp.t
  -> wake:Agent_protocol.Completion.wake
  -> (t, Agent_protocol.Error.t) result

val revalidate
  :  state:Session_state.t
  -> staged:Agent_protocol.Delivery.t list
  -> limits:Staged_notifications.limits
  -> t
  -> (unit, Agent_protocol.Error.t) result
