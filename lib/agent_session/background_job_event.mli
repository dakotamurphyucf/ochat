(** Resolve the generic job's moderator source from its immutable creating
    invocation/event. Absence is explicit: a source-free job must not inherit a
    moderator merely because one was installed later. No script effects run. *)
val source
  :  state:Session_state.t
  -> Agent_protocol.Job.t
  -> (Agent_protocol.Invocation.observer option, Agent_protocol.Error.t) result

(** Construct the private frame only for the current session/generation and
    exact captured source. The caller separately checks delivery/queue ownership. *)
val frame
  :  state:Session_state.t
  -> observer:Agent_protocol.Invocation.observer
  -> Agent_protocol.Job.t
  -> (Chat_response.Background_delivery.t, Agent_protocol.Error.t) result
