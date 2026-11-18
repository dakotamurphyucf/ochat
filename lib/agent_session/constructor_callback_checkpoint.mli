(** Pure factual planning for the actual constructor callback. Validates the
    Manager callback and original known checkpoint independently of full raw
    metadata, then preserves that carrier while capturing only source facts and
    unavailable structural rows. This result lends no execution permission;
    the actor must own the constructor and commit it before its Running receipt. *)
type t =
  { index : Run_state.t
  ; source : Agent_protocol.Run_source.t
  ; moderator : Jsonaf.t
  }

val prepare
  :  Session_state.t
  -> manager:Chat_response.Moderator_manager.t
  -> callback:Chat_response.Moderator_manager.Callback.t
  -> (t, Agent_protocol.Error.t) result
