open! Core

(** Adopt an authored hook's user state through the installed moderator's
    universal checkpoint codec. Lifecycle effects, queue and overlay changes
    require their normal independently selected host owners. *)
val prepare
  :  Jsonaf.t option
  -> outcome:Chat_response.Moderation.Outcome.t
  -> snapshot:Session.Moderator_state.Identity_snapshot.t
  -> (Jsonaf.t option, Agent_protocol.Error.t) result
