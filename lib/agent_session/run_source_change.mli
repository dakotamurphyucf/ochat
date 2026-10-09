(** Actual actor mutation admission for installed moderator identity changes.
    Checkpoints/restoration of the matching source retain epoch; replacement or
    removal retires old run/action custody in the same delta as installation.
    This helper never interprets a script-selected source as authorization. *)
val prepare
  :  Session_state.t
  -> delta:Session_delta.t
  -> now:Agent_protocol.Timestamp.t
  -> (Session_delta.t, Agent_protocol.Error.t) result
