(** Source/reset control disposition selected by the sole actor before candidate
    validation. It preserves definitions/cursors and existing pauses; no external
    cancellation or execution evidence is inferred. *)
val prepare_change
  :  Session_state.t
  -> delta:Session_delta.t
  -> (Session_delta.t, Agent_protocol.Error.t) result
