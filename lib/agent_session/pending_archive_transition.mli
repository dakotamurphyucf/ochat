(** Exact bounded expiry archives for the ordered native transition. This pure
    adapter validates references and returns immutable custody records; the
    existing persistence owner must retain/publish them before state installation. *)
val collect
  :  Session_state.t
  -> delta:Session_delta.t
  -> limits:Document_schema.Limits.t
  -> (Pending_archive.t list, Agent_protocol.Error.t) result
