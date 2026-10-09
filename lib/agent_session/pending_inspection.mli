(** Pure public projection of the sole admitted queue/disposition state. The host
    must establish current visibility/transcript authority first and supply its
    principal-scoped history projection. No runtime activation or read authority
    is granted by this module. *)
type t = Session_state.t

type projection =
  Agent_protocol.History.entry
  -> (Agent_protocol.Public_history.t, Agent_protocol.Error.t) result

val item
  :  Pending_input_document.t
  -> project:projection
  -> (Agent_protocol.Pending_query.Item.t, Agent_protocol.Error.t) result

(** Unknown or expired disposition yields Unavailable even if another canonical
    entry happens to have the requested ID; never infers original admission. *)
val lookup
  :  t
  -> history_id:Agent_protocol.History.Id.t
  -> project:projection
  -> (Agent_protocol.Pending_query.Outcome.t, Agent_protocol.Error.t) result

(** Additional to actual current host writer permission. A retained disposition
    carries its immutable original owner; absence/legacy ownership fails closed. *)
val authorize_control
  :  t
  -> history_id:Agent_protocol.History.Id.t
  -> principal:Agent_protocol.Id.Principal.t
  -> (unit, Agent_protocol.Error.t) result
