(** Retained-window inference reads over an initialized connection. Optional
    support is checked against its actual successful initialization. Feature
    selection is not authority: the server still checks session visibility and
    Diagnostics before exposing detailed safe configuration/diagnostic fields.
    No lifetime total, provider DTO or private request body is reconstructed. *)

val summary
  :  Connection.t
  -> Agent_protocol.Id.Session.t
  -> (Agent_protocol.Inference_query.Summary.t, Agent_protocol.Error.t) result

val observations
  :  Connection.t
  -> Agent_protocol.Inference_query.Request.t
  -> (Agent_protocol.Inference_query.Response.t, Agent_protocol.Error.t) result
