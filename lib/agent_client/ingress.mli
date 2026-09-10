(** Submit registered data over an initialized protocol-1.1 connection. No
    attachment, transcript access or caller-supplied producer identity is needed.
    The authenticated principal still needs ingress.submit and the exact grant. *)
val submit
  :  Connection.t
  -> Agent_protocol.Ingress.Submit_request.t
  -> (Agent_protocol.Ingress.Acknowledgement.t, Agent_protocol.Error.t) result
