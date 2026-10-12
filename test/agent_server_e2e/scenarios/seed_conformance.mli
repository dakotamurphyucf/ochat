(** Submit bounded complete neutral developer/assistant/call/result evidence into
    a new stopped conversation at the authentic current Catalog revision. Check
    exact full read evidence, code identity, original-key receipt/replay and changed
    seed conflict without altering the original history. Public projections are
    inspected only; they are never converted into canonical inputs. *)
val check
  :  spec:Agent_protocol.Session.Spec.t
  -> request:
       (Agent_protocol.Command.t
        -> (Agent_protocol.Public.Result.t, Agent_protocol.Error.t) result)
  -> key_prefix:string
  -> unit
