(** Offline authored workflow: actual startup finishes without model/tool calls. *)
val prompt : string

(** Exercise real wire admission, read-only and stale-CAS rejection, original-key
    retries and receipt reconciliation on an initialized transport. The supplied
    newly created session has not consumed its startup callback. *)
val check
  :  Agent_protocol.Public.Result.Create.t
  -> request:
       (Agent_protocol.Command.t
        -> (Agent_protocol.Public.Result.t, Agent_protocol.Error.t) result)
  -> key_prefix:string
  -> unit
