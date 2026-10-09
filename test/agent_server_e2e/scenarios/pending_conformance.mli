(** Shared pending-control script for each supported transport. The caller owns
    the initialized connection and the preseeded stopped session. This script
    never starts inference or creates an alternate pending queue. *)
val observe
  :  request:
       (Agent_protocol.Command.t
        -> (Agent_protocol.Public.Result.t, Agent_protocol.Error.t) result)
  -> session_id:Agent_protocol.Id.Session.t
  -> key_prefix:string
  -> unit
