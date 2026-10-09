(** Read retained run projections through the caller's initialized transport.
    Uses the actual committed admission and advertised host; never attaches,
    executes work or assumes asynchronous orchestration has already finished. *)
val check
  :  Agent_protocol.Run_receipt.t
  -> session_id:Agent_protocol.Id.Session.t
  -> request:
       (Agent_protocol.Command.t
        -> (Agent_protocol.Public.Result.t, Agent_protocol.Error.t) result)
  -> unit
