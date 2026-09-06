(** In-process transport adapter used by embedded TUI and stdio modes. *)

val create
  :  request:
       (Agent_protocol.Command.t
        -> (Agent_protocol.Method_result.t, Agent_protocol.Error.t) result)
  -> notifications:Agent_protocol.Envelope.t Eio.Stream.t
  -> close:(unit -> unit)
  -> Connection.t
