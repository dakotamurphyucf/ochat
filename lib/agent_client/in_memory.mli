(** In-process transport adapter used by embedded TUI and stdio modes. *)

val create
  :  request:
       (Agent_protocol.Command.t
        -> (Agent_protocol.Public.Result.t, Agent_protocol.Error.t) result)
  -> notifications:Agent_protocol.Envelope.t Eio.Stream.t
  -> close:(unit -> unit)
  -> Connection.t

(** In-process adapter using the same closed Developer callback as HTTP and
    Socket. The caller's handler returns raw success JSON; Connection still
    validates response method, bounds and affinity. *)
val create_with_developer
  :  request:
       (Agent_protocol.Command.t
        -> (Agent_protocol.Public.Result.t, Agent_protocol.Error.t) result)
  -> request_developer:
       (bound:Transport.Developer_response_bound.t
        -> Agent_protocol.Developer_wire.Request.t
        -> (Jsonaf.t, Agent_protocol.Error.t) result)
  -> notifications:Agent_protocol.Envelope.t Eio.Stream.t
  -> close:(unit -> unit)
  -> Connection.t

(** In-process adapter for the same closed REPL lane used by HTTP and Unix
    sockets. The caller supplies no extra notification reader or retry loop. *)
val create_with_developer_repl
  :  request:
       (Agent_protocol.Command.t
        -> (Agent_protocol.Public.Result.t, Agent_protocol.Error.t) result)
  -> request_developer:
       (bound:Transport.Developer_response_bound.t
        -> Agent_protocol.Developer_wire.Request.t
        -> (Jsonaf.t, Agent_protocol.Error.t) result)
  -> request_repl:
       (max_request_bytes:int
        -> bound:Transport.Repl_response_bound.t
        -> Agent_protocol.Developer_repl_wire.Request.t
        -> (Jsonaf.t, Agent_protocol.Error.t) result)
  -> notifications:Agent_protocol.Envelope.t Eio.Stream.t
  -> close:(unit -> unit)
  -> Connection.t
