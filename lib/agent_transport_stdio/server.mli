open! Core

(** Full-duplex NDJSON server over caller-owned input and output flows. Stdout
    remains protocol-only; diagnostics belong in the supplied error sink.
    Cancellation propagates unchanged with its original backtrace and never calls
    [on_error]. Connection/outgoing cleanup joins before returning or raising;
    the caller retains ownership of both flows. Malformed input/framing and other
    input failures retain the existing [on_error] reporting contract. *)

val run
  :  sw:Eio.Switch.t
  -> dispatcher:Agent_server.Dispatcher.t
  -> close_connection:(Agent_server.Connection_context.t -> unit)
  -> principal:Agent_protocol.Principal.t
  -> connection_id:string
  -> input:_ Eio.Flow.source
  -> output:_ Eio.Flow.sink
  -> max_line_length:int
  -> outgoing_capacity:int
  -> max_attachments:int
  -> on_error:(Agent_protocol.Error.t -> unit)
  -> unit

(** Explicit original-authentication proof for socket or other authenticated streams. *)
val run_authenticated
  :  sw:Eio.Switch.t
  -> dispatcher:Agent_server.Dispatcher.t
  -> close_connection:(Agent_server.Connection_context.t -> unit)
  -> actor:Operator_authorization.t
  -> connection_id:string
  -> input:_ Eio.Flow.source
  -> output:_ Eio.Flow.sink
  -> max_line_length:int
  -> outgoing_capacity:int
  -> max_attachments:int
  -> on_error:(Agent_protocol.Error.t -> unit)
  -> unit
