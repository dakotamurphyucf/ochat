open! Core

(** Full-duplex NDJSON server over caller-owned input and output flows. Stdout
    remains protocol-only; diagnostics belong in the supplied error sink. *)

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
