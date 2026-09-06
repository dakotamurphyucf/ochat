open! Core

(** Full-duplex NDJSON gateway from caller-owned stdio flows to a typed daemon
    client connection. The gateway never owns daemon session lifetime. *)

(** [run ~sw ~connection ~input ~output ~max_line_length
    ~outgoing_capacity ~on_error] forwards request envelopes and asynchronous
    daemon notifications. EOF closes the client connection and detaches its
    remote attachments. *)
val run
  :  sw:Eio.Switch.t
  -> connection:Agent_client.Connection.t
  -> input:_ Eio.Flow.source
  -> output:_ Eio.Flow.sink
  -> max_line_length:int
  -> outgoing_capacity:int
  -> on_error:(Agent_protocol.Error.t -> unit)
  -> unit
