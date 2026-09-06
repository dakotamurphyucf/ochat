open! Core

(** Piaf HTTP transport for the Ochat agent protocol. Logical HTTP
    connections survive individual request and SSE stream lifetimes. *)

val run
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> address:Eio.Net.Sockaddr.stream
  -> dispatcher:Agent_server.Dispatcher.t
  -> registry:Agent_server.Session_registry.t
  -> blob_store:Agent_store.Blob_store.t
  -> health:(include_details:bool -> Agent_protocol.Health.Response.t)
  -> close_connection:(Agent_server.Connection_context.t -> unit)
  -> authenticate:
       (Agent_server.Authenticator.Request_identity.t
        -> string option
        -> (Agent_protocol.Principal.t, Agent_protocol.Error.t) result)
  -> max_body_bytes:int
  -> max_batch_size:int
  -> batch_concurrency:int
  -> outgoing_capacity:int
  -> max_connections:int
  -> max_attachments:int
  -> idle_connection_timeout:float
  -> on_error:(exn -> unit)
  -> unit
