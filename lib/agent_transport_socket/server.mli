open! Core

(** Local Unix-domain socket server using the same full-duplex NDJSON
    connection implementation as stdio. *)

(** [prepare_path ~env ~socket_path] validates that the parent directory is
    owned by the daemon UID and inaccessible to group/other users. It removes
    a stale socket only after an Eio connection probe is refused and the
    socket inode is revalidated. *)
val prepare_path
  :  env:Eio_unix.Stdenv.base
  -> socket_path:string
  -> (unit, Agent_protocol.Error.t) result

(** [serve] authenticates and serves one accepted Eio socket until EOF or a
    protocol failure. Authentication happens before any request is read. *)
val serve
  :  dispatcher:Agent_server.Dispatcher.t
  -> close_connection:(Agent_server.Connection_context.t -> unit)
  -> authenticate:
       ('tag Eio.Net.stream_socket_ty Eio.Resource.t
        -> Eio.Net.Sockaddr.stream
        -> (Agent_protocol.Principal.t, Agent_protocol.Error.t) result)
  -> max_line_length:int
  -> outgoing_capacity:int
  -> max_attachments:int
  -> on_protocol_error:(Agent_protocol.Error.t -> unit)
  -> 'tag Eio.Net.stream_socket_ty Eio.Resource.t
  -> Eio.Net.Sockaddr.stream
  -> unit

val run
  :  sw:Eio.Switch.t
  -> net:[> ([> `Generic ] as 'tag) Eio.Net.ty ] Eio.Resource.t
  -> socket_path:string
  -> backlog:int
  -> dispatcher:Agent_server.Dispatcher.t
  -> close_connection:(Agent_server.Connection_context.t -> unit)
  -> authenticate:
       ('tag Eio.Net.stream_socket_ty Eio.Resource.t
        -> Eio.Net.Sockaddr.stream
        -> (Agent_protocol.Principal.t, Agent_protocol.Error.t) result)
  -> max_line_length:int
  -> outgoing_capacity:int
  -> max_attachments:int
  -> on_error:(exn -> unit)
  -> on_protocol_error:(Agent_protocol.Error.t -> unit)
  -> unit
