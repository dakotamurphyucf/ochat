(** Minimal local peer for Responses WebSocket tests. The client under test
    still drives the real HTTP upgrade and sends masked application frames. *)

val headers : ?first_byte:string -> Eio.Buf_read.t -> string * (string * string) list
val upgrade : [> Eio.Flow.sink_ty ] Eio.Resource.t -> (string * string) list -> unit
val client_message : ?first_byte:string -> Eio.Buf_read.t -> Jsonaf.t
val server_frame : ?first:int -> string -> string

(** Serve independent loopback connections for the duration of the callback.
    The endpoint is local and ephemeral. Each accepted flow is closed when its
    handler returns or the fixture is cancelled. *)
val with_server
  :  Eio_unix.Stdenv.base
  -> (Eio_unix.Net.stream_socket_ty Eio.Resource.t -> unit)
  -> (Eio.Switch.t -> string -> 'a)
  -> 'a
