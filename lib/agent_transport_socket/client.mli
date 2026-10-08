open! Core

(** Full-duplex Unix-domain socket client with request correlation and a
    bounded notification queue. Cancelling a response wait releases its local
    correlation entry without replaying the command. A failed or cancelled
    request write closes the channel because its JSON line may be incomplete;
    the owning connection retains any uncertain mutation intent. *)

val connect
  :  sw:Eio.Switch.t
  -> net:_ Eio.Net.t
  -> socket_path:string
  -> max_line_length:int
  -> notification_capacity:int
  -> Agent_client.Connection.t
