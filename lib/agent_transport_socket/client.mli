open! Core

(** Full-duplex Unix-domain socket client with request correlation and a
    bounded notification queue. *)

val connect
  :  sw:Eio.Switch.t
  -> net:_ Eio.Net.t
  -> socket_path:string
  -> max_line_length:int
  -> notification_capacity:int
  -> Agent_client.Connection.t
