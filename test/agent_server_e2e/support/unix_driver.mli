open Core

(** Typed and raw clients for the daemon's real Unix-domain socket. *)

type raw

type raw_read =
  | Envelope of Agent_protocol.Envelope.t
  | End_of_file
  | Timeout
  | Invalid_response of Agent_protocol.Error.t
[@@deriving sexp]

(** [connect ~sw ~env ~socket_path] opens the common typed client over the
    daemon's Unix socket. *)
val connect
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> socket_path:string
  -> Agent_client.Connection.t

(** [initialize connection] performs Protocol 1.0 initialization. *)
val initialize
  :  Agent_client.Connection.t
  -> (Agent_protocol.Initialize.Response.t, Agent_protocol.Error.t) result

(** [ping connection ~payload] performs a typed liveness request. *)
val ping
  :  Agent_client.Connection.t
  -> payload:Jsonaf.t option
  -> (Agent_protocol.Ping.Response.t, Agent_protocol.Error.t) result

(** [next_notification connection ~clock ~timeout_seconds] waits for one
    notification without leaving a blocked collector fiber behind. *)
val next_notification
  :  Agent_client.Connection.t
  -> clock:_ Eio.Time.clock
  -> timeout_seconds:float
  -> [ `Closed | `Notification of Agent_protocol.Envelope.t | `Timeout ]

(** [connect_raw] opens an Eio-owned socket for framing and error tests. *)
val connect_raw
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> socket_path:string
  -> max_response_bytes:int
  -> raw

(** [send_line raw line] writes one NDJSON record. *)
val send_line : raw -> string -> unit

(** [read_envelope raw ~clock ~timeout_seconds] reads and decodes one server
    record, distinguishing timeout and EOF. *)
val read_envelope : raw -> clock:_ Eio.Time.clock -> timeout_seconds:float -> raw_read

(** [close_raw raw] closes the socket immediately. *)
val close_raw : raw -> unit
