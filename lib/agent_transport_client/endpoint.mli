open! Core

(** Client-selected daemon transport endpoint. Bearer credentials remain
    private and are never included in endpoint descriptions or errors. *)

type t

type kind =
  | Unix_socket
  | Http
[@@deriving compare, equal, sexp_of]

(** [create ~home ~bearer_token value] validates a Unix-socket or HTTP daemon
    endpoint. Unix paths must be absolute after optional [~/] expansion.
    Bearer credentials are accepted only for HTTP endpoints. *)
val create
  :  home:string option
  -> bearer_token:string option
  -> string
  -> (t, Agent_protocol.Error.t) result

(** [load_bearer_token ~env ~path] loads one nonempty HTTP bearer token through
    Eio. Relative paths resolve against the process working directory. *)
val load_bearer_token
  :  env:Eio_unix.Stdenv.base
  -> path:string
  -> (string, Agent_protocol.Error.t) result

val kind : t -> kind

(** [description t] returns a credential-free endpoint description suitable
    for diagnostics. *)
val description : t -> string

(** [connect t ~sw ~env ~notification_capacity] opens the selected transport.
    Connection failures are returned as retryable protocol errors. *)
val connect
  :  t
  -> sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> notification_capacity:int
  -> (Agent_client.Connection.t, Agent_protocol.Error.t) result
