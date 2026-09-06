open! Core

(** Instance-scoped authentication for agent-server transports. *)

module Request_identity : sig
  type t =
    { client_address : Eio.Net.Sockaddr.stream
    ; headers : (string * string) list
    }
end

type bearer_validator =
  now:Agent_protocol.Timestamp.t
  -> token:string
  -> (Agent_protocol.Principal.t, Agent_protocol.Error.t) result

type t

(** [load_static_file] loads hashed static bearer-token records using Eio.
    Token records are immutable for the lifetime of the authenticator. *)
val load_static_file
  :  env:Eio_unix.Stdenv.base
  -> path:string
  -> (t, Agent_protocol.Error.t) result

(** [validate_static_file] validates a token file without retaining secrets. *)
val validate_static_file
  :  env:Eio_unix.Stdenv.base
  -> path:string
  -> (unit, string) result

(** [authenticate_bearer] hashes and compares [token] in constant time. *)
val authenticate_bearer
  :  t
  -> now:Agent_protocol.Timestamp.t
  -> token:string
  -> (Agent_protocol.Principal.t, Agent_protocol.Error.t) result

(** [authenticate_reverse_proxy] accepts asserted principal/scope headers only
    when the direct TCP peer exactly matches [trusted_addresses]. Headers from
    every other peer are ignored so direct clients cannot spoof proxy identity. *)
val authenticate_reverse_proxy
  :  trusted_addresses:string list
  -> principal_header:string
  -> scopes_header:string
  -> Request_identity.t
  -> (Agent_protocol.Principal.t option, Agent_protocol.Error.t) result
