open Core

(** Authenticated HTTP probes for daemon health. *)

(** [get_health env ~port ~token] requests [/v1/health] over IPv4 loopback and
    decodes the typed health response. *)
val get_health
  :  Eio_unix.Stdenv.base
  -> port:int
  -> token:string
  -> (Agent_protocol.Health.Response.t, string) result
