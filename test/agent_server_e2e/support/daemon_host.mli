open Core

(** In-process daemon host for E2E-only dependency injection. It binds the
    production HTTP transport and preserves the production daemon lifecycle. *)

val with_
  :  Eio_unix.Stdenv.base
  -> Config_fixture.t
  -> options:Agent_server.Daemon.options
  -> (Eio.Switch.t -> Agent_server.Daemon.t -> unit)
  -> unit
