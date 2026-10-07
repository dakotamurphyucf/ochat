open Core

(** In-process daemon host for E2E-only dependency injection. It binds the
    production HTTP transport and preserves the production daemon lifecycle. *)

(** Explicit selected synthetic backend for E2E cases that construct runtimes but
    never request model output. Any actual backend dispatch fails the fixture.
    The supplied authentication, authority and lifecycle options are preserved. *)
val with_offline_inference : Agent_server.Daemon.options -> Agent_server.Daemon.options

val with_
  :  Eio_unix.Stdenv.base
  -> Config_fixture.t
  -> options:Agent_server.Daemon.options
  -> (Eio.Switch.t -> Agent_server.Daemon.t -> unit)
  -> unit
