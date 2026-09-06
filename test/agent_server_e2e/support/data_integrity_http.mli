open Core

(** Exercise real loopback HTTP against the production in-process daemon host.
    Restart closes and reopens the same durable daemon store. *)
val test_audit : Eio_unix.Stdenv.base -> Temporary_environment.t -> unit

val test_blobs : Eio_unix.Stdenv.base -> Temporary_environment.t -> unit
val test_export : Eio_unix.Stdenv.base -> Temporary_environment.t -> unit
