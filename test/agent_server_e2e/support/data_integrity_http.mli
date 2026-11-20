open Core

(** Exercise real loopback HTTP against the production in-process daemon host.
    Restart closes and reopens the same durable daemon store. *)
val test_audit : Eio_unix.Stdenv.base -> Temporary_environment.t -> unit

val test_blobs : Eio_unix.Stdenv.base -> Temporary_environment.t -> unit
val test_recovery_assets : Eio_unix.Stdenv.base -> Temporary_environment.t -> unit
val test_export : Eio_unix.Stdenv.base -> Temporary_environment.t -> unit

val fixture
  :  Eio_unix.Stdenv.base
  -> Temporary_environment.t
  -> string
  -> Config_fixture.t

val connect
  :  ?features:string list
  -> sw:Eio.Switch.t
  -> Eio_unix.Stdenv.base
  -> Config_fixture.t
  -> string
  -> Http_driver.t * Agent_protocol.Id.Principal.t

val connection : Http_driver.t -> Agent_client.Connection.t

val create
  :  ?mode:Agent_protocol.Session.attachment_mode
  -> Http_driver.t
  -> string
  -> Agent_protocol.Session.t * Agent_protocol.Session.Attachment.t
