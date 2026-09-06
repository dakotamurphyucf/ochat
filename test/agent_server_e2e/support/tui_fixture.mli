open Core

val require : bool -> string -> unit
val ok : ('a, Agent_protocol.Error.t) result -> 'a
val create : Eio_unix.Stdenv.base -> Temporary_environment.t -> string -> Config_fixture.t

val with_daemon
  :  Eio_unix.Stdenv.base
  -> Config_fixture.t
  -> (Eio.Switch.t -> Agent_client.Connection.t -> 'a)
  -> 'a

val tui_executable : Eio_unix.Stdenv.base -> string
val environment : Temporary_environment.t -> string array

(** Replace the embedded host's network capability with fail-closed operations,
    including DNS. Prevent a broken moderator fixture from using an ambient key. *)
val offline_environment : Eio_unix.Stdenv.base -> Eio_unix.Stdenv.base

val bearer_file : Config_fixture.t -> string
val await : Eio_unix.Stdenv.base -> (unit -> 'a option) -> 'a

(** Read only checksummed journal frames in the fixture's private temporary root.
    An incomplete last frame is ignored until a later read; never repair or lock
    the running local host's store. *)
val local_events : Temporary_environment.t -> Agent_protocol.Event.Durable.t list

val assert_user : Agent_protocol.History.entry list -> string -> unit
