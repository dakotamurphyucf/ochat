open Core

(** Share isolated process and exact-state oracles between crash and recovery tests. *)
val fail : string -> 'a

val require : bool -> string -> unit
val protocol_ok : ('a, Agent_protocol.Error.t) result -> 'a
val store_ok : ('a, Agent_store.Store_error.t) result -> 'a
val key : string -> Agent_protocol.Idempotency_key.t
val path : Eio_unix.Stdenv.base -> string -> Eio.Fs.dir_ty Eio.Path.t
val read : Eio_unix.Stdenv.base -> string -> string
val write : Eio_unix.Stdenv.base -> string -> string -> unit

val fixture
  :  Eio_unix.Stdenv.base
  -> Support.Temporary_environment.t
  -> string
  -> Support.Config_fixture.t

val with_client
  :  sw:Eio.Switch.t
  -> Eio_unix.Stdenv.base
  -> Support.Config_fixture.t
  -> (Support.Http_driver.t -> 'a)
  -> 'a

val request
  :  Support.Http_driver.t
  -> Agent_protocol.Command.t
  -> Agent_protocol.Method_result.t

val create_session : Support.Http_driver.t -> Agent_protocol.Method_result.Create.t

val get
  :  Support.Http_driver.t
  -> Agent_protocol.Id.Session.t
  -> Agent_protocol.Snapshot.t

(** Observe recovery through actor-owned public snapshots while the daemon is
    alive. Read raw checkpoints only after killing/joining the child: snapshots
    and journal segments can be replaced or retired during live recovery. *)
val await_notifications
  :  Eio_unix.Stdenv.base
  -> Support.Process_manager.t
  -> Support.Http_driver.t
  -> Support.Background_fixture.session
  -> provider_prefix:string
  -> calls:int
  -> count:int
  -> Agent_protocol.Snapshot.t

val require_equal : string -> ('a -> Sexp.t) -> 'a -> 'a -> unit

(** [assert_snapshot expected actual] compares every projected field, including
    history IDs, payloads, order, effective history, permissions, jobs and schedules.
    Only recovery's update timestamp and advancing revision/event counters differ. *)
val assert_snapshot : Agent_protocol.Snapshot.t -> Agent_protocol.Snapshot.t -> unit

val wait_ready : Eio_unix.Stdenv.base -> Support.Daemon_process.t -> unit
val stop : Eio_unix.Stdenv.base -> Support.Daemon_process.t -> unit

val start
  :  sw:Eio.Switch.t
  -> Eio_unix.Stdenv.base
  -> Support.Config_fixture.t
  -> Support.Daemon_process.t

val with_daemon
  :  Eio_unix.Stdenv.base
  -> Support.Config_fixture.t
  -> (Support.Http_driver.t -> 'a)
  -> 'a

val seed : Eio_unix.Stdenv.base -> Support.Config_fixture.t -> Agent_protocol.Snapshot.t
val session_directory : Support.Config_fixture.t -> Agent_protocol.Id.Session.t -> string

val current_journal
  :  Eio_unix.Stdenv.base
  -> Support.Config_fixture.t
  -> Agent_protocol.Id.Session.t
  -> string

val snapshot_directory : Support.Config_fixture.t -> Agent_protocol.Id.Session.t -> string

(** [child ...] reexecutes the opt-in scenario's private [child.*] case. No
    shared dispatcher changes or production executable flags are required. *)
val child
  :  sw:Eio.Switch.t
  -> Eio_unix.Stdenv.base
  -> Support.Temporary_environment.t
  -> case:string
  -> arguments:string list
  -> Support.Process_manager.t

val await_marker : Eio_unix.Stdenv.base -> Support.Process_manager.t -> string -> unit
val kill : Eio_unix.Stdenv.base -> Support.Process_manager.t -> unit
val terminate : Eio_unix.Stdenv.base -> Support.Process_manager.t -> unit
