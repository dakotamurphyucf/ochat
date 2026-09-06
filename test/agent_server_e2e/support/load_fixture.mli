(** Isolated stock-daemon load fixture and bounded protocol helpers. *)
open Core

(** [integer name default] reads a positive integer override, rejecting zero,
    negatives and malformed input. *)
val integer : string -> int -> int

(** [configure env temporary] installs a pure counter moderator and private load
    limits; it does not change production defaults or use a live provider. *)
val configure : Eio_unix.Stdenv.base -> Temporary_environment.t -> Config_fixture.t

(** [request client command] requires a successful protocol response. *)
val request : Http_driver.t -> Agent_protocol.Command.t -> Agent_protocol.Method_result.t

(** [now env] reads the Eio wall clock in seconds. *)
val now : Eio_unix.Stdenv.base -> float

(** [wait env description predicate] polls for at most 90 seconds. Predicate
    failures retain the supplied diagnostic context. *)
val wait : Eio_unix.Stdenv.base -> string -> (unit -> bool) -> unit

(** [attach client summary key] creates a read/write attachment using [key]. *)
val attach
  :  Http_driver.t
  -> Agent_protocol.Session.t
  -> string
  -> Background_fixture.session

(** [stop client session key] requests a graceful stop with idempotency [key]. *)
val stop : Http_driver.t -> Background_fixture.session -> string -> unit

(** [detach client session key] removes the fixture attachment using [key]. *)
val detach : Http_driver.t -> Background_fixture.session -> string -> unit

(** [health client] requires that the fixture daemon reports ready. *)
val health : Http_driver.t -> Agent_protocol.Health.Response.t

(** [loaded client] reads the loaded-actor count from daemon health details. *)
val loaded : Http_driver.t -> int

(** [await_schedules env client session count] requires exactly [count] schedules,
    each delivered once, and no moderator failure. *)
val await_schedules
  :  Eio_unix.Stdenv.base
  -> Http_driver.t
  -> Background_fixture.session
  -> int
  -> unit

(** [with_daemon env f] owns a private stock daemon, client and temporary roots
    for [f], then shuts down and cleans them on success or failure. *)
val with_daemon
  :  Eio_unix.Stdenv.base
  -> (Eio.Switch.t -> Config_fixture.t -> Daemon_process.t -> Http_driver.t -> 'a)
  -> 'a
