open Core

(** HTTP-only session controls and bounded waits for the background scenario. *)
type session =
  { summary : Agent_protocol.Session.t
  ; attachment_id : Agent_protocol.Id.Attachment.t
  }

val require : bool -> string -> unit
val protocol_ok : ('a, Agent_protocol.Error.t) result -> 'a
val result_ok : ('a, string) result -> 'a
val key : string -> Agent_protocol.Idempotency_key.t
val request : Http_driver.t -> Agent_protocol.Command.t -> Agent_protocol.Method_result.t
val reserve_port : Eio_unix.Stdenv.base -> int
val save : Config_fixture.t -> string -> string -> unit

val with_client
  :  sw:Eio.Switch.t
  -> Eio_unix.Stdenv.base
  -> Config_fixture.t
  -> (Http_driver.t -> 'a)
  -> 'a

val close_client : Http_driver.t -> unit
val create : Http_driver.t -> string -> session

val attach
  :  Http_driver.t
  -> Agent_protocol.Session.t
  -> string
  -> session * Agent_protocol.Method_result.Attach.replay

(** [attach_after client summary key cursor] reconnects with the last consumed
    durable sequence; [None] requests a current snapshot. *)
val attach_after
  :  Http_driver.t
  -> Agent_protocol.Session.t
  -> string
  -> int64 option
  -> session * Agent_protocol.Method_result.Attach.replay

val snapshot : Http_driver.t -> session -> Agent_protocol.Snapshot.t
val await : Eio_unix.Stdenv.base -> string -> (unit -> 'a option) -> 'a

val await_snapshot
  :  Eio_unix.Stdenv.base
  -> Http_driver.t
  -> session
  -> string
  -> (Agent_protocol.Snapshot.t -> bool)
  -> Agent_protocol.Snapshot.t

val start
  :  sw:Eio.Switch.t
  -> Eio_unix.Stdenv.base
  -> Config_fixture.t
  -> int
  -> Daemon_process.t

val stop : Eio_unix.Stdenv.base -> Daemon_process.t -> unit

val schedule
  :  Http_driver.t
  -> session
  -> string
  -> string
  -> int
  -> Agent_protocol.Schedule.t

val schedule_with_policy
  :  Http_driver.t
  -> session
  -> string
  -> string
  -> int
  -> Agent_protocol.Schedule.misfire
  -> Agent_protocol.Schedule.t

val events : Http_driver.t -> session -> string -> Agent_protocol.Event.Durable.t list

(** [events_since client session key cursor] requires retained replay after
    [cursor]. Use saved pre-disconnect traces when earlier segments were pruned. *)
val events_since
  :  Http_driver.t
  -> session
  -> string
  -> int64
  -> Agent_protocol.Event.Durable.t list

(** [checkpoint env fixture session] reads a checksummed snapshot and replays
    complete journal frames without repairing or writing live daemon storage. *)
val checkpoint
  :  Eio_unix.Stdenv.base
  -> Config_fixture.t
  -> session
  -> Agent_session.Session_state.t option

val moderator_state : Agent_session.Session_state.t -> string
