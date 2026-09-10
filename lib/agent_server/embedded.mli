open! Core

(** Process-local host using the same daemon composition, session actor, and
    client protocol path as networked sessions. *)

type options =
  { prompt_file : string
  ; workspace : string
  ; tool_dir : string
  ; home : string
  ; data_root : string option
  ; start_immediately : bool
  ; permission_profile : Config.Permission_profile.t
  ; attachment_mode : Agent_protocol.Session.attachment_mode
  ; event_capacity : int
  }

type t

val default_permission_profile : Config.Permission_profile.t

(** [daemon_options] supplies the shared runtime's trusted host configuration,
    including provider adapters, policy and internal extension qualification.
    Defaults match [Daemon.default_options]. The embedded host always derives
    extension host metadata from [data_root], keeps process-bound liveness and
    starts no network listener. A durable data root preserves data; it does not
    keep jobs running after the embedding process exits. Startup initializes the
    Unix cryptographic RNG before allocating IDs or a transient data root. *)
val start
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> ?daemon_options:Daemon.options
  -> options
  -> (t, Agent_protocol.Error.t) result

val connection : t -> Agent_client.Connection.t
val session_id : t -> Agent_protocol.Id.Session.t
val attachment : t -> Agent_protocol.Session.Attachment.t
val dispatcher : t -> Dispatcher.t
val principal : t -> Agent_protocol.Principal.t
val connect : t -> Agent_client.Connection.t
val close_connection : t -> Connection_context.t -> unit
val close : t -> unit
