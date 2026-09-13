(** [run env ~config_path ~marker] starts a real daemon and HTTP listener with
    a deterministic model callback requesting [append_to_file]. Pause in the
    actual tool write after the complete marker reaches the filesystem but
    before returning to the tool worker, so no tool-completion acknowledgement
    can occur. The parent must SIGKILL/reap this test-only host. *)
val run : Eio_unix.Stdenv.base -> config_path:string -> marker:string -> unit

(** Shared private host setup for deterministic crash scenarios. *)
val load_config : Eio_unix.Stdenv.base -> string -> Agent_server.Config.t

val listener
  :  sw:Eio.Switch.t
  -> Eio_unix.Stdenv.base
  -> Agent_server.Daemon.t
  -> Agent_server.Config.t
  -> Agent_server.Daemon.options
  -> unit
