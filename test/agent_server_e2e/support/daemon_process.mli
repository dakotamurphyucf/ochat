open Core

(** Supervision for the real [ochat-agent-server] executable. *)

type t

(** [pid t] identifies only the fixture-owned daemon for process resource sampling. *)
val pid : t -> int

type readiness_error =
  | Timeout
  | Exited_before_ready of Process_manager.result
[@@deriving sexp]

(** [run_cli] executes the real server CLI with [arguments] and captures its
    complete bounded result. *)
val run_cli
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> fixture:Config_fixture.t
  -> arguments:string list
  -> Process_manager.result

(** [start] launches the real daemon using [config_path]. *)
val start
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> fixture:Config_fixture.t
  -> config_path:string
  -> t

(** [start_with_environment_overrides] replaces inherited child environment
    values with explicit test-owned values before launching the daemon. *)
val start_with_environment_overrides
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> fixture:Config_fixture.t
  -> environment_overrides:(string * string) list
  -> config_path:string
  -> t

(** [start_in_directory] launches the daemon with an explicit process cwd. *)
val start_in_directory
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> fixture:Config_fixture.t
  -> cwd:Eio.Fs.dir_ty Eio.Path.t
  -> config_path:string
  -> t

(** [start_in_directory_with_environment_overrides] launches the daemon with an
    explicit process cwd and replaces inherited environment values with the
    supplied test-owned values. Relative diagnostic files belong to [cwd]. *)
val start_in_directory_with_environment_overrides
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> fixture:Config_fixture.t
  -> cwd:Eio.Fs.dir_ty Eio.Path.t
  -> environment_overrides:(string * string) list
  -> config_path:string
  -> t

(** [wait_ready] waits until authenticated detailed health reports ready. *)
val wait_ready
  :  t
  -> env:Eio_unix.Stdenv.base
  -> timeout_seconds:float
  -> (Agent_protocol.Health.Response.t, readiness_error) result

(** [health] fetches health with [token]. *)
val health
  :  t
  -> env:Eio_unix.Stdenv.base
  -> token:string
  -> (Agent_protocol.Health.Response.t, string) result

(** [stop] requests graceful shutdown and returns the fully reaped result. *)
val stop
  :  t
  -> env:Eio_unix.Stdenv.base
  -> grace_seconds:float
  -> Process_manager.termination

(** [result] returns the result after exit without blocking. *)
val result : t -> Process_manager.result option

(** [signal t signal] sends an operating-system signal to the daemon. *)
val signal : t -> int -> unit

(** [stdout] returns the retained daemon stdout. *)
val stdout : t -> Process_manager.output

(** [stderr] returns the retained daemon stderr. *)
val stderr : t -> Process_manager.output
