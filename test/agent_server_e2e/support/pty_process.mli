open Core

(** Eio-owned real pseudo-terminal. Only grant/unlock/name and termios inspection
    borrow native descriptors, through the Eio system-thread boundary. Dimensions
    are installed and verified using an Eio-spawned stty on the slave. Opens the
    slave through Eio POSIX with O_NOCTTY so a headless session-leader harness
    never acquires it as its controlling terminal or receives teardown SIGHUP. *)
type t

val spawn
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> cwd:Eio.Fs.dir_ty Eio.Path.t
  -> environment:string array
  -> columns:int
  -> rows:int
  -> string list
  -> t

(** [output t] is a bounded raw terminal transcript, never the user's terminal. *)
val output : t -> string

val send : t -> string -> unit
val await_text : t -> clock:_ Eio.Time.clock -> string -> unit

(** [await_exit t] waits at most five seconds. A timeout is a test failure,
    not a successful graceful exit; switch cleanup still owns process reaping. *)
val await_exit : t -> clock:_ Eio.Time.clock -> Eio.Process.exit_status

(** Compare all exposed OCaml termios fields with the pre-launch state and require the
    emitted alternate-screen, cursor, and bracketed-paste restoration sequences. *)
val assert_restored : t -> clock:_ Eio.Time.clock -> unit
