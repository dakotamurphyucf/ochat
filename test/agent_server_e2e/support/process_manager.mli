open Core

(** Eio child-process supervision for end-to-end scenarios. *)

type exit =
  | Exited of int
  | Signaled of int
[@@deriving equal, sexp]

type output =
  { contents : string
  ; bytes_seen : int
  ; truncated : bool
  }
[@@deriving sexp]

type result =
  { exit : exit
  ; stdout : output
  ; stderr : output
  }
[@@deriving sexp]

type termination =
  { forced : bool
  ; result : result
  }
[@@deriving sexp]

type readiness_error =
  | Timeout
  | Exited_before_ready of result
[@@deriving sexp]

type t

(** [spawn] starts [argv], drains stdout and stderr independently, and retains
    at most [max_output_bytes] from each stream while counting all bytes. The
    owning switch must remain live until [await] or [terminate] completes. *)
val spawn
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> ?cwd:Eio.Fs.dir_ty Eio.Path.t
  -> ?environment:string array
  -> max_output_bytes:int
  -> string list
  -> t

(** [pid t] is the operating-system process identifier. *)
val pid : t -> int

(** [signal t signal] sends [signal] if the process is still live. *)
val signal : t -> int -> unit

(** [await t] waits for process exit and reaping, then allows 500ms for pipe EOF.
    Readers still blocked are cancelled with a further 500ms cleanup bound;
    affected captures are marked [truncated]. Reader errors are propagated. *)
val await : t -> result

(** [poll_result t] never waits for EOF: return [None] until the child is reaped
    and both readers finish, even if descendants retain the output pipes. *)
val poll_result : t -> result option

(** [terminate t] sends SIGTERM, waits [grace_seconds], then sends SIGKILL if
    needed. The post-SIGKILL wait is bounded by two seconds; failure to finish
    raises [Eio.Time.Timeout]. The returned result is reaped, with incomplete
    drainage marked [truncated]. *)
val terminate : t -> clock:_ Eio.Time.clock -> grace_seconds:float -> termination

(** [stdout t] returns the currently retained stdout capture. *)
val stdout : t -> output

(** [stderr t] returns the currently retained stderr capture. *)
val stderr : t -> output

(** [wait_for_stdout t] polls retained stdout until [ready] accepts it, the
    process exits, or [timeout_seconds] elapses. *)
val wait_for_stdout
  :  t
  -> clock:_ Eio.Time.clock
  -> timeout_seconds:float
  -> ready:(string -> bool)
  -> (unit, readiness_error) Result.t
