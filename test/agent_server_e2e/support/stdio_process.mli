open Core

(** Interactive Eio child process for full-duplex NDJSON stdio tests. *)

type t

type stdout_item =
  | Envelope of Agent_protocol.Envelope.t
  | Invalid_line of
      { line : string
      ; error : Agent_protocol.Error.t
      }
  | Read_error of string
  | End_of_file
[@@deriving sexp]

(** [spawn] starts [argv] with writable stdin and continuously drains stdout
    and stderr. Lines are limited to 1MiB and the decoded queue to 1024 items.
    Overflow reports [Read_error] and marks capture truncated while continuing
    to drain the child; it never blocks the reader on an unconsumed queue. *)
val spawn
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> ?cwd:Eio.Fs.dir_ty Eio.Path.t
  -> ?environment:string array
  -> max_output_bytes:int
  -> string list
  -> t

(** [send_line t line] writes one NDJSON input record. *)
val send_line : t -> string -> unit

(** [pid t] identifies the supervised child for process-lifetime assertions. *)
val pid : t -> int

(** [send_string t value] writes raw process input without adding a newline. *)
val send_string : t -> string -> unit

(** [close_stdin t] sends EOF exactly once. *)
val close_stdin : t -> unit

(** [next_stdout] waits for the next decoded stdout record, read failure, or
    EOF, bounded by [timeout_seconds]. *)
val next_stdout
  :  t
  -> clock:_ Eio.Time.clock
  -> timeout_seconds:float
  -> [ `Item of stdout_item | `Timeout ]

(** [await t] waits for process exit, then bounds drainage as [Process_manager.await]. *)
val await : t -> Process_manager.result

(** [poll_result t] returns a completed result without blocking. *)
val poll_result : t -> Process_manager.result option

(** [terminate] terminates and reaps with the same deadlines and incomplete
    capture reporting as [Process_manager.terminate]. *)
val terminate
  :  t
  -> clock:_ Eio.Time.clock
  -> grace_seconds:float
  -> Process_manager.termination

(** Current bounded output snapshots. *)
val stdout : t -> Process_manager.output

val stderr : t -> Process_manager.output
