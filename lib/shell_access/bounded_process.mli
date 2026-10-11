open Core

(** One bounded native process invocation. No Git commands, transport, cache, authentication
    or aggregate-storage policy belongs here. *)
module Error : sig
  type t =
    | Invalid_request
    | Unsupported_platform
    | Spawn_failed
    | Process_failed
    | Io_failed
    | Output_limit
    | Timed_out
  [@@deriving equal, sexp_of]
end

module Limits : sig
  type t

  (** Finite limits: request, wall, CPU and memory positive; output/file size
      nonnegative; open files at least 3. Request bytes charge executable, argv and env
      including terminators plus stdin, before copying or spawning. Output bytes
      charge stdout and stderr together, including bytes discarded from stderr.
      Kernel limits are per process/file, not aggregate process-tree resources
      or an aggregate storage quota. Core dumps are always disabled. Platforms
      may reject limits below the inherited process footprint at fork; the
      requested bound is never raised to make spawning succeed. *)
  val create
    :  maximum_request_bytes:int
    -> maximum_output_bytes:int
    -> maximum_wall_time:Time_ns.Span.t
    -> cpu_seconds_per_process:int
    -> memory_bytes_per_process:int
    -> file_size_bytes:int
    -> open_files_per_process:int
    -> t Or_error.t
end

module Outcome : sig
  type t

  (** A nonzero exit is an outcome: the typed Git adapter interprets it by
      operation. No heuristic interpretation of stderr is performed here. *)
  val status : t -> Eio.Process.exit_status

  val stdout : t -> string
  val stderr_bytes : t -> int
end

type t

(** Validates an absolute executable path, NUL-free arguments and environment,
    distinct nonempty environment names without '=', and the request byte cap.
    [arguments] excludes argv[0], which is the exact executable path. No PATH
    lookup, inherited environment, shell evaluation or executable admission is
    performed. The host admits the executable and selects allowed commands;
    creation alone grants no authority. Stdin is arbitrary exact bytes.
    Input values are immutable; the request can be reused after any invocation. *)
val create
  :  limits:Limits.t
  -> executable:string
  -> arguments:string list
  -> environment:(string * string) list
  -> stdin:string
  -> (t, Error.t) Result.t

(** Borrows a retained open native directory descriptor for the invocation;
    [fchdir] uses that object, never a native path reopened in the parent. Caller
    keeps the owning directory resource alive. No writable directory or storage
    quota is created here. Each invocation owns its child session/process group,
    a C-only live anchor, three joined pipe pumps and direct-anchor reaper. Only child descriptors
    0..2 survive exec; child-only rlimits do not modify the parent.

    A monotonic deadline covers spawn and operation. On executable exit, timeout,
    error or cancellation, signal the owned group before releasing/reaping its
    anchor identity, close owned pipe endpoints, join pumps, and reap the direct
    anchor. The executable status is distinct from anchor termination. Unexpected
    anchor death or protocol failure returns Process_failed after best-effort
    stop/reaping; it does not claim all helpers stopped when signal delivery fails. Cleanup is protected from cancellation and may exceed the operational
    deadline while the OS completes termination/reaping. No partial outcome is
    returned on failure. Stdout is exact on success; stderr is counted/discarded,
    never returned or logged. Read-ahead is bounded by one fixed pump chunk per
    stream; cap failure stops the invocation rather than draining unbounded data.

    Unexpected/checkpoint exceptions and cancellation are re-raised with original
    identity/backtrace after cleanup. Callbacks run outside native/child locks.
    This owns direct-child reaping and group signal delivery, not universal
    grandchild reaping or control of descendants which deliberately escape the
    group. The admitted helper family must not daemonize/escape it. Unsupported
    native group/wait/limit behavior fails closed; no weaker leader-only fallback. *)
val run
  :  t
  -> cwd:Eio_unix.Fd.t
  -> clock:_ Eio.Time.Mono.t
  -> checkpoint:(unit -> unit)
  -> (Outcome.t, Error.t) Result.t
