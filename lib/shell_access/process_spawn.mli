open Core

(** Child-only setup integrated into spawning. No companion executable or
    self-execution is involved. Only standard streams and the private channel's
    descriptors (0 through 4) may be inherited through this manager. *)
val manager
  :  cpu_seconds:int option
  -> memory_bytes:int option
  -> file_size_bytes:int option
  -> open_files:int option
  -> close_extra_fds:bool
  -> Eio_unix.Process.mgr_ty Eio.Resource.t

(** Internal infrastructure for [Bounded_process]. Existing manager behavior is
    unchanged. Group helpers must remain in the child's session/group. *)
module Owned_group : sig
  module Limits : sig
    type t

    val create
      :  cpu_seconds:int
      -> memory_bytes:int
      -> file_size_bytes:int
      -> open_files:int
      -> t Or_error.t
  end

  type error =
    | Unsupported_platform
    | Spawn_failed
    | Process_failed
  [@@deriving equal, sexp_of]

  type t

  (** Single-consumer, one-shot wait. The owner calls this exactly once; concurrent
      or repeated calls would consume the same private protocol and are invalid.
      [stop] remains idempotent independently of whether awaiting has begun. *)
  val await : t -> (Eio.Process.exit_status, error) Result.t

  (** Idempotent stop: before Go, no executable exists and only the anchor is
      killed; after Go, the live group is signalled once before anchor reaping.
      [await] or switch release subsequently joins direct-child reaping. *)
  val stop : t -> unit

  (** Uses borrowed retained descriptors and invocation-local immutable arrays.
      Only 0..2 survive exec. A C-only direct-child anchor retains group identity.
      Await returns the executable status, then signals its live group before
      reaping the direct anchor; release does likewise under
      cancellation protection. No pipe pumps or grandchild reap are supplied.
      Unexpected anchor loss or invalid protocol is Process_failed, never a
      successful executable status. Cleanup attempts group stop and joins direct
      reaping, without claiming denied signals stopped all helpers. Expected
      failures contain no argv/env/native error-pipe payload. *)
  val spawn
    :  sw:Eio.Switch.t
    -> limits:Limits.t
    -> cwd:Eio_unix.Fd.t
    -> executable:string
    -> argv:string array
    -> env:string array
    -> stdin:Eio_unix.Fd.t
    -> stdout:Eio_unix.Fd.t
    -> stderr:Eio_unix.Fd.t
    -> (t, error) Result.t
end
