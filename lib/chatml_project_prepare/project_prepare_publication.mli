open Core
module Captured = Chatml_source
module Result = Project_prepare_result
module Error = Project_prepare_error
module Deadline = Project_prepare_deadline

module Expected : sig
  type t =
    | Absent
    | Exact of Captured.t
end

type state =
  | Available
  | Busy
  | Uncertain
  | Closed
[@@deriving equal, sexp_of]

(** Borrowed opened directory capability and caller-owned exclusive coordination.
    The caller retains both until this switch's cleanup has revoked admission,
    cancelled and joined active work. No lock is held across preparation callbacks.
    There is no public manual close operation or automatic reconciliation/retry.
    Operations run on the switch's Eio domain; concurrent/reentrant fibers are
    admitted through the same owner. *)
type t

(** Admit a bounded portable child basename, bounded expected source bytes and
    an opened-directory capability with empty relative path. Retains immutable
    expected bytes and registers switch-only cleanup; performs no filesystem read.
    A later invocation checks expected bytes before its preparation callback.
    Native directory authority and exclusive coordination are caller obligations. *)
val open_owned
  :  [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
  -> sw:Eio.Switch.t
  -> basename:string
  -> expected:Expected.t
  -> max_lock_bytes:int
  -> (t, Agent_store.Store_error.t) Core.Result.t

val state : t -> state

(** Private fixed selected-lock publication path. A concurrent/reentrant call
    fails before invoking prepare. The shared deadline must have started before
    this invocation's first expected read and must also govern preparation.

    A joined owner-switch worker checks expected bytes with bounded retained-
    directory Durable_file.load_bounded_in, invokes prepare exactly once, checks
    the completed lock byte ceiling, and rechecks expected bytes before committing.
    Length/byte comparisons poll in bounded chunks. Unexpected exceptions and
    cancellation retain identity/backtrace; a prewriter failure leaves Available.

    The immutable completed Result and commit contents are allocated before the
    final cancellation/checkpoint/deadline poll. Then writer entry makes the owner
    Uncertain and Durable_file.replace_in Flush_file_and_directory runs protected.
    There is no postcommit progress/deadline poll. Delivery checks pending caller
    cancellation without a callback before acknowledging the result. Only delivery
    of its successful result installs
    the new expected capture and restores Available. Any writer error/exception
    leaves Uncertain; no rollback, retry or cached old selection is exposed. An
    open failure after creation but before returning ownership may leave an
    unacknowledged temporary; this owner does not delete unknown artifacts.

    Caller cancellation cancels and joins the worker before propagating. If the
    writer was entered, protected writing may finish after cancellation, but the
    undelivered result leaves Uncertain even when replacement succeeded. Switch
    cancellation revokes admission immediately; release cancels and joins, then
    leaves Closed before
    the caller may release its directory/coordination ownership. *)
val run
  :  t
  -> deadline:Deadline.t
  -> clock:_ Eio.Time.Mono.t
  -> checkpoint:(unit -> unit)
  -> prepare:(unit -> (Result.t, Error.t) Core.Result.t)
  -> (Result.t, Error.t) Core.Result.t
