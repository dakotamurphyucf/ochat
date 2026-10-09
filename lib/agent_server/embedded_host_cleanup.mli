open! Core

(** Concrete switch-scoped embedded Daemon and optional transient namespace owner.
    Private serialized close coordinator; admission and successful release differ.
    All acquisition, adoption and close calls run on the host owning Eio domain;
    fibers on that domain may share the serialized close coordinator. This is not
    a cross-domain owner. No background retry/global owner. Scope exit retries the same actual ownership
    once if explicit closure/construction failed; exceptional finalizer reports both
    original and latest cleanup diagnostics, no survival promise past scope teardown. *)
type t

module Connection_owner : sig
  (** Actual client admission plus its retained context-detach capability. *)
  type t

  (** Trusted construction transfers both exact capabilities. [close_actual] must
      serialize concurrent calls, mark success only after detach completes, and
      retry unfinished detach even after client close becomes a no-op. The same
      closure must be used by the client's transport close callback. *)
  val create : connection:Agent_client.Connection.t -> close_actual:(unit -> unit) -> t

  val connection : t -> Agent_client.Connection.t
end

module Failure : sig
  type t

  val primary : t -> Session_registry.Cleanup_failure.t
  val cleanup : t -> Session_registry.Cleanup_failure.t
end

exception Cleanup_failed of Failure.t

(** Transfer actual successful Daemon immediately before initialization can fail.
    Caller must never independently delete transient root after this transfer. *)
val create
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> daemon:Daemon.t
  -> temporary_root:string option
  -> t

(** Adopt this actual owned connection before initialization/creation can yield.
    Closes admission before detaching it; failed connection cleanup is retained before
    daemon/store release. A second/late adoption is an invariant violation. *)
val adopt_connection_exn : t -> Connection_owner.t -> unit

val is_closing : t -> bool

(** Close admission before yielding; join Daemon successfully before removing
    namespace. Successful stages remain completed across retries; errors raise with
    original diagnostic/backtrace and retain this concrete owner until scope exit. *)
val close : t -> unit

(** Construction callback success transfers this same cleanup capability into Host.
    On failure immediate close runs protected, preserves original rejection or
    exception/backtrace, retaining latest secondary failure for scope exit retry.
    Failed immediate cleanup fatally fails the supplied owning Switch with both
    diagnostics to cancel/drain its fibers and permit exceptional finalization;
    callers must give this host its intended ownership scope. It cannot return a
    usable host or report ordinary cleanup success with live unowned resources. *)
val protect
  :  t
  -> (unit -> ('a, Agent_protocol.Error.t) Result.t)
  -> ('a, Agent_protocol.Error.t) Result.t

(** Reject before constructing on closed admission. The trusted constructor must
    not yield; call on the host owning Eio domain, where admission check,
    construction and ownership transfer cannot interleave with closure. Ownership
    transfers immediately and exactly once on return.
    Failed close retains the actual connection for retry before daemon release. *)
val adopt_additional_connection
  :  t
  -> create:(unit -> Connection_owner.t)
  -> (Agent_client.Connection.t, Agent_protocol.Error.t) result
