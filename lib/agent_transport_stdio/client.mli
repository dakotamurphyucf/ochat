open! Core

(** First-party Eio-owned child process client for an NDJSON stdio server.
    Child stderr is drained separately into a finite tail and never parsed as
    protocol output. Command, Developer and REPL requests share the same typed
    Agent_client.Connection and installed-capability checks as socket/HTTP;
    this module creates no retry ledger or authority. *)

module Limits : sig
  type t

  (** All capacities are strictly positive. [shutdown_grace_seconds] is finite
      and positive. Cleanup waits one grace after closing stdin, one after
      TERM, and one after KILL. The three waits have a finite total timeout
      budget; the owning Eio switch remains responsible for final process
      release if an unkillable child exceeds it. [max_stderr_bytes] bounds
      retained diagnostics, not total drained bytes. *)
  val create
    :  max_line_length:int
    -> notification_capacity:int
    -> max_stderr_bytes:int
    -> shutdown_grace_seconds:float
    -> (t, Agent_protocol.Error.t) result
end

type t

(** Run one child inside this lexical Eio owner. [argv] names the trusted
    executable and its arguments and must be nonempty. Pipes, one stdout
    protocol reader, a concurrent bounded stderr drain, and the child process
    are all owned by the nested switch. The callback may make arbitrarily many
    sequential or concurrent typed calls through [connection]. Retaining the
    returned Connection does not extend its lifetime: it is closed when this
    bracket ends. On callback return, error, exception or cancellation,
    cleanup closes the channel, terminates a child that ignores EOF, and reaps
    it when reappable within the stated budget. It then closes/cancels the
    parent readers even if a descendant inherited a pipe and kept it open.
    Original cancellation/error/backtrace takes priority over cleanup
    diagnostics. *)
val with_spawned
  :  env:Eio_unix.Stdenv.base
  -> argv:string list
  -> limits:Limits.t
  -> f:(t -> ('a, Agent_protocol.Error.t) result)
  -> ('a, Agent_protocol.Error.t) result

(** Access is restricted to the owning Eio domain. *)
val connection : t -> Agent_client.Connection.t

(** Idempotent early retirement within [with_spawned]. It cancels pending
    requests without replay, closes stdin/stdout, waits for natural exit within
    the configured grace, then signals/kills and reaps if needed. Repeated calls
    return the same exit status. Concurrent callers share one completion, with
    no duplicate signal or reap. A caller-local cancellation stops only that
    wait; the switch-owned cleanup continues. If the owning switch is cancelled,
    its process release hook reaps the child and settles waiting callers before
    [with_spawned] exits. No child or reader is left alive after success. A
    caller outside the owning Eio domain is rejected. *)
val close_and_wait : t -> (Eio.Process.exit_status, Agent_protocol.Error.t) result

(** Finite diagnostic tail; may be read while the child runs on its owning Eio
    domain. No stderr byte enters a protocol Envelope. *)
val stderr_tail : t -> string
