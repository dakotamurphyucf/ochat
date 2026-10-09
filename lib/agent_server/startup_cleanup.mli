open! Core

(** One switch-scoped startup owner. No background retry, global owner, or host is
    created by this guard. Registry cleanup precedes operator and Store release. *)
type t

module Failure : sig
  type t

  val primary : t -> Session_registry.Cleanup_failure.t
  val cleanup : t -> Session_registry.Cleanup_failure.t
end

exception Cleanup_failed of Failure.t

(** Registers one exceptional switch finalizer for an unfinished startup failure.
    If immediate cleanup fails, actual registry/operator/Store capabilities remain
    in this scope and later release attempts the same ordered drain once. A repeated
    failure raises with both diagnostics; it never reports cleanup complete or
    promises resources survive process/switch teardown. *)
val create : sw:Eio.Switch.t -> store:Agent_store.Session_store.t -> t

val adopt_registry_exn : t -> Session_registry.t -> unit
val adopt_operator_exn : t -> Provider_operator_port.t -> unit

(** Preserve original Error or exception/backtrace if ordered cleanup also fails.
    On success the actual host takes ownership; this startup guard stops releasing
    its capabilities. Caller retains the established host shutdown contract. *)
val protect
  :  t
  -> (unit -> ('a, Agent_protocol.Error.t) Result.t)
  -> ('a, Agent_protocol.Error.t) Result.t

(** Existing operator scope finalizer may release only after startup drain or
    successful host ownership transfer, never ahead of a pending registry join.
    Actual successful release is tracked to avoid duplicate cleanup. *)
val release_operator_on_scope_exit : t -> unit
