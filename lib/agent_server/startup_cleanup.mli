open! Core

(** One switch-scoped startup owner. No background retry or second host is
    created by this guard. An installed developer host joins before the sole
    scheduler; scheduler children join before registry, operator and Store
    release. *)
type t

module Failure : sig
  type t

  val primary : t -> Session_registry.Cleanup_failure.t
  val cleanup : t -> Session_registry.Cleanup_failure.t
end

exception Cleanup_failed of Failure.t

(** Registers one exceptional switch finalizer for an unfinished startup failure.
    If immediate cleanup fails, actual joined owners and Store capabilities remain
    in this scope and later release attempts the same ordered drain. A repeated
    failure raises with both diagnostics; it never reports cleanup complete or
    promises resources survive process/switch teardown. *)
val create : sw:Eio.Switch.t -> store:Agent_store.Session_store.t -> t

val adopt_registry_exn : t -> Session_registry.t -> unit
val adopt_operator_exn : t -> Provider_operator_port.t -> unit
val adopt_authoring_exn : t -> Authoring_installation.t -> unit

(** Adopt the bounded Developer event hub immediately after its construction,
    before Host creation can yield or fail. Failed startup joins event pumps
    before the existing Developer Host and scheduler owners. *)
val adopt_developer_events_exn
  :  t
  -> close_and_wait:(unit -> (unit, Agent_protocol.Error.t) result)
  -> unit

(** Retain this exact daemon-owned developer close operation before the
    scheduler starts. It joins command callbacks, Root workers and source
    owners while the scheduler remains serviceable. No caller receives an
    execution capability from this cleanup-only callback. *)
val adopt_developer_exn
  :  t
  -> close_and_wait:(unit -> (unit, Agent_protocol.Error.t) result)
  -> unit

(** A started scheduler is adopted before further yielding startup work.
    Failed startup joins its existing scheduler fibers before registry/Store
    cleanup. The developer host is joined first while dispatch is serviceable. *)
val adopt_job_scheduler_exn : t -> Job_scheduler.t -> unit

val adopt_start_scheduler_exn : t -> Start_scheduler.t -> unit

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
