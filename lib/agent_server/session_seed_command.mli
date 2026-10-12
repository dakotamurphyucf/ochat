open! Core

(** Private original session.seed receipt dispatch. Borrows the Handler's actual
    store/mutex/Registry and Factory; introduces no journal or mutable transaction
    owner. The host outlives this value. *)
type t

val create
  :  factory:Session_factory.t
  -> registry:Session_registry.t
  -> store:Agent_store.Idempotency_store.t
  -> mutex:Eio.Mutex.t
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> t

(** Reauthorize the original current principal before replay or fresh Protected
    Pending acknowledgment. Authorization/checkpoint callbacks run OUTSIDE the
    receipt mutex, including recheck after admission. Preserve exceptions/backtrace
    outside the mutex. Complete original canonical request binds full transcript,
    selector, Spec and key. No initializer/runtime runs during seed creation.

    The short mutex serializes lookup/claim or durable receipt completion only;
    the receipt store necessarily performs IO there. Release it before Factory
    creation, workspace/source/journal recovery or borrowed callbacks. Known
    Rejected becomes Failure; Uncertain/exception retains Pending and never runs
    creation again. Created remains unpublished until exact durable Success with
    accepted sequence one; verify the returned complete record because an existing
    terminal is returned unchanged. Known Rejected completion likewise verifies the exact
    intended Failure, protected retention and absent accepted sequence. Then Registry.add
    transfers its stopped unloaded entry;
    completion/registration failure closes the unpublished entry, preserving final
    durable evidence. Cleanup uses existing retained-owner rollback diagnostics rather than
    suppressing close failures. Cancellation after durable Success may be observed while
    the command is committed. Replay returns the original ID, no new attachment.

    Existing Pending reconciliation is a separately bounded Factory operation
    before dispatch/receipt lookup (also actual startup), never creation retry. *)
val dispatch
  :  t
  -> principal:Agent_protocol.Principal.t
  -> request:Agent_protocol.Session_seed.Request.t
  -> authorize:(unit -> (unit, Agent_protocol.Error.t) Result.t)
  -> checkpoint:(unit -> unit)
  -> (Agent_protocol.Session_seed.Result.t, Agent_protocol.Error.t) Result.t
