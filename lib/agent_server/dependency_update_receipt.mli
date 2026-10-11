open! Core
module Selection = Agent_protocol.Prompt_dependency_selection

(** Narrow dependency-update receipt operation. Borrows the handler's existing
    store and SAME idempotency mutex; no second durable journal or source owner.
    The existing store's exclusive directory lifetime outlives this value. *)
type t

val create
  :  store:Agent_store.Idempotency_store.t
  -> mutex:Eio.Mutex.t
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> t

(** Compute the canonical typed request's digest/key before lock admission. Under
    a short protected mutex operation, reauthorize and replay/conflict/acknowledge
    a fresh Protected Pending record. Then release the mutex before execute or
    any borrowed checkpoint. Same-key Pending never executes. Protected receipts
    retain the existing one-day metadata expiry without being automatically pruned.

    [authorize] must be the original actor's nonyielding currentness/scope check,
    checked at admission before lookup/replay or recording Pending. Terminal
    completion records an already-admitted effect and does not rerun authorization
    or the borrowed checkpoint after acknowledgment. Capture exceptions INSIDE the mutex and
    rethrow with original backtrace after unlocking, so unexpected host guards or
    storage exceptions cannot poison that mutex. [now] is the existing host clock
    projection, not caller-supplied wire time.

    Complete only honest Rejected or request-matching committed success. A claimed
    success for another request is Interrupted/unknown, leaving Pending because
    effects may already be committed. Uncertainty, cancellation, checkpoint or
    unexpected exceptions leave Pending. Terminal completion failure returns an
    unknown protocol outcome; it never rolls back catalog/history or claims the
    receipt and publication are atomically coupled. No automatic retry or inference
    from displayed generation. Callback and checkpoint exceptions retain identity.
    [execute] is borrowed and not retained. *)
val dispatch
  :  t
  -> principal_id:Agent_protocol.Id.Principal.t
  -> request:Selection.Update_request.t
  -> authorize:(unit -> (unit, Agent_protocol.Error.t) Result.t)
  -> checkpoint:(unit -> unit)
  -> execute:
       (checkpoint:(unit -> unit)
        -> (Selection.Update_result.t, Dependency_administration.Failure.t) Result.t)
  -> (Selection.Update_result.t, Agent_protocol.Error.t) Result.t
