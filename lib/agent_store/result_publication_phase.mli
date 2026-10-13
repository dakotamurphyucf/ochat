(** The shared, private preparation/attempt/acknowledgement state machine used
    by conversation and developer result publishers. This serializes one
    already selected result; it owns no identity, filesystem path, Store
    transition, output codec or disclosure policy. Reentry from a callback or
    child fiber inheriting its owner scope fails with a typed error before
    waiting on the same lock; independent fibers serialize normally. *)
open! Core

type 'stage t

val create : unit -> 'stage t

(** Recreate only after the owning publisher has read and verified its exact
    persisted private intent and staged bytes. This is process-local custody,
    not a new publication or permission to discard. *)
val restored_attempted : unit -> 'stage t

(** Invoke the owner's idempotent exact intent-save and stage callback under
    the preparation mutex. An ambiguous failure leaves prior custody intact.
    A Retained/Discarded preparation cannot be restaged. *)
val ensure_prepared
  :  'stage t
  -> stage:(unit -> ('stage, Store_error.t) result)
  -> (unit, Store_error.t) result

(** These operations preserve the existing conversation phase policy: an
    Allocated token cannot be discarded, including after a partial or
    ambiguous first stage, and a Prepared token discards only after its exact
    stage and marker are removed. Eio cancellation and unexpected exceptions
    preserve their original identity/backtrace through the mutex boundary. *)

(** Ensure the exact intent and bytes, then mark the token Attempted before
    returning to the developer worker's sole Store terminal CAS. Once entered,
    discard cannot remove the private marker on any later error/cancellation. *)
val ensure_attempted
  :  'stage t
  -> stage:(unit -> ('stage, Store_error.t) result)
  -> (unit, Store_error.t) result

(** Existing conversation publication runs its persist callback under the same
    non-discardable phase transition. Typed callback error leaves Attempted;
    successful persistence enters Retained before best-effort marker cleanup.
    Unexpected exception/cancellation retains the attempted phase and original
    exception/backtrace. [persist] must not reenter this preparation. *)
val commit
  :  'stage t
  -> persist:(unit -> ('a, Agent_protocol.Error.t) result)
  -> cleanup:(unit -> (unit, Store_error.t) result)
  -> ('a, Agent_protocol.Error.t) result

(** Developer publication calls this only after its external owner checked an
    actual Store-issued Installed receipt for the exact selected terminal.
    Mark Retained first. A typed marker-removal error is best-effort; an
    unexpected exception propagates after the phase is settled. The worker
    separately releases its installed Claim/capacity before propagating. *)
val acknowledge_installed
  :  'stage t
  -> cleanup:(unit -> (unit, Store_error.t) result)
  -> (unit, Store_error.t) result

(** Delete only a proved never-attempted preparation. A failed stage or marker
    removal leaves the token Prepared for exact retry; Attempted and Retained
    cannot be discarded through this operation. *)
val discard
  :  'stage t
  -> discard:('stage -> (unit, Store_error.t) result)
  -> (unit, Store_error.t) result
