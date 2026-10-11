open Core
module Accounting = Project_prepare_accounting
module Error = Project_prepare_error
module Limits = Project_prepare_limits
module Policy = Project_prepare_policy

(** Invocation-owned aggregate ledger and one monotonic deadline. No attempt,
    callback, clock, mutable accounting or native authority enters the result. *)
type t

(** Cancellation and the caller checkpoint propagate unchanged. Deadline expiry
    aborts this invocation through a private signal, never through a user-visible
    exception constructor. Even if an inner callback catches the signal, the
    poisoned invocation cannot publish a successful result. *)
val poll : t -> unit

(** Validate the entire nonnegative delta before replacing accounting. Overflow
    is distinct from configured capacity. No partial charge or refund on failure. *)
val charge : t -> Accounting.t -> (unit, Error.t) Result.t

val accounting : t -> Accounting.t

(** Preserve the first expected stage failure and revoke subsequent admission,
    including failures before any reservation/effect. Catching the returned Error
    cannot make this invocation eligible to complete or retry another stage. *)
val reject : t -> Error.t -> ('a, Error.t) Result.t

(** Snapshot after used and all in-flight reservations. This is not admission:
    reserve/charge must still succeed before starting work. *)
val remaining : t -> Accounting.t

module Reservation : sig
  (** One in-flight stage ceiling, affine to its attempt and completable once. *)
  type t
end

(** Reserve capacity before a backend whose receipt arrives only at bracket end.
    Other stages see remaining capacity after ALL outstanding reservations. *)
val reserve : t -> Accounting.t -> (Reservation.t, Error.t) Result.t

(** Replace one reservation with its successfully observed receipt, bounded by
    that reservation in every dimension. Release unused headroom only on success;
    failure poisons the whole attempt and never authorizes retry. Final publication
    requires every reservation to have completed. *)
val finish : t -> Reservation.t -> actual:Accounting.t -> (unit, Error.t) Result.t

(** Remaining per-stage limits never exceed literal policy or whole-attempt
    capacity. Zero/incompatible stage allowance rejects before invoking it. *)
val roots_limits : t -> (Chatml_local_roots.Limits.t, Error.t) Result.t

val local_limits : t -> (Chatml_local_capture.Limits.t, Error.t) Result.t
val git_object_limits : t -> (Chatml_git_catalog.Limits.t, Error.t) Result.t
val git_native_limits : t -> (Chatml_git_host.Limits.t, Error.t) Result.t
val composition_limits : t -> (Chatml_project_build.Limits.t, Error.t) Result.t

(** The supplied deadline starts before any publication expected-lock read and
    is created with this limit's wall_time and the same monotonic clock. It is
    shared with the publication owner, never reset at preparation entry. The final
    poll occurs before returning a candidate to that owner; atomic publication
    must never be wrapped with a postcommit checkpoint. *)
val run
  :  limits:Limits.t
  -> policy:Policy.t
  -> deadline:Project_prepare_deadline.t
  -> clock:_ Eio.Time.Mono.t
  -> checkpoint:(unit -> unit)
  -> f:(t -> ('a, Error.t) Result.t)
  -> ('a * Accounting.t, Error.t) Result.t
