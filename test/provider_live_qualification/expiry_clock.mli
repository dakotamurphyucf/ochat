open! Core

type t

type state =
  | Live
  | Armed
  | Restored_by_oauth
  | Restored_cleanup
[@@deriving equal, sexp_of]

type error =
  | Already_armed
  | Invalid_expiry
  | Expiry_not_future
[@@deriving equal, sexp_of]

val create : _ Eio.Time.clock -> t
val host_clock : t -> float Eio.Time.clock_ty Eio.Resource.t
val oauth_validation_clock : t -> float Eio.Time.clock_ty Eio.Resource.t

(** Test-only credential Host wall clock, including its operator clock. Arm only
    after warming the real channel, with no operator/login/status calls while
    armed. Ordinary inference admission sees known expiry + 1ms. Real OAuth
    validation returns real time and restores this override before registry
    replacement validation. Driver/network/monotonic clocks stay real; all
    sleep_until calls forward unchanged. No token/grant mutation. *)
val advance_to_expiry : t -> expires_at_ms:int64 -> (unit, error) Result.t

(** Invoke in finally on every armed operation, including cancellation/error.
    Idempotent; cleanup never counts as an OAuth validation reset. *)
val restore : t -> unit

val state : t -> state
val oauth_reset_count : t -> int

(** Deterministic tests of only the clock mechanism; no credentials or network. *)
val self_check : unit -> unit
