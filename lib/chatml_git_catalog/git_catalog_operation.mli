open Core

(** Private joined ownership of one semantic operation at a time. No state lock
    across callbacks, no transport or independent timeout/budget. *)
type t

type error =
  | Busy
  | Closed

val create : Eio.Switch.t -> t
val is_active : t -> bool

(** Caller exceptions retain identity/backtrace after stop/join. Callback executes
    in an owner-switch worker; concurrent/reentrant calls do not execute f. *)
val run : t -> f:(unit -> 'a) -> ('a, error) Result.t

(** Protected stop/join before clearing owner state; rejects subsequent run. *)
val close : t -> unit
