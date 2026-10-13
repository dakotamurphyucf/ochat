(** Private Eio owner lock for one selected result or publisher cache. A callback
    and its inherited child fibers cannot reenter while the ancestor still holds
    the lock. A child resumed after that scope ends may acquire it normally;
    independent fibers wait and serialize normally. *)
open! Core

type t

val create : unit -> t

(** [on_reentry] returns the caller's typed error without acquiring the mutex.
    Unexpected callback exceptions and cancellation keep their original
    identity/backtrace through the protected owner boundary. *)
val with_owner : t -> on_reentry:(unit -> 'a) -> (unit -> 'a) -> 'a

(** Read-only queries have no result error channel. Recursive use is programmer
    misuse and raises [Invalid_argument] before waiting on the owner's lock. *)
val with_read : t -> (unit -> 'a) -> 'a
