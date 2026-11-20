(** One host-owned allowance shared across source reads in a single call. *)
open! Core

module Store = Agent_store.Session_store

type t

type open_error =
  | Denied
  | Invalid_limits

(** Bounded in-memory recheck of admitted structural metadata. This scope has
    no Store, archive, or selected-text capability and closes on return. *)
val with_metadata_scope : checkpoint:(unit -> unit) -> f:(t -> 'a) -> 'a

(** Only raised by bounded charge operations in the active source scope. *)
exception Exhausted

(** A low-level operation has no live current borrow. *)
exception Inactive

(** The hard ceilings are eight archive opens, 64 MiB of cumulative source
    work and 4 MiB of examined text. A supplied limit can lower its ceiling.
    The allowance cannot be used after the callback returns or raises. *)
val with_scope
  :  ?max_source_opens:int
  -> ?max_source_bytes:int
  -> ?max_text_bytes:int
  -> ?checkpoint:(unit -> unit)
  -> Store.t
  -> handle:Store.Handle.t
  -> f:(t -> 'a)
  -> ('a, open_error) result

(** Require the exact Store and Handle for this borrow. A call with a foreign
    domain or closed scope, or a concurrent or reentrant borrow, does no work. *)
val borrow : t -> Store.t -> handle:Store.Handle.t -> f:(unit -> 'a) -> 'a option

(** Require a live borrow in the owner domain before acting. Cancellation
    and exceptions from the host checkpoint propagate. Negative charges raise
    [Invalid_argument] without changing the allowance. *)
val checkpoint : t -> unit

val charge_source : t -> int -> unit
val charge_selected : t -> int -> unit

val measure_json
  :  t
  -> limits:Document_schema.Limits.t
  -> Jsonaf.t
  -> (int, Document_schema.Error.t) result

(** The host-bound reader is borrowed only for this archive attempt. A local
    frame cap never resets the aggregate allowance. [filename] is a single
    archive basename under the owning Handle. *)
val read_archive
  :  t
  -> filename:string
  -> max_bytes:int
  -> (string, Agent_store.Store_error.t) result
