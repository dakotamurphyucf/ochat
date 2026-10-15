open! Core
module Store = Agent_store.Developer_run_store

(** Private immutable receipts for lifecycle-owned failures and interruption.
    Construction admits every variant against the exact Store terminal,
    aggregate-command and whole-document byte caps before callbacks can run.
    These values carry no Source, Root or execution authority. *)
type t

val create : Store.Limits.t -> (t, Agent_protocol.Error.t) result
val interrupted : t -> Store.Command.Receipt.Kind.t -> Store.Command.Receipt.t

val choose
  :  t
  -> Store.Command.Receipt.Kind.t
  -> cancelled:bool
  -> typed_failure:bool
  -> Store.Command.Receipt.t
