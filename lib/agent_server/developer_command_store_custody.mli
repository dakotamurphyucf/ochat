open! Core
module Store = Agent_store.Developer_run_store

(** Private exact Store.Command.Pending custody. The durable Store remains the
    only writer and pending map; this module merely retains its physical token
    and immutable selected terminal across a bounded prewrite retry. It never
    invokes a command callback or mints a fresh Start. *)
type t

type kind =
  | Reserve
  | Start
  | Finish

type reconciliation =
  | Installed of Store.Command.View.t
  | Retired_absent

val create
  :  handle:Store.handle
  -> token:Store.Command.Pending.t
  -> kind:kind
  -> admit:(unit -> bool)
  -> t

val handle : t -> Store.handle
val kind : t -> kind

(** Same physical Pending, same chosen after-image, at most [remaining]
    proven prewrite rebases. Unknown or attempted writes never rebase. *)
val commit : t -> remaining:int -> (Store.Command.installed, Store.Error.t) result

(** Reconcile only this exact token. Absent Reserve/Start is retired; Absent
    Finish retries the same selected terminal. Diverged requires the Store's
    proven prewrite reprepare, never a cached absence. *)
val reconcile : t -> (reconciliation, Store.Error.t) result
