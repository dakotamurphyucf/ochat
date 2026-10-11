open Core
module Receipt = Agent_store.Prompt_source_receipt
module Owner = Prompt_source_owner

(** Original operator intent and runtime-scoped source-read availability, distinct
    from captured code/tool/native execution authority. One actual root lease,
    shared by private descendants through their ancestor's resource lifetime. *)
type t

(** Missing old receipt remains captured-restorable and cannot automatically gain
    live-source authority. No native read/open from metadata. For a present receipt,
    only actual CURRENT catalog selection may duplicate its retained root after
    exact original-intent comparison. Expected unavailable/denied roots retain their
    original receipt and finite reason; cancellation/exceptions propagate. *)
val create
  :  Receipt.t option
  -> selection:Prompt_catalog.Source_selection.t option
  -> sw:Eio.Switch.t
  -> checkpoint:(unit -> unit)
  -> t

val legacy : t
val receipt : t -> Receipt.t option

type availability =
  | Missing_receipt
  | Live
  | Unavailable of Owner.Error.t
  | Revoked
  | Closed

(** Reflect current owner/shared-policy state, not an initial cached decision. *)
val availability : t -> availability

(** No new descriptors/permissions. Caller retains original ancestor resource
    switch through all borrowers. Closing borrowed view never closes ancestor. *)
val borrow : t -> t

(** Only a root-owned loan closes its descriptor; borrowed views are no-ops.
    Idempotent; native switch finalizer repeats safely. Join actual borrowers
    before root release. Does not revoke operator policy or captured code. *)
val close : t -> unit

(** No root/Local getter; same original-root Scope. Missing/denied source fails
    explicitly, never substitutes artifact materialization or ambient filesystem. *)
val with_scope
  :  t
  -> f:(Owner.Scope.t -> ('a, Owner.Error.t) Result.t)
  -> ('a, Owner.Error.t) Result.t

(** Attachment admission: original root responsibility is claimed once; borrowed
    views have no close responsibility and may be shared. Existing runtime source
    ownership must remain legacy before attachment. No effects or checkpoints. *)
val claim : t -> unit Or_error.t

val is_legacy : t -> bool
