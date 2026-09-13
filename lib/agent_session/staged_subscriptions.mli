(** Actor-local ordered subscription mutations. This registry checks receipts
    and optimistic record transitions, not caller authority. The actor must
    validate the live moderator/source, generation, quotas and completion schema. *)
type t

type limits =
  { max_active : int
  ; max_retained : int
  ; default_lifetime_ms : int
  ; max_lifetime_ms : int
  }

(** Host-configurable admission limits. Defaults: 64 active, 4096 retained,
    one-hour default lifetime and 24-hour maximum. Never evict unresolved records
    to admit new work. V1 ceilings are 1024 active and 24 hours. *)
val default_limits : limits

val validate_limits : limits -> (unit, Agent_protocol.Error.t) result
val create : unit -> t
val is_empty : t -> bool

(** Uncommitted creations across all owners, for conservative shared admission. *)
val reservations : t -> int

(** Each mutation receives a distinct receipt, even an idempotent terminal no-op.
    Another owner cannot stage the same subscription concurrently. Source-less
    legacy records cannot be staged through the moderator lifecycle. *)
val stage
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> previous:Agent_protocol.Subscription.t option
  -> next:Agent_protocol.Subscription.t
  -> (int, Agent_protocol.Error.t) result

val find
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> id:Agent_protocol.Id.Subscription.t
  -> Agent_protocol.Subscription.t option

(** Select surviving receipts in original execution order. Validate every
    selected predecessor against the durable lookup or preceding selected delta.
    No selection is changed on error. *)
val select
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> receipts:int list
  -> lookup:(Agent_protocol.Id.Subscription.t -> Agent_protocol.Subscription.t option)
  -> (unit, Agent_protocol.Error.t) result

(** Revalidate the selected sequence against the current durable state immediately
    before the actor save. Includes creation followed by completion as two deltas. *)
val selected
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> lookup:(Agent_protocol.Id.Subscription.t -> Agent_protocol.Subscription.t option)
  -> (Agent_protocol.Subscription.t list, Agent_protocol.Error.t) result

(** Catch rollback is newest-first within its owner; foreign or out-of-order
    receipts fail. A missing already-discarded receipt is harmless. *)
val abort
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> receipt:int
  -> (unit, Agent_protocol.Error.t) result

(** Remove all receipts after either a successful save or whole-owner abort. *)
val release_owner : t -> owner:Agent_protocol.Job.launch_owner -> unit

val values : t -> Agent_protocol.Subscription.t list
val abort_all : t -> unit
