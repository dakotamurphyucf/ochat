(** Actor-owned registration/revocation receipts. Subscription lookup is mandatory
    at stage, selection and commit; missing validators never fall back to approval.
    External event admission is separate and cannot use this moderator stager. *)
type t

type limits =
  { max_active : int
  ; max_retained : int
  ; max_retained_bytes : int
  ; registration : External_ingress.limits
  }

val default_limits : limits
val validate_limits : limits -> (unit, Agent_protocol.Error.t) result
val create : unit -> t
val is_empty : t -> bool
val values : t -> External_ingress.t list

val find
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> id:Agent_protocol.Id.Capability.t
  -> External_ingress.t option

val stage
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> previous:External_ingress.t option
  -> next:External_ingress.t
  -> subscription:Agent_protocol.Subscription.t
  -> (int, Agent_protocol.Error.t) result

val select
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> receipts:int list
  -> lookup:(Agent_protocol.Id.Capability.t -> External_ingress.t option)
  -> subscription:
       (Agent_protocol.Id.Subscription.t -> int -> Agent_protocol.Subscription.t option)
  -> (unit, Agent_protocol.Error.t) result

val selected
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> lookup:(Agent_protocol.Id.Capability.t -> External_ingress.t option)
  -> subscription:
       (Agent_protocol.Id.Subscription.t -> int -> Agent_protocol.Subscription.t option)
  -> (External_ingress.t list, Agent_protocol.Error.t) result

val abort
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> receipt:int
  -> (unit, Agent_protocol.Error.t) result

val release_owner : t -> owner:Agent_protocol.Job.launch_owner -> unit
val abort_all : t -> unit

(** Place registrations before a later selected subscription epoch invalidates
    them. Creation can depend on a subscription created in the same transaction;
    registration/revocation order for each identity remains unchanged. *)
val ordered_changes
  :  subscriptions:Agent_protocol.Subscription.t list
  -> registrations:External_ingress.t list
  -> Session_delta.t list

(** Aggregate unique registration IDs across durable/provisional versions. Count
    the largest serialized version and conservatively active versions. Reserve
    8 KiB per registration for bounded revocation growth; no receipt is evicted.
    The eventual external submission path must apply this check to its candidate
    as well before committing queued payloads. *)
val check_capacity
  :  limits:limits
  -> generation:int
  -> now:Agent_protocol.Timestamp.t
  -> subscriptions:Agent_protocol.Subscription.t list
  -> values:External_ingress.t list
  -> (unit, Agent_protocol.Error.t) result
