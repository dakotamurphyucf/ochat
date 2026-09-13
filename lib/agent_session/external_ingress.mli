open Core

(** Pure, durable admission rules for scoped external DATA events. This module
    never authenticates a transport, grants capabilities, invokes a handler or
    marks a subscription complete. The actor must authorize registration and
    serialize [prepare] with saving the returned state and enqueueing its event.
    A registration identifier is not a bearer credential. *)

type limits =
  { max_payload_bytes : int
  ; max_payload_depth : int
  ; max_receipts : int
  ; rate_count : int
  ; rate_window_ms : int
  }
[@@deriving equal, sexp]

type context =
  { id : Agent_protocol.Id.Capability.t
  ; session_id : Agent_protocol.Id.Session.t
  ; generation : int
  ; subscription_id : Agent_protocol.Id.Subscription.t
  ; epoch : int
  ; source : Agent_protocol.Invocation.observer
  ; producer : Agent_protocol.Id.Principal.t
  ; namespace : string
  ; schema : Jsonaf.t
  ; created_at : Agent_protocol.Timestamp.t
  ; expires_at : Agent_protocol.Timestamp.t
  ; limits : limits
  }
[@@deriving equal, sexp]

type receipt = private
  { id : Agent_protocol.Id.Ingress_event.t
  ; key : Agent_protocol.Idempotency_key.t
  ; payload : Jsonaf.t
  ; payload_sha256 : string
  ; accepted_at : Agent_protocol.Timestamp.t
  }
[@@deriving equal, sexp]

type t = private
  { context : context
  ; receipts : receipt list
  ; revoked : string option
  }
[@@deriving equal, sexp]

type admission =
  | Duplicate of receipt
  | Accepted of t * receipt

val default_limits : limits

(** Required after decoding retained state, before it enters the actor. Validates
    identities, bounded policy/schema/payloads and receipt hashes/uniqueness. *)
val validate : t -> (unit, Agent_protocol.Error.t) result

(** Bind an active, source-owned subscription epoch. The host supplies the
    permitted producer and limits; caller JSON does not establish this authority.
    Event namespaces must begin with [external.] and carry no internal variant. *)
val create
  :  context
  -> subscription:Agent_protocol.Subscription.t
  -> (t, Agent_protocol.Error.t) result

(** Recheck authenticated producer, current owner/source/lifetime and exact
    namespace before inspecting retry receipts. A repeated key with identical
    canonical JSON returns its saved receipt without allocating an event ID or
    consuming rate/capacity, including after its subscription finishes or advances
    epoch. Current source/generation, producer, lifetime and explicit revocation
    still apply to that read. Changed payload conflicts. A new event is retained
    or explicitly rejected; no receipt is evicted to make space. Rate history
    survives restore and conservatively counts future timestamps after rollback.
    Schema validation is bounded and never resolves external references. *)
val prepare
  :  t
  -> session_id:Agent_protocol.Id.Session.t
  -> generation:int
  -> source:Agent_protocol.Invocation.observer
  -> subscription:Agent_protocol.Subscription.t
  -> producer:Agent_protocol.Id.Principal.t
  -> namespace:string
  -> key:Agent_protocol.Idempotency_key.t
  -> payload:Jsonaf.t
  -> now:Agent_protocol.Timestamp.t
  -> create_event_id:(unit -> Agent_protocol.Id.Ingress_event.t)
  -> (admission, Agent_protocol.Error.t) result

(** Idempotent explicit revocation. The first bounded reason remains inspectable;
    it does not delete prior receipts or cancel the associated subscription. *)
val revoke : t -> reason:string -> (t, Agent_protocol.Error.t) result

(** Capture exact immutable queue metadata for an accepted receipt. Admission
    timestamp stays in the receipt so the host can assign it at actual commit. *)
val delivery_frame
  :  t
  -> receipt
  -> (Chat_response.Ingress_delivery.t, Agent_protocol.Error.t) result

(** Validate retained ownership without requiring the subscription to remain
    active; older epochs and terminal outcomes retain their audit receipts. *)
val validate_owner
  :  t
  -> Agent_protocol.Subscription.t
  -> (unit, Agent_protocol.Error.t) result

(** Store transition guard: immutable binding, append-only receipts, one new
    event per admission, no unrevocation or receipt eviction. The actor must
    additionally authorize admission, check current session/source, and commit
    the new receipt with queue insertion atomically. *)
val validate_transition
  :  subscription:Agent_protocol.Subscription.t
  -> previous:t option
  -> t
  -> (unit, Agent_protocol.Error.t) result
