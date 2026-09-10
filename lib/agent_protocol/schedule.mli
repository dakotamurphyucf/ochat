(** Durable one-shot ChatML event schedules. *)

type misfire =
  | Deliver_once_immediately
  | Skip_if_expired
  | Fail
[@@deriving compare, equal, sexp]

type status =
  | Scheduled
  | Delivering
  | Delivered
  | Cancelled
  | Failed of Error.t
[@@deriving sexp]

type due =
  | At of Timestamp.t
  | After_ms of int
[@@deriving sexp]

(** Host-captured moderator authority and optional one-time subscription epoch
    binding. A legacy schedule without this record grants no script authority. *)
type ownership =
  { source : Invocation.observer
  ; creator : Job.launch_owner
  ; subscription : (Id.Subscription.t * int) option [@sexp.option]
  }
[@@deriving equal, sexp]

type t =
  { id : Id.Schedule.t
  ; session_id : Id.Session.t
  ; generation : int
  ; payload : Jsonaf.t
  ; created_at : Timestamp.t
  ; next_due_at : Timestamp.t
  ; misfire : misfire
  ; status : status
  ; delivery_count : int
  ; last_delivery_at : Timestamp.t option
  ; ownership : ownership option [@sexp.option]
    (** Owned schedules use a version-2 JSON envelope. Legacy records retain
        their original flat encoding; old readers reject the owned envelope. *)
  ; delivery_cancellation : string option [@sexp.option]
    (** Immutable cancellation of an enqueued callback, separate from the retained
        Delivered status/count/time. Present only on owned Delivered records with
        one delivery. These records require JSON envelope version 3; prior records
        keep their existing encoding and cannot silently lose a cancellation. *)
  }
[@@deriving sexp]

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result

(** Validate owned records and their lifecycle without changing the legacy
    unowned schedule contract. Binding is allowed once while still scheduled;
    source, creator, identity, payload and due time stay immutable. An enqueued
    owned callback may acquire one immutable delivery cancellation without changing
    the schedule's delivered status, count, timestamp or payload. *)
val validate : t -> (unit, Error.t) result

val validate_transition : previous:t option -> t -> (unit, Error.t) result

module List_request : sig
  type t =
    { session_id : Id.Session.t
    ; page : Page.Request.t
    ; status : string option
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Get_request : sig
  type t =
    { session_id : Id.Session.t
    ; schedule_id : Id.Schedule.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Create_request : sig
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; payload : Jsonaf.t
    ; due : due
    ; misfire : misfire
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Cancel_request : sig
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; schedule_id : Id.Schedule.t
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Mutation_response : sig
  type nonrec t =
    { schedule : t
    ; mutation : Mutation_result.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end
