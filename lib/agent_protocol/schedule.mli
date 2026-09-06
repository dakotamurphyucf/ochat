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
  }
[@@deriving sexp]

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result

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
