(** Payload-free work occurrence projections. Current security-view and transcript
    visibility are required by the server; decoding grants no control capability. *)
module Key : sig
  type t =
    | Job of
        { id : Id.Job.t
        ; attempt : int
        }
    | Schedule of Id.Schedule.t
    | Invocation of Id.Invocation.t
    | Subscription of Id.Subscription.t
    | Delivery of Id.Delivery.t
    | Moderator_execution of Id.Moderator_execution.t
  [@@deriving compare, equal, sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Status : sig
  type t =
    | Accepted
    | Running
    | Waiting_approval
    | Waiting_work
    | Succeeded
    | Failed
    | Cancelled
    | Interrupted
    | Unsupported
  [@@deriving compare, equal, sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Delivery_state : sig
  type t =
    | Not_applicable
    | Pending
    | Acknowledged
    | Discarded
  [@@deriving compare, equal, sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

type t = private
  { session : Session_ref.t
  ; generation : int
  ; key : Key.t
  ; status : Status.t
  ; delivery : Delivery_state.t
  ; revision : int64
  }
[@@deriving sexp]

val create
  :  session:Session_ref.t
  -> generation:int
  -> key:Key.t
  -> status:Status.t
  -> delivery:Delivery_state.t
  -> revision:int64
  -> (t, Error.t) result

val compare_key : t -> t -> int
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result

module Query : sig
  type t =
    { session : Session_ref.t
    ; page : Page.Request.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end
