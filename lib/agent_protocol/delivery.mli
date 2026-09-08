(** Durable notification intent. Committing a delivery is only valid in the
    actor transaction that inserts its matching history entry. *)
type source =
  | Moderator
  | Job_adapter
  | External_ingress
[@@deriving compare, equal, sexp]

type context =
  { id : Id.Delivery.t
  ; session_id : Id.Session.t
  ; generation : int
  ; invocation_id : Id.Invocation.t option
  ; work : Invocation.work option
  ; correlation : string
  ; source : source
  ; completion : Completion.t
  ; wake : Completion.wake
  ; created_at : Timestamp.t
  }
[@@deriving sexp]

type status =
  | Pending
  | Committed of
      { history_id : History_entry.Id.t
      ; at : Timestamp.t
      }
  | Failed of Invocation.tool_error
[@@deriving sexp]

type t = private
  { context : context
  ; attempt : int
  ; status : status
  }
[@@deriving sexp]

val create : context -> (t, Error.t) result
val validate : t -> (unit, Error.t) result
val commit : t -> history_id:History_entry.Id.t -> now:Timestamp.t -> (t, Error.t) result
val fail : t -> Invocation.tool_error -> (t, Error.t) result
val retry : t -> max_attempts:int -> (t, Error.t) result
val validate_transition : previous:t option -> t -> (unit, Error.t) result
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
