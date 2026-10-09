(** Payload-free attention observed from existing session/work owners. Reading is
    not acknowledgement; unresolved rows never disappear because a client read them. *)
module Reason : sig
  type t =
    | Approval
    | Input_required
    | Failure
    | Completion_pending
  [@@deriving compare, equal, sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Attention : sig
  type entity =
    | Permission of Id.Permission.t
    | Operation of Id.Operation.t
    | Work of Session_work.Key.t
    | Session
  [@@deriving compare, equal, sexp]

  type t = private
    { entity : entity
    ; reason : Reason.t
    ; unresolved : bool
    ; expired : bool
    }
  [@@deriving sexp]

  val create
    :  entity:entity
    -> reason:Reason.t
    -> unresolved:bool
    -> expired:bool
    -> (t, Error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
  val compare_key : t -> t -> int
end

module Transient : sig
  (** Unloaded/restarted snapshots have no durable active-call/progress authority. *)
  type t =
    | Unavailable
    | Live of
        { tool_calls : int
        ; agent_calls : int
        }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

type t = private
  { summary : Session_activity_summary.t
  ; attention : Attention.t list
  ; work_count : int
  ; transient : Transient.t
  ; usage : Inference_query.Summary.t
  }
[@@deriving sexp]

val create
  :  summary:Session_activity_summary.t
  -> attention:Attention.t list
  -> work_count:int
  -> transient:Transient.t
  -> usage:Inference_query.Summary.t
  -> (t, Error.t) result

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
