(** Private pure receipt relations. These DATA values never prove admission,
    writer absence, a live source, or a canonical acknowledgement. *)
module P = Agent_protocol

module Cause = Policy_cause_data
module PC = Policy_canonical_usage
module Occurrence = Goal_descendant_occurrence

val invalid : string -> ('a, P.Error.t) result
val domain : ('a, Document_schema.Error.t) result -> ('a, P.Error.t) result
val add : int64 -> int64 -> (int64, P.Error.t) result
val pending_limits : Document_schema.Limits.t

module Pending : sig
  type t = private
    { owner : P.Session_ref.t
    ; goal_id : P.Id.Goal.t
    ; stream_id : string
    ; nonce : int64
    ; next_prefix : int64
    ; occurrence : Occurrence.t
    ; cause : Cause.Key.t
    ; work : Cause.Allocation_work.t
    ; parent_event : P.Id.Moderator_execution.t
    ; caller_origin : P.Inference_origin.t option
    ; child_invocation : P.Id.Invocation.t
    ; admission_sha256 : string
    ; forwarding_sha256 : string option
    }
  [@@deriving equal, sexp_of]

  val forwarding_sha256 : t -> string option
  val owner : t -> P.Session_ref.t
  val goal_id : t -> P.Id.Goal.t
  val stream_id : t -> string
  val nonce : t -> int64
  val next_prefix : t -> int64
  val occurrence : t -> Occurrence.t
  val cause : t -> Cause.Key.t
  val work : t -> Cause.Allocation_work.t
  val shape : Document_schema.Shape.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, P.Error.t) result
  val capacity_projection : t -> Jsonaf.t
end

module Request : sig
  type t

  val create
    :  Cause.t
    -> target:Cause.Goal_target.t
    -> work:Cause.Allocation_work.t
    -> occurrence:Occurrence.t
    -> (t, P.Error.t) result

  val stream : t -> PC.Stream.t
  val cause : t -> Cause.t
  val work : t -> Cause.Allocation_work.t
  val occurrence : t -> Occurrence.t
  val pending : t -> nonce:int64 -> next_prefix:int64 -> (Pending.t, P.Error.t) result
  val matches : t -> Pending.t -> bool
  val shape : Document_schema.Shape.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, P.Error.t) result
end

val validate_pending : Pending.t -> stream:PC.Stream.t -> (unit, P.Error.t) result
val confirms : Pending.t -> projection:PC.Projection.t -> (unit, P.Error.t) result
