(** Ordered optimistic mutations owned by one moderator execution. Actor-side
    authority and quotas remain outside this reusable receipt registry. *)
module type Value = sig
  module Id : sig
    type t [@@deriving compare, equal, hash, sexp]
  end

  type t

  val name : string
  val id : t -> Id.t
  val equal : t -> t -> bool
  val validate_staging : t -> (unit, Agent_protocol.Error.t) result

  val validate_transition
    :  previous:t option
    -> t
    -> (unit, Agent_protocol.Error.t) result
end

module Make (Value : Value) : sig
  type t

  val create : unit -> t
  val is_empty : t -> bool
  val reservations : t -> int
  val find : t -> owner:Agent_protocol.Job.launch_owner -> id:Value.Id.t -> Value.t option

  val stage
    :  t
    -> owner:Agent_protocol.Job.launch_owner
    -> previous:Value.t option
    -> next:Value.t
    -> (int, Agent_protocol.Error.t) result

  val select
    :  t
    -> owner:Agent_protocol.Job.launch_owner
    -> receipts:int list
    -> lookup:(Value.Id.t -> Value.t option)
    -> (unit, Agent_protocol.Error.t) result

  val selected
    :  t
    -> owner:Agent_protocol.Job.launch_owner
    -> lookup:(Value.Id.t -> Value.t option)
    -> (Value.t list, Agent_protocol.Error.t) result

  val abort
    :  t
    -> owner:Agent_protocol.Job.launch_owner
    -> receipt:int
    -> (unit, Agent_protocol.Error.t) result

  val release_owner : t -> owner:Agent_protocol.Job.launch_owner -> unit
  val values : t -> Value.t list
  val abort_all : t -> unit
end
