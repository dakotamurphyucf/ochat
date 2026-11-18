(** Private immutable subset representation and validated complete JSON profile.
    The exported Policy_canonical_usage wrapper alone retains universal unknown
    children and admits closed reasons. No value here is execution authority. *)
module P = Agent_protocol

module Cause = Policy_cause_data
module Member = Cause.Member
module Target = Cause.Goal_target
module Coverage = Cause.Coverage
module Work = Cause.Allocation_work
module Fact = Goal_descendants.Canonical.Fact
module Marks = Canonical_usage_fold.Marks

val invalid : string -> ('a, P.Error.t) result
val domain : ('a, Document_schema.Error.t) result -> ('a, P.Error.t) result
val ledger : ('a, Inference_ledger.Error.t) result -> ('a, P.Error.t) result
val fold : ('a, Canonical_usage_fold.Error.t) result -> ('a, P.Error.t) result
val counter : ('a, Goal_accounting_counter.error) result -> ('a, P.Error.t) result
val add : int64 -> int64 -> (int64, P.Error.t) result
val increment : int64 -> (int64, P.Error.t) result
val digest : Jsonaf.t -> string
val limits : Document_schema.Limits.t
val projection_limits : Document_schema.Limits.t
val max_causes : int
val max_memberships : int
val max_streams : int
val max_bindings : int
val marks_json : Marks.t -> Jsonaf.t
val marks_of_json : Jsonaf.t -> (Marks.t, P.Error.t) result
val marks_shape : Document_schema.Shape.t

module Stream : sig
  type t =
    { id : string
    ; caller : P.Session_ref.t
    ; caller_source : Policy_caller_source.t
    ; callee : P.Session_ref.t
    ; callee_source : P.Run_source.t
    ; path : Agent_store.Delegation_store.Reference.t list
    ; target : Target.t
    ; forwarding_route : Cause.Forwarding.Route.t option
    ; raw : Jsonaf.t option
    }

  val of_cause : Cause.t -> Target.t -> t
  val id : t -> string
  val caller : t -> P.Session_ref.t
  val callee : t -> P.Session_ref.t
  val callee_source : t -> P.Run_source.t
  val path : t -> Agent_store.Delegation_store.Reference.t list
  val target : t -> Target.t
  val forwarding_route : t -> Cause.Forwarding.Route.t option
  val known_json : t -> Jsonaf.t
  val equal : t -> t -> bool
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, P.Error.t) result
  val shape : Document_schema.Shape.t
end

module Choice : sig
  type t =
    { target : Target.t
    ; coverage : Coverage.t
    }

  val target : t -> Target.t
  val coverage : t -> Coverage.t
  val equal : t -> t -> bool
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, P.Error.t) result
  val shape : Document_schema.Shape.t
end

module Head : sig
  type t =
    { cause : Cause.Key.t
    ; work : Work.t
    ; canonical : Fact.t
    }

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, P.Error.t) result
  val shape : Document_schema.Shape.t
end

module Membership : sig
  type t =
    { member : Member.t
    ; cause : Cause.Key.t
    ; parent : Member.t option
    ; projection_sha256 : string
    }

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, P.Error.t) result
  val shape : Document_schema.Shape.t
end

module Binding : sig
  type t =
    { ordinal : int64
    ; cause : Cause.Key.t
    ; work : Work.t
    ; canonical : Fact.t
    ; choices : Choice.t list
    }

  val ordinal : t -> int64
  val cause : t -> Cause.Key.t
  val choices : t -> Choice.t list
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, P.Error.t) result
  val shape : Document_schema.Shape.t
end

module Partition : sig
  type t =
    { stream : Stream.t
    ; settled : Goal_usage.t
    ; admitted_attempts : int64
    ; head : Head.t option
    }

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, P.Error.t) result
  val shape : Document_schema.Shape.t
end

type t =
  { callee : P.Session_ref.t
  ; max_inherited_scopes : int
  ; basis_revision : int64
  ; basis_marks : Marks.t
  ; ledger_revision : int64
  ; marks : Marks.t
  ; causes : Cause.t list
  ; memberships : Membership.t list
  ; streams : Partition.t list
  ; bindings : Binding.t list
  }

val equal : t -> t -> bool
val validate : t -> (unit, P.Error.t) result
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, P.Error.t) result
val shape : Document_schema.Shape.t
val find_cause : t -> Cause.Key.t -> Cause.t option
val find_member : t -> Member.t -> Membership.t option
val find_stream : t -> string -> Partition.t option
val ordinal : Fact.t -> (int64, P.Error.t) result
val overlay : Choice.t -> bool
val selected : t -> Binding.t -> Stream.t -> bool

module Source_work : sig
  type t =
    { member : Member.t
    ; parent : Member.t option
    ; projection_sha256 : string
    ; ready : bool
    ; attempt : int option
    ; operation_id : P.Id.Operation.t option
    ; invocation_id : P.Id.Invocation.t option
    }

  val retained_queued_parent
    :  memberships:Membership.t list
    -> Cause.t
    -> Policy_native_ancestry.Queued_job.t
    -> (Member.t, P.Error.t) result

  val capture_native
    :  memberships:Membership.t list
    -> Policy_native_ancestry.t
    -> Cause.t
    -> Member.t
    -> (t option, P.Error.t) result

  val capture
    :  memberships:Membership.t list
    -> Cause.Context.t
    -> Cause.t
    -> Member.t
    -> (t option, P.Error.t) result

  val matches : Membership.t -> t -> bool
end
