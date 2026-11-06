open! Core

module Profile : sig
  type t [@@deriving equal, sexp_of]

  val create
    :  max_scopes:int
    -> max_roots:int
    -> max_branches:int
    -> max_encoded_bytes:int
    -> child_max_encoded_bytes:int
    -> (t, Agent_protocol.Error.t) result

  val max_scopes : t -> int
  val max_roots : t -> int
  val max_branches : t -> int
  val max_encoded_bytes : t -> int
  val child_max_encoded_bytes : t -> int

  val restrict_to_children
    :  t
    -> Child_contributions.t
    -> (t, Agent_protocol.Error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
  val shape : Document_schema.Shape.t
end

type head =
  { scope : Agent_store.Delegation_goal_origin.Scope.t
  ; included : Goal_descendant_included.t option
  }
[@@deriving equal]

type root =
  { goal_id : Agent_protocol.Id.Goal.t
  ; branches : Goal_descendant_branch.t list
  }
[@@deriving equal]

type t =
  { profile : Profile.t
  ; session : Agent_protocol.Session_ref.t
  ; origin : Agent_store.Delegation_goal_origin.t option
  ; heads : head list
  ; roots : root list
  }
[@@deriving equal]

val invalid : string -> ('a, Agent_protocol.Error.t) result
val domain : ('a, Document_schema.Error.t) result -> ('a, Agent_protocol.Error.t) result
val limits : Document_schema.Limits.t
val validate : t -> (unit, Agent_protocol.Error.t) result
val shape : Document_schema.Shape.t
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
val find_root : t -> Agent_protocol.Id.Goal.t -> root option

val find_branch
  :  root
  -> Agent_store.Delegation_goal_origin.Scope.t
  -> Goal_descendant_branch.t option

val set_root : t -> root -> t
val set_branch : t -> Goal_descendant_branch.t -> (t, Agent_protocol.Error.t) result

val advance_head
  :  head
  -> Goal_descendant_included.t
  -> (head, Agent_protocol.Error.t) result
