(** Shared immutable vocabulary for policy causes and flat forwarding. DATA only. *)
module Goal_target : sig
  type t [@@deriving equal, sexp_of]

  val local
    :  goal_id:Agent_protocol.Id.Goal.t
    -> episode:Agent_protocol.Goal_episode.t
    -> (t, Agent_protocol.Error.t) result

  val inherited : Agent_store.Delegation_goal_origin.Scope.t -> t
  val owner : t -> Agent_protocol.Session_ref.t
  val goal_id : t -> Agent_protocol.Id.Goal.t
  val source : t -> Agent_protocol.Run_source.t

  type view =
    | Local of
        { goal_id : Agent_protocol.Id.Goal.t
        ; episode : Agent_protocol.Goal_episode.t
        }
    | Inherited of Agent_store.Delegation_goal_origin.Scope.t

  val view : t -> view
  val root : t -> string
  val shape : Document_schema.Shape.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
  (* Pure immutable descriptor: neither constructor proves an actual Goal. *)
end

module Key : sig
  type t [@@deriving compare, equal, sexp_of]

  val to_string : t -> string
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
end

module Member : sig
  type kind =
    | Event
    | Invocation
    | Model_job
    | Async_job
  [@@deriving compare, equal, sexp_of]

  type t [@@deriving compare, equal, sexp_of]

  val event
    :  Agent_protocol.Id.Moderator_execution.t
    -> generation:int
    -> (t, Agent_protocol.Error.t) result

  val invocation
    :  Agent_protocol.Id.Invocation.t
    -> generation:int
    -> (t, Agent_protocol.Error.t) result

  val model_job
    :  Agent_protocol.Id.Job.t
    -> generation:int
    -> (t, Agent_protocol.Error.t) result

  (** Native Async_tool lifetime; carries ancestry only and never names an H
      allocation. Actual Job.kind must be checked by the native-prefix resolver. *)
  val async_job
    :  Agent_protocol.Id.Job.t
    -> generation:int
    -> (t, Agent_protocol.Error.t) result

  val generation : t -> int
  val kind : t -> kind
  val id : t -> string
  val key : t -> string
  val shape : Document_schema.Shape.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
  (* Stable source-work key. Job Attempt number belongs to allocation below,
     not a renewed cause for the same immutable Job lifetime. *)
end

module Allocation_work : sig
  type t =
    | Event of Agent_protocol.Id.Moderator_execution.t
    | Invocation of Agent_protocol.Id.Invocation.t
    | Model_job_attempt of
        { id : Agent_protocol.Id.Job.t
        ; attempt : int
        }
    | Async_job_attempt of
        { id : Agent_protocol.Id.Job.t
        ; attempt : int
        }
  [@@deriving equal, sexp_of]

  (** Job work retains its actual kind and positive claimed attempt. The member
      identifies its stable lifetime; allocation/forwarding independently check
      the attempt against the current native row. DATA grants no execution. *)
  val member : t -> generation:int -> (Member.t, Agent_protocol.Error.t) result

  val shape : Document_schema.Shape.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
end

module Coverage : sig
  type t =
    | Covered_local
    | Covered_physical
    | Causal_overlay
  [@@deriving equal, sexp_of]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
end

val invalid : string -> ('a, Agent_protocol.Error.t) result
val domain : ('a, Document_schema.Error.t) result -> ('a, Agent_protocol.Error.t) result
val limits : Document_schema.Limits.t
val digest : Jsonaf.t -> string
val session_shape : Document_schema.Shape.t
val source_shape : Document_schema.Shape.t
val origin_shape : Document_schema.Shape.t

val key_for
  :  parent_event:Agent_protocol.Id.Moderator_execution.t
  -> callee:Agent_protocol.Session_ref.t
  -> generation:int
  -> Key.t

val validate_path
  :  caller:Agent_protocol.Session_ref.t
  -> callee:Agent_protocol.Session_ref.t
  -> callee_source:Agent_protocol.Run_source.t
  -> path:Agent_store.Delegation_store.Reference.t list
  -> (unit, Agent_protocol.Error.t) result
