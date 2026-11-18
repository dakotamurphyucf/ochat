(* A registered caller may have an actual installed moderator or an explicit
    verified unmoderated artifact identity. Missing evidence is neither case.
    Unmoderated callers may carry inherited targets; direct Local targets and
    every policy callee require their actual installed Run_source. DATA only. *)
(** Validated immutable policy cause descriptors and finite source-work views.
    DATA only: no constructor/decode grants authority or canonical membership.
    Route is nonempty caller-to-callee ancestor order: each lower Reference
    child equals previous parent; final parent/generation equal callee source.
    Same server/no repeated session. Full inherited targets above callee stay
    separate. References are DATA; actual intermediate authenticity is runtime.
    Complete cause JSON is bounded256KiB/depth72/16384fields/32768nodes. *)
module Caller_source = Policy_caller_source

module Goal_target = Policy_cause_leaf.Goal_target
module Key = Policy_cause_leaf.Key
module Member = Policy_cause_leaf.Member
module Allocation_work = Policy_cause_leaf.Allocation_work
module Coverage = Policy_cause_leaf.Coverage
module Bounds = Policy_forwarding.Bounds

module Seed : sig
  type t = Policy_forwarding.Seed.t

  val key : t -> Key.t
  val caller : t -> Agent_protocol.Session_ref.t
  val caller_source : t -> Caller_source.t
  val callee : t -> Agent_protocol.Session_ref.t
  val callee_source : t -> Agent_protocol.Run_source.t
  val shape : Document_schema.Shape.t
end

module Forwarding : sig
  type t = Policy_forwarding.t

  module Frame : sig
    type t = Policy_forwarding.Frame.t

    val seed : t -> Seed.t
    val work : t -> Allocation_work.t
  end

  module Route = Policy_forwarding.Route

  module Witness : sig
    type t = Policy_forwarding.Witness.t

    val route : t -> Route.t
    val known_json : t -> Jsonaf.t
    val known_sha256 : t -> string
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
    val shape : Document_schema.Shape.t
  end

  val bounds : t -> Bounds.t
  val targets : t -> Goal_target.t list
  val frames : t -> Frame.t list
  val issuing_session : t -> Agent_protocol.Session_ref.t
  val issuing_source : t -> Agent_protocol.Run_source.t
  val equal : t -> t -> bool
  val sexp_of_t : t -> Sexplib0.Sexp.t
  val for_target : t -> target:Goal_target.t -> (Witness.t, Agent_protocol.Error.t) result

  val check_next
    :  t
    -> caller:Agent_protocol.Session_ref.t
    -> caller_source:Caller_source.t
    -> callee:Agent_protocol.Session_ref.t
    -> callee_source:Agent_protocol.Run_source.t
    -> path:Agent_store.Delegation_store.Reference.t list
    -> (unit, Agent_protocol.Error.t) result

  val known_json : t -> Jsonaf.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
  val shape : Document_schema.Shape.t
end

type t

(** Pure route DATA validation shared by full causes and standalone stream
    projections. References do not authenticate live intermediate sources. *)
val validate_path
  :  caller:Agent_protocol.Session_ref.t
  -> callee:Agent_protocol.Session_ref.t
  -> callee_source:Agent_protocol.Run_source.t
  -> path:Agent_store.Delegation_store.Reference.t list
  -> (unit, Agent_protocol.Error.t) result

val create
  :  parent:Agent_protocol.Moderator_execution.t
  -> callee:Agent_protocol.Session_ref.t
  -> callee_source:Agent_protocol.Run_source.t
  -> caller:Agent_protocol.Session_ref.t
  -> caller_source:Caller_source.t
  -> caller_origin:Agent_protocol.Inference_origin.t option
  -> path:Agent_store.Delegation_store.Reference.t list
  -> targets:Goal_target.t list
  -> max_inherited_scopes:int
  -> (t, Agent_protocol.Error.t) result

val key : t -> Key.t
val equal : t -> t -> bool
val targets : t -> Goal_target.t list
val parent_event : t -> Agent_protocol.Id.Moderator_execution.t
val callee : t -> Agent_protocol.Session_ref.t
val callee_source : t -> Agent_protocol.Run_source.t
val caller : t -> Agent_protocol.Session_ref.t
val caller_source : t -> Caller_source.t
val caller_origin : t -> Agent_protocol.Inference_origin.t option
val child_invocation : t -> Agent_protocol.Id.Invocation.t
val admission_sha256 : t -> string
val path : t -> Agent_store.Delegation_store.Reference.t list
val max_inherited_scopes : t -> int
val matches_event : t -> Agent_protocol.Moderator_execution.t -> bool
val shape : Document_schema.Shape.t
val limits : Document_schema.Limits.t
val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
val to_json : t -> Jsonaf.t
(* Field presence/raw preservation is owned by enclosing domain document,
   whose complete-original-child admission enforces these same limits. *)

module Context : sig
  type t = private
    { session_id : Agent_protocol.Id.Session.t
    ; generation : int
    ; current_source : Agent_protocol.Run_source.t option
    ; events : Agent_protocol.Moderator_execution.t list
    ; invocations : Agent_protocol.Invocation.t list
    ; jobs : Agent_protocol.Job.t list
    ; runs : Run_state.t option
    ; accounts : Goal_accounts.t option
    ; catalog : Goal_catalog.t option
    ; descendants : Goal_descendants.t option
    }

  val create
    :  session_id:Agent_protocol.Id.Session.t
    -> generation:int
    -> current_source:Agent_protocol.Run_source.t option
    -> events:Agent_protocol.Moderator_execution.t list
    -> invocations:Agent_protocol.Invocation.t list
    -> jobs:Agent_protocol.Job.t list
    -> runs:Run_state.t option
    -> accounts:Goal_accounts.t option
    -> catalog:Goal_catalog.t option
    -> descendants:Goal_descendants.t option
    -> (t, Agent_protocol.Error.t) result
  (* Finite native record view, constructed at real ordered prefixes. Does not
     depend on Session_state (avoids type cycle), perform I/O or prove liveness.
     current_source is existing actual installation/checkpoint projection;
     complete State verifies it, runtime597 additionally authenticates issuer.
     Historical accepted membership may outlive these physical source rows.
     NEW membership/allocation requires exact extant current prefix row. *)
end

val seed : t -> Seed.t
val forwarding : t -> Forwarding.t option
val known_json : t -> Jsonaf.t

(* Native owned encoding. [to_json] retains complete copied raw children;
    identity and occurrence hashes use this known projection only. *)
module Source_child : sig
  type cause = t
  type t

  val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
  val value : t -> cause
  val raw : t -> Jsonaf.t
end

val append_forwarding
  :  Source_child.t
  -> work:Allocation_work.t
  -> member_projection_sha256:string
  -> (Forwarding.t, Agent_protocol.Error.t) result

(* All prefix/seed/targets/raw projections derive from this SAME source child.
    This pure DATA constructor proves neither canonical membership nor liveness. *)
val create_forwarded
  :  parent:Agent_protocol.Moderator_execution.t
  -> callee:Agent_protocol.Session_ref.t
  -> callee_source:Agent_protocol.Run_source.t
  -> caller:Agent_protocol.Session_ref.t
  -> caller_source:Caller_source.t
  -> caller_origin:Agent_protocol.Inference_origin.t option
  -> path:Agent_store.Delegation_store.Reference.t list
  -> direct_targets:Goal_target.t list
  -> forwarded:Forwarding.t
  -> max_inherited_scopes:int
  -> (t, Agent_protocol.Error.t) result
