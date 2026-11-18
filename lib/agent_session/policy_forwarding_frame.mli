(** Flat, finite immutable policy forwarding DATA. Decoding and constructors do
    not establish actual canonical membership, ancestry authenticity or authority.
    Complete raw values retain the existing Cause 256 KiB/depth72 budget. *)
module P = Agent_protocol

module D = Document_schema
module L = Policy_cause_leaf
module R = Agent_store.Delegation_store

module Bounds : sig
  type t [@@deriving equal, sexp_of]

  val create : max_inherited_scopes:int -> (t, P.Error.t) result
  val max_inherited_scopes : t -> int
  val max_direct_targets : t -> int
  val max_forwarded_targets : t -> int

  (* Checked D+1 and (D+1)^2. No target truncation or capacity escalation. *)
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, P.Error.t) result
  val shape : D.Shape.t
end

module Seed : sig
  type t = private
    { key : L.Key.t
    ; parent_event : P.Id.Moderator_execution.t
    ; caller : P.Session_ref.t
    ; caller_source : Policy_caller_source.t
    ; caller_origin : P.Inference_origin.t option
    ; callee : P.Session_ref.t
    ; callee_source : P.Run_source.t
    ; child_invocation : P.Id.Invocation.t
    ; admission_sha256 : string
    ; path : R.Reference.t list
    ; bounds : Bounds.t
    }
  [@@deriving equal, sexp_of]

  val create
    :  key:L.Key.t
    -> parent_event:P.Id.Moderator_execution.t
    -> caller:P.Session_ref.t
    -> caller_source:Policy_caller_source.t
    -> caller_origin:P.Inference_origin.t option
    -> callee:P.Session_ref.t
    -> callee_source:P.Run_source.t
    -> child_invocation:P.Id.Invocation.t
    -> admission_sha256:string
    -> path:R.Reference.t list
    -> bounds:Bounds.t
    -> (t, P.Error.t) result

  val key : t -> L.Key.t
  val caller : t -> P.Session_ref.t
  val caller_source : t -> Policy_caller_source.t
  val callee : t -> P.Session_ref.t
  val callee_source : t -> P.Run_source.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, P.Error.t) result
  val shape : D.Shape.t
end

module Frame : sig
  type t

  (** Identity is SHA256 of the entire known Seed/work/member-projection/cause
      body, excluding this identity and raw extensions. It pins the occurrence,
      including Job attempt; a cause hash alone does not. DATA only. *)
  val frame_sha256 : t -> string

  val seed : t -> Seed.t
  val work : t -> L.Allocation_work.t
  val known_json : t -> Jsonaf.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, P.Error.t) result
  val shape : D.Shape.t

  val create
    :  seed:Seed.t
    -> work:L.Allocation_work.t
    -> member_projection_sha256:string
    -> cause_known_sha256:string
    -> raw_seed:Jsonaf.t
    -> (t, P.Error.t) result
end

val validate_chain : bounds:Bounds.t -> Seed.t list -> (unit, P.Error.t) result
val validate_local : L.Goal_target.t -> Seed.t -> (unit, P.Error.t) result
val validate_frames : bounds:Bounds.t -> Frame.t list -> (unit, P.Error.t) result
