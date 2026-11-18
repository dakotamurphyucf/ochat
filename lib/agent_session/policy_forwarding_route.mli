(** Flat, finite immutable policy forwarding DATA. Decoding and constructors do
    not establish actual canonical membership, ancestry authenticity or authority.
    Complete raw values retain the existing Cause 256 KiB/depth72 budget. *)
module P = Agent_protocol

module D = Document_schema
module L = Policy_cause_leaf
module R = Agent_store.Delegation_store
module Bounds = Policy_forwarding_frame.Bounds
module Seed = Policy_forwarding_frame.Seed
module Frame = Policy_forwarding_frame.Frame

module Route : sig
  type t [@@deriving equal]

  val target : t -> L.Goal_target.t
  val known_json : t -> Jsonaf.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, P.Error.t) result
  val shape : D.Shape.t

  (** Validate exact endpoint/source and the complete source-to-new-callee route.
      Stable descriptor contains no Event/Invocation/Job/attempt correlation. *)
  val check_next
    :  t
    -> caller:P.Session_ref.t
    -> caller_source:Policy_caller_source.t
    -> callee:P.Session_ref.t
    -> callee_source:P.Run_source.t
    -> path:R.Reference.t list
    -> (unit, P.Error.t) result
end

module Witness : sig
  type t

  (** SAME immutable frames selected from this target's introduction. DATA only;
      no copy, currentness, caller authority or wire representation change. *)
  val frames : t -> Frame.t list

  val route : t -> Route.t
  val known_json : t -> Jsonaf.t
  val known_sha256 : t -> string
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, P.Error.t) result

  (** Root-specific occurrence frames. Local introduction is checked against its
      actual first Seed Run origin; raw children survive separately from hashes. *)
  val shape : D.Shape.t
end
