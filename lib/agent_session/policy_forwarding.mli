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
module Route = Policy_forwarding_route.Route
module Witness = Policy_forwarding_route.Witness

type t

val bounds : t -> Bounds.t
val targets : t -> L.Goal_target.t list
val frames : t -> Frame.t list
val issuing_session : t -> P.Session_ref.t
val issuing_source : t -> P.Run_source.t
val equal : t -> t -> bool
val sexp_of_t : t -> Sexplib0.Sexp.t
val known_json : t -> Jsonaf.t
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, P.Error.t) result
val shape : D.Shape.t
val for_target : t -> target:L.Goal_target.t -> (Witness.t, P.Error.t) result

val check_next
  :  t
  -> caller:P.Session_ref.t
  -> caller_source:Policy_caller_source.t
  -> callee:P.Session_ref.t
  -> callee_source:P.Run_source.t
  -> path:R.Reference.t list
  -> (unit, P.Error.t) result

(** Internal composition constructor, used only by Cause.Source_child. Every
    input projection is derived from that SAME validated raw Cause child; no
    public Cause facade exposes independently supplied prefix/seed/target inputs.
    All prior frames/introductions and unknown wrapper children are copied;
    new roots get this Seed's index. Each raw target must decode to its paired
    known value, and existing roots must retain EXACTLY the introduction's raw
    target child. Conflicting duplicate representations reject without merging. *)
val append
  :  t option
  -> seed:Seed.t
  -> raw_seed:Jsonaf.t
  -> targets:(L.Goal_target.t * Jsonaf.t) list
  -> work:L.Allocation_work.t
  -> member_projection_sha256:string
  -> cause_known_sha256:string
  -> (t, P.Error.t) result
