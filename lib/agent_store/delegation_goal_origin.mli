open! Core

(** Immutable DATA identifying the first-child branch and creator of each
    inherited Goal root. Decoding or construction confers no authority; the
    runtime owner authenticates ancestry and the actual lower admission.
    Historical Admission absence remains Unknown, distinct from [unrelated]. *)
module Anchor : sig
  type t [@@deriving equal, sexp]

  val create
    :  parent_session_id:Agent_protocol.Id.Session.t
    -> parent_generation:int
    -> principal_id:Agent_protocol.Id.Principal.t
    -> key:Agent_protocol.Idempotency_key.t
    -> child_session_id:Agent_protocol.Id.Session.t
    -> transaction_id:Agent_protocol.Id.Transaction.t
    -> (t, Agent_protocol.Error.t) result

  val parent_session_id : t -> Agent_protocol.Id.Session.t
  val parent_generation : t -> int
  val principal_id : t -> Agent_protocol.Id.Principal.t
  val key : t -> Agent_protocol.Idempotency_key.t
  val child_session_id : t -> Agent_protocol.Id.Session.t
  val transaction_id : t -> Agent_protocol.Id.Transaction.t
  val shape : Document_schema.Shape.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
end

module Scope : sig
  type t [@@deriving equal, sexp]

  (** Owner equals the creator episode's session and the branch's parent
      session; source equals the creator admission's immutable source. *)
  val create
    :  owner:Agent_protocol.Session_ref.t
    -> goal_id:Agent_protocol.Id.Goal.t
    -> source:Agent_protocol.Run_source.t
    -> creator_episode:Agent_protocol.Goal_episode.t
    -> branch:Anchor.t
    -> (t, Agent_protocol.Error.t) result

  val owner : t -> Agent_protocol.Session_ref.t
  val goal_id : t -> Agent_protocol.Id.Goal.t
  val source : t -> Agent_protocol.Run_source.t
  val creator_episode : t -> Agent_protocol.Goal_episode.t
  val branch : t -> Anchor.t
  val compare_root : t -> t -> int

  (** Stable SHA256 identity of the encoded owner/Goal pair; no authority. *)
  val root_id : t -> string

  val shape : Document_schema.Shape.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
end

type t [@@deriving equal, sexp]

val unrelated : t

(** Nonempty vector in strictly increasing [Scope.compare_root] order; repeated
    roots reject even when their other frozen fields differ. [max_scopes] is
    positive and captured by the enclosing owner. The complete native origin
    also fits 256 KiB, depth 64, 8192 fields and 16384 nodes; enclosing Factory
    depth and document bounds remain stricter. *)
val associated : max_scopes:int -> Scope.t list -> (t, Agent_protocol.Error.t) result

val scopes : t -> Scope.t list
val shape : Document_schema.Shape.t
val to_json : t -> Jsonaf.t

(** Complete raw preflight with the supplied limits before recursive decoding.
    A zero scope bound accepts only Unrelated; negative bounds reject.
    Uses the same constructors as native admission. Compatible unknown fields
    are retained by the existing enclosing document carrier, not by this DATA. *)
val of_json
  :  max_scopes:int
  -> limits:Document_schema.Limits.t
  -> Jsonaf.t
  -> (t, Agent_protocol.Error.t) result
