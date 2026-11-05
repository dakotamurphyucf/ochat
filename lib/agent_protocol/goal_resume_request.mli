(** Immutable explicit operator-decision DATA for the current nonterminal Goal
    episode. It grants no Run, source, constructor or continuation authority and
    never describes a new Run admission. The actual Actor authenticates the
    retained physical binding and both original authorizations separately. *)
type t [@@deriving equal, sexp_of]

(** All counters are nonnegative, [source] is validated and its generation must
    match [generation]. IDs and key use the same validation as their protocol
    constructors. These request fences remain exact across source preparation;
    later acknowledged bookkeeping does not rewrite the original request. *)
val create
  :  session_id:Id.Session.t
  -> generation:int
  -> goal_id:Id.Goal.t
  -> run_id:Id.Run.t
  -> source:Run_source.t
  -> expected_goal_revision:int64
  -> expected_catalog_revision:int64
  -> expected_run_revision:int64
  -> key:Idempotency_key.t
  -> (t, Error.t) result

val session_id : t -> Id.Session.t
val generation : t -> int
val goal_id : t -> Id.Goal.t
val run_id : t -> Id.Run.t
val source : t -> Run_source.t
val expected_goal_revision : t -> int64
val expected_catalog_revision : t -> int64
val expected_run_revision : t -> int64
val key : t -> Idempotency_key.t

(** Domain-separated SHA-256 of the complete canonical request. This is request
    identity only; no caller principal, receipt or live authority is inferred. *)
val digest : t -> (string, Error.t) result

(** Bounded complete immutable request; decoding calls the same constructor. *)
val to_json : t -> Jsonaf.t

val of_json : Jsonaf.t -> (t, Error.t) result
