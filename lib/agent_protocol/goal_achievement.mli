(** Achievement requests retain action receipt associations, never another Run
    lifecycle. Constructors validate evidence relationships, not host authority. *)
module Request : sig
  type t [@@deriving equal, sexp_of]

  val create
    :  episode:Goal_episode.t
    -> run:Run.t
    -> receipt:Run_receipt.t
    -> action:Run_action.t
    -> evidence:Goal_text.Reason.t
    -> (t, Error.t) result

  val episode : t -> Goal_episode.t
  val receipt : t -> Run_receipt.t
  val evidence : t -> Goal_text.Reason.t
  val requested_result : t -> Run_result_reference.t option
  val action : t -> Run_action.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

type t [@@deriving equal, sexp_of]

(** Requires actual matching Completed terminal evidence. An accepted Finish
    alone cannot construct achievement. The actor authenticates this evidence. *)
val create : Request.t -> run:Run.t -> receipt:Run_receipt.t -> (t, Error.t) result

(** Reconstitutes immutable proof by hashing canonical Completed(requested_result)
    against the exact terminal receipt. No caller authority is inferred. *)
val of_evidence : Request.t -> Goal_episode.Settlement.t -> (t, Error.t) result

val request : t -> Request.t
val receipt : t -> Run_receipt.t
val settlement : t -> Goal_episode.Settlement.t
val to_json : t -> Jsonaf.t

(** Decoding validates stored associations but grants no completion authority. *)
val of_json : Jsonaf.t -> (t, Error.t) result
