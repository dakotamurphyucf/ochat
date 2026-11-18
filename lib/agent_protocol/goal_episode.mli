(** Immutable reference to an actual Workflow admission. Stored values convey no
    authority; the actor authenticates the supplied Run and receipt. *)
type t [@@deriving equal, sexp_of]

val create : run:Run.t -> admission:Run_receipt.t -> (t, Error.t) result
val admission : t -> Run_receipt.t
val session : t -> Session_ref.t
val matches_run : t -> Run.t -> bool
val validate_terminal : t -> run:Run.t -> receipt:Run_receipt.t -> (unit, Error.t) result
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result

(** Action and Terminal receipts strictly advance the original Admission Run
    revision; an immediate Terminal may share its Action revision.

    Immutable bounded terminal evidence for a retired episode. This is evidence
    retained from one terminal commit, not a writable copy of Run lifecycle. *)
type episode = t [@@deriving equal, sexp_of]

module Settlement : sig
  type t [@@deriving equal, sexp_of]

  val create : episode -> run:Run.t -> receipt:Run_receipt.t -> (t, Error.t) result
  val episode : t -> episode
  val receipt : t -> Run_receipt.t
  val to_json : t -> Jsonaf.t

  (** Stores only an exact terminal receipt association, never an outcome or
      achievement classification. Decoding checks episode/source/revisions; the
      actor authenticates the receipt. [create] additionally checks its digest
      against the supplied actual terminal Run. *)
  val of_json : Jsonaf.t -> (t, Error.t) result
end
