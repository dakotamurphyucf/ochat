open! Core
module Error = Protocol_error

(** Bounded operator control of the host's installed dependency selection.
    These values contain no source authority, repository/pin overrides or native
    paths. Both methods require [configuration.admin]; construction grants none. *)
module Generation : sig
  type t [@@deriving compare, equal, sexp]

  (** Nonnegative generation of the actual persisted dependency-history index. *)
  val of_int : int -> (t, Error.t) Result.t

  val to_int : t -> int
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
  val to_json : t -> Jsonaf.t
end

module Status_request : sig
  (** The status method accepts exactly an empty JSON object. *)
  val of_json : Jsonaf.t -> (unit, Error.t) Result.t

  val to_json : unit -> Jsonaf.t
end

module Status : sig
  module History : sig
    type t =
      | Tracked of Generation.t
      | Missing
      | Invalid
    [@@deriving equal, sexp]
  end

  module Publication : sig
    type t =
      | Available
      | Busy
      | Uncertain
      | Closed
    [@@deriving equal, sexp]
  end

  type t =
    { history : History.t
    ; publication : Publication.t
    }
  [@@deriving equal, sexp]

  (** Last installed history and current admission state. Busy/Uncertain do not
      establish the generation currently on disk or authorize a new acquisition.
      A read neither repairs missing history nor retries publication. *)
  val to_json : t -> Jsonaf.t

  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end

module Update_request : sig
  type t [@@deriving equal, sexp]

  (** Admit 1..1024 distinct typed definition IDs, then canonicalize typed-ID
      ordering. Count admission precedes derived allocations. Unknown/disabled
      targets and expected-generation affinity remain actual host checks before
      acquisition. The bounded existing key identifies this exact mutation. *)
  val create
    :  definitions:Id.Prompt_definition.t list
    -> expected_generation:Generation.t
    -> idempotency_key:Idempotency_key.t
    -> (t, Error.t) Result.t

  val definitions : t -> Id.Prompt_definition.t list
  val expected_generation : t -> Generation.t
  val idempotency_key : t -> Idempotency_key.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end

module Update_result : sig
  module Target : sig
    type outcome =
      | Ready of Id.Prompt_revision.t
      | Unavailable of { reason : string }
    [@@deriving equal, sexp]

    type t =
      { definition_id : Id.Prompt_definition.t
      ; outcome : outcome
      }
    [@@deriving equal, sexp]
  end

  type t [@@deriving equal, sexp]

  (** Exactly one installed index transition: next generation is previous + 1,
      checked without overflow. Admit 1..1024 distinct target IDs and canonicalize
      their order. Unavailable reasons must be nonempty, NUL-free and <=4096 UTF-8
      bytes; the host supplies redacted diagnostics, never raw credentials/paths.
      Publication success does not claim all targets' fresh sources were usable:
      unavailable targets retain their previous successful dependency history. *)
  val create
    :  previous_generation:Generation.t
    -> generation:Generation.t
    -> targets:Target.t list
    -> (t, Error.t) Result.t

  val previous_generation : t -> Generation.t
  val generation : t -> Generation.t
  val targets : t -> Target.t list

  (** Validate the exact target set and previous generation against the request;
      clients reject a successful response that describes another mutation. *)
  val matches_request : t -> Update_request.t -> bool

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end
