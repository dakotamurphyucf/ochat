open Core

(** Reviewed instruction bytes embedded at build time. Lookup performs no I/O,
    installed-directory search, authored override or runtime authority grant. *)
module Id : sig
  type t [@@deriving equal, compare, sexp_of]

  (** Exact ASCII [a-z][a-z0-9_-]*, at most64bytes. No case folding or aliases. *)
  val of_string : string -> t Or_error.t

  val to_string : t -> string
end

module Asset : sig
  type t

  val id : t -> Id.t
  val title : t -> string
  val contents : t -> string
  val sha256 : t -> string
  val byte_length : t -> int
end

module Error : sig
  type t = Unknown_id of Id.t [@@deriving equal, sexp_of]
end

(** The initial catalog contains [chatml]. Other identifiers, including [skills],
    reject until their actual instruction contents are reviewed and embedded.
    Captured document replay uses retained output and never repeats lookup. *)
val find : Id.t -> (Asset.t, Error.t) Result.t
