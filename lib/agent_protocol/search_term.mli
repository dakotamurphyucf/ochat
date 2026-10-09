(** A nonempty UTF-8 literal of at most 256 bytes. ASCII letters match without
    regard to case; every other UTF-8 byte matches exactly. No normalization,
    regular expressions, tokenization or locale-sensitive folding. *)
type t [@@deriving equal, sexp_of]

val create : string -> (t, Error.t) result
val text : t -> string
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
