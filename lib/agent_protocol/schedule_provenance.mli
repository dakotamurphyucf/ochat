(** Immutable Schedule creator evidence. Historical absence and unavailable
    origin are distinct from affirmative [Unbound]. No live authority is stored. *)
type t [@@deriving equal, sexp]

(** Required persistence semantics shared by stored Schedule creator evidence and
    moderator queue occurrence metadata. Enclosing Store state/delta readers
    explicitly recognize this feature; it grants no execution permission. *)
val persistence_semantics : string

val create
  :  source:Run_source.t
  -> origin:Inference_origin.t option
  -> (t, Error.t) result

val source : t -> Run_source.t
val origin : t -> Inference_origin.t option
val validate : t -> (unit, Error.t) result
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
