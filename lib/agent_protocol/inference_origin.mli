(** Immutable attribution evidence for an admitted invocation or Job. This is
    neither execution permission nor live Run ownership. Historical absence is
    represented by the enclosing optional field, never by [Unbound]. *)
type t =
  | Unbound
  | Run of Id.Run.t
[@@deriving equal, sexp]

val validate : t -> (unit, Error.t) result
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
