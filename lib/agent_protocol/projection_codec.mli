(** Shared admission boundary for protocol 2 transcript envelopes. *)
val limits : Document_schema.Limits.t

val validate : Jsonaf.t -> (unit, Error.t) result
val string_result : ('a, string) result -> ('a, Error.t) result
val optional : string -> 'a option -> ('a -> Jsonaf.t) -> (string * Jsonaf.t) list
