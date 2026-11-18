(** DATA claim only: [create] does not observe a canonical ACK. The enclosing
    update checks actual before/after canonical evidence; runtime owns real ACK. *)
type t [@@deriving equal, sexp_of]

val create : Goal_descendant_pending.t -> t
val pending : t -> Goal_descendant_pending.t
val shape : Document_schema.Shape.t
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
