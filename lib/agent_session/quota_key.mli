(** Root-agent prompt quota key within a workspace conflict domain. *)

type t =
  { conflict_domain : string
  ; prompt_id : Agent_protocol.Id.Prompt_definition.t
  }
[@@deriving compare, sexp]

val to_jsonaf : t -> Jsonaf.t
val of_jsonaf : Jsonaf.t -> (t, Agent_protocol.Error.t) result
val shape : Document_schema.Shape.t
