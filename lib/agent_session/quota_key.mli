(** Root-agent prompt quota key within a workspace conflict domain. *)

type t =
  { conflict_domain : string
  ; prompt_id : Agent_protocol.Id.Prompt_definition.t
  }
[@@deriving compare, sexp]
