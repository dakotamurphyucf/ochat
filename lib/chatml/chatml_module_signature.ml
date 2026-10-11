open Core

module Requirement = struct
  type t =
    { name : string
    ; parameter : string
    ; span : Source.span
    }
  [@@deriving sexp]
end

type t = item list

and item =
  { desc : desc
  ; span : Source.span
  }

and desc =
  | Type of Chatml_type_expr.Declaration.t
  | Value of
      { name : string
      ; annotation : Chatml_type_expr.t
      ; requirements : Requirement.t list
      }
  | Module of
      { name : string
      ; signature : t
      }
[@@deriving sexp]
