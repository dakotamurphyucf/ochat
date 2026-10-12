open Core

module Requirement = struct
  type t =
    { name : Chatml_syntax_name.t
    ; parameter : Chatml_syntax_name.t
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
      { name : Chatml_syntax_name.t
      ; annotation : Chatml_type_expr.t
      ; requirements : Requirement.t list
      }
  | Module of
      { name : Chatml_syntax_name.t
      ; signature : t
      }
[@@deriving sexp]
