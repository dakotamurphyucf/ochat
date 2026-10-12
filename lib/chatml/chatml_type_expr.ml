open Core

module Name = struct
  type t =
    { modules : Chatml_syntax_name.t list
    ; name : Chatml_syntax_name.t
    }
  [@@deriving sexp]

  let local name = { modules = []; name = Chatml_syntax_name.unlocated name }

  let to_string t =
    String.concat ~sep:"." (List.map (t.modules @ [ t.name ]) ~f:Chatml_syntax_name.name)
  ;;
end

(** Type syntax with byte-based source spans. Parameters are lexical names,
    separate from inferred/generalized type variables. *)
type t =
  { desc : desc
  ; span : Source.span
  }

and desc =
  | TEName of Name.t
  | TEParameter of string
  | TEArrow of t * t
  | TEFunction of t list * t
  | TETuple of t Chatml_tuple.t
  | TEConstr of Name.t * t list
  | TERecord of (Chatml_syntax_name.t * t) list
  | TEVariant of (Chatml_syntax_name.t * t list) list
[@@deriving sexp]

module Declaration = struct
  type nonrec t =
    { name : Chatml_syntax_name.t
    ; parameters : Chatml_syntax_name.t list
    ; body : t
    }
  [@@deriving sexp]
end
