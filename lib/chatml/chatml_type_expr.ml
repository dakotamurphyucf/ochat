open Core

module Name = struct
  type t =
    { modules : string list
    ; name : string
    }
  [@@deriving sexp]

  let local name = { modules = []; name }
  let to_string t = String.concat ~sep:"." (t.modules @ [ t.name ])
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
  | TEConstr of Name.t * t list
  | TERecord of (string * t) list
  | TEVariant of (string * t list) list
[@@deriving sexp]

module Declaration = struct
  type nonrec t =
    { name : string
    ; parameters : string list
    ; body : t
    }
  [@@deriving sexp]
end
