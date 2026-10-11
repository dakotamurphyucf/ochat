open Core

(** Type syntax with byte-based source spans. Parameters are lexical names,
    separate from inferred/generalized type variables. *)
type t =
  { desc : desc
  ; span : Source.span
  }

and desc =
  | TEName of string
  | TEParameter of string
  | TEArrow of t * t
  | TEFunction of t list * t
  | TEConstr of string * t list
  | TERecord of (string * t) list
  | TEVariant of (string * t list) list
[@@deriving sexp]

module Declaration : sig
  type nonrec t =
    { name : string
    ; parameters : string list
    ; body : t
    }
  [@@deriving sexp]
end
