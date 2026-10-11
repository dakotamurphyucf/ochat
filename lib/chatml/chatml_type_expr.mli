open Core

module Name : sig
  (** A source type name, optionally qualified by its lexical module path.
      Module components and the final type name retain their source spelling. *)
  type t =
    { modules : string list
    ; name : string
    }
  [@@deriving sexp]

  val local : string -> t
  val to_string : t -> string
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
