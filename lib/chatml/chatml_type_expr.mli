open Core

module Name : sig
  (** A source type name, optionally qualified by its lexical module path.
      Each component retains its source spelling and optional exact parser site;
      qualified occurrence identity does not depend on a shared owner span. *)
  type t =
    { modules : Chatml_syntax_name.t list
    ; name : Chatml_syntax_name.t
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
  | TERecord of (Chatml_syntax_name.t * t) list
  | TEVariant of (Chatml_syntax_name.t * t list) list
[@@deriving sexp]

module Declaration : sig
  type nonrec t =
    { name : Chatml_syntax_name.t
    ; parameters : Chatml_syntax_name.t list
    ; body : t
    }
  [@@deriving sexp]
end
