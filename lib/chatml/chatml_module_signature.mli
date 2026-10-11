open Core

module Requirement : sig
  (** Located source syntax for a signature constraint. Parsing preserves the
      name; interface checking admits only supported constraints (currently
      [equality]) on parameters quantified by that value specification. *)
  type t =
    { name : string
    ; parameter : string
    ; span : Source.span
    }
  [@@deriving sexp]
end

(** A source-level transparent module interface, shared by companion [.chatmli]
    files and inline signatures. Items remain in declaration order. These are
    unchecked syntax: parsing does not establish namespace uniqueness, type
    validity, universal value claims or compatibility with an implementation.
    All spans are byte ranges in the original interface source. *)
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
