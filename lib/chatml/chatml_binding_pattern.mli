open! Core

(** Located irrefutable binders. A tuple has at least two children. Names and
    tuple positions retain their original byte ranges. Duplicate names are
    rejected by the checker across the entire binding or parameter list;
    repeated wildcards introduce no bindings. This representation cannot express
    literals, records, variants, or other refutable patterns. *)
type t =
  { value : kind
  ; span : Source.span
  }

and kind =
  | Name of string
  | Wildcard
  | Tuple of t Chatml_tuple.t
[@@deriving sexp]

(** Named leaves in depth-first, left-to-right order. Calls [checkpoint] for
    every visited node, including wildcards. Exceptions propagate unchanged;
    neither the tree nor callback is retained or mutated. *)
val binders : ?checkpoint:(unit -> unit) -> t -> (string * Source.span) list

(** Counts named leaves without allocating a binding list. Same polling and
    traversal order as [binders]; useful for admission before staging values. *)
val count : ?checkpoint:(unit -> unit) -> t -> int
