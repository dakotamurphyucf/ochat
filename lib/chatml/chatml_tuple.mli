open! Core

(** An ordered immutable product of at least two elements. Container operations
    preserve element order and share elements; they do not copy mutable children
    or retain callbacks. Runtime admission/resource limits remain with callers. *)
type 'a t = private
  { first : 'a
  ; second : 'a
  ; rest : 'a list
  }
[@@deriving sexp, compare]

val create : 'a -> 'a -> rest:'a list -> 'a t

(** [None] exactly for empty and singleton lists. *)
val of_list : 'a list -> 'a t option

val to_list : 'a t -> 'a list
val length : 'a t -> int
val map : 'a t -> f:('a -> 'b) -> 'b t
val iter : 'a t -> f:('a -> unit) -> unit
val fold : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc
