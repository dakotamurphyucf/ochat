open Core

(** Invocation-private immutable text pieces. Input slices retain original strings;
    concatenation and indentation do not build intermediate encoded strings. *)
type t

val text : Conversion_budget.t -> string -> t
val char : Conversion_budget.t -> char -> t
val repeat : Conversion_budget.t -> char -> int -> t
val concat : Conversion_budget.t -> t list -> t
val strip : Conversion_budget.t -> t -> t
val escape_markdown : Conversion_budget.t -> string -> t

(** Complete length measurement BEFORE full output buffer allocation. Emits exact
    measured slices through the same owner, with checked expansion and polling. *)
val to_string : Conversion_budget.t -> t -> string

val byte_length : t -> int
