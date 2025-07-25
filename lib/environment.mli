(** String-indexed environments.

    This module is a thin wrapper around {!Stdlib.Map} specialised to
    [string] keys.  It exposes the full {!Stdlib.Map.S} interface under the
    same types and adds a couple of helpers that are commonly needed when
    manipulating compiler-style environments.  Unless stated otherwise, all
    complexity guarantees and semantics are identical to those of the
    underlying {!Stdlib.Map} implementation. *)

(* We re-export the whole {!Stdlib.Map.S} signature specialised to strings so
   that users can rely on the familiar interface. *)

include Map.S with type key = string

(** [of_list bindings] builds an environment initialised with the bindings in
    [bindings].

    If the list contains several pairs with the same key, the *last* one wins
    – i.e. the resulting environment is the same as if the pairs were folded
    from left to right with {!add}.

    Example creating an environment with two bindings:
    {[ let env = of_list [ "x", 1; "y", 2 ] in
       find_opt "x" env = Some 1 ]} *)
val of_list : (string * 'a) list -> 'a t

(** [merge lhs rhs] is the left-biased union of two environments.

    All bindings of [lhs] are kept.  For every key that is present *only* in
    [rhs], the corresponding binding is inserted.  If a key exists in both
    maps, the value from [lhs] is retained – in other words, [lhs] “wins”.

    Example keeping the value from the left map:
    {[
      let lhs = of_list [ "x", 1 ] in
      let rhs = of_list [ "x", 0; "y", 2 ] in
      merge lhs rhs |> find_opt "x" = Some 1  (* value from [lhs] *)
    ]} *)
val merge : 'a t -> 'a t -> 'a t
