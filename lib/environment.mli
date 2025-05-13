(** Compiler environment - Map from string keys to values. *)

(* We expose the full interface of [Map.S] specialised to [string] keys, and
   then add a couple of convenience helpers that are frequently useful when
   dealing with compiler environments. *)

include Map.S with type key = string

(** [of_list bindings] builds an environment from [[bindings]].  Later bindings
    overwrite earlier ones, mimicking the behaviour of successive calls to
    [add]. *)
val of_list : (string * 'a) list -> 'a t

(** [merge lhs rhs] returns an environment that contains all the bindings of
    [lhs] and, for the keys that only exist in [rhs], the bindings from
    [rhs].   When a key is present in both maps, the binding from [lhs] is
    kept, i.e. [lhs] “wins”. *)
val merge : 'a t -> 'a t -> 'a t

