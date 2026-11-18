(** Lexical routing to the actual host-issued callback transaction. Neither a
    scope nor an integer receipt authorizes a Goal mutation. The Actor must verify
    the exact current owner, native result and issued subset at every consumer. *)
type handlers =
  { deliverable : int -> (bool, string) result
  ; accept : int -> (unit, string) result
  ; rollback : int -> unit
  ; prepare : int list -> (unit, string) result
  }

type t

val create : handlers -> t
val close : t -> unit
val is_open : t -> bool
val same : t -> t -> bool

(** Explicit lexical bindings also work on synchronous native worker domains.
    Unbound synchronous callers have no Goal scope. Binding and restoration do
    not issue or extend authority; the exact original scope lifetime still applies. *)
val with_scope : t -> (unit -> 'a) -> 'a

val current : unit -> t option
val handlers : t -> handlers

(** Collection exists only around the real interpreter's joined Tool.call.
    Native domain handoff carries this exact context and cannot prolong it. *)
type collection

val collect : t option -> (unit -> 'a) -> 'a * int list
val capture : unit -> collection option
val with_context : collection option -> (unit -> 'a) -> 'a
val owner : collection -> t
val emit : int -> (unit, string) result
