open Core

(** A compiler-owned cooperative checkpoint. Sources, sites and checked type
    annotations never retain it beyond the compilation that supplies it. *)
type t = unit -> unit

(** [protect t ~f] supplies [f] with a callback that preserves the origin of
    exceptions raised by [t]. Parser/typechecker domain handlers cannot mistake
    them for ordinary compiler diagnostics. On leaving this boundary, restore
    the original exception and raw backtrace.

    Only exceptions from the supplied checkpoint are tagged. Other exceptions
    from [f], including interpreter/control exceptions, propagate untouched.
    The protected callback must not escape [f]'s dynamic extent. No global state
    or mutable current-origin field is used. Nested protection remains safe. *)
val protect : t -> f:(t -> 'a) -> 'a
