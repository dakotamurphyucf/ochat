open Core

(** A compiler-owned cooperative checkpoint. Sources, sites and checked type
    annotations never retain it beyond the compilation that supplies it. *)
type t = unit -> unit

(** [protect t ~f] supplies [f] with a callback that preserves the origin of
    exceptions raised by [t]. Parser/typechecker domain handlers cannot mistake
    them for ordinary compiler diagnostics. On leaving this boundary, restore
    the original exception and raw backtrace.

    [is_control_exception] defaults to false. A runtime may select its own control
    exceptions to propagate immediately, with their original raw backtrace,
    without tagging them across a scheduling boundary. The classifier must be
    total, non-yielding and non-raising; it must not select compiler-domain errors
    that require origin protection. All unselected checkpoint exceptions retain
    the generative tag and original-identity restoration above. This pure module
    neither depends on a runtime nor classifies its exceptions by name.

    Only exceptions from the supplied checkpoint are tagged. Other exceptions
    from [f], including interpreter/control exceptions, propagate untouched.
    The protected callback must not escape [f]'s dynamic extent. No global state
    or mutable current-origin field is used. Nested protection remains safe. *)
val protect : ?is_control_exception:(exn -> bool) -> t -> f:(t -> 'a) -> 'a
