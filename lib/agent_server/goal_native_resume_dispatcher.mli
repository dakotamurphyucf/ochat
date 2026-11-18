(** Finite driver in the existing per-entry scheduler fiber, after joined
    callback/runtime leases. Owns no queue, fiber, writer, detached execution or
    authority registry. One real accepted Invocation selection per pass; only the
    genuine Owner reservation can consume it. Restart never replays DATA. *)
type t

val create : factory:Session_factory.t -> t
val process_entry : t -> Session_registry.entry -> (unit, Agent_protocol.Error.t) result
val close : t -> unit
