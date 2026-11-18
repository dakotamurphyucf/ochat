open! Core

(** One physical policy resource's synchronous constructor graph. This is factual
    cleanup custody, never an inference allocator or initialization authority. *)
type slot

type t

module Failure : sig
  type t =
    | Protocol of Agent_protocol.Error.t
    | Raised of exn * Stdlib.Printexc.raw_backtrace

  val capture : (unit -> ('a, Agent_protocol.Error.t) result) -> ('a, t) result
  val resume : ('a, t) result -> ('a, Agent_protocol.Error.t) result
end

val create : unit -> slot
val pending : slot -> bool

(** Reserve before the actual graph creation's first yield. Only one actual
    constructor is possible on the resource. The Cell validates its Lease first. *)
val reserve : slot -> (t, Agent_protocol.Error.t) result

(** Immediate nonyielding transfer of the cancellation-protected creation result.
    No source/Lease check can discard an already issued graph. Wrong token/domain
    is an invariant error; another slot is never mutated. *)
val allocated : t -> Graph_tracking.t -> unit

val inference : t -> Agent_session.Runtime_builder.Pending_legacy_resume.Inference.t

(** Only after graph creation returned failure, with no future lexical force.
    Never clears a graph that was actually allocated. *)
val abandon_unallocated : t -> unit

(** The elected callback has actually exited; no ACK or graph completion implied. *)
val callback_exited : t -> unit

(** After callback exit: same graph seal/finish, concurrent attempt joins, failure
    retained. Positive finish clears only this exact owner. No physical releases. *)
val finish : t -> (unit, Agent_protocol.Error.t) result

(** Existing outside-lock resource cleanup after actual source/callback joins.
    A still-running callback or creation cannot be force-finished. *)
val close_pending : slot -> (unit, Agent_protocol.Error.t) result
