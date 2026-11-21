open! Core
module P = Agent_protocol
module A = Agent_session

(** Bounded, daemon-local capture reservations for actual submitted ChatMD.
    The owning Actor only signals a committed canonical occurrence. It never
    waits for another mailbox or performs asset I/O on this path. *)
type t

type reservation

val create : unit -> t

val reserve
  :  t
  -> handle:Agent_store.Session_store.Handle.t
  -> authorize:(A.Session_state.t -> (unit, P.Error.t) result)
  -> reservation option

(** The exact submitted row is known and still has not entered the mailbox.
    Large rows and abandoned or already armed reservations are ineligible. *)
val arm : reservation -> P.History.entry -> bool

val release : t -> reservation -> unit
val await : reservation -> P.Recovery_window.Citation.t option
val idle : A.Session_state.t -> bool

(** Delivered only from the owning writer after durable publication. Select
    the matching member of this accepted append, never a queued input, older
    occurrence, generated citation, or unrelated row with identical content.
    Yield the exact selected member only at a settled idle boundary so optional
    retention does not interrupt the adopting operation's canonical writes. *)
val committed
  :  t
  -> handle:Agent_store.Session_store.Handle.t
  -> A.Session_state.t
  -> P.Event.Durable.t list
  -> unit
