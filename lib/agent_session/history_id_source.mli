open! Core

(** Allocates history IDs only from blocks whose high-water mark has already
    been committed by the session actor. *)

type reservation =
  { first_sequence : int64
  ; reserved_through : int64
  }

type t

val create
  :  namespace:string
  -> block_size:int
  -> reserve:(count:int -> (reservation, Agent_protocol.Error.t) result)
  -> (t, Agent_protocol.Error.t) result

val allocate : t -> (History_entry.Id.t, Agent_protocol.Error.t) result

(** [discard_reserved t] abandons unused cached IDs after an administrative
    history replacement. The next allocation obtains a fresh actor reservation. *)
val discard_reserved : t -> unit

val remaining : t -> int
val namespace : t -> string
val next_reserved_sequence : t -> int
val validate : t -> History_entry.t list -> (unit, Agent_protocol.Error.t) result

(** Adapts the durable source for the shared response engine. Allocation waits
    for the actor-backed reservation callback before exposing an ID. *)
val as_history_entry_source : t -> History_entry.Id_source.t
