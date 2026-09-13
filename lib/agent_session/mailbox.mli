(** Fiber-safe bounded command mailbox plus a separate bounded transient lane.
    Priority and normal commands share [capacity] slots; transient updates have
    another [capacity] slots and cannot crowd out commands. Dequeue order is
    priority, normal, then transient. Transient callers should use [try_push]. *)

type priority =
  | Priority
  | Normal
  | Transient

type 'a t

val create : capacity:int -> 'a t
val push : 'a t -> priority:priority -> 'a -> (unit, Agent_protocol.Error.t) result
val try_push : 'a t -> priority:priority -> 'a -> bool

(** [pop t] waits cancellably for the next item. Cancelling an empty wait
    neither closes the mailbox nor consumes a later item. *)
val pop : 'a t -> 'a option

val close : 'a t -> unit
val length : 'a t -> int
