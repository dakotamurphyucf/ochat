(** Fiber-safe bounded two-lane mailbox. Priority messages are always removed
    before normal messages already present. *)

type priority =
  | Priority
  | Normal

type 'a t

val create : capacity:int -> 'a t
val push : 'a t -> priority:priority -> 'a -> (unit, Agent_protocol.Error.t) result
val try_push : 'a t -> priority:priority -> 'a -> bool

(** [pop t] waits cancellably for the next item. Cancelling an empty wait
    neither closes the mailbox nor consumes a later item. *)
val pop : 'a t -> 'a option

val close : 'a t -> unit
val length : 'a t -> int
