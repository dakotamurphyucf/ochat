(** Cancellable subprocess output drainage, independent of consumers and waiters. *)
type t

val start : sw:Eio.Switch.t -> (unit -> unit) -> t
val finished : t -> bool
val interrupted : t -> bool
val await : t -> unit

(** Cancel only this reader; completion is resolved even during owner cancellation. *)
val stop : t -> unit

(** Allow 500ms for EOF, then cancel incomplete readers and allow 500ms for cleanup.
    Incomplete capture is observable through [interrupted]. *)
val drain : clock:_ Eio.Time.clock -> t list -> unit
