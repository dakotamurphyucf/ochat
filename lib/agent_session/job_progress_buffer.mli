(** Actor-owned bounded accumulation. No persistence, callbacks, locks or effects.
    Oversized/invalid updates and updates beyond the attempt budget are dropped. *)
type t

val create : unit -> t
val valid : Ochat_function.Progress.t -> bool
val update : t -> Ochat_function.Progress.t -> unit
val snapshot : t -> Agent_protocol.Job_progress.t option
