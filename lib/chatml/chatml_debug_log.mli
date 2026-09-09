open Core

type sink = string -> unit

val set_sink : sink -> unit
val clear_sink : unit -> unit
val emit_line : string -> unit
val emitf : ('a, unit, string, unit) format4 -> 'a

(** Render only when a sink is installed. Callers should bound previews
    independently of execution policy and apply their own sink failure policy. *)
val emit : (unit -> string) -> unit
