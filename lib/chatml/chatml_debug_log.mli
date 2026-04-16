open Core

type sink = string -> unit

val set_sink : sink -> unit
val clear_sink : unit -> unit
val emit_line : string -> unit
val emitf : ('a, unit, string, unit) format4 -> 'a
