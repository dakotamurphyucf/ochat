open! Core
module L = Chatml.Chatml_lang

val parse : string -> L.program
val resolved : string -> L.resolved_program
val run : string -> L.value
val require : string -> accepted:bool -> unit
val assert_checkpoints : f:((unit -> unit) -> unit) -> unit
