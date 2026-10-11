open! Core

val parse : string -> Chatml.Chatml_lang.program

val check
  :  string
  -> (Chatml_typechecker.checked_program, Chatml_typechecker.diagnostic) Result.t

val run : string -> Chatml.Chatml_lang.value
val require : string -> accepted:bool -> unit
val assert_checkpoints : f:((unit -> unit) -> unit) -> unit
