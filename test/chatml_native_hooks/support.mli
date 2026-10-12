open Core
module Contract = Chatml.Chatml_compiler_contract

val ok : ('a, 'b) Result.t -> 'a
val checkpoint : unit -> unit
val id : string -> Chatml_source.Id.t

val input
  :  Contract.t
  -> (string * string * (string * string) list) list
  -> Chatml_prepared_program.t

val script : string -> Chatmd_shell_spec.Chatmd_script_spec.t
val compiler_limits : Chatml_compilation.limits
val context : Chatml.Chatml_lang.value
val event : Chatml.Chatml_lang.value
