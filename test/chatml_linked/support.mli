open Core
module P = Chatml_prepared_program
module Contract = Chatml.Chatml_compiler_contract

val native_contract
  :  ?requirements:(string * Chatml.Chatml_builtin_spec.ty) list
  -> unit
  -> Contract.t

val input
  :  Contract.t
  -> ?interfaces:(string * string * (string * string) list) list
  -> (string * string * (string * string) list) list
  -> P.t

val compiled : Contract.t -> P.t -> Chatml_linked_program.t
val paths : P.Module_id.t list -> string list
val owner : Chatml_diagnostic.t -> string
