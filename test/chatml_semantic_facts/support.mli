open Core
module P = Chatml_prepared_program
module Contract = Chatml.Chatml_compiler_contract
module Recorder = Chatml_typechecker.Semantic

val checkpoint : unit -> unit

val contract
  :  ?requirements:(string * Chatml.Chatml_builtin_spec.ty) list
  -> unit
  -> Contract.t

val input
  :  Contract.t
  -> ?interfaces:(string * string * (string * string) list) list
  -> (string * string * (string * string) list) list
  -> P.t

val limits
  :  ?facts:int
  -> ?edges:int
  -> ?labels:int
  -> ?work:int
  -> unit
  -> Recorder.Limits.t

val checked : Contract.t -> P.t -> Chatml_checked_graph.t
val facts : Contract.t -> P.t -> Recorder.Settled.t
