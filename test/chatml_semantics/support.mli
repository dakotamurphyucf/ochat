open Core
module A = Chatml_semantics
module Q = A.Snapshot
module S = Chatml_source
module P = Chatml_prepared_program
module Contract = Chatml.Chatml_compiler_contract

val checkpoint : unit -> unit

val contract
  :  ?requirements:(string * Chatml.Chatml_builtin_spec.ty) list
  -> unit
  -> Contract.t

val input
  :  Contract.t
  -> ?environment:string
  -> ?interfaces:(string * string * (string * string) list) list
  -> (string * string * (string * string) list) list
  -> P.t

val limits
  :  ?facts:int
  -> ?edges:int
  -> ?labels:int
  -> ?work:int
  -> ?input_bytes:int
  -> ?nodes:int
  -> unit
  -> A.Limits.t

val analyze
  :  ?limits:A.Limits.t
  -> ?checkpoint:(unit -> unit)
  -> Contract.t
  -> P.t
  -> (A.t, A.Error.t) Result.t

val complete : Contract.t -> P.t -> Q.t
val source : P.t -> string -> S.t
val point : Q.t -> P.t -> string -> int -> Q.Point.t
val references : Q.t -> Q.Point.t -> Q.Reference.t list
val definition : Q.t -> P.t -> string -> int -> Q.Symbol.t
val context : Q.t -> Q.Point.t -> Q.Context.t
val query_limits : ?results:int -> ?work:int -> unit -> Q.Query_limits.t
val names : Q.t -> Q.Binding.t list -> string list
val candidates : Q.t -> Q.Context.t -> Q.Namespace.t -> Q.Binding.t list
val node : Q.t -> A.Frozen.Id.t -> A.Frozen.Node.t
val dereference : Q.t -> A.Frozen.Id.t -> A.Frozen.Id.t
