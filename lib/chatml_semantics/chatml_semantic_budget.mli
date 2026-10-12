open Core
module Limits = Chatml_semantic_limits
module Error = Chatml_semantic_error

(** Invocation-local admission only; never retained by publication. The fresh
    callback-origin boundary protects expected resource escapes from arbitrary
    caller exceptions, including domain-error constructor collisions. *)
type t

val create : Limits.t -> t

val run
  :  t
  -> checkpoint:(unit -> unit)
  -> f:(checkpoint:(unit -> unit) -> 'a)
  -> ('a, Error.t) Result.t

(** Checked-add and admit before allocation/copy. Every row/index/member edge
    and label bytes in final Complete/Partial publication use this same ledger. *)
val admit : t -> facts:int -> edges:int -> labels:int -> unit

val reject : Error.t -> 'a
val take_exn : ('a, Error.t) Result.t -> 'a
