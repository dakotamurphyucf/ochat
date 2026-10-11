open Core

(** Temporary bounded pure-query execution. No callback or resource state is
    retained in the snapshot. Original checkpoint exceptions are protected from
    the query's own typed rejection escape. *)
module Limits : sig
  type t

  val create : max_results:int -> max_work_steps:int -> t Or_error.t
  val max_results : t -> int
end

module Error : sig
  type t =
    | Foreign_point
    | Foreign_symbol
    | Foreign_context
    | Unknown_source
    | Unknown_module
    | Invalid_location
    | Nonzero_point_range
    | Query_work_limit
  [@@deriving sexp_of]
end

val run
  :  Limits.t
  -> checkpoint:(unit -> unit)
  -> f:(checkpoint:(unit -> unit) -> 'a)
  -> ('a, Error.t) Result.t

val reject : Error.t -> 'a
val compare_string : string -> string -> checkpoint:(unit -> unit) -> int

val equal_source
  :  Chatml_source.Id.t
  -> Chatml_source.Id.t
  -> checkpoint:(unit -> unit)
  -> bool
