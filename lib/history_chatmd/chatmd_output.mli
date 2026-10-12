open! Core

(** Internal output owner for the two actual ChatMD renderers. *)
module Error : sig
  type t =
    | Invalid_limits
    | Output_limit
    | Work_limit
    | Json_error of Document_schema.Error.t
  [@@deriving equal, sexp_of]
end

module Limits : sig
  type t

  val create
    :  max_bytes:int
    -> max_work_steps:int
    -> json_limits:Document_schema.Limits.t
    -> (t, Error.t) Result.t
end

type t

val render
  :  Limits.t
  -> checkpoint:(unit -> unit)
  -> f:(t -> unit)
  -> (string, Error.t) Result.t

val render_unbounded : f:(t -> unit) -> string
val text : t -> string -> unit
val quoted : t -> ?prefix:string -> ?suffix:string -> string -> unit
val json : t -> Jsonaf.t -> unit
val charge : t -> steps:int -> unit
