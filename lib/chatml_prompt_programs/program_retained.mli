open Core
module Captured = Chatml_source
module Inventory = Program_inventory

module Program : sig
  type t

  type input =
    | Provided
    | Selected of Captured.t

  val key : t -> string
  val binding : t -> Program_binding.t
  val compiler_contract : t -> Captured.Digest.t
  val input : t -> input
end

type t

module Error : sig
  type t =
    | Invalid_key
    | Invalid_work_limit
    | Work_limit_exceeded
    | Missing_key of string
    | Ambiguous_key of string
    | Provided_key of string
    | Invalid_inventory of string
end

(** Whole-call work bound; poll each metadata/source visit and string chunk.
    Input was admitted by Program_document. Validate exact revision membership
    for Selected sources; return no partial projection on error/cancellation. *)
val create
  :  Inventory.t
  -> max_work_steps:int
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

val programs : t -> Program.t list

val selected_lock
  :  t
  -> key:string
  -> max_work_steps:int
  -> checkpoint:(unit -> unit)
  -> (Captured.t, Error.t) Result.t
