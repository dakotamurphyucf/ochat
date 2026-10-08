(** Child history helpers. Invocation_id is an allocator/UI namespace, never a
    globally unique inference attempt identity. Actual attempts use the explicit
    host Neutral_turn.Identity port. *)
module Invocation_id : sig
  type t

  val create : unit -> t
  val to_string : t -> string
end

val allocator : parent_namespace:string -> Invocation_id.t -> History_entry.Allocator.t

val history_entries
  :  allocator:History_entry.Allocator.t
  -> history:History_entry.t list
  -> arguments:string
  -> call_id:string
  -> History_entry.t list
