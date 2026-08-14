(** A provider stream event correlated with its eventual canonical history
    occurrence. [source] scopes nested response aliases and is not itself
    canonical identity. *)
type t =
  { entry_id : History_entry.Id.t
  ; source : string option
  ; event : Openai.Responses.Response_stream.t
  }

module Registry : sig
  type t

  val create : allocator:History_entry.Allocator.t -> t
  val create_scope : t -> int

  val observe
    :  t
    -> scope:int
    -> source:string option
    -> Openai.Responses.Response_stream.t
    -> History_entry.Id.t option

  val tool_output
    :  t
    -> scope:int
    -> source:string option
    -> call_id:string
    -> History_entry.Id.t

  val find_item
    :  t
    -> scope:int
    -> source:string option
    -> Openai.Responses.Item.t
    -> History_entry.Id.t option
end

(** [observe registry ~source event] idempotently correlates [event]. Events
    without a canonical item identity return [None]. Conflicting aliases
    raise rather than overwrite an existing correlation. *)
val observe
  :  Registry.t
  -> scope:int
  -> source:string option
  -> Openai.Responses.Response_stream.t
  -> t option
